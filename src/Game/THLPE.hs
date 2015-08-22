{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE RecordWildCards            #-}

module Game.THLPE
  ( THLPE ()
  , GodAnswer (..)
  , GodType (..)
  , GodName (..)
  , GodQuestion ()
  , runTHLPE
  , askTo
  , ifIAsked
  , solution
  ) where

--------------------------------------------------------------------------------
import           Control.Monad.Trans.RWS (RWS, ask, runRWS, state, tell)
import           Data.Bool               (bool)
import           Data.List               (permutations)
import           Data.Monoid             (Sum, getSum, (<>))
import           System.Random           (StdGen, getStdRandom, mkStdGen,
                                          random)
import           Test.QuickCheck         (Arbitrary, arbitrary, elements)
--------------------------------------------------------------------------------

data GodAnswer
  = Da
  | Ja
  deriving (Show, Eq, Enum)

data GodType
  = TrueGod
  | FalseGod
  | RandomGod
  deriving (Show, Eq, Enum)

data GodName
  = GodA
  | GodB
  | GodC
  deriving (Show, Eq, Enum)

data THLPESetting
  = THLPESetting { _godA      :: GodType
                 , _godB      :: GodType
                 , _godC      :: GodType
                 , _translate :: Bool -> GodAnswer
                 }

instance Show THLPESetting where
  show (THLPESetting{..})
    = "THLPESetting { " <> show _godA <>
                   ", " <> show _godB <>
                   ", " <> show _godC <>
                   ", (\\case True  -> " <> show (_translate True) <>
                           "; False -> " <> show (_translate False) <>
                   ")}"

instance Arbitrary THLPESetting where
  arbitrary = do
    [a, b, c] <- elements $ permutations [TrueGod, FalseGod, RandomGod]
    tr        <- elements $ [bool Ja Da, bool Da Ja]
    return $ THLPESetting a b c tr

instance Arbitrary StdGen where
  arbitrary = mkStdGen <$> arbitrary

type GodM a = RWS THLPESetting (Sum Int) StdGen a

newtype THLPE a = THLPE { unTHLPE :: GodM a }
  deriving (Functor, Applicative, Monad)
newtype GodQuestion a = GodQuestion { unGodQuestion :: GodM a }
  deriving (Functor, Applicative, Monad)

godTypeI :: GodName -> GodM GodType
godTypeI GodA = _godA <$> ask
godTypeI GodB = _godB <$> ask
godTypeI GodC = _godC <$> ask

godType :: GodName -> GodQuestion GodType
godType = GodQuestion . godTypeI

runTHLPE :: StdGen -> THLPESetting -> THLPE (GodType, GodType, GodType) -> (Bool, (StdGen, Int))
runTHLPE gen set (THLPE r) =
  let (ans, s, w) = runRWS r set gen
  in  (ans == (_godA set, _godB set, _godC set), (s, getSum w))

runTHLPE' :: THLPESetting -> THLPE (GodType, GodType, GodType) -> IO (Bool, Int)
runTHLPE' s g = getStdRandom $ \gen -> case runTHLPE gen s g of (b, (g, i)) -> ((b, i), g)

askTo :: GodName -> GodQuestion Bool -> THLPE GodAnswer
askTo n (GodQuestion q) = THLPE $ do
  tell 1
  t <- godTypeI n
  translate <- _translate <$> ask
  translate <$> case t of
    TrueGod   -> q
    FalseGod  -> not <$> q
    RandomGod -> state random

ifIAsked :: GodName -> GodQuestion Bool -> GodQuestion GodAnswer
ifIAsked n = GodQuestion  . unTHLPE . askTo n

-- | https://en.wikipedia.org/wiki/The_Hardest_Logic_Puzzle_Ever#The_solution
solution :: THLPE (GodType, GodType, GodType)
solution = do
  -- Q1: Ask god B, "If I asked you 'Is A Random?', would you say
  -- ja?". If B answers ja, either B is Random (and is answering
  -- randomly), or B is not Random and the answer indicates that A is
  -- indeed Random. Either way, C is not Random. If B answers da,
  -- either B is Random (and is answering randomly), or B is not
  -- Random and the answer indicates that A is not Random. Either way,
  -- you know the identity of a god who is not Random.
  r1 <- askTo GodB $
          ifIAsked GodB (godType GodA <&> (== RandomGod)) <&> (== Ja)
  let notRandomGod = case r1 of Ja -> GodC
                                Da -> GodA

  -- Q2: Go to the god who was identified as not being Random by the
  -- previous question (either A or C), and ask him: "If I asked you
  -- 'Are you False?', would you say ja?". Since he is not Random, an
  -- answer of da indicates that he is True and an answer of ja
  -- indicates that he is False.
  r2 <- askTo notRandomGod $
          ifIAsked notRandomGod (godType notRandomGod <&> (== FalseGod)) <&> (== Ja)
  let notRandomGodType = case r2 of Ja -> FalseGod
                                    Da -> TrueGod

  -- Q3: Ask the same god the question: "If I asked you 'Is B
  -- Random?', would you say ja?". If the answer is ja, B is Random;
  -- if the answer is da, the god you have not yet spoken to is
  -- Random.
  r3 <- askTo notRandomGod $
          ifIAsked notRandomGod (godType GodB <&> (== RandomGod)) <&> (== Ja)
  let randomGod = case (r3, notRandomGod) of (Ja, _)    -> GodB
                                             (Da, GodA) -> GodC
                                             (Da, GodC) -> GodA

  -- The remaining god can be identified by elimination.
  let f n =  if | randomGod    == n -> RandomGod
                | notRandomGod == n -> notRandomGodType
                | otherwise         -> if notRandomGodType == TrueGod
                                       then FalseGod else TrueGod

  return $ (f GodA, f GodB, f GodC)

  where (<&>) = flip fmap

