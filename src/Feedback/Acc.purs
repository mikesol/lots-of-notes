module Feedback.Acc where

import Prelude

import Control.Comonad.Cofree (Cofree, deferCofree)
import Control.Plus (empty)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.Lens (_1, over, traversed)
import Data.List (List(..), (:), (..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Typelevel.Num (class Lt, class Pred, D0, D32, d31, pred)
import Feedback.Types (Acc, TriggerAudio(..))
import Math (pow)
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange')
import WAGS.Create.Optionals (subgraphSingleSetter)

l2cf :: forall a. Number -> NonEmpty List (Tuple Number a) -> Cofree Identity (Tuple Number a)
l2cf n i = go 0.0 i
  where
  go x (a :| Nil) = deferCofree (\_ -> Tuple a $ Identity (go (x + n) (over (traversed <<< _1) (add (x + n)) i)))
  go x (a :| (b : c)) = deferCofree (\_ -> Tuple a $ Identity (go x (b :| c)))

seq0 :: NonEmpty List (Number /\ Number)
seq0 = (0.0 /\ 54.0) :| mapWithIndex (\i n -> toNumber (i + 1) * 0.05 /\ (toNumber n + 56.0)) (0 .. 30)

seq1 :: NonEmpty List (Number /\ Number)
seq1 = (0.0 /\ 54.0) :| mapWithIndex (\i n -> toNumber (i + 1) * 0.1 /\ (toNumber n + 56.0)) (0 .. 20)

seq2 :: NonEmpty List (Number /\ Number)
seq2 = (0.0 /\ 54.0) :| mapWithIndex (\i n -> toNumber (i + 1) * 0.2 /\ (toNumber n + 56.0)) (0 .. 15)

midiToCps :: Number -> Number
midiToCps i = 440.0 * (2.0 `pow` ((i - 69.0) / 12.0))


mfy :: forall f g. Functor f => Functor g => f (g Number) -> f (g Number)
mfy = (map <<< map) midiToCps

initialAcc :: Acc
initialAcc =
  { triggers
  , stagedAudio: empty
  , notes0: l2cf 1.65 (mfy seq0)
  , notes1: l2cf 1.75 (mfy seq1)
  , notes2: l2cf 3.3 (mfy seq2)
  }

class CofreeN2S (n :: Type) where
  cofreeN2S :: n -> Cofree Identity TriggerAudio

instance CofreeN2S D0 where
  cofreeN2S i = deferCofree
    ( \_ -> Tuple
        ( TriggerAudio
            ( \{ note, timeOffset } -> ichange' (Proxy :: _ "oscs")
                (subgraphSingleSetter i (Just (timeOffset /\ note)))
            )
        )
        (Identity triggers)
    )
else instance
  ( Lt n D32
  , Pred n n'
  , CofreeN2S n'
  ) =>
  CofreeN2S n where
  cofreeN2S i = deferCofree
    ( \_ -> Tuple
        ( TriggerAudio
            ( \{ note, timeOffset } ->
                ichange' (Proxy :: _ "oscs") (subgraphSingleSetter i (Just (timeOffset /\ note)))
            )
        )
        (Identity (cofreeN2S (pred i)))
    )

triggers :: Cofree Identity TriggerAudio
triggers = cofreeN2S d31
