module Feedback.Types where

import Prelude

import Control.Comonad.Cofree (Cofree)
import Data.Identity (Identity)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Feedback.FullGraph (FullGraph)
import WAGS.Control.Indexed (IxWAG)
import WAGS.Run (RunAudio, RunEngine)

data Trigger = Thunk

type TimeAndNote = Number /\ Number

type VisualNote =
  { starts :: Number
  , keyMatch :: Maybe Number
  }

type AudioNote =
  { starts :: Number
  , note :: Number
  }

type World = {}

type Res = {}

newtype TriggerAudio = TriggerAudio
  ( forall proof
     . { note :: Number, timeOffset :: Number }
    -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Unit
  )

unTriggerAudio
  :: TriggerAudio
  -> forall proof
   . { note :: Number, timeOffset :: Number }
  -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Unit
unTriggerAudio (TriggerAudio ta) = ta

type Acc =
  { triggers :: Cofree Identity TriggerAudio
  , stagedAudio :: Array AudioNote
  , notes0 :: Cofree Identity TimeAndNote
  , notes1 :: Cofree Identity TimeAndNote
  , notes2 :: Cofree Identity TimeAndNote
  }