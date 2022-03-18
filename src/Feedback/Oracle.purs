module Feedback.Oracle where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Array as Array
import Data.Identity (Identity)
import Data.Lens (Lens', set, view)
import Data.Lens.Record (prop)
import Data.Newtype (unwrap)
import Data.Tuple (fst, snd)
import Feedback.FullGraph (FullGraph)
import Feedback.Types (Acc, Res, Trigger, World, TimeAndNote, unTriggerAudio)
import Type.Proxy (Proxy(..))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Lib.Cofree (cofreeTraversal_)
import WAGS.Run (RunAudio, RunEngine, TriggeredScene(..))

doPlays :: forall proof. Number -> Acc -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Acc
doPlays time acc = do
  let split = Array.span (\tp -> tp.starts - time < 0.15) acc.stagedAudio
  triggers <- cofreeTraversal_
    ( \tp a -> do
        unTriggerAudio a { note: tp.note, timeOffset: max 0.0 (tp.starts - time) }
    )
    split.init
    acc.triggers
  pure (acc { stagedAudio = split.rest, triggers = triggers })

modifyAcc :: Lens' Acc (Cofree Identity TimeAndNote) -> Number -> Number -> Acc -> Acc
modifyAcc lnz time wdw acc =
  let
    cf = view lnz acc
    nxt = extract cf
  in
    if fst nxt < time + wdw then modifyAcc lnz time wdw
      ( set lnz (unwrap $ unwrapCofree cf)
          ( acc
              { stagedAudio = acc.stagedAudio <>
                  [ { starts: fst nxt
                    , note: snd nxt
                    }
                  ]
              }
          )
      )
    else acc

oracle
  :: forall proof
   . TriggeredScene Trigger World ()
  -> Acc
  -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Acc
oracle
  (TriggeredScene { time })
  a' =
  let
    a = modifyAcc (prop (Proxy :: _ "notes0")) time 0.1
      $ modifyAcc (prop (Proxy :: _ "notes1")) time 0.1
      $ modifyAcc (prop (Proxy :: _ "notes2")) time 0.1 a'
  in
    doPlays time a
