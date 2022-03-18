module Feedback.FullGraph where

import Prelude

import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Num (D32)
import Data.Vec (Vec)
import WAGS.Graph.AudioUnit (Subgraph, TGain, TSpeaker, TSubgraph, TTriangleOsc)
import WAGS.Interpret (AsSubgraph)

type SubgraphSig = Subgraph
  ()
  ( AsSubgraph
      "buffy"
      ()
      (Maybe (Number /\ Number))
  )
  (Vec D32 (Maybe (Number /\ Number)))

type SubgraphGraph =
  ( buffy :: TGain /\ { sosc :: Unit }
  , sosc :: TTriangleOsc /\ {}
  )

type FullGraph =
  ( speaker :: TSpeaker /\ { mainFader :: Unit }
  , mainFader :: TGain /\ { oscs :: Unit }
  , oscs ::
      TSubgraph D32 "buffy"
        ()
        (Maybe (Number /\ Number)) /\ {}
  )
