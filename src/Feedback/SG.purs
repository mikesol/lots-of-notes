module Feedback.SG where

import Prelude

import Control.Plus (empty)
import Data.Array.NonEmpty (fromNonEmpty)
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Vec (fill)
import Feedback.FullGraph (SubgraphGraph, SubgraphSig)
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange')
import WAGS.Control.Functions.Subgraph as SG
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0)
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Graph.Parameter (AudioEnvelope(..), AudioOnOff(..), _offOn)
import WAGS.Interpret (class AudioInterpret, AsSubgraph(..))
import WAGS.Patch (ipatch)

createFrameSub
  :: forall res audio engine
   . AudioInterpret audio engine
  => IxWAG audio engine Frame0 res () SubgraphGraph Unit
createFrameSub = ipatch
  { microphone: empty
  , mediaElement: empty
  , subgraphs: { wavs }
  , tumults: {}
  }

subFrameLoop
  :: forall res proof audio engine
   . AudioInterpret audio engine
  => Maybe (Number /\ Number)
  -> Unit
  -> IxWAG audio engine proof res SubgraphGraph SubgraphGraph Unit
subFrameLoop Nothing _ = pure unit
subFrameLoop (Just (timeOffset /\ note)) _ =
  ichange' (Proxy :: _ "sosc")
    { freq: note
    , onOff: AudioOnOff { onOff: _offOn, timeOffset }
    } *> ichange' (Proxy :: _ "buffy")
    ( AudioEnvelope
        { duration: 0.3
        , values: fromNonEmpty (0.0 :| [ 0.4, 0.2, 0.1, 0.0 ])
        , timeOffset
        }
    )

wavs :: SubgraphSig
wavs = CTOR.Subgraph
  { subgraphMaker: AsSubgraph
      ( const $ SG.istart (\_ -> createFrameSub) (SG.iloop subFrameLoop)
      )
  , envs: fill $ const Nothing
  }
