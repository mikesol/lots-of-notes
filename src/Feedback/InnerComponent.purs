module Feedback.InnerComponent where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Ref as Ref
import FRP.Event (makeEvent, subscribe)
import Feedback.Control (Action(..), State)
import Feedback.Engine (piece)
import Feedback.Oracle (oracle)
import Feedback.SG (wavs)
import Feedback.Setup (setup)
import Feedback.Types ( Res, Trigger(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import WAGS.Interpret (close, context, makeFFIAudioSnapshot)
import WAGS.Run (TriggeredRun, runNoLoop)
import Web.HTML (window)
import Web.HTML.Window (requestAnimationFrame)

component :: forall query input output m. MonadEffect m => MonadAff m =>  H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }

initialState :: forall input. input -> State
initialState _ =
  { unsubscribe: pure unit
  , audioCtx: Nothing
  }

classes :: forall r p. Array String -> HP.IProp (class :: String | r) p
classes = HP.classes <<< map H.ClassName

render :: forall m. State -> H.ComponentHTML Action () m
render st = HH.div [ classes [ "w-full", "h-full" ] ]
  [ HH.div [ classes [ "flex", "flex-row", "w-full", "h-full" ] ]
      [ HH.div [ classes [ "flex-grow" ] ] [ HH.div_ [] ]
      , HH.div [ classes [ "flex-grow-0", "flex", "flex-col" ] ]
          [ HH.div [ classes [ "flex-grow" ] ]
              case st.audioCtx of
                Nothing ->
                  [ HH.button
                      [ classes [ "text-2xl", "m-5", "bg-indigo-500", "p-3", "rounded-lg", "text-white", "hover:bg-indigo-400" ], HE.onClick \_ -> StartAudio ]
                      [ HH.text "Start audio" ]
                  ]
                Just _ ->
                  [ HH.button
                      [ classes [ "text-2xl", "m-5", "bg-indigo-500", "p-3", "rounded-lg", "text-white", "hover:bg-indigo-400" ], HE.onClick \_ -> StopAudio ]
                      [ HH.text "Stop audio" ]
                  ]
          ]
      , HH.div [ classes [ "flex-grow" ] ] []
      ]
  ]

handleAction :: forall output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction  = case _ of
  StartAudio -> do
    handleAction  StopAudio
    audioCtx <- H.liftEffect context
    ffiAudio <- H.liftEffect $ makeFFIAudioSnapshot audioCtx
    let
      animationFrameEvent = makeEvent \k -> do
        w <- window
        running <- Ref.new true
        let
          ff = void $ flip requestAnimationFrame w do
            r' <- Ref.read running
            when r' do
              -- Log.info  "running"
              k Thunk
              ff
        ff
        pure $ Ref.write false running
    unsubscribe <-
      H.liftEffect
        $ subscribe
            ( runNoLoop
                (animationFrameEvent)
                (pure {}                )
                {}
                ffiAudio
                (piece wavs setup oracle)
            )
            ( \(_ :: TriggeredRun Res ()) -> pure unit)
    H.modify_ _
      { unsubscribe = unsubscribe
      , audioCtx = Just audioCtx
      }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    H.liftEffect do
      for_ audioCtx close
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
