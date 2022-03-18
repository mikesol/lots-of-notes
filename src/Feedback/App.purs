module Feedback.App where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Feedback.InnerComponent as InnerComponent
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type State = {  }

data Action = Initialize

component :: forall query input output m. MonadEffect m => MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render: render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

type Slots = (canvas :: forall query. H.Slot query Void Unit)
_canvas = Proxy :: Proxy "canvas"

initialState :: forall input. input -> State
initialState _ = {  }

klz :: forall r a. Array String -> IProp (class :: String | r) a
klz = HP.classes <<< map ClassName

render :: forall m. MonadEffect m => MonadAff m => State -> H.ComponentHTML Action Slots m
render { } =
  HH.div [ klz [ "w-screen", "h-screen" ] ]
      [ HH.slot_ _canvas unit InnerComponent.component unit]

handleAction :: forall output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  Initialize -> pure unit
