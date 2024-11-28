module Protag.Impl.Game where

import Prelude
import Protag.Common

import Control.Applicative (pure)
import Control.Monad.State.Class (put)
import Data.Lens ((%=))
import Data.Lens.Record (prop)
import Data.Variant as V
import Halogen (put)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Protag.Component.Widget as Component.Widget
import Type.Proxy (Proxy(..))

game_impl :: GameImpl
game_impl =
  { initialState:
      { title: "Protag"
      }
  , render: \state ->
      HH.div []
        [ HH.h1 [] [ HH.text state.title ]
        -- , HH.slot (Proxy @"widget") "current_scene" (Component.Widget.component scene1_impl) ?a ?a
        ]
  }

scene1_impl :: WidgetImpl {} { counter :: Int }
scene1_impl =
  { initialState: const { counter: 0 }
  , initialize: pure unit
  , render: \{ counter } ->
      HH.div
        [ HP.style "display: flex; flex-direction: column; gap: 1em;" ]
        [ HH.button
            [ HE.onClick (const (prop (Proxy @"counter") %= (_ + 1))) ]
            [ HH.text "increment" ]
        , HH.text $ "counter: " <> show counter
        ]
  }
