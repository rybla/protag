module Protag.Scene where

import Prelude
import Protag.Common

import Control.Monad.Trans.Class (lift)
import Data.Lens ((%=), (.=))
import Effect.Class.Console as Console
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Protag.Utility (prop)

getSceneComponent :: SceneIndex -> SceneComponent
getSceneComponent = case _ of
  MenuSceneIndex -> menu_component
  ExampleSceneIndex -> example_component

--------------------------------------------------------------------------------

menu_component :: SceneComponent
menu_component = makeSceneComponent
  { initialState: {}
  , initialize: do
      Console.log "[menu.initialize]"
  , render: \{} ->
      HH.div []
        [ HH.div [] [ HH.h2 [] [ HH.text "menu" ] ]
        , HH.div []
            [ HH.button
                [ HE.onClick $ const do
                    prop @"scene_index" .= ExampleSceneIndex # lift
                ]
                [ HH.text "example scene" ]
            ]
        ]
  }

--------------------------------------------------------------------------------

example_component :: SceneComponent
example_component = makeSceneComponent
  { initialState:
      { counter: 0 }
  , initialize: do
      Console.log "[example.initialize]"
  , render: \state ->
      HH.div []
        [ HH.div [] [ HH.h2 [] [ HH.text "example" ] ]
        , HH.div []
            [ HH.button
                [ HE.onClick $ const do
                    prop @"scene_index" .= MenuSceneIndex # lift
                ]
                [ HH.text "menu" ]
            ]
        , HH.div []
            [ HH.button
                [ HE.onClick $ const do
                    prop @"player_health" %= (_ + 1) # lift
                ]
                [ HH.text "heal" ]
            ]
        , HH.div [] [ HH.text $ "counter = " <> show state.counter ]
        , HH.div []
            [ HH.button
                [ HE.onClick $ const do
                    prop @"counter" %= (_ + 1)
                ]
                [ HH.text "increment" ]
            ]
        ]
  }
