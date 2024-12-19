module Protag.Scene where

import Prelude

import Control.Monad.State (get)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Lens ((%=), (.=))
import Data.Maybe (fromMaybe')
import Data.String as String
import Effect.Class.Console as Console
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Protag.Common (SceneComponent, SceneIndex(..), makeSceneComponent)
import Protag.Utility (bug, prop)

getSceneComponent :: SceneIndex -> SceneComponent
getSceneComponent = case _ of
  MenuSceneIndex -> menu_component
  ExampleSceneIndex -> example_component
  IntroSceneIndex -> intro_component

--------------------------------------------------------------------------------

menu_component :: SceneComponent
menu_component = makeSceneComponent
  { initialState: {}
  , initialize: do
      Console.log "[menu.initialize]"
  , render: \{} ->
      HH.div [ HP.style "box-shadow: 0 0 0 1px black; padding: 0.5em; display: flex; flex-direction: column; gap: 0.5em;" ]
        [ HH.div [ HP.style "font-size: 2em;" ] [ HH.text "menu" ]
        , HH.div []
            [ HH.button
                [ HE.onClick $ const do
                    prop @"scene_index" .= IntroSceneIndex # lift
                ]
                [ HH.text "start" ]
            ]
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

intro_component :: SceneComponent
intro_component = makeSceneComponent
  { initialState:
      { dialogue_index: 0 }
  , initialize: do
      Console.log "[intro.initialize]"
  , render: \state ->
      HH.div [ HP.style "box-shadow: 0 0 0 1px black; padding: 0.5em; display: flex; flex-direction: column; gap: 0.5em;" ]
        [ HH.div [ HP.style "font-size: 2em;" ]
            [ HH.text "intro" ]
        , HH.div [ HP.style "display: flex; flex-direction: row; gap: 0.5em;" ]
            [ HH.div []
                [ HH.button
                    [ HE.onClick $ const do
                        { dialogue_index } <- get
                        if dialogue_index >= (dialogue # Array.length) - 1 then
                          prop @"scene_index" .= ExampleSceneIndex # lift
                        else do
                          prop @"dialogue_index" %= (_ + 1)
                    ]
                    [ HH.text "next" ]
                ]
            , HH.div []
                [ HH.text $ show (state.dialogue_index + 1) <> " / " <> show (dialogue # Array.length) ]
            ]
        , HH.div [ HP.style "padding: 0.5em;" ]
            [ HH.text (dialogue Array.!! state.dialogue_index # fromMaybe' \_ -> bug "impossible") ]
        ]
  }
  where
  dialogue =
    """
In the fractured skies of Aetheris, a world suspended between shimmering oceans of liquid light and an endless void, floating islands drift along invisible currents of arcane energy. Here, civilizations thrive in harmony with colossal skybeasts, whose bioluminescent scales power the ever-glowing spires of their cities. The air hums with the ancient songs of the Eyrlock, immortal stewards of the ether, who guard the secrets of the Horizon Gates—mystical portals said to lead to realms where time and space unravel. But as whispers of a dying light spread across the skies, an uncharted island emerges, carrying the promise of salvation—or the undoing of all that soars.
  """ # String.trim # String.split (String.Pattern ". ")

--------------------------------------------------------------------------------

example_component :: SceneComponent
example_component = makeSceneComponent
  { initialState:
      { counter: 0 }
  , initialize: do
      Console.log "[example.initialize]"
  , render: \state ->
      HH.div [ HP.style "box-shadow: 0 0 0 1px black; padding: 0.5em; display: flex; flex-direction: column; gap: 0.5em;" ]
        [ HH.div [ HP.style "font-size: 2em;" ]
            [ HH.text "example" ]
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
                    prop @"player" <<< prop @"health" %= (_ + 1) # lift
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
