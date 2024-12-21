module Protag.Scene where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Lens ((%=), (.=))
import Data.String as String
import Effect.Class.Console as Console
import Halogen.HTML as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Protag.Common (DialogueItem(..), SceneComponent, SceneIndex(..), makeSceneComponent, makeSceneComponent')
import Protag.Utility (prop)

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
      HH.div [ HP.style "box-shadow: 0 0 0 1px black inset; padding: 0.5em; display: flex; flex-direction: column; gap: 0.5em;" ]
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
intro_component = makeSceneComponent'
  { title: HH.div [] [ HH.text "Introduction" ]
  , initialState: {}
  , initialize: pure unit
  , background_image_src: "/assets/approaching_snowy_town.png"
  , dialogue: intro_dialogue
  }

intro_dialogue :: forall state slots. Array (DialogueItem state slots)
intro_dialogue =
  [ [ Inject_DialogueItem [ HH.button [ HP.classes [ H.ClassName "wispy-scroll" ] ] [ HH.text "go into the town, quick!" ] ] ]
  , String.trim >>> String.split (String.Pattern "\n") >>> map (String.trim >>> HH.text >>> Array.singleton >>> Inject_DialogueItem) $
      """
In the fractured skies of Aetheris, a world suspended between shimmering oceans of liquid light and an endless void, floating islands drift along invisible currents of arcane energy.
Here, civilizations thrive in harmony with colossal skybeasts, whose bioluminescent scales power the ever-glowing spires of their cities.
The air hums with the ancient songs of the Eyrlock, immortal stewards of the ether, who guard the secrets of the Horizon Gates—mystical portals said to lead to realms where time and space unravel.
But as whispers of a dying light spread across the skies, an uncharted island emerges, carrying the promise of salvation—or the undoing of all that soars.
  """

  ] # Array.fold

--------------------------------------------------------------------------------

example_component :: SceneComponent
example_component = makeSceneComponent
  { initialState:
      { counter: 0 }
  , initialize: do
      Console.log "[example.initialize]"
  , render: \state ->
      HH.div [ HP.style "box-shadow: 0 0 0 1px black inset; padding: 0.5em; display: flex; flex-direction: column; gap: 0.5em;" ]
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
