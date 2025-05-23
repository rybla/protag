module Protag.Scene where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Lens ((%=), (.=))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class.Console as Console
import Halogen.HTML (HTML)
import Halogen.HTML as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Protag.Common (DialogueItem(..), SceneComponent, SceneIndex(..), SceneHTML, makeSceneComponent, makeSceneComponent')
import Protag.Utility (bug, prop)

getSceneComponent :: SceneIndex -> SceneComponent
getSceneComponent = case _ of
  MenuSceneIndex -> menu_component
  ExampleSceneIndex -> example_component
  IntroSceneIndex -> intro_component
  TownSceneIndex -> town_component
  MountainSceneIndex -> mountain_component

--------------------------------------------------------------------------------

menu_component :: SceneComponent
menu_component = makeSceneComponent
  { initialState: {}
  , initialize: do
      Console.log "[menu.initialize]"
  , render: \{} ->
      HH.div [ HP.style "box-shadow: 0 0 0 1px black inset; padding: 0.5em; display: flex; flex-direction: column; gap: 0.5em;" ]
        [ HH.div [ HP.style "font-size: 2em;" ]
            [ HH.text "menu" ]
        , HH.div []
            [ simple_button
                [ HH.text "start" ]
                do prop @"scene_index" .= IntroSceneIndex # lift
            ]
        , HH.div []
            [ simple_button
                [ HH.text "example scene" ]
                do prop @"scene_index" .= ExampleSceneIndex # lift
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
  , dialogue: Array.fold $
      [ [ Inject_DialogueItem
            [ simple_button
                [ HH.text "this button doesnt do anything" ]
                do pure unit
            ]
        ]
      , parse_text_dialogue_items $
          """
In the fractured skies of Aetheris, a world suspended between shimmering oceans of liquid light and an endless void, floating islands drift along invisible currents of arcane energy. In the fractured skies of Aetheris, a world suspended between shimmering oceans of liquid light and an endless void, floating islands drift along invisible currents of arcane energy. In the fractured skies of Aetheris, a world suspended between shimmering oceans of liquid light and an endless void, floating islands drift along invisible currents of arcane energy. In the fractured skies of Aetheris, a world suspended between shimmering oceans of liquid light and an endless void, floating islands drift along invisible currents of arcane energy. In the fractured skies of Aetheris, a world suspended between shimmering oceans of liquid light and an endless void, floating islands drift along invisible currents of arcane energy. In the fractured skies of Aetheris, a world suspended between shimmering oceans of liquid light and an endless void, floating islands drift along invisible currents of arcane energy. In the fractured skies of Aetheris, a world suspended between shimmering oceans of liquid light and an endless void, floating islands drift along invisible currents of arcane energy. In the fractured skies of Aetheris, a world suspended between shimmering oceans of liquid light and an endless void, floating islands drift along invisible currents of arcane energy.
Here, civilizations thrive in harmony with colossal skybeasts, whose bioluminescent scales power the ever-glowing spires of their cities."""
      , [ Inject_DialogueItem
            [ HH.span [ HP.style "display: block;" ]
                [ HH.text """The air hums with the ancient songs of the Eyrlock, immortal stewards of the ether, who guard the """
                , HH.span
                    [ HP.classes [ HH.ClassName "mystic-keyword" ]
                    , HP.style "display: inline;"
                    ]
                    [ HH.text "secret" ]
                , HH.text """s of the Horizon Gates—mystical portals said to lead to realms where time and space unravel."""
                ]
            ]
        ]
      , parse_text_dialogue_items $
          """
But as whispers of a dying light spread across the skies, an uncharted island emerges, carrying the promise of salvation—or the undoing of all that soars.
... blah blah blah ..."""
      , [ Inject_DialogueItem
            [ simple_button
                [ HH.text "go into the town" ]
                do lift $ prop @"scene_index" .= TownSceneIndex
            , simple_button
                [ HH.text "go out into the mountains" ]
                do lift $ prop @"scene_index" .= MountainSceneIndex
            ]
        ]
      ]
  }

town_component :: SceneComponent
town_component = makeSceneComponent'
  { title: HH.div [] [ HH.text "A Small Town in the Mountains" ]
  , initialState: {}
  , initialize: pure unit
  , background_image_src: "/assets/in_snowy_town.png"
  , dialogue: Array.fold
      [ parse_text_dialogue_items $
          """
The snow lies thick and unbroken upon the cobbled streets of the mountain town, muffling every footstep into silence.
Each breath is a pale ghost escaping into the crisp winter air, and the scent of woodsmoke curls from crooked chimneys, carrying with it the faint tang of pine and something sharper, something unplaceable.
The sun is a pale disk, low and weary in the sky, casting long blue shadows that seem to stretch out like skeletal fingers across the snowdrifts piled against stone walls.
The buildings huddle together, their steep roofs heavy with frost, their narrow windows glinting like wary eyes.
Occasionally, a curtain twitches as you pass, and you catch a glimpse of a face—there, and gone—leaving only a sense of fleeting unease.
Down an alley, a faint sound drifts through the stillness—a distant chime, like glass struck gently by metal.
The bell tower at the center of town looms high and dark against the white-washed sky, its ancient stones blackened by time and weather, but its clock face remains still, frozen at an hour no one speaks of aloud.
A few townsfolk shuffle past, bundled in furs, their heads bowed and eyes fixed on the ground, as though to meet a stranger’s gaze might invite an unspoken consequence.
The town feels like a held breath, a moment suspended on the edge of something vast and unnameable.
Whatever secret sleeps beneath the snow and shadowed eaves, it stirs faintly now, felt more than known—a presence just beneath the thin crust of ordinary winter silence."""
      ]
  }

mountain_component :: SceneComponent
mountain_component = makeSceneComponent'
  { title: HH.div [] [ HH.text "Among the Mountains" ]
  , initialState: {}
  , initialize: pure unit
  , background_image_src: "/assets/in_the_mountains.png"
  , dialogue: parse_text_dialogue_items $
      """
The mountain trail winds upward, a pale ribbon of packed snow threading through skeletal trees that creak faintly in the biting wind.
Frost clings to every branch like spun glass, glittering coldly beneath the thin winter sun.
Each step crunches softly, and the air carries an almost crystalline clarity, so sharp it feels like breathing in shards of ice.
Far below, the isolated town lies quiet and still, its rooftops dusted with white, its secrets tucked beneath a quilt of snow and shadow.
Above, the sky is vast and pale, washed out like old parchment, and the peaks rise jagged and unyielding, crowned with clouds that glow faintly gold at their edges.
But there’s something else here—something older than the stones beneath your boots or the brittle trees clawing at the sky.
It hums just at the edge of hearing, like a faint chorus carried on the wind.
A crow calls out in the distance, its voice hollow and knowing, before it takes flight, black wings cutting stark against the white expanse.
Occasionally, you catch a flicker of movement from the corner of your eye—a shimmer in the snowdrifts, a shadow gliding across the ridge above you where no figure stands.
The air feels heavy with a presence, as if the mountain itself were watching, breathing, waiting.
There are places along the trail where the snow seems untouched by time, where the silence deepens into something almost sacred.
You pause at an overlook, the world sprawling out before you in hushed grandeur, and the faintest scent of something herbal—sage, perhaps, or pine resin—drifts past, as though carried by unseen hands.
Whatever power hums beneath the skin of this place, it feels vast and patient, curled deep in the marrow of the mountain, watching and listening with ancient, half-lidded eyes."""
  }

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
            [ simple_button
                [ HH.text "menu" ]
                do prop @"scene_index" .= MenuSceneIndex # lift
            ]
        , HH.div []
            [ simple_button
                [ HH.text "heal" ]
                do prop @"player" <<< prop @"health" %= (_ + 1) # lift
            ]
        , HH.div [] [ HH.text $ "counter = " <> show state.counter ]
        , HH.div []
            [ simple_button
                [ HH.text "increment" ]
                do
                  prop @"counter" %= (_ + 1)
            ]
        ]
  }

--------------------------------------------------------------------------------
-- utilities
--------------------------------------------------------------------------------

parse_text_dialogue_items :: forall state slots. String -> Array (DialogueItem state slots)
parse_text_dialogue_items = String.trim >>> String.split (String.Pattern "\n") >>> map (String.trim >>> parse_richtext >>> Inject_DialogueItem)

parse_richtext :: forall state slots. String -> Array (SceneHTML state slots)
-- parse_richtext = bug ""
parse_richtext str =
  case str # splitAtPattern (String.Pattern start) of
    Nothing -> [ HH.text str ]
    Just (str0 /\ str') -> case str' # splitAtPattern (String.Pattern mid) of
      Nothing -> bug "error: missing mid"
      Just (str1 /\ str'') -> case str'' # splitAtPattern (String.Pattern end) of
        Nothing -> bug "error: missing end"
        -- str1 is label
        -- str2 is value
        Just (str2 /\ str''') -> [ HH.text str0, ?a ] <> parse_richtext str'''

  -- case str # String.indexOf (String.Pattern start) of
  --   Nothing -> [ HH.text str ]
  --   Just i0 -> 
  --     let str' = str # String.drop (i0 + String.length start ) in 
  --     case str' # String.indexOf (String.Pattern mid) of 
  --         Nothing -> bug "missing mid"
  --         Just i1 -> 
  --             let str'' = str' # String.take i1 
  --                 str''' = str'' # String.drop (i1  + String.length mid)
  --             in 
  --                 case str''' # String.indexOf (String.Pattern end)
  where
  start = "{{"
  mid = "|"
  end = "}}"

splitAtPattern :: String.Pattern -> String -> Maybe (String /\ String)
splitAtPattern (String.Pattern pat) str = do
  i <- str # String.indexOf (String.Pattern pat)
  let str0 = str # String.take i
  let str1 = str # String.drop (i + String.length pat)
  pure (str0 /\ str1)

simple_button :: forall w i. Array (HTML w i) -> i -> HTML w i
simple_button kids action =
  HH.button
    [ HP.classes [ H.ClassName "wispy-scroll" ]
    , HE.onClick $ const action
    ]
    kids
