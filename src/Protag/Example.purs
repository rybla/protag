module Protag.Example where

import Prelude

import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Protag.Common (InputGameState, SceneIndex(..))

input_game_state_1 :: InputGameState
input_game_state_1 =
  { player:
      { name: "Kellan Veylor"
      , health: 20
      , description: """Kellan Veylor is a rugged, enigmatic ranger who roams the shadowed forests and forgotten ruins of the realm, guided by an unyielding moral compass and a mysterious past."""
      , status: """Kellan Veylor is feeling normal."""
      , inventory: Map.fromFoldable
          [ "small sword" /\ 1
          , "gold coin" /\ 4
          ]
      }
  , scene_index: IntroSceneIndex
  }