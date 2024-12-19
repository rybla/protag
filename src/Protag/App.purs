module Protag.App where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Protag.Common (InputGameState, SceneIndex(..))

component = H.mkComponent { initialState, eval, render }
  where
  initialState _ =
    { game_state:
        { player_name: "Kellan Veylor"
        , player_health: 10
        , scene_index: MenuSceneIndex
        } :: InputGameState
    }

  eval = H.mkEval H.defaultEval

  render _ =
    HH.div
      []
      []
