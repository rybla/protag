module Protag.App where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Protag.Common (InputGameState, SceneIndex(..))

component = H.mkComponent { initialState, eval, render }
  where
  initialState _ =
    { game_state:
        { player: { name: "Kellan Veylor", health: 10 }
        , scene_index: MenuSceneIndex
        } :: InputGameState
    }

  eval = H.mkEval H.defaultEval

  render _ =
    HH.div
      []
      []
