module Protag.Game where

import Prelude

import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Protag.Common (GameAction(..), GameComponent)
import Protag.Scene (getSceneComponent)
import Type.Prelude (Proxy(..))

component :: GameComponent
component = H.mkComponent { initialState, eval, render }
  where
  initialState input =
    { player: input.game_state.player
    , scene_index: input.game_state.scene_index
    }

  eval = H.mkEval H.defaultEval
    { initialize = pure $ GameAction do
        Console.log "[game.initialize]"
    , handleAction = case _ of
        -- SceneAction ms -> do
        --   { scene_index } <- get
        --   s <- ms
        --   -- Note that this will make sure to _only_ set the state of a scene
        --   -- that has the same shape of state as the scene from which `ms` came
        --   -- from since scene types are determined by `scene_index` (since if 
        --   -- the `scene_index` of the current scene is different from how it 
        --   -- started before `ms` was run, the `H.tell` below won't do anything).
        --   H.tell (Proxy @"scene") (show scene_index) $ PutSceneState s
        GameAction mu -> mu
    }

  render state =
    HH.div
      [ HP.style "flex-grow: 1; display: flex; flex-direction: row;" ]
      [ HH.slot (Proxy @"scene") (show state.scene_index) (getSceneComponent state.scene_index) {} identity
      , HH.div
          [ HP.style "flex-shrink: 0; width: calc(200px - 2em); padding: 1em; background-color:rgba(196, 164, 132, 0.5); display: flex; flex-direction: column; gap: 0.5em;" ]
          [ HH.text $ "player = " <> show state.player
          , HH.div [] [ HH.text $ "... other properties ..." ]
          ]
      ]
