module Protag.Game where

import Prelude

import Control.Monad.State (get)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Protag.Common (GameAction(..), GameComponent, SceneQuery(..))
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
        GameAction mu -> mu
        SceneAction ms -> do
          { scene_index } <- get
          s <- ms
          -- Note that this will make sure to _only_ set the state of a scene
          -- that has the same shape of state as the scene from which `ms` came
          -- from since scene types are determined by `scene_index` (since if 
          -- the `scene_index` of the current scene is different from how it 
          -- started before `ms` was run, the `H.tell` below won't do anything).
          H.tell (Proxy @"scene") (show scene_index) $ PutSceneState s
    }

  render state =
    HH.div
      [ HP.style "box-shadow: 0 0 0 1px black; padding: 0.5em; display: flex; flex-direction: column; gap: 0.5em;" ]
      [ HH.div
          [ HP.style "font-size: 2em;" ]
          [ HH.text "Protag" ]
      , HH.div
          [ HP.style "display: flex; flex-direction: row; gap: 0.5em;" ]
          [ HH.div
              [ HP.style "flex-grow: 1;" ]
              [ HH.slot (Proxy @"scene") (show state.scene_index) (getSceneComponent state.scene_index) {} identity ]
          , HH.div
              [ HP.style "flex-shrink: 0; width: 400px; box-shadow: 0 0 0 1px black; padding: 0.5em; display: flex; flex-direction: column; gap: 0.5em;" ]
              [ HH.div [] [ HH.text $ "player = " <> show state.player ]
              ]
          ]
      ]
