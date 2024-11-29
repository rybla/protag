module Protag.Game where

import Prelude
import Protag.Common

import Control.Monad.State (get)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Protag.Scene (getSceneComponent)
import Type.Prelude (Proxy(..))

component :: GameComponent
component = H.mkComponent { initialState, eval, render }
  where
  initialState {} =
    { player_name: "Kellan Veylor"
    , player_health: 10
    , scene_index: MenuSceneIndex
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
      []
      [ HH.div
          []
          [ HH.div [] [ HH.text $ "player_name = " <> state.player_name ]
          , HH.div [] [ HH.text $ "player_health = " <> show state.player_health ]
          ]
      , HH.slot (Proxy @"scene") (show state.scene_index) (getSceneComponent state.scene_index) {} identity
      ]
