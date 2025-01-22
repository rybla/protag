module Protag.Game where

import Prelude

import Control.Monad.State (get)
import Data.Array as Array
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Protag.Common (GameAction(..), GameComponent, GameState, SceneQuery(..), GameInput)
import Protag.Scene (getSceneComponent)
import Type.Prelude (Proxy(..))

component :: GameComponent
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: GameInput -> GameState
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
      [ HP.style "flex-grow: 1; display: flex; flex-direction: row;" ]
      [ HH.slot (Proxy @"scene") (show state.scene_index) (getSceneComponent state.scene_index) {} identity
      , HH.div
          [ HP.style "flex-shrink: 0; width: 200px; height: 700px; padding: 1em; overflow-y: scroll;" ]
          [ HH.table []
              [ render_game_state_prop "name" $ HH.text state.player.name
              , render_game_state_prop "health" $ HH.text $ show state.player.health
              , render_game_state_prop "status" $ HH.text state.player.status
              , render_game_state_prop "description" $ HH.text state.player.description
              , render_game_state_prop "inventory" $
                  if state.player.inventory # Map.isEmpty then HH.text "empty"
                  else
                    HH.ul []
                      $ map (\(name /\ count) -> HH.li [] $ [ HH.text name ] <> if count == 1 then [] else [ HH.text $ " (" <> show count <> ")" ])
                      $ Map.toUnfoldable state.player.inventory
              ]
          ]
      ]
  render_game_state_prop key val =
    HH.tr
      []
      [ HH.td
          [ HP.style "padding: 0 0.5;" ]
          [ HH.div
              [ HP.style "font-style: italic; font-size: 0.8em; color: brown;" ]
              [ HH.text key ]
          , HH.div
              [ HP.style "" ]
              [ val ]
          ]
      ]
