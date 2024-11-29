module Protag.Game where

import Prelude
import Protag.Common

import Control.Monad.Trans.Class (lift)
import Control.Monad.State (execStateT, get, put)
import Data.Lens ((%=), (.=))
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Protag.Utility (prop)
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

getSceneComponent :: SceneIndex -> SceneComponent
getSceneComponent = case _ of
  MenuSceneIndex -> menu_component
  ExampleSceneIndex -> example_component

makeSceneComponent
  :: forall state slots
   . { initialState :: state
     , initialize :: SceneAction state
     , render :: state -> SceneHTML state slots
     }
  -> SceneComponent
makeSceneComponent args = H.mkComponent { initialState, eval, render }
  where
  initialState {} = args.initialState
  eval = H.mkEval H.defaultEval
    { handleQuery = case _ of
        PutSceneState s a -> do
          put $ fromOpaqueSceneState $ s
          pure (pure a)
    , initialize = pure args.initialize
    , handleAction = \action -> do
        state <- get
        let m = execStateT action state
        H.raise $ SceneAction $ map toOpaqueSceneState m
    }
  render = args.render

menu_component :: SceneComponent
menu_component = makeSceneComponent
  { initialState: {}
  , initialize: do
      Console.log "[menu.initialize]"
  , render: \{} ->
      HH.div []
        [ HH.div [] [ HH.h2 [] [ HH.text "menu" ] ]
        , HH.div []
            [ HH.button
                [ HE.onClick $ const do
                    prop @"scene_index" .= ExampleSceneIndex # lift
                ]
                [ HH.text "example scene" ]
            ]
        ]
  }

example_component :: SceneComponent
example_component = makeSceneComponent
  { initialState:
      { counter: 0 }
  , initialize: do
      Console.log "[example.initialize]"
  , render: \state ->
      HH.div []
        [ HH.div [] [ HH.h2 [] [ HH.text "example" ] ]
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
                    prop @"player_health" %= (_ + 1) # lift
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
