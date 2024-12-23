module Protag.Game where

import Prelude

import Control.Monad.Free (runFreeM)
import Control.Monad.Free as Free
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Lens ((%=), (+=), (.=))
import Data.Maybe (fromMaybe', maybe')
import Data.Unfoldable (none)
import Data.Variant (case_)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console as Console
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Protag.Common (GameAction(..), GameComponent, GameInput, GameM, GameState, SceneIndex(..), WidgetComponent)
import Protag.Interaction (InteractionF(..), InteractionT(..))
import Protag.Language (Instruction, InstructionF(..), clearWidget, print, prompt)
import Protag.Utility (bug, inj, on, prop)
import Type.Prelude (Proxy(..))
import Web.DOM.NonElementParentNode as Web.DOM.NonElementParentNode
import Web.HTML as Web.HTML
import Web.HTML.HTMLDocument as Web.HTML.HTMLDocument
import Web.HTML.HTMLInputElement as Web.HTML.HTMLInputElement
import Web.HTML.Window as Web.HTML.Window

component :: GameComponent
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: GameInput -> GameState
  initialState input =
    { player: input.game_state.player
    , scene_index: input.game_state.scene_index
    , messages: none
    , widget_index: 0
    , mb_widget: none
    }

  eval = H.mkEval H.defaultEval
    { initialize = pure $ GameAction do
        Console.log "[game.initialize]"
        print $ HH.text "print1"
        print $ HH.text "print2"
        print $ HH.text "print3"
        reply <- prompt $ HH.text "what is your name?"
        print $ HH.text $ "your name is: " <> show reply
        pure unit
    , handleAction = case _ of
        GameAction m -> m # runInstruction
    }

  render state =
    HH.div
      [ HP.style "flex-grow: 1; display: flex; flex-direction: row;" ]
      [ HH.div
          [ HP.style "display: flex; flex-direction: column;" ]
          ( [ renderScene state
            , state.messages # map \msg -> HH.div [] [ msg # HH.fromPlainHTML ]
            , state.mb_widget # foldMap \widget ->
                [ HH.slot (Proxy @"widget") state.widget_index widget {} identity ]
            ] # Array.fold
          )
      -- , HH.div
      --     [ HP.style "flex-shrink: 0; width: calc(200px - 2em); padding: 1em; background-color:rgba(196, 164, 132, 0.5); display: flex; flex-direction: column; gap: 0.5em;" ]
      --     [ HH.text $ "player = " <> show state.player
      --     , HH.div [] [ HH.text $ "... other properties ..." ]
      --     ]
      ]

  renderScene state = case state.scene_index of
    MenuSceneIndex -> [ HH.div [] [ HH.text "MenuSceneIndex" ] ]
    ExampleSceneIndex -> [ HH.div [] [ HH.text "ExampleSceneIndex" ] ]
    IntroSceneIndex -> [ HH.div [] [ HH.text "IntroSceneIndex" ] ]
    TownSceneIndex -> [ HH.div [] [ HH.text "TownSceneIndex" ] ]
    MountainSceneIndex -> [ HH.div [] [ HH.text "MountainSceneIndex" ] ]

prompt_component :: { msg :: PlainHTML, k :: String -> Instruction Unit } -> WidgetComponent
prompt_component { msg, k } = H.mkComponent { initialState, eval, render }
  where
  initialState _ = {}

  eval = H.mkEval H.defaultEval
    { handleAction = case_
        # on @"GameAction"
            ( \ga -> do
                Console.log "[prompt.eval] GameAction"
                H.raise ga
            )
    }

  render _ =
    HH.div
      []
      [ HH.div []
          [ msg # HH.fromPlainHTML ]
      , HH.input
          [ HP.id "input" ]
      , HH.button
          [ HE.onClick $ const $ inj @"GameAction" $ GameAction do
              window <- Web.HTML.window # liftEffect
              document <- window # Web.HTML.Window.document # liftEffect
              input_elem <- document # Web.HTML.HTMLDocument.toNonElementParentNode # Web.DOM.NonElementParentNode.getElementById "input" # liftEffect >>= maybe' (\_ -> bug "impossible") pure
              input_str <- input_elem # Web.HTML.HTMLInputElement.fromElement # fromMaybe' (\_ -> bug "impossible") # Web.HTML.HTMLInputElement.value # liftEffect
              clearWidget
              k input_str
          ]
          [ HH.text "Submit" ]
      ]

runInstruction :: Instruction Unit -> GameM Unit
runInstruction (InteractionT ff) = ff # runFreeM case _ of
  Lift ma -> ma # lift
  Interact (ClearWidget ma) -> do
    prop @"mb_widget" .= none
    ma # lift
  Interact (Prompt msg k) -> do
    prop @"widget_index" += 1
    prop @"mb_widget" .= pure (prompt_component { msg, k: k >>> Lift >>> Free.wrap >>> InteractionT })
    pure unit # pure
  Interact (Print msg ma) -> do
    prop @"messages" %= flip Array.snoc msg
    ma # liftAff

{-
subcomponent = H.mkComponent { initialState, eval, render }
  where
  initialState _ = {}

  eval = H.mkEval H.defaultEval

  render _ = HH.div [] []
-}