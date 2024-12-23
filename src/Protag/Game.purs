module Protag.Game where

import Prelude

import Control.Applicative (pure)
import Control.Monad.Free (Free, runFreeM)
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Lens ((%=), (.=))
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', maybe, maybe')
import Data.Unfoldable (none)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console as Console
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Protag.Common (GameAction(..), GameComponent, GameInput, GameM, GameState, WidgetComponent)
import Protag.Interaction (InteractionF, InteractionT(..), runInteractionT)
import Protag.Language (Instruction, InstructionF(..))
import Protag.Utility (bug, prop, todo)
import Type.Prelude (Proxy(..))
import Web.DOM.NonElementParentNode (getElementById)
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
    , handleAction = case _ of
        GameAction m -> m # run
    }

  render state =
    HH.div
      [ HP.style "flex-grow: 1; display: flex; flex-direction: row;" ]
      [ HH.div
          []
          ( [ renderScene state
            , state.mb_widget # foldMap \widget ->
                [ HH.slot (Proxy @"widget") state.widget_index widget {} identity ]
            ] # Array.fold
          )

      , HH.div
          [ HP.style "flex-shrink: 0; width: calc(200px - 2em); padding: 1em; background-color:rgba(196, 164, 132, 0.5); display: flex; flex-direction: column; gap: 0.5em;" ]
          [ HH.text $ "player = " <> show state.player
          , HH.div [] [ HH.text $ "... other properties ..." ]
          ]
      ]

  renderScene state = todo ""

run :: Instruction Unit -> GameM Unit
run = runInteractionT liftAff
  case _ of
    Prompt msg k -> do
      prop @"mb_widget" .= pure (prompt_component { msg, k })
      -- "reply" # k # liftAff
      todo ""
    Print msg ma -> do
      prop @"messages" %= flip Array.snoc msg
      ma # liftAff

prompt_component :: forall a. { msg :: String, k :: String -> Aff a } -> WidgetComponent
prompt_component { msg, k } = H.mkComponent { initialState, eval, render }
  where
  initialState _ = {}

  eval = H.mkEval H.defaultEval
    { handleAction = H.raise }

  render _ = HH.div
    []
    [ HH.div [] [ HH.text msg ]
    , HH.input
        [ HP.id "input" ]
    , HH.button
        [ HE.onClick $ const $ GameAction do
            window <- Web.HTML.window # liftEffect
            document <- window # Web.HTML.Window.document # liftEffect
            input_elem <- document # Web.HTML.HTMLDocument.toNonElementParentNode # Web.DOM.NonElementParentNode.getElementById "input" # liftEffect >>= maybe' (\_ -> bug "impossible") pure
            input_str <- input_elem # Web.HTML.HTMLInputElement.fromElement # fromMaybe' (\_ -> bug "impossible") # Web.HTML.HTMLInputElement.value # liftEffect
            k input_str # liftAff # void
        ]
        [ HH.text "Submit" ]
    ]

{-
subcomponent = H.mkComponent { initialState, eval, render }
  where
  initialState _ = {}

  eval = H.mkEval H.defaultEval

  render _ = HH.div [] []
-}