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
import Effect.Aff.Class (liftAff)
import Effect.Class.Console as Console
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Protag.Common (GameAction(..), GameComponent, GameInput, GameM, GameState, WidgetComponent, GameHTML)
import Protag.Interaction (InteractionF(..), InteractionT(..))
import Protag.Language (Instruction, InstructionF(..), unExistsChoice)
import Protag.Utility (class MapRowLabels, bug, inj, mapRowLabels, on, prop, unExistsCons)
import Protag.Variant (Variant, case_)
import Protag.Variant as V
import Type.Prelude (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.NonElementParentNode as Web.DOM.NonElementParentNode
import Web.HTML as Web.HTML
import Web.HTML.HTMLDocument as Web.HTML.HTMLDocument
import Web.HTML.HTMLInputElement as Web.HTML.HTMLInputElement
import Web.HTML.Window as Web.HTML.Window

component
  :: forall scenes
   . { story :: Instruction scenes Unit
     , renderScene :: GameState scenes -> GameHTML scenes
     }
  -> GameComponent scenes
component params = H.mkComponent { initialState, eval, render }
  where
  initialState :: GameInput scenes -> GameState scenes
  initialState input =
    { scene: input.inputGameState.scene
    , player: input.inputGameState.player
    , messages: none
    , widget_index: 0
    , mb_widget: none
    }

  eval = H.mkEval H.defaultEval
    { initialize = pure $ GameAction do
        Console.log "[game.initialize]"
        params.story
    , handleAction = case _ of
        GameAction m -> m # runInstruction
    }

  render state =
    HH.div
      [ HP.style "flex-grow: 1; display: flex; flex-direction: row;" ]
      [ HH.div
          [ HP.style "flex-grow: 1; display: flex; flex-direction: column;" ]
          ( [ [ params.renderScene state ]
            , [ HH.div
                  [ HP.style "padding: 1em; overflow-y: scroll; max-height: 300px;" ]
                  (state.messages # map \msg -> HH.div [] [ msg # HH.fromPlainHTML ])
              ]
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

--------------------------------------------------------------------------------
-- prompt_component
--------------------------------------------------------------------------------

prompt_component :: forall scenes. { msg :: PlainHTML, k :: String -> Instruction scenes Unit } -> WidgetComponent scenes
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
      [ HP.style "padding: 1em;" ]
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
              k input_str
          ]
          [ HH.text "Submit" ]
      ]

--------------------------------------------------------------------------------
-- choice_component
--------------------------------------------------------------------------------

choice_component
  :: forall scenes opts
   . Homogeneous opts Unit
  => MapRowLabels opts
  => { msg :: PlainHTML
     , opts :: Proxy opts
     , render_opt :: Variant opts -> PlainHTML
     , k :: Variant opts -> Instruction scenes Unit
     }
  -> WidgetComponent scenes
choice_component { msg, opts, render_opt, k } = H.mkComponent { initialState, eval, render }
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
      [ HP.style "padding: 1em;" ]
      [ HH.div []
          [ msg # HH.fromPlainHTML ]
      , HH.div []
          ( opts
              # mapRowLabels
                  ( unExistsCons \x ->
                      let
                        opt = V.inj x (unsafeCoerce unit)
                      in
                        HH.button
                          [ HE.onClick $ const $ inj @"GameAction" $ GameAction $ k opt ]
                          [ render_opt opt # HH.fromPlainHTML ]
                  )
              # Array.fromFoldable
          )
      ]

--------------------------------------------------------------------------------
-- runInstruction
--------------------------------------------------------------------------------

runInstruction :: forall scenes. Instruction scenes Unit -> GameM scenes Unit
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
  Interact (Choice msg c) -> c # unExistsChoice \opts render_opt k -> do
    prop @"widget_index" += 1
    prop @"mb_widget" .= pure (choice_component { msg, opts, render_opt, k: k >>> Lift >>> Free.wrap >>> InteractionT })
    pure unit # pure
  Interact (SetScene scene ma) -> do
    prop @"scene" .= scene
    ma # lift

{-
subcomponent = H.mkComponent { initialState, eval, render }
  where
  initialState _ = {}

  eval = H.mkEval H.defaultEval

  render _ = HH.div [] []
-}