module Protag.Common where

import Prelude

import Control.Monad.State (StateT, execStateT, get, put)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Array as Array
import Data.Const (Const)
import Data.Generic.Rep (class Generic)
import Data.Lens ((+=))
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query (getHTMLElementRef)
import Protag.Utility (bug, mapAction_ComponentHTML, prop, transformStateT)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.DOMTokenList as Web.DOM.DOMTokenList
import Web.DOM.Element as Web.DOM.Element
import Web.HTML.HTMLElement as Web.HTML.HTMLElement

--------------------------------------------------------------------------------
-- Game
--------------------------------------------------------------------------------

type GameComponent = H.Component GameQuery GameInput GameOutput Aff
type GameHTML = H.ComponentHTML GameAction GameSlots Aff
type GameM = H.HalogenM GameState GameAction GameSlots GameOutput Aff

type GameQuery = Const Void :: Type -> Type
type GameInput =
  { game_state :: InputGameState
  }

type GameOutput = Void

data GameAction
  = GameAction (GameM Unit)
  | SceneAction (GameM OpaqueSceneState)

type GameSlots =
  ( scene :: H.Slot SceneQuery SceneOutput SceneSlotId
  )

--------------------------------------------------------------------------------
-- GameState
--------------------------------------------------------------------------------

type GameState = GameState_ ()

type InputGameState = GameState_ ()

type GameState_ r =
  { player :: Player
  , scene_index :: SceneIndex
  | r
  }

type Player =
  { name :: String
  , health :: Int
  }

data SceneIndex
  = MenuSceneIndex
  | ExampleSceneIndex
  | IntroSceneIndex
  | TownSceneIndex
  | MountainSceneIndex

derive instance Generic SceneIndex _

instance Show SceneIndex where
  show x = genericShow x

instance EncodeJson SceneIndex where
  encodeJson x = genericEncodeJson x

instance DecodeJson SceneIndex where
  decodeJson x = genericDecodeJson x

--------------------------------------------------------------------------------
-- Scene
--------------------------------------------------------------------------------

type SceneComponent = H.Component SceneQuery SceneInput SceneOutput Aff
type SceneHTML state slots = H.ComponentHTML (SceneAction state) slots Aff

data SceneQuery a = PutSceneState OpaqueSceneState a

type SceneInput = {}
type SceneOutput = GameAction
type SceneSlotId = String
type SceneAction state = StateT state GameM Unit

data DialogueItem state slots = Inject_DialogueItem (Array (SceneHTML state slots))

makeSceneComponent'
  :: forall state slots
   . { title :: SceneHTML state slots
     , initialState :: state
     , initialize :: SceneAction state
     , background_image_src :: String
     , dialogue :: Array (DialogueItem state slots)
     }
  -> SceneComponent
makeSceneComponent' args = H.mkComponent { initialState, eval, render }
  where
  dialogue_item_ref = H.RefLabel "dialogue_item"

  initialState _ =
    { special: args.initialState
    , dialogue_index: 0
    }

  eval = H.mkEval H.defaultEval
    { handleQuery = case _ of
        PutSceneState s a -> do
          put $ fromOpaqueSceneState s
          (dialogue_item_ref # getHTMLElementRef) >>= case _ of
            Nothing -> pure unit
            Just e -> do
              classList <- e # Web.HTML.HTMLElement.toElement # Web.DOM.Element.classList # liftEffect
              Web.DOM.DOMTokenList.remove classList "fade-in-shimmer-text-container" # liftEffect
              e # Web.HTML.HTMLElement.offsetHeight # liftEffect # void
              Web.DOM.DOMTokenList.add classList "fade-in-shimmer-text-container" # liftEffect
              pure unit
          pure $ pure a
    , initialize = args.initialize # transformStateT (prop @"special") # pure
    , handleAction = \action -> do
        state <- get
        let m = execStateT action state
        H.raise $ SceneAction $ map toOpaqueSceneState m
    }

  render state =
    let

      dialogue_length = args.dialogue # Array.length
      dialogue_item = args.dialogue Array.!! state.dialogue_index # fromMaybe' \_ -> bug "impossible"
    in
      HH.div [ HP.style "flex-grow: 1; height: calc(800px - 2.5em); display: flex; flex-direction: column;" ]
        [ HH.div
            [ HP.classes [ H.ClassName "shimmer-text-container" ]
            , HP.style "font-size: 2em; padding: 0.5em; background-color:rgba(169, 125, 81, 0.5);"
            ]
            [ args.title # mapAction_ComponentHTML (transformStateT (prop @"special")) ]
        , HH.div [ HP.style "width: 100%; display: flex; flex-direction: row; justify-content: center; align-items: center; background-color: black;" ]
            [ HH.img
                [ HP.style "max-height: 400px; max-width: 100%;"
                , HP.src args.background_image_src
                ]
            ]
        , HH.div [ HP.style "background-color: rgb(186, 173, 147); padding: 0.5em; display: flex; flex-direction: row; justify-content: space-between; align-items: center; user-select: none" ] $
            [ if not (state.dialogue_index < dialogue_length - 1) then []
              else
                [ HH.div []
                    [ HH.button
                        [ HP.classes [ H.ClassName "magic-button" ]
                        , HE.onClick $ const $ prop @"dialogue_index" += 1
                        ]
                        [ HH.div [] [ HH.text "next" ] ]
                    ]
                ]
            , [ HH.div [] [] ] -- spacer
            , [ HH.div []
                  [ HH.text $ show (state.dialogue_index + 1) <> " / " <> show dialogue_length ]
              ]
            ] # Array.fold
        , HH.div
            [ HP.style "flex-grow: 1; min-height: 0; overflow-x: scroll; padding: 0.5em; line-height: 1.5em; background-color: rgb(245, 227, 192);"
            , HP.classes [ H.ClassName "fade-in-shimmer-text-container" ]
            , HP.ref dialogue_item_ref
            ]
            [ renderDialogueItem dialogue_item ]
        ]

  renderDialogueItem (Inject_DialogueItem es) =
    HH.div [ HP.style "display: flex; flex-direction: column; gap: 0.5em;" ]
      (es # map (mapAction_ComponentHTML (transformStateT (prop @"special"))))

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

--------------------------------------------------------------------------------
-- OpaqueSceneState
--------------------------------------------------------------------------------

foreign import data OpaqueSceneState :: Type

toOpaqueSceneState :: forall a. a -> OpaqueSceneState
toOpaqueSceneState = unsafeCoerce

fromOpaqueSceneState :: forall a. OpaqueSceneState -> a
fromOpaqueSceneState = unsafeCoerce
