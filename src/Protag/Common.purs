module Protag.Common where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Const (Const)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Protag.Language (Instruction)

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

data GameAction = GameAction (Instruction Unit)

type GameSlots =
  ( widget :: WidgetSlot Int
  )

--------------------------------------------------------------------------------
-- GameState
--------------------------------------------------------------------------------

type GameState = GameState_
  ( messages :: Array PlainHTML
  , mb_widget :: Maybe WidgetComponent
  , widget_index :: Int
  )

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
-- Widget
--------------------------------------------------------------------------------

type WidgetComponent = H.Component WidgetQuery WidgetInput WidgetOutput Aff
type WidgetQuery = Const Void :: Type -> Type
type WidgetInput = {}
type WidgetOutput = GameAction
type WidgetSlot = H.Slot WidgetQuery WidgetOutput

