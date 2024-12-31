module Protag.Common where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Const (Const)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Protag.Variant (Variant)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Protag.Language (Instruction)

--------------------------------------------------------------------------------
-- Game
--------------------------------------------------------------------------------

type Game scenes state =
  { params :: GameParams scenes state
  , input :: GameInput scenes
  }

type GameParams scenes state =
  { story :: Instruction scenes Unit
  , renderScene :: GameState scenes state -> GameHTML scenes
  , initialState :: state
  }

--------------------------------------------------------------------------------
-- Game
--------------------------------------------------------------------------------

type GameComponent scenes = H.Component GameQuery (GameInput scenes) GameOutput Aff
type GameHTML scenes = H.ComponentHTML (GameAction scenes) (GameSlots scenes) Aff
type GameM scenes state = H.HalogenM (GameState scenes state) (GameAction scenes) (GameSlots scenes) GameOutput Aff

type GameQuery = Const Void :: Type -> Type
type GameInput scenes =
  { inputGameState :: InputGameState scenes
  }

type GameOutput = Void

data GameAction scenes = GameAction (Instruction scenes Unit)

type GameSlots scenes =
  ( widget :: WidgetSlot scenes Int
  )

--------------------------------------------------------------------------------
-- GameState
--------------------------------------------------------------------------------

-- automatically initialized fields of GameState
type GameState scenes state = GameState_ scenes
  ( messages :: Array PlainHTML
  , mb_widget :: Maybe (WidgetComponent scenes)
  , widget_index :: Int
  , state :: state
  )

type InputGameState scenes = GameState_ scenes ()

-- manually-initialized fields of GameState
type GameState_ scenes r =
  { scene :: Variant scenes
  | r
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

type WidgetComponent scenes = H.Component WidgetQuery WidgetInput (WidgetOutput scenes) Aff
type WidgetQuery = Const Void :: Type -> Type
type WidgetInput = {}
type WidgetOutput scenes = GameAction scenes
type WidgetSlot scenes = H.Slot WidgetQuery (WidgetOutput scenes)

