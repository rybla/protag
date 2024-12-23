module Protag.Common where

import Prelude

import Control.Monad.State (StateT)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Const (Const)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import Halogen as H

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

data GameAction = GameAction (GameM Unit)

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

data SceneQuery :: forall k. k -> Type
data SceneQuery a

type SceneInput = {}
type SceneOutput = GameAction
type SceneSlotId = String
type SceneAction state = StateT state GameM Unit

