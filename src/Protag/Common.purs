module Protag.Common where

import Prelude

import Control.Monad.State (StateT)
import Data.Const (Const)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import Halogen as H
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
-- Game
--------------------------------------------------------------------------------

type GameComponent = H.Component GameQuery GameInput GameOutput Aff
type GameHTML = H.ComponentHTML GameAction GameSlots Aff
type GameM = H.HalogenM GameState GameAction GameSlots GameOutput Aff

type GameQuery = Const Void :: Type -> Type
type GameInput = {}
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

type GameState =
  { player_name :: String
  , player_health :: Int
  , scene_index :: SceneIndex
  }

data SceneIndex
  = MenuSceneIndex
  | ExampleSceneIndex

derive instance Generic SceneIndex _

instance Show SceneIndex where
  show x = genericShow x

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

--------------------------------------------------------------------------------
-- OpaqueSceneState
--------------------------------------------------------------------------------

foreign import data OpaqueSceneState :: Type

toOpaqueSceneState :: forall a. a -> OpaqueSceneState
toOpaqueSceneState = unsafeCoerce

fromOpaqueSceneState :: forall a. OpaqueSceneState -> a
fromOpaqueSceneState = unsafeCoerce
