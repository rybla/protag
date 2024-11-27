module Protag.Common where

import Prelude

import Data.Const (Const)
import Data.Exists (Exists)
import Effect.Aff (Aff)
import Halogen (Component, ComponentHTML, Slot)
import Protag.Interaction (InteractionT)

--------------------------------------------------------------------------------
-- Game
--------------------------------------------------------------------------------

type GameImpl =
  { initialState :: GameState
  , render :: GameState -> GameComponentHTML
  }

type GameComponentQuery = Const Void :: Type -> Type
type GameComponentInput = {}
type GameComponentOutput = Void

type GameComponent = Component GameComponentQuery GameComponentInput GameComponentOutput Aff
type GameComponentHTML = ComponentHTML GameComponentAction GameComponentSlots Aff
type GameComponentSlots =
  ( scene :: SceneSlot String
  , widget :: WidgetSlot (GameM Unit) String
  )

data GameComponentAction
  = InitializeGame
  | InteractGame (GameM Unit)

type GameState =
  { title :: String
  }

data GameF m (a :: Type) = GameF (m a)
type GameM = InteractionT GameF Aff

--------------------------------------------------------------------------------
-- Scene
--------------------------------------------------------------------------------

type SceneSlot = Slot SceneComponentQuery (GameM Unit)

type SceneImplE = Exists SceneImpl

newtype SceneImpl state = SceneImpl
  { initialState :: state
  , render :: state -> SceneComponentHTML state
  }

type SceneComponentQuery = Const Void :: Type -> Type
type SceneComponentInput = {}
data SceneComponentOutput

type SceneComponent = Component SceneComponentQuery SceneComponentInput SceneComponentOutput Aff
type SceneComponentHTML state = ComponentHTML (SceneComponentAction state) (SceneComponentSlots state) Aff
type SceneComponentSlots state = (widget :: WidgetSlot (SceneM state Unit) String)

data SceneComponentAction state
  = InitializeScene
  | InteractScene (SceneM state Unit)

type SceneM state = InteractionT (SceneF state) Aff
data SceneF (state :: Type) m (a :: Type) = SceneF (m a)

--------------------------------------------------------------------------------
-- Widget
--------------------------------------------------------------------------------

type WidgetSlot = Slot WidgetComponentQuery

type WidgetComponentQuery = Const Void :: Type -> Type
