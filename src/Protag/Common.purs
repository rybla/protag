module Protag.Common where

import Prelude

import Data.Const (Const)
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

type GameComponentQuery = Const Void

type GameComponentInput =
  { impl :: GameImpl
  }

type GameComponentOutput = Void

type GameComponent = Component GameComponentQuery GameComponentInput GameComponentOutput Aff
type GameComponentHTML = ComponentHTML GameComponentAction GameComponentSlots Aff
type GameComponentSlots = (scene :: SceneSlot Int)

data GameComponentAction
  = InitializeGame
  | InteractGame (GameM Unit)

type GameState =
  { title :: String
  }

data GameF m a = GameF (m a)
type GameM = InteractionT GameF Aff

--------------------------------------------------------------------------------
-- Scene
--------------------------------------------------------------------------------

type SceneImpl state =
  { initialState :: state
  , render :: state -> SceneComponentHTML state
  }

data SceneComponentQuery a
type SceneComponentInput = {}
data SceneComponentOutput

type SceneSlot = Slot SceneComponentQuery (GameM Unit)

type SceneComponent = Component SceneComponentQuery SceneComponentInput SceneComponentOutput Aff
type SceneComponentHTML state = ComponentHTML (SceneComponentAction state) (SceneComponentSlots state) Aff
type SceneComponentSlots state = (widget :: WidgetSlot (SceneM state Unit) Int)

data SceneComponentAction state
  = InitializeScene
  | InteractScene (SceneM state Unit)

type SceneM state = InteractionT (SceneF state) Aff
data SceneF state m a = SceneF (m a)

--------------------------------------------------------------------------------
-- Widget
--------------------------------------------------------------------------------

data WidgetComponentQuery a

type WidgetSlot = Slot WidgetQuery

data WidgetQuery a

