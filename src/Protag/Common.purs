module Protag.Common where

import Prelude

import Control.Monad.State (StateT)
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
  ( widget :: WidgetSlot WidgetMUnitE String
  )

data GameComponentAction
  = InitializeGame
  | EffectGame (GameM Unit)

type GameState =
  { title :: String
  }

data GameF m (a :: Type) = GameF (m a)
type GameM = InteractionT GameF Aff

--------------------------------------------------------------------------------
-- Widget
--------------------------------------------------------------------------------

data WidgetMUnit state = WidgetMUnit (WidgetM state Unit)
type WidgetMUnitE = Exists WidgetMUnit

type WidgetImpl input state =
  { initialState :: input -> state
  , initialize :: WidgetM state Unit
  , render :: state -> ComponentHTML (WidgetM state Unit) WidgetComponentSlots Aff
  }

type WidgetSlot = Slot WidgetComponentQuery

-- data WidgetComponentQuery state a =
--   GetStateWidget ()
data WidgetComponentQuery a

type WidgetComponent input = Component WidgetComponentQuery input WidgetMUnitE Aff

-- type WidgetComponentHTML input state = ComponentHTML (WidgetComponentAction input state) WidgetComponentSlots Aff

data WidgetComponentAction input state
  = InitializeWidget
  | ReceiveWidget input
  | EffectWidget (WidgetM state Unit)

type WidgetM state = StateT state GameM

type WidgetComponentSlots = () :: Row Type

