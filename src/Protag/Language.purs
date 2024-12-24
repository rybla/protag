module Protag.Language where

import Prelude

import Control.Monad.Free (liftF)
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Halogen.HTML (PlainHTML)
import Protag.Interaction (InteractionF(..), InteractionT(..))
import Protag.Utility (class MapRowLabels)
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)

type Instruction = InstructionT Aff
type InstructionT = InteractionT InstructionF

data InstructionF m (a :: Type)
  = Prompt PlainHTML (String -> m a)
  | Choice PlainHTML (ExistsChoice m a)
  | ClearWidget (m a)
  | Print PlainHTML (m a)

derive instance Functor m => Functor (InstructionF m)

clearWidget :: forall m. Applicative m => InteractionT InstructionF m Unit
clearWidget = InteractionT $ liftF $ Interact $ ClearWidget (pure unit)

prompt :: forall m. Applicative m => PlainHTML -> InstructionT m String
prompt msg = InteractionT $ liftF $ Interact $ Prompt msg pure

print :: forall m. Applicative m => PlainHTML -> InstructionT m Unit
print msg = InteractionT $ liftF $ Interact $ Print msg (pure unit)

choice
  :: forall m a @opts
   . Applicative m
  => Homogeneous opts Unit
  => MapRowLabels opts
  => PlainHTML
  -> Proxy opts
  -> (Variant opts -> PlainHTML)
  -> (Variant opts -> a)
  -> InstructionT m a
choice msg opts render_opt c = InteractionT $ liftF $ Interact $ Choice msg $ mkExistsChoice opts render_opt (c >>> pure)

--------------------------------------------------------------------------------
-- ExistsChoice
--------------------------------------------------------------------------------

newtype ExistsChoice m a = ExistsChoice (forall r. ExistsChoiceK m a r -> r)
type ExistsChoiceK m (a :: Type) r =
  forall opts
   . Homogeneous opts Unit
  => MapRowLabels opts
  => Proxy opts
  -> (Variant opts -> PlainHTML)
  -> (Variant opts -> m a)
  -> r

instance Functor m => Functor (ExistsChoice m) where
  map f = unExistsChoice \opts render_opt k -> mkExistsChoice opts render_opt (map f <<< k)

mkExistsChoice
  :: forall m a opts
   . Homogeneous opts Unit
  => MapRowLabels opts
  => Proxy opts
  -> (Variant opts -> PlainHTML)
  -> (Variant opts -> m a)
  -> ExistsChoice m a
mkExistsChoice opts render_opt x = ExistsChoice \k -> k opts render_opt x

unExistsChoice :: forall m a r. ExistsChoiceK m a r -> ExistsChoice m a -> r
unExistsChoice k1 (ExistsChoice k2) = k2 k1
