module Protag.Language where

import Prelude

import Control.Monad.Free (liftF)
import Effect.Aff (Aff)
import Halogen.HTML (PlainHTML)
import Protag.Interaction (InteractionF(..), InteractionT(..))

type Instruction = InstructionT Aff
type InstructionT = InteractionT InstructionF

data InstructionF m (a :: Type)
  = Prompt PlainHTML (String -> m a)
  | ClearWidget (m a)
  | Print PlainHTML (m a)

derive instance Functor m => Functor (InstructionF m)

clearWidget :: forall m. Applicative m => InteractionT InstructionF m Unit
clearWidget = InteractionT $ liftF $ Interact $ ClearWidget (pure unit)

prompt :: forall m. Applicative m => PlainHTML -> InstructionT m String
prompt msg = InteractionT $ liftF $ Interact $ Prompt msg pure

print :: forall m. Applicative m => PlainHTML -> InstructionT m Unit
print msg = InteractionT $ liftF $ Interact $ Print msg (pure unit)

