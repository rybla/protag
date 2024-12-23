module Protag.Language where

import Prelude

import Control.Monad.Free (liftF)
import Effect.Aff (Aff)
import Protag.Interaction (InteractionF(..), InteractionT(..))

type Instruction = InteractionT InstructionF Aff

data InstructionF m (a :: Type)
  = Prompt String (String -> m a)
  | Print String (m a)

derive instance Functor m => Functor (InstructionF m)

prompt :: forall m. Applicative m => String -> InteractionT InstructionF m String
prompt str = InteractionT $ liftF $ Interact $ Prompt str pure

print :: forall m. Applicative m => String -> InteractionT InstructionF m Unit
print str = InteractionT $ liftF $ Interact $ Print str (pure unit)

