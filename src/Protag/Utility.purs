module Protag.Utility where

import Prelude

import Control.Monad.State (StateT, get, put, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (bimap)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Record as Data.Lens.Record
import Data.Profunctor.Strong (class Strong)
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant)
import Data.Variant as V
import Halogen (ComponentHTML)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Cons)
import Type.Prelude (Proxy(..))

todo :: forall a. String -> a
todo msg = unsafeCrashWith $ "TODO: " <> msg

prop :: forall @l r1 r2 r a b. IsSymbol l => Cons l a r r1 => Cons l b r r2 => (forall p. Strong p => p a b -> p (Record r1) (Record r2))
prop = Data.Lens.Record.prop (Proxy :: Proxy l)

bug :: forall a. String -> a
bug msg = unsafeCrashWith $ "BUG: " <> msg

inj :: forall @x a r1 r2. Cons x a r1 r2 => IsSymbol x => a -> Variant r2
inj = V.inj (Proxy @x)

on :: forall @x a b r1 r2. Cons x a r1 r2 => IsSymbol x => (a -> b) -> (Variant r1 -> b) -> Variant r2 -> b
on = V.on (Proxy @x)

transformStateT :: forall s1 s2 m a. Monad m => Lens' s2 s1 -> StateT s1 m a -> StateT s2 m a
transformStateT l m1 = do
  s2 <- get
  a /\ s1 <- runStateT m1 (s2 ^. l) # lift
  put $ s2 # l .~ s1
  pure a

mapAction_ComponentHTML
  :: forall action action' slots m
   . (action -> action')
  -> ComponentHTML action slots m
  -> ComponentHTML action' slots m
mapAction_ComponentHTML f = bimap (map f) f

newtype ExistsCons r = ExistsCons (forall z. ExistsConsK r z -> z)

type ExistsConsK :: Row Type -> Type -> Type
type ExistsConsK r z = forall x a r_. IsSymbol x => Cons x a r_ r => Proxy x -> z

mkExistsCons :: forall r. ExistsConsK r (ExistsCons r)
mkExistsCons a = ExistsCons \k -> k a

unExistsCons :: forall r z. ExistsConsK r z -> ExistsCons r -> z
unExistsCons k1 (ExistsCons k2) = k2 k1