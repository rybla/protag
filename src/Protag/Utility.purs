module Protag.Utility where

import Prelude

import Data.Lens.Record as Data.Lens.Record
import Data.Profunctor.Strong (class Strong)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as V
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