module Protag.Component.Widget where

import Prelude
import Protag.Common

import Control.Monad.State (execStateT, get, put)
import Data.Bifunctor (bimap)
import Data.Exists (mkExists)
import Halogen (HalogenM)
import Halogen as H

component :: forall input state. WidgetImpl input state -> WidgetComponent input
component impl = H.mkComponent { initialState, eval, render }
  where
  initialState = impl.initialState

  eval = H.mkEval H.defaultEval
    { initialize = pure InitializeWidget
    , receive = ReceiveWidget >>> pure
    , handleAction = case _ of
        InitializeWidget -> runWidgetM impl.initialize
        ReceiveWidget input -> put $ initialState input
        EffectWidget m -> runWidgetM m
    }

  runWidgetM :: WidgetM state Unit -> HalogenM _ _ _ _ _ Unit
  runWidgetM m = do
    H.raise (m # WidgetMUnit # mkExists)
    pure unit

  render state = impl.render state # bimap (map EffectWidget) EffectWidget
