module Protag.Component.Scene where

-- -- import Prelude
-- import Protag.Common

-- import Effect.Aff (Aff)
-- import Halogen (Component)
-- import Halogen as H
-- import Halogen.HTML as HH
-- import Halogen.HTML.Properties as HP

-- component :: forall state. SceneImpl state -> Component SceneComponentQuery SceneComponentInput SceneComponentOutput Aff
-- component (SceneImpl impl) = H.mkComponent { initialState, eval, render }
--   where
--   initialState {} =
--     { state: impl.initialState
--     }

--   eval = H.mkEval H.defaultEval

--   render { state } =
--     HH.div
--       [ HP.classes [ HH.ClassName "Scene" ] ]
--       [ impl.render state ]

