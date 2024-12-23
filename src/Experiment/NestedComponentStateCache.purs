module Experiment.NestedComponentStateCache where

-- import Prelude

-- import Data.Lens ((%=), (.=))
-- import Data.Lens.Record (prop)
-- import Effect (Effect)
-- import Halogen as H
-- import Halogen.Aff as HA
-- import Halogen.HTML as HH
-- import Halogen.HTML.Events as HE
-- import Halogen.HTML.Properties as HP
-- import Halogen.VDom.Driver as HVD
-- import Type.Proxy (Proxy(..))

-- main :: Effect Unit
-- main = do
--   HA.runHalogenAff (HVD.runUI mainComponent {} =<< HA.awaitBody)

-- mainComponent = H.mkComponent { initialState, eval, render }
--   where
--   initialState {} =
--     { index: 0
--     }

--   eval = H.mkEval H.defaultEval
--     { handleAction = \index -> do
--         prop (Proxy @"index") .= index
--     }

--   render { index } =
--     HH.div
--       []
--       [ HH.div [] [ HH.button [ HE.onClick (const 0) ] [ HH.text "set mode to 0" ] ]
--       , HH.div [] [ HH.button [ HE.onClick (const 1) ] [ HH.text "set mode to 1" ] ]
--       , HH.div [] [ HH.button [ HE.onClick (const 2) ] [ HH.text "set mode to 2" ] ]
--       , HH.slot_ (Proxy @"kid1") index kid1Component { label: "kid1, index " <> show index, value: 0 }
--       ]

-- -- ive only gone through one seriously frightening car driving experience, and i had really fortunate instincts that i still surprise myself remembering, and thinking about it still gives me a bit of anxiety
-- kid1Component = H.mkComponent { initialState, eval, render }
--   where
--   initialState = identity

--   eval = H.mkEval H.defaultEval
--     { handleAction = const do
--         prop (Proxy @"value") %= (_ + 1)
--     }

--   render { label, value } =
--     HH.div
--       [ HP.style "margin: 1em; padding: 1em; border: 1px solid black;" ]
--       [ HH.div [] [ HH.text label ]
--       , HH.div [] [ HH.button [ HE.onClick (const unit) ] [ HH.text "increment" ] ]
--       , HH.div [] [ HH.text $ "value: " <> show value ]
--       ]

