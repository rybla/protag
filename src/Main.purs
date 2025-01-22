module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver as HVD
import Protag.App as Protag.App

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI Protag.App.component {} =<< HA.awaitBody)

-- module Main where

-- import Prelude

-- import Effect (Effect)
-- import Effect.Aff.Class (class MonadAff)
-- import Halogen (modify_)
-- import Halogen as H
-- import Halogen.Aff as HA
-- import Halogen.HTML as HH
-- import Halogen.VDom.Driver as HVD

-- main :: Effect Unit
-- main = HA.runHalogenAff (HVD.runUI appComponent {} =<< HA.awaitBody)

-- appComponent :: forall query input output m. MonadAff m => H.Component query input output m
-- appComponent = H.mkComponent { initialState, eval, render }
--   where
--   initialState _ =
--     { content: "hello"
--     }

--   eval = H.mkEval H.defaultEval
--     { handleAction = const $ modify_ (\state -> state { content = state.content <> " world" }) }

--   render state =
--     HH.div []
--       [ HH.button [] []
--       , HH.text state.content
--       ]

