module Cypress ((~), CypressM, Cy, runCypress, Query, Elements) where

import Prelude
import Effect (Effect)
import Control.Monad.Reader.Trans (runReaderT)

import Cypress.Cy as C
import Cypress.Query as Q
import Cypress.Elements as E

type Cy = C.Cy

runCypress :: forall a. CypressM a -> Cy -> Effect a
runCypress = runReaderT

infixl 1 bind as ~

type CypressM = C.CypressM
type Elements = E.Elements
type Query = Q.Query