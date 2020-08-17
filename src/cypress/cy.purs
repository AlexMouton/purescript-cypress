module Cypress.Cy where

import Prelude
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Effect (Effect)

foreign import data Cy :: Type
type CypressM = ReaderT Cy Effect

infixl 1 bind as ~

runCypress :: forall a. CypressM a -> Cy -> Effect a
runCypress = runReaderT