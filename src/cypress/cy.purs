module Cypress.Cy where

import Control.Monad.Reader.Trans (ReaderT)
import Effect (Effect)

foreign import data Cy :: Type
type CypressM = ReaderT Cy Effect
