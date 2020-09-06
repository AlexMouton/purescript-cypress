module Cypress.Cy where

import Control.Monad.Reader.Trans (ReaderT)
import Effect (Effect)
import Effect.Aff (Aff)

foreign import data Cy :: Type
type CypressM = ReaderT Cy Aff
