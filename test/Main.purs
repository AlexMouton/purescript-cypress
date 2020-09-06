module Test.Main where

import Cypress.Actions
import Prelude

import Control.Bind (pure)
import Cypress (Cy, CypressM, Elements, runCypress, (~))
import Cypress.Actions as C
import Cypress.Chai (LengthOf(..), Not(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Prelude (Unit, bind, discard, void, ($))

carousel :: CypressM Elements
carousel =
  get "a#nav-hamburger-menu"
     -- nav-sprite nav-logo-ext
  ~ first
  ~ click

amazon :: CypressM Unit
amazon = do
  _ <- visit "/"
  _ <- carousel
  _ <- get ".hmenu-close-icon"
     ~ click
  _ <- get "#twotabsearchtextbox"
     ~ typ "HEHEHE"
  _ <- visit "/dp/B07X6C9RMF/ref=ods_gw_vicc_blinkmini_apr_anc"
  _ <- containsSelector "a" "Blink Home Security"
  go "back"

  e <- exec "ls -la"
  C.log e.stdout

main :: Cy -> Effect Unit
main cy = do
  launchAff_ $ runCypress amazon cy
