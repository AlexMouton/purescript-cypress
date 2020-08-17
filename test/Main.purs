module Test.Main where

import Prelude

import Effect (Effect)
import Data.Maybe (Maybe(..))

import Cypress.Cy
import Cypress.Actions
import Cypress.Actions as C
import Cypress.Foreign
import Cypress.Chai (LengthOf(..), Not(..))
import Cypress.Elements

amazon :: CypressM Unit
amazon = do
  visit "/"
  void $ get "a.a-carousel-goto-prevpage"
      -- ~ should (LengthOf 1 :: LengthOf Elements)
    ~ should (Not $ LengthOf 2 :: Not (LengthOf Elements))
    ~ first
    ~ click
  void $ get "#twotabsearchtextbox"
    ~ typ "HEHEHE"
  visit "/dp/B07X6C9RMF/ref=ods_gw_vicc_blinkmini_apr_anc"
  void $ contains { content : "Blink Home Security", selector: Just "a", options: Nothing }
  go "back"

  exec "ls -la"
    ~ thn _.stdout
    ~ C.log

main :: Cy -> Effect Unit
main cy = do
  runCypress amazon cy
