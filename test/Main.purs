module Test.Main where

import Prelude

import Effect (Effect)
import Data.Maybe (Maybe(..))

import Cypress
import Cypress as C
import Cypress.Actions
import Cypress.Actions as C
import Cypress.Foreign
import Cypress.Chai (LengthOf(..), Not(..))
import Cypress.Elements

amazon :: CypressM Unit
amazon = do
  visit "/"
  void $ get { action: Selector "a.a-carousel-goto-prevpage", options: Nothing }
      -- ~ should (LengthOf 1 :: LengthOf Elements)
    ~ should (Not $ LengthOf 2 :: Not (LengthOf Elements))
    ~ first
    ~ click
  void $ get { action: Selector "#twotabsearchtextbox", options: Nothing }
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
