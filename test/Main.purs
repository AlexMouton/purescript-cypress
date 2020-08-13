module Test.Main where

import Prelude

import Effect (Effect)
import Cypress (Cy, CypressM, click, exec, first, get, go, runCypress, should, thn, typ, visit, (~))
import Cypress as C

amazon :: CypressM Unit
amazon = do
  visit "/"
  _ <- get "a.a-carousel-goto-prevpage"
    ~ should "have.length" 1
    ~ first
    ~ click
  _ <- get "#twotabsearchtextbox"
    ~ typ "HEHEHE"
  visit "/dp/B07X6C9RMF/ref=ods_gw_vicc_blinkmini_apr_anc"
  go "back"

  exec "ls -la"
    ~ thn _.stdout
    ~ C.log

main :: Cy -> Effect Unit
main cy = do
  runCypress amazon cy
