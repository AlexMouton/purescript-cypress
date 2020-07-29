module Main where

import Prelude
import Foreign (Foreign)

import Effect (Effect)
import Cypress (get, go, should, typ, runCypress, visit, first, click)

main :: Foreign -> Effect Unit
main = runCypress $ do
  visit "/"
  _ <- get "a.a-carousel-goto-prevpage"
       >>= should "have.length" 1
       >>= first
       >>= click
  _ <- get "#twotabsearchtextbox" >>= typ "HEHEHE"
  visit "/dp/B07X6C9RMF/ref=ods_gw_vicc_blinkmini_apr_anc"
  go "back"
