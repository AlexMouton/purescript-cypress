module Cypress.Promise where

import Cypress.Cy
import Foreign
import Prelude

import Control.Promise (Promise)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, EffectFn5, EffectFn6, EffectFn7)

type EffectFnP1 a r = EffectFn1 a (Promise r)
type EffectFnP2 a b r = EffectFn2 a b (Promise r)
type EffectFnP3 a b c r = EffectFn3 a b c (Promise r)
type EffectFnP4 a b c d r = EffectFn4 a b c d (Promise r)
type EffectFnP5 a b c d e r = EffectFn5 a b c d e (Promise r)
type EffectFnP6 a b c d e f r = EffectFn6 a b c d e f (Promise r)
type EffectFnP7 a b c d e f g r = EffectFn7 a b c d e f g (Promise r)
