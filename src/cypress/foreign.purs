module Cypress.Foreign where

import Prelude
-- import Data.Functor (class Functor)
import Foreign (Foreign)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, EffectFn5)
import Data.Maybe (Maybe)

import Cypress.Ask (naskC1)
import Cypress.Query (Query)
import Cypress.Elements (Elements)
import Cypress (Cy, CypressM)

newtype Clock = Clock Foreign

type Document = Foreign
type Window = Foreign
type Location = Foreign
type Cookie = Foreign
-- {
--     name
--     value
--     path
--     domain
--     httpOnly
--     secure
--     expiry
--     sameSite (will only be returned if the experimentalGetCookiesSameSite configuration value is true)
-- }

foreign import andFn :: forall a. EffectFn3 String Int (Query a) (Query a)

foreign import asFn :: forall a. EffectFn2 String (Query a) (Query a)

foreign import blurFn :: EffectFn1 (Query Elements) (Query Elements)

foreign import checkFn :: EffectFn1 (Query Elements) (Query Elements)

foreign import childrenFn :: EffectFn2 String (Query Elements) (Query Elements)

foreign import clearFn :: EffectFn1 (Query Elements) (Query Elements)

-- root
foreign import clearCookieFn :: EffectFn2 String Cy Unit

-- root
foreign import clearCookiesFn :: EffectFn1 Cy Unit

-- root
foreign import clearLocalStorageFn :: EffectFn2 String Cy Unit

foreign import clickFn :: EffectFn1 (Query Elements) (Query Elements)

-- root
foreign import clockFn :: EffectFn1 Cy (Clock)

foreign import closestFn :: EffectFn2 String (Query Elements) (Query Elements)

-- Contains [both]
type ContainsOptions = {
  matchCase :: Boolean, -- true 	Check case sensitivity
  log :: Boolean,  -- true 	Displays the command in the Command log
  timeout :: Int -- defaultCommandTimeout 	Time to wait for .contains() to resolve before timing outy
}
type ContainsProps = { content :: String, selector :: Maybe String, options :: Maybe ContainsOptions }
foreign import containsFn :: forall a. EffectFn4 (Maybe a -> Boolean) (Maybe a -> a) ContainsProps Cy (Query Elements)
foreign import containsqFn :: forall a. EffectFn5 (Maybe a -> Boolean) (Maybe a -> a) ContainsProps (Query Elements) Cy (Query Elements)

foreign import dblclickFn :: EffectFn1 (Query Elements) (Query Elements)

-- both
foreign import debugFn :: EffectFn1 Cy Unit

-- root
foreign import documentFn :: EffectFn1 Cy (Query Document)

-- foreign import eachFn :: EffectFn2 String Cy Unit

foreign import endFn :: forall a. EffectFn1 (Query a) Unit

foreign import eqFn :: EffectFn2 Int (Query Elements) (Query Elements)

-- root
type ResultExec =
  { code :: Int
  , stdout :: String
  , stderr :: String
  }

foreign import execFn :: EffectFn2 String Cy ResultExec

foreign import filterFn :: EffectFn2 String (Query Elements) (Query Elements)

foreign import findFn :: EffectFn2 String (Query Elements) (Query Elements)

foreign import firstFn :: forall a. EffectFn1 (Query a) (Query a)

--  root
foreign import fixtureFn :: EffectFn2 String Cy Foreign

foreign import focusFn :: EffectFn1 (Query Elements) (Query Elements)

-- root
foreign import focusedFn :: EffectFn1 Cy (Query Elements)

-- root
type GetOptions =
  { log :: Maybe Boolean -- true 	Displays the command in the Command log
  , timeout :: Maybe Int --  defaultCommandTimeout 	Time to wait for cy.get() to resolve before timing out
  , withinSubject :: Maybe String -- null 	Element to search for children in. If null, search begins from root-level DOM element
  }

data GetAction = Selector String | Alias String

type GetProps = { action :: GetAction, options :: Maybe GetOptions }

foreign import getFn :: forall a. EffectFn5 (Maybe a -> Boolean) (Maybe a -> a) (GetAction -> String) GetProps Cy (Query Elements)

--  root
foreign import getCookieFn :: EffectFn2 String Cy String

--  root
foreign import getCookiesFn :: EffectFn1 Cy String

--  root
foreign import goFn :: EffectFn2 String Cy Unit

--root
foreign import hashFn :: EffectFn1 Cy String

--  'cy doesnt have hover'
-- foreign import hoverFn :: EffectFn1 (Query Elements) (Query Elements)

-- foreign import invokeFn :: EffectFn2 String Cy Unit

-- foreign import itsFn :: EffectFn2 String Query Foreign

foreign import lastFn :: EffectFn1 (Query Elements) (Query Elements)
last :: (Query Elements) -> CypressM (Query Elements)
last = naskC1 lastFn

-- root
foreign import locationFn :: EffectFn1 Cy Location

-- root
foreign import logFn :: EffectFn2 String Cy Unit

foreign import nextFn :: EffectFn1 (Query Elements) (Query Elements)

foreign import nextAllFn :: EffectFn1 (Query Elements) (Query Elements)

foreign import nextUntilFn :: EffectFn2 String (Query Elements) (Query Elements)

foreign import notFn :: EffectFn2 String (Query Elements) (Query Elements)

foreign import parentFn :: EffectFn1 (Query Elements) (Query Elements)

foreign import parentsFn :: EffectFn1 (Query Elements) (Query Elements)

foreign import parentsUntilFn :: EffectFn2 String (Query Elements) (Query Elements)

--  both
foreign import pauseFn :: EffectFn1 Cy Unit

foreign import prevFn :: EffectFn1 (Query Elements) (Query Elements)

foreign import prevAllFn :: EffectFn1 (Query Elements) (Query Elements)

foreign import prevUntilFn :: EffectFn2 String (Query Elements) (Query Elements)

-- root
foreign import readFileFn :: EffectFn2 String Cy Foreign

-- root
foreign import reloadFn :: EffectFn1 Cy Unit

-- root
-- foreign import requestFn :: EffectFn2 String Cy Unit

foreign import rightclickFn :: EffectFn1 (Query Elements) (Query Elements)

-- root
-- foreign import rootFn :: EffectFn1 Cy Query

-- root
-- foreign import routeFn :: EffectFn2 String Cy Unit

-- both
foreign import screenshotFn :: EffectFn1 Cy Unit

foreign import scrollIntoViewFn :: EffectFn1 (Query Elements) (Query Elements)

-- both
-- foreign import scrollToFn :: EffectFn2 String Cy Unit

foreign import selectFn :: EffectFn2 (Array String) (Query Elements) (Query Elements)

-- root
-- foreign import serverFn :: EffectFn2 String Cy Unit

-- root
foreign import setCookieFn :: EffectFn3 String String Cy Cookie

foreign import should0Fn :: forall a. EffectFn2 String (Query a) (Query a)
foreign import should1Fn :: forall a b. EffectFn3 String b (Query a) (Query a)
foreign import should2Fn :: forall a b. EffectFn4 String b b (Query a) (Query a)

foreign import siblingsFn :: EffectFn1 (Query Elements) (Query Elements)

-- foreign import spreadFn :: EffectFn2 String Cy Unit

-- root
-- foreign import spyFn :: EffectFn2 String Cy Unit

-- root
-- foreign import stubFn :: EffectFn2 String Cy Unit

-- must be form
foreign import submitFn :: EffectFn1 (Query Elements) (Query Elements)

-- root
-- foreign import taskFn :: EffectFn2 String Cy Unit

foreign import thenFn :: forall a b. EffectFn2 (a -> b) a b

-- root
foreign import tickFn :: EffectFn2 Int Cy Clock

-- root
foreign import titleFn :: EffectFn1 Cy (Query String)

foreign import triggerFn :: EffectFn2 String (Query Elements) (Query Elements)

foreign import typeFn :: EffectFn2 String (Query Elements) (Query Elements)

foreign import uncheckFn :: EffectFn1 (Query Elements) (Query Elements)

--  root
foreign import urlFn :: EffectFn1 Cy String

-- root
foreign import viewportFn :: EffectFn3 Int Int Cy Unit

-- root
foreign import visitFn :: EffectFn2 String Cy Unit

-- root
foreign import waitFn :: EffectFn2 Int Cy Unit

-- root
foreign import windowFn :: EffectFn1 Cy Window

foreign import withinFn :: forall a b. EffectFn3 ((Query a) -> (Query b)) (Query a) Cy (Query b)

-- root
foreign import wrapFn :: forall a. EffectFn2 a Cy (Query a)

-- root
-- foreign import writeFileFn :: EffectFn2 String Cy Unit

-- root
foreign import xpathFn :: forall a. EffectFn5 (Maybe a -> Boolean) (Maybe a -> a) String GetOptions Cy (Query Elements)

