module Cypress.Actions where

import Prelude
import Data.Maybe
import Data.Maybe as M
import Foreign
import Partial.Unsafe (unsafePartial)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, EffectFn5)
import Data.Maybe (Maybe)

import Cypress.Ask
import Cypress.Chai
import Cypress.Cy
import Cypress.Elements
import Cypress.Query
import Cypress.Promise

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

type OptionsLft =
  { log :: Boolean
  , force :: Boolean
  , timeout :: Int
  }

type OptionsLf =
  { log :: Boolean
  , force :: Boolean
  }

type OptionsLt =
  { log :: Boolean
  , timeout :: Int
  }

type OptionsL =
  { log :: Boolean
  }


fromJust :: forall a. Maybe a -> a
fromJust = unsafePartial M.fromJust

type IsJust a = (Maybe a -> Boolean)
type FromJust a = (Maybe a -> a)

-- foreign import andFn :: forall a. EffectFnP4 String Int a Cy a
-- and :: forall a. String -> Int -> a -> CypressM a
-- and = askC4 andFn

-- foreign import asFn :: forall a. EffectFnP3 String a Cy a
-- as :: forall a. String -> a -> CypressM a
-- as = askC3 asFn

foreign import blurFn :: forall a. EffectFnP5 (IsJust a) (FromJust a) (Maybe OptionsLft) Elements Cy Elements
blurOpt :: Maybe OptionsLft -> Elements -> CypressM Elements
blurOpt = askC5 blurFn isJust fromJust

blur :: Elements -> CypressM Elements
blur = blurOpt Nothing


foreign import checkValsFn :: forall a. EffectFnP6 (IsJust a) (FromJust a) (Array String) (Maybe OptionsLft) Elements Cy Elements
checkValsOpt :: Array String -> (Maybe OptionsLft) -> Elements -> CypressM Elements
checkValsOpt = askC6 checkValsFn isJust fromJust

checkVals a = checkValsOpt a Nothing

checkValOpt :: String -> (Maybe OptionsLft) -> Elements -> CypressM Elements
checkValOpt a = checkValsOpt [a]

checkVal a = checkValOpt a Nothing


foreign import checkFn :: forall a. EffectFnP5 (IsJust a) (FromJust a) (Maybe OptionsLft) Elements Cy Elements
checkOpt :: Maybe OptionsLft -> Elements -> CypressM Elements
checkOpt = askC5 checkFn isJust fromJust

check = checkOpt Nothing


foreign import childrenFn :: forall a. EffectFnP6 (IsJust a) (FromJust a) (Maybe String) (Maybe OptionsLf) Elements Cy Elements
childrenOpt :: Maybe String -> Maybe OptionsLf -> Elements -> CypressM Elements
childrenOpt =  askC6 childrenFn isJust fromJust

children = childrenOpt Nothing Nothing


foreign import clearFn :: forall a. EffectFnP5 (IsJust a) (FromJust a) (Maybe OptionsLft) Elements Cy Elements
clearOpt :: Maybe OptionsLft -> Elements -> CypressM Elements
clearOpt = askC5 clearFn isJust fromJust

clear = clearOpt Nothing


-- root
foreign import clearCookieFn :: forall a. EffectFnP5 (IsJust a) (FromJust a) String (Maybe OptionsLt) Cy Unit
clearCookieOpt :: String -> (Maybe OptionsLt) -> CypressM Unit
clearCookieOpt = askC5 clearCookieFn isJust fromJust

clearCookie :: String -> CypressM Unit
clearCookie s = clearCookieOpt s Nothing


-- root
foreign import clearCookiesFn :: forall a. EffectFnP4 (IsJust a) (FromJust a) (Maybe OptionsLt) Cy Unit
clearCookiesOpt :: (Maybe OptionsLt) -> CypressM Unit
clearCookiesOpt = askC4 clearCookiesFn isJust fromJust

clearCookies :: CypressM Unit
clearCookies = clearCookiesOpt Nothing



-- root
-- TODO: regex
foreign import clearLocalStorageFn :: forall a. EffectFnP5 (IsJust a) (FromJust a) String (Maybe OptionsL) Cy Unit
clearLocalStorageOpt :: String -> (Maybe OptionsL) -> CypressM Unit
clearLocalStorageOpt = askC5 clearLocalStorageFn isJust fromJust

clearLocalStorage :: String -> CypressM Unit
clearLocalStorage s = clearLocalStorageOpt s Nothing


type  ClickOptions =
  { altKey :: Boolean
  , ctrlKey :: Boolean
  , log :: Boolean
  , force :: Boolean
  , metaKey :: Boolean
  , multiple :: Boolean
  , shiftKey :: Boolean
  , timeout :: Number
  }

data Position = TopLeft | Top | TopRight | Left | Center | Right | BottomLeft | Bottom | BottomRight

positionToString :: Position -> String
positionToString = case _ of
  TopLeft -> "topLeft"
  Top -> "top"
  TopRight -> "topRight"
  Left -> "left"
  Center -> "center"
  Right -> "right"
  BottomLeft -> "bottomLeft"
  Bottom -> "bottom"
  BottomRight -> "bottomRight"

foreign import clickFn :: forall a. EffectFnP5 (IsJust a) (FromJust a) (Maybe ClickOptions) Elements Cy Elements
clickOpt :: Maybe ClickOptions -> Elements -> CypressM Elements
clickOpt =  askC5 clickFn isJust fromJust

click :: Elements -> CypressM Elements
click = clickOpt Nothing

foreign import clickXyFn :: forall a. EffectFnP7 (IsJust a) (FromJust a) Number Number (Maybe ClickOptions) Elements Cy Elements
clickXyOpt :: Number -> Number -> Maybe ClickOptions -> Elements -> CypressM Elements
clickXyOpt = askC7 clickXyFn isJust fromJust

clickXy :: Number -> Number -> Elements -> CypressM Elements
clickXy x y = clickXyOpt x y Nothing


foreign import clickPosFn :: forall a. EffectFnP6 (IsJust a) (FromJust a) String (Maybe ClickOptions) Elements Cy Elements
clickPosOpt :: Position -> Maybe ClickOptions -> Elements -> CypressM Elements
clickPosOpt p = askC6 clickPosFn isJust fromJust (positionToString p)

clickPos :: Position -> Elements -> CypressM Elements
clickPos p = clickPosOpt p Nothing


-- root
foreign import clockFn :: EffectFnP1 Cy Clock
clock :: CypressM Clock
clock = askC1 clockFn


foreign import closestFn :: EffectFnP3 String Elements Cy Elements
closest :: String -> Elements -> CypressM Elements
closest = askC3 closestFn


-- Contains [both]
type ContainsOptions = {
  matchCase :: Boolean, -- true 	Check case sensitivity
  log :: Boolean,  -- true 	Displays the command in the Command log
  timeout :: Int -- defaultCommandTimeout 	Time to wait for .contains() to resolve before timing outy
}

type ContainsProps = { content :: String, selector :: Maybe String, options :: Maybe ContainsOptions }

foreign import containsFn :: forall a. EffectFnP4 (IsJust a) (FromJust a) ContainsProps Cy Elements
foreign import containsqFn :: forall a. EffectFnP5 (IsJust a) (FromJust a) ContainsProps Elements Cy Elements

contains :: String -> CypressM Elements
contains s = containsOpt { selector: Nothing, content: s, options: Nothing }

containsSelector :: String -> String -> CypressM Elements
containsSelector a b = containsOpt { selector: Just a, content: b, options: Nothing }

containsOpt :: ContainsProps -> CypressM Elements
containsOpt = askC4 containsFn isJust fromJust

containsq :: String -> Elements -> CypressM Elements
containsq a = containsqOpt { selector: Nothing, content: a, options: Nothing }

containsqSelector :: String -> String -> Elements -> CypressM Elements
containsqSelector a b = containsqOpt { selector: Just a, content: b, options: Nothing }

containsqOpt :: ContainsProps -> Elements -> CypressM Elements
containsqOpt = askC5 containsqFn isJust fromJust

foreign import dblclickFn :: EffectFnP2 Elements Cy Elements
dblclick :: Elements -> CypressM Elements
dblclick = askC2 dblclickFn

-- both
foreign import debugFn :: forall a. EffectFnP2 a Cy Unit
debug :: forall a. a -> CypressM Unit
debug = askC2 debugFn

-- root
foreign import documentFn :: EffectFnP1 Cy Document
document :: CypressM Document
document = askC1 documentFn

-- foreign import eachFn :: EffectFnP2 String Cy Unit
-- each :: String -> CypressM Unit
-- each = askC2 eachFn

-- foreign import endFn :: forall a. EffectFnP2 a Cy Unit
-- end :: forall a. a -> CypressM Unit
-- end = askC2 endFn

-- foreign import eqFn :: EffectFnP2 Int Elements Elements
-- eq :: Int -> Elements -> CypressM Elements
-- eq = askC2 eqFn

-- root
type ResultExec =
  { code :: Int
  , stdout :: String
  , stderr :: String
  }

foreign import execFn :: EffectFnP2 String Cy ResultExec
exec :: String -> CypressM ResultExec
exec = askC2 execFn

foreign import filterFn :: EffectFnP3 String Elements Cy Elements
filter :: String -> Elements -> CypressM Elements
filter = askC3 filterFn

foreign import findFn :: EffectFnP3 String Elements Cy Elements
find :: String -> Elements -> CypressM Elements
find = askC3 findFn

foreign import firstFn :: forall a. EffectFnP2 a Cy a
first :: forall a. a -> CypressM a
first = askC2 firstFn

--  root
foreign import fixtureFn :: EffectFnP2 String Cy Foreign
fixture :: String -> CypressM Foreign
fixture = askC2 fixtureFn

foreign import focusFn :: EffectFnP2 Elements Cy Elements
focus :: Elements -> CypressM Elements
focus = askC2 focusFn

-- root
foreign import focusedFn :: EffectFnP1 Cy Elements
focused :: CypressM Elements
focused = askC1 focusedFn

-- root
type GetOptions =
  { log :: Maybe Boolean -- true 	Displays the command in the Command log
  , timeout :: Maybe Int --  defaultCommandTimeout 	Time to wait for cy.get() to resolve before timing out
  , withinSubject :: Maybe String -- null 	Element to search for children in. If null, search begins from root-level DOM element
  }

data GetAction = Selector String | Alias String

type GetProps = { action :: GetAction, options :: Maybe GetOptions }

foreign import getFn :: forall a. EffectFnP5 (IsJust a) (FromJust a) (GetAction -> String) GetProps Cy Elements

actionString :: GetAction -> String
actionString = case _ of
  Selector a -> a
  Alias a -> "@" <> a

getOpt :: GetProps -> CypressM Elements
getOpt = askC5 getFn isJust fromJust actionString

get :: String -> CypressM Elements
get s = getOpt { action: Selector s, options: Nothing }

alias :: String -> CypressM Elements
alias s = getOpt { action: Alias s, options: Nothing }

--  root
foreign import getCookieFn :: EffectFnP2 String Cy String
getCookie :: String -> CypressM String
getCookie = askC2 getCookieFn

--  root
foreign import getCookiesFn :: EffectFnP1 Cy Foreign
getCookies :: CypressM Foreign
getCookies = askC1 getCookiesFn

--  root

foreign import goFn :: EffectFnP2 String Cy Unit
go :: String -> CypressM Unit
go = askC2 goFn

back :: CypressM Unit
back = go "back"

forward :: CypressM Unit
forward = go "forward"

--root
foreign import hashFn :: EffectFnP1 Cy String
hash :: CypressM String
hash = askC1 hashFn

--  'cy doesnt have hover'
-- foreign import hoverFn :: EffectFnP1 Elements Elements
-- hover :: Elements -> CypressM Elements
-- hover = askC1 hoverFn

-- foreign import invokeFn :: EffectFnP2 String Cy Unit
-- invoke :: String -> CypressM Unit
-- invoke = askC2 invokeFn

-- foreign import itsFn :: EffectFnP2 String Query Foreign
-- its :: String -> Elements -> CypressM String
-- its = askC2 itsFn

-- root
foreign import locationFn :: EffectFnP1 Cy Location
location :: CypressM Location
location = askC1 locationFn

-- root
foreign import logFn :: EffectFnP2 String Cy Unit
log :: String -> CypressM Unit
log = askC2 logFn

foreign import nextFn :: EffectFnP2 Elements Cy Elements
next :: Elements -> CypressM Elements
next = askC2 nextFn

foreign import nextAllFn :: EffectFnP2 Elements Cy Elements
nextAll :: Elements -> CypressM Elements
nextAll = askC2 nextAllFn

foreign import nextUntilFn :: EffectFnP3 String Elements Cy Elements
nextUntil :: String -> Elements -> CypressM Elements
nextUntil = askC3 nextUntilFn

foreign import notFn :: EffectFnP3 String Elements Cy Elements
not :: String -> Elements -> CypressM Elements
not = askC3 notFn

foreign import parentFn :: EffectFnP2 Elements Cy Elements
parent :: Elements -> CypressM Elements
parent = askC2 parentFn

foreign import parentsFn :: EffectFnP2 Elements Cy Elements
parents :: Elements -> CypressM Elements
parents = askC2 parentsFn

foreign import parentsUntilFn :: EffectFnP3 String Elements Cy Elements
parentsUntil :: String -> Elements -> CypressM Elements
parentsUntil = askC3 parentsUntilFn

--  both
foreign import pauseFn :: EffectFnP1 Cy Unit
pause :: CypressM Unit
pause = askC1 pauseFn

foreign import prevFn :: EffectFnP2 Elements Cy Elements
prev :: Elements -> CypressM Elements
prev = askC2 prevFn

foreign import prevAllFn :: EffectFnP2 Elements Cy Elements
prevAll :: Elements -> CypressM Elements
prevAll = askC2 prevAllFn


foreign import prevUntilFn :: EffectFnP3 String Elements Cy Elements
prevUntil :: String -> Elements -> CypressM Elements
prevUntil = askC3 prevUntilFn

-- root
foreign import readFileFn :: EffectFnP2 String Cy Foreign
readFile :: String -> CypressM Foreign
readFile = askC2 readFileFn

-- root
foreign import reloadFn :: EffectFnP1 Cy Unit
reload :: CypressM Unit
reload = askC1 reloadFn

-- root
-- foreign import requestFn :: EffectFnP2 String Cy Unit
-- request :: String -> CypressM Unit
-- request = askC2 requestFn

foreign import rightclickFn :: EffectFnP2 Elements Cy Elements
rightclick :: Elements -> CypressM Elements
rightclick = askC2 rightclickFn

-- root
-- foreign import rootFn :: EffectFnP1 Cy Query
-- root :: CypressM Query
-- root = askC1 rootFn

-- root
-- foreign import routeFn :: EffectFnP2 String Cy Unit
-- route :: String -> CypressM Unit
-- route = askC2 routeFn

-- both
foreign import screenshotFn :: EffectFnP1 Cy Unit
screenshot :: CypressM Unit
screenshot = askC1 screenshotFn

foreign import scrollIntoViewFn :: EffectFnP2 Elements Cy Elements
scrollIntoView :: Elements -> CypressM Elements
scrollIntoView = askC2 scrollIntoViewFn

-- both
-- foreign import scrollToFn :: EffectFnP2 String Cy Unit
-- scrollTo :: String -> CypressM Unit
-- scrollTo = askC2 scrollToFn

foreign import selectFn :: EffectFnP3 (Array String) Elements Cy Elements
select :: Array String -> Elements -> CypressM Elements
select = askC3 selectFn

-- root
-- foreign import serverFn :: EffectFnP2 String Cy Unit
-- server :: String -> CypressM Unit
-- server = askC2 serverFn

-- root
foreign import setCookieFn :: EffectFnP3 String String Cy Cookie
setCookie :: String -> String -> CypressM Cookie
setCookie = askC3 setCookieFn

should :: forall a b. Should a b => a -> b -> CypressM b
should = toShould

foreign import siblingsFn :: EffectFnP2 Elements Cy Elements
siblings :: Elements -> CypressM Elements
siblings = askC2 siblingsFn

-- foreign import spreadFn :: EffectFnP2 String Cy Unit
-- spread :: String -> CypressM Unit
-- spread = askC2 spreadFn

-- root
-- foreign import spyFn :: EffectFnP2 String Cy Unit
-- spy :: String -> CypressM Unit
-- spy = askC2 spyFn

-- root
-- foreign import stubFn :: EffectFnP2 String Cy Unit
-- stub :: String -> CypressM Unit
-- stub = askC2 stubFn

-- must be form
foreign import submitFn :: EffectFnP2 Elements Cy Elements
submit :: Elements -> CypressM Elements
submit = askC2 submitFn

-- root
-- foreign import taskFn :: EffectFnP2 String Cy Unit
-- task :: String -> CypressM Unit
-- task = askC2 taskFn

-- Then is redundant in Aff
-- foreign import thenFn :: forall a b. EffectFnP3 (a -> b) a Cy b
-- thn :: forall a b. (a -> b) -> a -> CypressM b
-- thn = askC3 thenFn

-- root
foreign import tickFn :: EffectFnP2 Int Cy Clock
tick :: Int -> CypressM Clock
tick = askC2 tickFn

-- root
foreign import titleFn :: EffectFnP1 Cy String
title :: CypressM String
title = askC1 titleFn

foreign import triggerFn :: EffectFnP3 String Elements Cy Elements
trigger :: String -> Elements -> CypressM Elements
trigger = askC3 triggerFn

foreign import typeFn :: EffectFnP3 String Elements Cy Elements
typ :: String -> Elements -> CypressM Elements
typ = askC3 typeFn

foreign import uncheckFn :: EffectFnP2 Elements Cy Elements
uncheck :: Elements -> CypressM Elements
uncheck = askC2 uncheckFn

--  root
foreign import urlFn :: EffectFnP1 Cy String
url :: CypressM String
url = askC1 urlFn

-- root
foreign import viewportFn :: EffectFnP3 Int Int Cy Unit
viewport :: Int -> Int -> CypressM Unit
viewport = askC3 viewportFn

-- root
foreign import visitFn :: EffectFnP2 String Cy Window
visit :: String -> CypressM Window
visit = askC2 visitFn

-- root
foreign import waitFn :: EffectFnP2 Int Cy Unit
wait :: Int -> CypressM Unit
wait = askC2 waitFn

-- root
foreign import windowFn :: EffectFnP1 Cy Window
window :: CypressM Window
window = askC1 windowFn

foreign import withinFn :: forall a b. EffectFnP3 (a -> b) a Cy b
within :: forall a b. (a -> b) -> a -> CypressM b
within = askC3 withinFn

-- root
foreign import wrapFn :: forall a. EffectFnP2 a Cy a
wrap :: forall a. a -> CypressM a
wrap = askC2 wrapFn

-- root
-- foreign import writeFileFn :: EffectFnP2 String Cy Unit
-- writeFile :: String -> CypressM Unit
-- writeFile = askC2 writeFileFn

foreign import xpathFn :: forall a. EffectFnP5 (IsJust a) (FromJust a) String GetOptions Cy Elements
xpath :: String -> CypressM Elements
xpath s = xpathOpt s { log: Just false, timeout: Nothing, withinSubject: Nothing }

xpathOpt :: String -> GetOptions -> CypressM Elements
xpathOpt = askC5 xpathFn isJust fromJust

attachFile :: String -> Elements -> CypressM  Elements
attachFile s = attachFileOpt s Nothing

foreign import attachFileFn :: forall a. EffectFnP6 (IsJust a) (FromJust a) String (Maybe { subjectType :: String }) Elements Cy Elements
attachFileOpt :: String -> Maybe { subjectType :: String } -> Elements -> CypressM  Elements
attachFileOpt = askC6 attachFileFn isJust fromJust


foreign import lastFn :: EffectFnP2 Elements Cy Elements
last :: Elements -> CypressM Elements
last = askC2 lastFn
