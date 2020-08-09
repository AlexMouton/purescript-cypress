module Cypress where

import Prelude
-- import Data.Functor (class Functor)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1, EffectFn2, runEffectFn2, EffectFn3, runEffectFn3)
import Foreign (Foreign)
import Control.Monad.Reader.Trans (runReaderT, ReaderT(..), ask)

foreign import data Cy :: Type

newtype Elements = Elements Foreign
newtype Document = Document Foreign
newtype Window = Window Foreign
newtype Clock = Clock Foreign

data Query a = Query a

type Exec = Foreign
-- {
--   code: 0,
--   stdout: "Files successfully built",
--   stderr: ""
-- }

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


type CypressM = ReaderT Cy Effect

infixl 1 bind as ~

runCypress :: forall a. CypressM a -> Cy -> Effect a
runCypress = runReaderT


askC1 :: forall b a. (EffectFn1 b a) -> ReaderT b Effect a
askC1 f = ReaderT $ do
  cy <- ask
  pure $ (runEffectFn1 f) cy

askC2 :: forall s a b. (EffectFn2 s b a) -> s -> ReaderT b Effect a
askC2 f s = ReaderT $ do
  cy <- ask
  pure $ (runEffectFn2 f) s cy

askC3 :: forall s t a b. (EffectFn3 s t b a) -> s -> t -> ReaderT b Effect a
askC3 f s t = ReaderT $ do
  cy <- ask
  pure $ (runEffectFn3 f) s t cy


naskC1 :: forall s a b. (EffectFn1 s a) -> s -> ReaderT b Effect a
naskC1 f s = ReaderT $ do
  pure $ (runEffectFn1 f) s

naskC2 :: forall s t a b. (EffectFn2 s t a) -> s -> t -> ReaderT b Effect a
naskC2 f s t = ReaderT $ do
  pure $ (runEffectFn2 f) s t

naskC3 :: forall s t u a b. (EffectFn3 s t u a) -> s -> t -> u -> ReaderT b Effect a
naskC3 f s t u = ReaderT $ do
  pure $ (runEffectFn3 f) s t u


foreign import andFn :: forall a. EffectFn3 String Int (Query a) (Query a)
and :: forall a. String -> Int -> Query a -> CypressM (Query a)
and = naskC3 andFn

foreign import asFn :: forall a. EffectFn2 String (Query a) (Query a)
as :: forall a. String -> (Query a) -> CypressM (Query a)
as = naskC2 asFn

foreign import blurFn :: EffectFn1 (Query Elements) (Query Elements)
blur :: (Query Elements) -> CypressM (Query Elements)
blur = naskC1 blurFn

foreign import checkFn :: EffectFn1 (Query Elements) (Query Elements)
check :: (Query Elements) -> CypressM (Query Elements)
check = naskC1 checkFn

foreign import childrenFn :: EffectFn2 String (Query Elements) (Query Elements)
children :: String -> (Query Elements) -> CypressM (Query Elements)
children = naskC2 childrenFn

foreign import clearFn :: EffectFn1 (Query Elements) (Query Elements)
clear :: (Query Elements) -> CypressM (Query Elements)
clear = naskC1 clearFn

foreign import clearCookieFn :: EffectFn2 String Cy Unit
clearCookie :: String -> CypressM Unit
clearCookie = askC2 clearCookieFn

foreign import clearCookiesFn :: EffectFn1 Cy Unit
clearCookies :: CypressM Unit
clearCookies = askC1 clearCookiesFn

foreign import clearLocalStorageFn :: EffectFn2 String Cy Unit
clearLocalStorage :: String -> CypressM Unit
clearLocalStorage = askC2 clearLocalStorageFn

foreign import clickFn :: EffectFn1 (Query Elements) (Query Elements)
click :: Query Elements -> CypressM (Query Elements)
click = naskC1 clickFn

foreign import clockFn :: EffectFn1 Cy (Clock)
clock :: CypressM Clock
clock = askC1 clockFn

foreign import closestFn :: EffectFn2 String (Query Elements) (Query Elements)
closest :: String -> Query Elements -> CypressM (Query Elements)
closest = naskC2 closestFn

foreign import containsFn :: EffectFn2 String (Query Elements) (Query Elements)
contains :: String -> Query Elements -> CypressM (Query Elements)
contains = naskC2 containsFn

foreign import dblclickFn :: EffectFn1 (Query Elements) (Query Elements)
dblclick :: Query Elements -> CypressM (Query Elements)
dblclick = naskC1 dblclickFn

foreign import debugFn :: EffectFn1 Cy Unit
debug :: CypressM Unit
debug = askC1 debugFn

foreign import documentFn :: EffectFn1 Cy (Query Document)
document :: CypressM (Query Document)
document = askC1 documentFn

-- foreign import eachFn :: EffectFn2 String Cy Unit
-- each :: String -> CypressM Unit
-- each = askC2 eachFn

foreign import endFn :: forall a. EffectFn1 (Query a) Unit
end :: forall a. Query a -> CypressM Unit
end = naskC1 endFn

foreign import eqFn :: EffectFn2 Int (Query Elements) (Query Elements)
eq :: Int -> Query Elements -> CypressM (Query Elements)
eq = naskC2 eqFn

foreign import execFn :: EffectFn2 String Cy Exec
exec :: String -> CypressM Exec
exec = askC2 execFn

foreign import filterFn :: EffectFn2 String (Query Elements) (Query Elements)
filter :: String -> Query Elements -> CypressM (Query Elements)
filter = naskC2 filterFn

foreign import findFn :: EffectFn2 String (Query Elements) (Query Elements)
find :: String -> (Query Elements) -> CypressM (Query Elements)
find = naskC2 findFn

foreign import firstFn :: forall a. EffectFn1 (Query a) (Query a)
first :: forall a. (Query a) -> CypressM (Query a)
first = naskC1 firstFn

foreign import fixtureFn :: EffectFn2 String Cy Foreign
fixture :: String -> CypressM Foreign
fixture = askC2 fixtureFn

foreign import focusFn :: EffectFn1 (Query Elements) (Query Elements)
focus :: Query Elements -> CypressM (Query Elements)
focus = naskC1 focusFn

foreign import focusedFn :: EffectFn1 Cy (Query Elements)
focused :: CypressM (Query Elements)
focused = askC1 focusedFn

foreign import getFn :: EffectFn2 String Cy (Query Elements)
get :: String -> CypressM (Query Elements)
get = askC2 getFn

foreign import getCookieFn :: EffectFn2 String Cy String
getCookie :: String -> CypressM String
getCookie = askC2 getCookieFn

foreign import getCookiesFn :: EffectFn1 Cy String
getCookies :: CypressM String
getCookies = askC1 getCookiesFn

foreign import goFn :: EffectFn2 String Cy Unit
go :: String -> CypressM Unit
go = askC2 goFn

foreign import hashFn :: EffectFn1 Cy String
hash :: CypressM String
hash = askC1 hashFn

foreign import hoverFn :: EffectFn1 (Query Elements) (Query Elements)
hover :: (Query Elements) -> CypressM (Query Elements)
hover = naskC1 hoverFn

-- foreign import invokeFn :: EffectFn2 String Cy Unit
-- invoke :: String -> CypressM Unit
-- invoke = askC2 invokeFn

-- foreign import itsFn :: EffectFn2 String Query Foreign
-- its :: String -> Query Elements -> CypressM String
-- its = askC2 itsFn

foreign import lastFn :: EffectFn1 (Query Elements) (Query Elements)
last :: (Query Elements) -> CypressM (Query Elements)
last = naskC1 lastFn

foreign import locationFn :: EffectFn1 Cy Location
location :: CypressM Location
location = askC1 locationFn

foreign import logFn :: EffectFn2 String Cy Unit
log :: String -> CypressM Unit
log = askC2 logFn

foreign import nextFn :: EffectFn1 (Query Elements) (Query Elements)
next :: (Query Elements) -> CypressM (Query Elements)
next = naskC1 nextFn

foreign import nextAllFn :: EffectFn1 (Query Elements) (Query Elements)
nextAll :: (Query Elements) -> CypressM (Query Elements)
nextAll = naskC1 nextAllFn

foreign import nextUntilFn :: EffectFn2 String (Query Elements) (Query Elements)
nextUntil :: String -> (Query Elements) -> CypressM (Query Elements)
nextUntil = naskC2 nextUntilFn

foreign import notFn :: EffectFn2 String (Query Elements) (Query Elements)
not :: String -> (Query Elements) -> CypressM (Query Elements)
not = naskC2 notFn

foreign import parentFn :: EffectFn1 (Query Elements) (Query Elements)
parent :: (Query Elements) -> CypressM (Query Elements)
parent = naskC1 parentFn

foreign import parentsFn :: EffectFn1 (Query Elements) (Query Elements)
parents :: (Query Elements) -> CypressM (Query Elements)
parents = naskC1 parentsFn

foreign import parentsUntilFn :: EffectFn2 String (Query Elements) (Query Elements)
parentsUntil :: String -> (Query Elements) -> CypressM (Query Elements)
parentsUntil = naskC2 parentsUntilFn

foreign import pauseFn :: EffectFn1 Cy Unit
pause :: CypressM Unit
pause = askC1 pauseFn

foreign import prevFn :: EffectFn1 (Query Elements) (Query Elements)
prev :: (Query Elements) -> CypressM (Query Elements)
prev = naskC1 prevFn

foreign import prevAllFn :: EffectFn1 (Query Elements) (Query Elements)
prevAll :: (Query Elements) -> CypressM (Query Elements)
prevAll = naskC1 prevAllFn

foreign import prevUntilFn :: EffectFn2 String (Query Elements) (Query Elements)
prevUntil :: String -> (Query Elements) -> CypressM (Query Elements)
prevUntil = naskC2 prevUntilFn

foreign import readFileFn :: EffectFn2 String Cy Foreign
readFile :: String -> CypressM Foreign
readFile = askC2 readFileFn

foreign import reloadFn :: EffectFn1 Cy Unit
reload :: CypressM Unit
reload = askC1 reloadFn

-- foreign import requestFn :: EffectFn2 String Cy Unit
-- request :: String -> CypressM Unit
-- request = askC2 requestFn

foreign import rightclickFn :: EffectFn1 (Query Elements) (Query Elements)
rightclick :: Query Elements -> CypressM (Query Elements)
rightclick = naskC1 rightclickFn

-- foreign import rootFn :: EffectFn1 Cy Query
-- root :: CypressM Query
-- root = askC1 rootFn

-- foreign import routeFn :: EffectFn2 String Cy Unit
-- route :: String -> CypressM Unit
-- route = askC2 routeFn

foreign import screenshotFn :: EffectFn1 Cy Unit
screenshot :: CypressM Unit
screenshot = askC1 screenshotFn

foreign import scrollIntoViewFn :: EffectFn1 (Query Elements) (Query Elements)
scrollIntoView :: (Query Elements) -> CypressM (Query Elements)
scrollIntoView = naskC1 scrollIntoViewFn

-- foreign import scrollToFn :: EffectFn2 String Cy Unit
-- scrollTo :: String -> CypressM Unit
-- scrollTo = askC2 scrollToFn

foreign import selectFn :: EffectFn2 String (Query Elements) (Query Elements)
select :: String -> (Query Elements) -> CypressM (Query Elements)
select = naskC2 selectFn

-- foreign import serverFn :: EffectFn2 String Cy Unit
-- server :: String -> CypressM Unit
-- server = askC2 serverFn

foreign import setCookieFn :: EffectFn3 String String Cy Cookie
setCookie :: String -> String -> CypressM Cookie
setCookie = askC3 setCookieFn

foreign import shouldFn :: forall a. EffectFn3 String Int (Query a) (Query a)
should :: forall a. String -> Int -> Query a -> CypressM (Query a)
should = naskC3 shouldFn

foreign import siblingsFn :: EffectFn1 (Query Elements) (Query Elements)
siblings :: (Query Elements) -> CypressM (Query Elements)
siblings = naskC1 siblingsFn

-- foreign import spreadFn :: EffectFn2 String Cy Unit
-- spread :: String -> CypressM Unit
-- spread = askC2 spreadFn

-- foreign import spyFn :: EffectFn2 String Cy Unit
-- spy :: String -> CypressM Unit
-- spy = askC2 spyFn

-- foreign import stubFn :: EffectFn2 String Cy Unit
-- stub :: String -> CypressM Unit
-- stub = askC2 stubFn

foreign import submitFn :: EffectFn1 (Query Elements) (Query Elements)
submit :: (Query Elements) -> CypressM (Query Elements)
submit = naskC1 submitFn

-- foreign import taskFn :: EffectFn2 String Cy Unit
-- task :: String -> CypressM Unit
-- task = askC2 taskFn

foreign import thenFn :: forall a b. EffectFn2 (a -> b) a b
thn :: forall a b. (a -> b) -> a -> CypressM b
thn = naskC2 thenFn

foreign import tickFn :: EffectFn2 Int Cy Clock
tick :: Int -> CypressM Clock
tick = askC2 tickFn

foreign import titleFn :: EffectFn1 Cy (Query String)
title :: CypressM (Query String)
title = askC1 titleFn

foreign import triggerFn :: EffectFn2 String (Query Elements) (Query Elements)
trigger :: String -> (Query Elements) -> CypressM (Query Elements)
trigger = naskC2 triggerFn

foreign import typeFn :: EffectFn2 String (Query Elements) (Query Elements)
typ :: String -> (Query Elements) -> CypressM (Query Elements)
typ = naskC2 typeFn

foreign import uncheckFn :: EffectFn1 (Query Elements) (Query Elements)
uncheck :: (Query Elements) -> CypressM (Query Elements)
uncheck = naskC1 uncheckFn

foreign import urlFn :: EffectFn1 Cy String
url :: CypressM String
url = askC1 urlFn

foreign import viewportFn :: EffectFn3 Int Int Cy Unit
viewport :: Int -> Int -> CypressM Unit
viewport = askC3 viewportFn

foreign import visitFn :: EffectFn2 String Cy Unit
visit :: String -> CypressM Unit
visit = askC2 visitFn

foreign import waitFn :: EffectFn2 Int Cy Unit
wait :: Int -> CypressM Unit
wait = askC2 waitFn

foreign import windowFn :: EffectFn1 Cy Window
window :: CypressM Window
window = askC1 windowFn


foreign import withinFn :: forall a b. EffectFn3 ((Query a) -> (Query b)) (Query a) Cy (Query b)
within :: forall a b. ((Query a) -> (Query b)) -> (Query a) -> CypressM (Query b)
within = askC3 withinFn

foreign import wrapFn :: forall a. EffectFn2 a Cy (Query a)
wrap :: forall a. a -> CypressM (Query a)
wrap = askC2 wrapFn

-- foreign import writeFileFn :: EffectFn2 String Cy Unit
-- writeFile :: String -> CypressM Unit
-- writeFile = askC2 writeFileFn

