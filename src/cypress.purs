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

foreign import andFn :: forall a. EffectFn3 String Int (Query a) (Query a)
foreign import asFn :: forall a. EffectFn2 String (Query a) (Query a)
foreign import blurFn :: EffectFn1 (Query Elements) (Query Elements)
foreign import checkFn :: EffectFn1 (Query Elements) (Query Elements)
foreign import childrenFn :: EffectFn2 String (Query Elements) (Query Elements)
foreign import clearFn :: EffectFn1 (Query Elements) (Query Elements)
foreign import clearCookieFn :: EffectFn2 String Cy Unit
foreign import clearCookiesFn :: EffectFn1 Cy Unit
foreign import clearLocalStorageFn :: EffectFn2 String Cy Unit
foreign import clickFn :: EffectFn1 (Query Elements) (Query Elements)
foreign import clockFn :: EffectFn1 Cy (Clock)
foreign import closestFn :: EffectFn2 String (Query Elements) (Query Elements)
foreign import containsFn :: EffectFn2 String (Query Elements) (Query Elements)
foreign import dblclickFn :: EffectFn1 (Query Elements) (Query Elements)
foreign import debugFn :: EffectFn1 Cy Unit
foreign import documentFn :: EffectFn1 Cy (Query Document)
-- foreign import eachFn :: EffectFn2 String Cy Unit
foreign import endFn :: forall a. EffectFn1 (Query a) Unit
foreign import eqFn :: EffectFn2 Int (Query Elements) (Query Elements)
foreign import execFn :: EffectFn2 String Cy Exec
foreign import filterFn :: EffectFn2 String (Query Elements) (Query Elements)
foreign import findFn :: EffectFn2 String (Query Elements) (Query Elements)
foreign import firstFn :: forall a. EffectFn1 (Query a) (Query a)
foreign import fixtureFn :: EffectFn2 String Cy Foreign
foreign import focusFn :: EffectFn1 (Query Elements) (Query Elements)
foreign import focusedFn :: EffectFn1 Cy (Query Elements)
foreign import getFn :: EffectFn2 String Cy (Query Elements)
foreign import getCookieFn :: EffectFn2 String Cy String
foreign import getCookiesFn :: EffectFn1 Cy String
foreign import goFn :: EffectFn2 String Cy Unit
foreign import hashFn :: EffectFn1 Cy String
foreign import hoverFn :: EffectFn1 (Query Elements) (Query Elements)
-- foreign import invokeFn :: EffectFn2 String Cy Unit
-- foreign import itsFn :: EffectFn2 String Query Foreign
foreign import lastFn :: EffectFn1 (Query Elements) (Query Elements)
foreign import locationFn :: EffectFn1 Cy Location
foreign import logFn :: EffectFn2 String Cy Unit
foreign import nextFn :: EffectFn1 (Query Elements) (Query Elements)
foreign import nextAllFn :: EffectFn1 (Query Elements) (Query Elements)
foreign import nextUntilFn :: EffectFn2 String (Query Elements) (Query Elements)
foreign import notFn :: EffectFn2 String (Query Elements) (Query Elements)
foreign import parentFn :: EffectFn1 (Query Elements) (Query Elements)
foreign import parentsFn :: EffectFn1 (Query Elements) (Query Elements)
foreign import parentsUntilFn :: EffectFn2 String (Query Elements) (Query Elements)
foreign import pauseFn :: EffectFn1 Cy Unit
foreign import prevFn :: EffectFn1 (Query Elements) (Query Elements)
foreign import prevAllFn :: EffectFn1 (Query Elements) (Query Elements)
foreign import prevUntilFn :: EffectFn2 String (Query Elements) (Query Elements)
foreign import readFileFn :: EffectFn2 String Cy Foreign
foreign import reloadFn :: EffectFn1 Cy Unit
-- foreign import requestFn :: EffectFn2 String Cy Unit
foreign import rightclickFn :: EffectFn1 (Query Elements) (Query Elements)
-- foreign import rootFn :: EffectFn1 Cy Query
-- foreign import routeFn :: EffectFn2 String Cy Unit
foreign import screenshotFn :: EffectFn1 Cy Unit
foreign import scrollIntoViewFn :: EffectFn1 (Query Elements) (Query Elements)
-- foreign import scrollToFn :: EffectFn2 String Cy Unit
foreign import selectFn :: EffectFn2 String (Query Elements) (Query Elements)
-- foreign import serverFn :: EffectFn2 String Cy Unit
foreign import setCookieFn :: EffectFn3 String String Cy Cookie
foreign import shouldFn :: forall a. EffectFn3 String Int (Query a) (Query a)
foreign import siblingsFn :: EffectFn1 (Query Elements) (Query Elements)
-- foreign import spreadFn :: EffectFn2 String Cy Unit
-- foreign import spyFn :: EffectFn2 String Cy Unit
-- foreign import stubFn :: EffectFn2 String Cy Unit
foreign import submitFn :: EffectFn1 (Query Elements) (Query Elements)
-- foreign import taskFn :: EffectFn2 String Cy Unit
-- foreign import thenFn :: forall a b. EffectFn2 (a -> b) a b
foreign import tickFn :: EffectFn2 Int Cy Clock
foreign import titleFn :: EffectFn1 Cy (Query String)
foreign import triggerFn :: EffectFn2 String (Query Elements) (Query Elements)
foreign import typeFn :: EffectFn2 String (Query Elements) (Query Elements)
foreign import uncheckFn :: EffectFn1 (Query Elements) (Query Elements)
foreign import urlFn :: EffectFn1 Cy String
foreign import viewportFn :: EffectFn3 Int Int Cy Unit
foreign import visitFn :: EffectFn2 String Cy Unit
foreign import waitFn :: EffectFn2 Int Cy Unit
foreign import windowFn :: EffectFn1 Cy Window
foreign import withinFn :: forall a b. EffectFn3 ((Query a) -> (Query b)) (Query a) Cy (Query b)
foreign import wrapFn :: forall a. EffectFn2 a Cy (Query a)
-- foreign import writeFileFn :: EffectFn2 String Cy Unit

type CypressM = ReaderT Cy Effect

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


and :: forall a. String -> Int -> Query a -> CypressM (Query a)
and = naskC3 andFn

as :: forall a. String -> (Query a) -> CypressM (Query a)
as = naskC2 asFn

blur :: (Query Elements) -> CypressM (Query Elements)
blur = naskC1 blurFn

check :: (Query Elements) -> CypressM (Query Elements)
check = naskC1 checkFn

children :: String -> (Query Elements) -> CypressM (Query Elements)
children = naskC2 childrenFn

clear :: (Query Elements) -> CypressM (Query Elements)
clear = naskC1 clearFn

clearCookie :: String -> CypressM Unit
clearCookie = askC2 clearCookieFn

clearCookies :: CypressM Unit
clearCookies = askC1 clearCookiesFn

clearLocalStorage :: String -> CypressM Unit
clearLocalStorage = askC2 clearLocalStorageFn

click :: Query Elements -> CypressM (Query Elements)
click = naskC1 clickFn

clock :: CypressM Clock
clock = askC1 clockFn

closest :: String -> Query Elements -> CypressM (Query Elements)
closest = naskC2 closestFn

contains :: String -> Query Elements -> CypressM (Query Elements)
contains = naskC2 containsFn

dblclick :: Query Elements -> CypressM (Query Elements)
dblclick = naskC1 dblclickFn

debug :: CypressM Unit
debug = askC1 debugFn

document :: CypressM (Query Document)
document = askC1 documentFn

-- each :: String -> CypressM Unit
-- each = askC2 eachFn

end :: forall a. Query a -> CypressM Unit
end = naskC1 endFn

eq :: Int -> Query Elements -> CypressM (Query Elements)
eq = naskC2 eqFn

exec :: String -> CypressM Exec
exec = askC2 execFn

filter :: String -> Query Elements -> CypressM (Query Elements)
filter = naskC2 filterFn

find :: String -> (Query Elements) -> CypressM (Query Elements)
find = naskC2 findFn

first :: forall a. (Query a) -> CypressM (Query a)
first = naskC1 firstFn

fixture :: String -> CypressM Foreign
fixture = askC2 fixtureFn

focus :: Query Elements -> CypressM (Query Elements)
focus = naskC1 focusFn

focused :: CypressM (Query Elements)
focused = askC1 focusedFn

get :: String -> CypressM (Query Elements)
get = askC2 getFn

getCookie :: String -> CypressM String
getCookie = askC2 getCookieFn

getCookies :: CypressM String
getCookies = askC1 getCookiesFn

go :: String -> CypressM Unit
go = askC2 goFn

hash :: CypressM String
hash = askC1 hashFn

hover :: (Query Elements) -> CypressM (Query Elements)
hover = naskC1 hoverFn

-- invoke :: String -> CypressM Unit
-- invoke = askC2 invokeFn

-- its :: String -> Query Elements -> CypressM String
-- its = askC2 itsFn

last :: (Query Elements) -> CypressM (Query Elements)
last = naskC1 lastFn

location :: CypressM Location
location = askC1 locationFn

log :: String -> CypressM Unit
log = askC2 logFn

next :: (Query Elements) -> CypressM (Query Elements)
next = naskC1 nextFn

nextAll :: (Query Elements) -> CypressM (Query Elements)
nextAll = naskC1 nextAllFn

nextUntil :: String -> (Query Elements) -> CypressM (Query Elements)
nextUntil = naskC2 nextUntilFn

not :: String -> (Query Elements) -> CypressM (Query Elements)
not = naskC2 notFn

parent :: (Query Elements) -> CypressM (Query Elements)
parent = naskC1 parentFn

parents :: (Query Elements) -> CypressM (Query Elements)
parents = naskC1 parentsFn

parentsUntil :: String -> (Query Elements) -> CypressM (Query Elements)
parentsUntil = naskC2 parentsUntilFn

pause :: CypressM Unit
pause = askC1 pauseFn

prev :: (Query Elements) -> CypressM (Query Elements)
prev = naskC1 prevFn

prevAll :: (Query Elements) -> CypressM (Query Elements)
prevAll = naskC1 prevAllFn

prevUntil :: String -> (Query Elements) -> CypressM (Query Elements)
prevUntil = naskC2 prevUntilFn

readFile :: String -> CypressM Foreign
readFile = askC2 readFileFn

reload :: CypressM Unit
reload = askC1 reloadFn

-- request :: String -> CypressM Unit
-- request = askC2 requestFn

rightclick :: Query Elements -> CypressM (Query Elements)
rightclick = naskC1 rightclickFn

-- root :: CypressM Query
-- root = askC1 rootFn

-- route :: String -> CypressM Unit
-- route = askC2 routeFn

screenshot :: CypressM Unit
screenshot = askC1 screenshotFn

scrollIntoView :: (Query Elements) -> CypressM (Query Elements)
scrollIntoView = naskC1 scrollIntoViewFn

-- scrollTo :: String -> CypressM Unit
-- scrollTo = askC2 scrollToFn

select :: String -> (Query Elements) -> CypressM (Query Elements)
select = naskC2 selectFn

-- server :: String -> CypressM Unit
-- server = askC2 serverFn

setCookie :: String -> String -> CypressM Cookie
setCookie = askC3 setCookieFn

should :: forall a. String -> Int -> Query a -> CypressM (Query a)
should = naskC3 shouldFn

siblings :: (Query Elements) -> CypressM (Query Elements)
siblings = naskC1 siblingsFn

-- spread :: String -> CypressM Unit
-- spread = askC2 spreadFn

-- spy :: String -> CypressM Unit
-- spy = askC2 spyFn

-- stub :: String -> CypressM Unit
-- stub = askC2 stubFn

submit :: (Query Elements) -> CypressM (Query Elements)
submit = naskC1 submitFn

-- task :: String -> CypressM Unit
-- task = askC2 taskFn

-- thn :: forall a b. (a -> b) -> a -> CypressM b
-- thn = naskC2 thenFn

tick :: Int -> CypressM Clock
tick = askC2 tickFn

title :: CypressM (Query String)
title = askC1 titleFn

trigger :: String -> (Query Elements) -> CypressM (Query Elements)
trigger = naskC2 triggerFn

typ :: String -> (Query Elements) -> CypressM (Query Elements)
typ = naskC2 typeFn

uncheck :: (Query Elements) -> CypressM (Query Elements)
uncheck = naskC1 uncheckFn

url :: CypressM String
url = askC1 urlFn

viewport :: Int -> Int -> CypressM Unit
viewport = askC3 viewportFn

visit :: String -> CypressM Unit
visit = askC2 visitFn

wait :: Int -> CypressM Unit
wait = askC2 waitFn

window :: CypressM Window
window = askC1 windowFn

within :: forall a b. ((Query a) -> (Query b)) -> (Query a) -> CypressM (Query b)
within = askC3 withinFn

wrap :: forall a. a -> CypressM (Query a)
wrap = askC2 wrapFn

-- writeFile :: String -> CypressM Unit
-- writeFile = askC2 writeFileFn

