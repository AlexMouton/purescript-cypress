module Cypress.Actions where

import Prelude
-- import Data.Functor (class Functor)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Data.Maybe (Maybe, isJust)
import Data.Maybe as M
import Effect (Effect)
import Foreign (Foreign)
import Partial.Unsafe (unsafePartial)

import Cypress
import Cypress.Foreign
import Cypress.Ask
import Cypress.Query
import Cypress.Elements
import Cypress.Chai

fromJust = unsafePartial M.fromJust

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

-- root
clearCookie :: String -> CypressM Unit
clearCookie = askC2 clearCookieFn

-- root
clearCookies :: CypressM Unit
clearCookies = askC1 clearCookiesFn

-- root
clearLocalStorage :: String -> CypressM Unit
clearLocalStorage = askC2 clearLocalStorageFn

click :: Query Elements -> CypressM (Query Elements)
click = naskC1 clickFn

-- root
clock :: CypressM Clock
clock = askC1 clockFn

closest :: String -> Query Elements -> CypressM (Query Elements)
closest = naskC2 closestFn

-- Contains [both]
contains :: ContainsProps -> CypressM (Query Elements)
contains = askC4 containsFn isJust fromJust

containsq :: ContainsProps -> Query Elements -> CypressM (Query Elements)
containsq = askC5 containsqFn isJust fromJust

dblclick :: Query Elements -> CypressM (Query Elements)
dblclick = naskC1 dblclickFn

-- both
debug :: CypressM Unit
debug = askC1 debugFn

-- root
document :: CypressM (Query Document)
document = askC1 documentFn

-- each :: String -> CypressM Unit
-- each = askC2 eachFn

end :: forall a. Query a -> CypressM Unit
end = naskC1 endFn

eq :: Int -> Query Elements -> CypressM (Query Elements)
eq = naskC2 eqFn

-- root
exec :: String -> CypressM ResultExec
exec = askC2 execFn

filter :: String -> Query Elements -> CypressM (Query Elements)
filter = naskC2 filterFn

find :: String -> (Query Elements) -> CypressM (Query Elements)
find = naskC2 findFn

first :: forall a. (Query a) -> CypressM (Query a)
first = naskC1 firstFn

--  root
fixture :: String -> CypressM Foreign
fixture = askC2 fixtureFn

focus :: Query Elements -> CypressM (Query Elements)
focus = naskC1 focusFn

-- root
focused :: CypressM (Query Elements)
focused = askC1 focusedFn

-- root
actionString :: GetAction -> String
actionString = case _ of
  Selector a -> a
  Alias a -> "@" <> a

get :: GetProps -> CypressM (Query Elements)
get = askC5 getFn isJust fromJust actionString

--  root
getCookie :: String -> CypressM String
getCookie = askC2 getCookieFn

--  root
getCookies :: CypressM String
getCookies = askC1 getCookiesFn

--  root
go :: String -> CypressM Unit
go = askC2 goFn

--root
hash :: CypressM String
hash = askC1 hashFn

--  'cy doesnt have hover'
-- hover :: (Query Elements) -> CypressM (Query Elements)
-- hover = naskC1 hoverFn

-- invoke :: String -> CypressM Unit
-- invoke = askC2 invokeFn

-- its :: String -> Query Elements -> CypressM String
-- its = askC2 itsFn

last :: (Query Elements) -> CypressM (Query Elements)
last = naskC1 lastFn

-- root
type Location = Foreign
location :: CypressM Location
location = askC1 locationFn

-- root
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

--  both
pause :: CypressM Unit
pause = askC1 pauseFn

prev :: (Query Elements) -> CypressM (Query Elements)
prev = naskC1 prevFn

prevAll :: (Query Elements) -> CypressM (Query Elements)
prevAll = naskC1 prevAllFn

prevUntil :: String -> (Query Elements) -> CypressM (Query Elements)
prevUntil = naskC2 prevUntilFn

-- root
readFile :: String -> CypressM Foreign
readFile = askC2 readFileFn

-- root
reload :: CypressM Unit
reload = askC1 reloadFn

-- root
-- request :: String -> CypressM Unit
-- request = askC2 requestFn

rightclick :: Query Elements -> CypressM (Query Elements)
rightclick = naskC1 rightclickFn

-- root
-- root :: CypressM Query
-- root = askC1 rootFn

-- root
-- route :: String -> CypressM Unit
-- route = askC2 routeFn

-- both
screenshot :: CypressM Unit
screenshot = askC1 screenshotFn

scrollIntoView :: (Query Elements) -> CypressM (Query Elements)
scrollIntoView = naskC1 scrollIntoViewFn

-- both
-- scrollTo :: String -> CypressM Unit
-- scrollTo = askC2 scrollToFn

select :: Array String -> (Query Elements) -> CypressM (Query Elements)
select = naskC2 selectFn

-- root
-- server :: String -> CypressM Unit
-- server = askC2 serverFn

-- root
setCookie :: String -> String -> CypressM Cookie
setCookie = askC3 setCookieFn

should :: forall a b. Should a b => a -> Query b -> CypressM (Query b)
should = toShould

siblings :: (Query Elements) -> CypressM (Query Elements)
siblings = naskC1 siblingsFn

-- spread :: String -> CypressM Unit
-- spread = askC2 spreadFn

-- root
-- spy :: String -> CypressM Unit
-- spy = askC2 spyFn

-- root
-- stub :: String -> CypressM Unit
-- stub = askC2 stubFn

-- must be form
submit :: (Query Elements) -> CypressM (Query Elements)
submit = naskC1 submitFn

-- root
-- task :: String -> CypressM Unit
-- task = askC2 taskFn

thn :: forall a b. (a -> b) -> a -> CypressM b
thn = naskC2 thenFn

-- root
tick :: Int -> CypressM Clock
tick = askC2 tickFn

-- root
title :: CypressM (Query String)
title = askC1 titleFn

trigger :: String -> (Query Elements) -> CypressM (Query Elements)
trigger = naskC2 triggerFn

typ :: String -> (Query Elements) -> CypressM (Query Elements)
typ = naskC2 typeFn

uncheck :: (Query Elements) -> CypressM (Query Elements)
uncheck = naskC1 uncheckFn

--  root
url :: CypressM String
url = askC1 urlFn

-- root
viewport :: Int -> Int -> CypressM Unit
viewport = askC3 viewportFn

-- root
visit :: String -> CypressM Unit
visit = askC2 visitFn

-- root
wait :: Int -> CypressM Unit
wait = askC2 waitFn

-- root
window :: CypressM Window
window = askC1 windowFn

within :: forall a b. ((Query a) -> (Query b)) -> (Query a) -> CypressM (Query b)
within = askC3 withinFn

-- root
wrap :: forall a. a -> CypressM (Query a)
wrap = askC2 wrapFn

-- root
-- writeFile :: String -> CypressM Unit
-- writeFile = askC2 writeFileFn

xpath :: String -> CypressM (Query Elements)
xpath = askC2 xpathFn
