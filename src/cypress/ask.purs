module Cypress.Ask where

import Cypress.Promise
import Prelude

import Control.Monad.Reader.Trans (ReaderT(..), ask)
import Control.Promise as P
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, EffectFn5, EffectFn6, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4, runEffectFn5, runEffectFn6, runEffectFn7)

askC1 :: forall r o. (EffectFnP1 r o) -> ReaderT r Aff o
askC1 f = ReaderT $ (\cy -> P.toAffE $ (runEffectFn1 f) cy)

askC2 :: forall a o r. (EffectFnP2 a r o) -> a -> ReaderT r Aff o
askC2 f a = ReaderT $ (\cy -> P.toAffE $ (runEffectFn2 f) a cy)

askC3 :: forall a b o r. (EffectFnP3 a b r o) -> a -> b -> ReaderT r Aff o
askC3 f a b = ReaderT $ (\cy -> P.toAffE $ (runEffectFn3 f) a b cy)

askC4 :: forall a b c o r. (EffectFnP4 a b c r o) -> a -> b -> c -> ReaderT r Aff o
askC4 f a b c = ReaderT $ (\cy -> P.toAffE $ (runEffectFn4 f) a b c cy)

askC5 :: forall a b c d o r. (EffectFnP5 a b c d r o) -> a -> b -> c -> d -> ReaderT r Aff o
askC5 f a b c d = ReaderT $ (\cy -> P.toAffE $ (runEffectFn5 f) a b c d cy)

askC6 :: forall a b c d e o r. (EffectFnP6 a b c d e r o) -> a -> b -> c -> d -> e -> ReaderT r Aff o
askC6 f a b c d e = ReaderT $ (\cy -> P.toAffE $ (runEffectFn6 f) a b c d e cy)

askC7 :: forall a b c d e g o r. (EffectFnP7 a b c d e g r o) -> a -> b -> c -> d -> e -> g -> ReaderT r Aff o
askC7 fn a b c d e f = ReaderT $ (\cy -> P.toAffE $ (runEffectFn7 fn) a b c d e f cy)

naskC1 :: forall a o r. (EffectFnP1 a o) -> a -> ReaderT r Aff o
naskC1 f a = ReaderT $ (\cy -> P.toAffE $ (runEffectFn1 f) a)

naskC2 :: forall a b o r. (EffectFnP2 a b o) -> a -> b -> ReaderT r Aff o
naskC2 f a b = ReaderT $ (\cy -> P.toAffE $ (runEffectFn2 f) a b)

naskC3 :: forall a b c o r. (EffectFnP3 a b c o) -> a -> b -> c -> ReaderT r Aff o
naskC3 f a b c = ReaderT $ (\cy -> P.toAffE $ (runEffectFn3 f) a b c)

naskC4 :: forall a b c d o r. (EffectFnP4 a b c d o) -> a -> b -> c -> d -> ReaderT r Aff o
naskC4 f a b c d = ReaderT $ (\cy -> P.toAffE $ (runEffectFn4 f) a b c d)

naskC5 :: forall a b c d e o r. (EffectFnP5 a b c d e o) -> a -> b -> c -> d -> e -> ReaderT r Aff o
naskC5 f a b c d e = ReaderT $ (\cy -> P.toAffE $ (runEffectFn5 f) a b c d e)
