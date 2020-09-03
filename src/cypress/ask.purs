module Cypress.Ask where

import Prelude
-- import Data.Functor (class Functor)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1, EffectFn2, runEffectFn2, EffectFn3, runEffectFn3, EffectFn4, runEffectFn4, EffectFn5, runEffectFn5, EffectFn6, runEffectFn6)
import Control.Monad.Reader.Trans (ReaderT(..), ask)

askC1 :: forall r o. (EffectFn1 r o) -> ReaderT r Effect o
askC1 f = ReaderT $ (\cy -> (runEffectFn1 f) cy)

askC2 :: forall a o r. (EffectFn2 a r o) -> a -> ReaderT r Effect o
askC2 f a = ReaderT $ (\cy -> (runEffectFn2 f) a cy)

askC3 :: forall a b o r. (EffectFn3 a b r o) -> a -> b -> ReaderT r Effect o
askC3 f a b = ReaderT $ (\cy -> (runEffectFn3 f) a b cy)

askC4 :: forall a b c o r. (EffectFn4 a b c r o) -> a -> b -> c -> ReaderT r Effect o
askC4 f a b c = ReaderT $ (\cy -> (runEffectFn4 f) a b c cy)

askC5 :: forall a b c d o r. (EffectFn5 a b c d r o) -> a -> b -> c -> d -> ReaderT r Effect o
askC5 f a b c d = ReaderT $ (\cy -> (runEffectFn5 f) a b c d cy)

askC6 :: forall a b c d e o r. (EffectFn6 a b c d e r o) -> a -> b -> c -> d -> e -> ReaderT r Effect o
askC6 f a b c d e = ReaderT $ (\cy -> (runEffectFn6 f) a b c d e cy)



naskC1 :: forall a o r. (EffectFn1 a o) -> a -> ReaderT r Effect o
naskC1 f a = ReaderT $ (\cy -> (runEffectFn1 f) a)

naskC2 :: forall a b o r. (EffectFn2 a b o) -> a -> b -> ReaderT r Effect o
naskC2 f a b = ReaderT $ (\cy -> (runEffectFn2 f) a b)

naskC3 :: forall a b c o r. (EffectFn3 a b c o) -> a -> b -> c -> ReaderT r Effect o
naskC3 f a b c = ReaderT $ (\cy -> (runEffectFn3 f) a b c)

naskC4 :: forall a b c d o r. (EffectFn4 a b c d o) -> a -> b -> c -> d -> ReaderT r Effect o
naskC4 f a b c d = ReaderT $ (\cy -> (runEffectFn4 f) a b c d)

naskC5 :: forall a b c d e o r. (EffectFn5 a b c d e o) -> a -> b -> c -> d -> e -> ReaderT r Effect o
naskC5 f a b c d e = ReaderT $ (\cy -> (runEffectFn5 f) a b c d e)
