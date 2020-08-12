module Cypress.Ask where

import Prelude
-- import Data.Functor (class Functor)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1, EffectFn2, runEffectFn2, EffectFn3, runEffectFn3)
import Control.Monad.Reader.Trans (ReaderT(..), ask)

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