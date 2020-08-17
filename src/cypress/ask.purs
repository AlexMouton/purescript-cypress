module Cypress.Ask where

import Prelude
-- import Data.Functor (class Functor)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1, EffectFn2, runEffectFn2, EffectFn3, runEffectFn3, EffectFn4, runEffectFn4, EffectFn5, runEffectFn5, EffectFn6, runEffectFn6)
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

askC4 :: forall s t u a b. (EffectFn4 s t u b a) -> s -> t -> u -> ReaderT b Effect a
askC4 f s t u = ReaderT $ do
  cy <- ask
  pure $ (runEffectFn4 f) s t u cy

askC5 :: forall s t u v a b. (EffectFn5 s t u v b a) -> s -> t -> u -> v -> ReaderT b Effect a
askC5 f s t u v = ReaderT $ do
  cy <- ask
  pure $ (runEffectFn5 f) s t u v cy

askC6 :: forall s t u v w a b. (EffectFn6 s t u v w b a) -> s -> t -> u -> v -> w -> ReaderT b Effect a
askC6 f s t u v w = ReaderT $ do
  cy <- ask
  pure $ (runEffectFn6 f) s t u v w cy


naskC1 :: forall s a b. (EffectFn1 s a) -> s -> ReaderT b Effect a
naskC1 f s = ReaderT $ do
  pure $ (runEffectFn1 f) s

naskC2 :: forall s t a b. (EffectFn2 s t a) -> s -> t -> ReaderT b Effect a
naskC2 f s t = ReaderT $ do
  pure $ (runEffectFn2 f) s t

naskC3 :: forall s t u a b. (EffectFn3 s t u a) -> s -> t -> u -> ReaderT b Effect a
naskC3 f s t u = ReaderT $ do
  pure $ (runEffectFn3 f) s t u

naskC4 :: forall s t u v a b. (EffectFn4 s t u v a) -> s -> t -> u -> v -> ReaderT b Effect a
naskC4 f s t u v = ReaderT $ do
  pure $ (runEffectFn4 f) s t u v
