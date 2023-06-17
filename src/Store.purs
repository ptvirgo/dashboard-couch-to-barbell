module Store (saveWorkout, loadWorkout) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)

import Effect
import Effect.Uncurried

import Core (Workout)

foreign import saveIt :: forall a. EffectFn1 a Unit

saveWorkout :: Workout -> Effect Unit
saveWorkout = runEffectFn1 saveIt


foreign import loadIt :: forall a. Effect (Nullable a)

loadWorkout :: Effect (Maybe Workout)
loadWorkout = toMaybe <$> loadIt
