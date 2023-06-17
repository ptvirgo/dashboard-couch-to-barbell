module Core where

import Prelude
import Data.Newtype (class Newtype, unwrap)

import Test.QuickCheck (class Arbitrary, arbitrary)

{- Language

Terms:

    - exercise:
        - movement (squat, bench-press, deadlift ...)
        - reps (repetitions of a movement)
        - sets (repetitions of reps)

    - weight (amount of additional weight lifted for a given exercise)
    - workout
        - Collection of exercises.  For CtB, there are 2 main workouts, labeled "Day A" and "Day B"
    - succeed
        - for purpose of the dashboard, also track whether the last attempt at a given exercise was completed, or needs to be tried again

-}

newtype Movement = Movement String

derive instance newtypeMovement :: Newtype Movement _

derive instance eqMovement :: Eq Movement
derive instance ordMovement :: Ord Movement

instance arbMovement :: Arbitrary Movement where
    arbitrary = Movement <$> arbitrary

instance showMovement :: Show Movement where
    show = unwrap

newtype Exercise =
    Exercise
        { movement :: Movement
        , reps :: Int
        , sets :: Int
        , weight :: Int
        , succeed :: Boolean
        }

derive instance eqExercise :: Eq Exercise
derive instance newtypeExercise :: Newtype Exercise _

instance arbExercise :: Arbitrary Exercise where
    arbitrary = (\movement reps sets weight succeed -> Exercise { movement, reps, sets, weight, succeed }) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

setWeight :: Exercise -> Int -> Exercise
setWeight (Exercise record) x = Exercise record { weight = x }

setSuccess :: Exercise -> Boolean -> Exercise
setSuccess (Exercise record) x = Exercise record { succeed = x }

type Workout = Array Exercise
