module Core where

import Prelude
import Data.Array
import Data.Newtype (class Newtype, unwrap)

import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen(..), suchThat, vectorOf)

{- Language

Terms:

    - exercise:
        - movement (squat, bench-press, deadlift ...)
        - reps (repetitions of a movement)
        - sets (repetitions of reps)

    - weight (amount of additional weight lifted for a given exercise)
    - workout
        - Collection of exercises.  For CtB, there are 2 main workouts, labeled "Day A" and "Day B"
    - success
        - for purpose of the dashboard, also track whether the last attempt at a given exercise was completed successfully, or needs to be tried again

-}


newtype Movement = Movement String

derive instance newtypeMovement :: Newtype Movement _

derive instance eqMovement :: Eq Movement
derive instance ordMovement :: Ord Movement

instance arbMovement :: Arbitrary Movement where
    arbitrary = Movement <$> arbitrary

instance showMovement :: Show Movement where
    show = show <<< unwrap

newtype Exercise =
    Exercise
        { movement :: Movement
        , reps :: Int
        , sets :: Int
        , weight :: Int
        , success :: Boolean
        }

derive instance eqExercise :: Eq Exercise
derive instance newtypeExercise :: Newtype Exercise _

instance arbExercise :: Arbitrary Exercise where
    arbitrary = (\movement reps sets weight success -> Exercise { movement, reps, sets, weight, success }) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

setWeight :: Exercise -> Int -> Exercise
setWeight (Exercise record) x = Exercise record { weight = x }

setSuccess :: Exercise -> Boolean -> Exercise
setSuccess (Exercise record) x = Exercise record { success = x }

type Workout = Array Exercise

genWorkout :: Gen Workout
genWorkout =
    suchThat
    (fromFoldable <$> vectorOf 3 arbitrary)
    (\exercises -> 3 == (length <<< nub <<< map (_.movement <<< unwrap) $ exercises))