module Main where

import Prelude

import Data.Maybe

import Effect (Effect)
import Effect.Console (log)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

import Web.DOM.ParentNode (QuerySelector(..))

import Core
import IntegerEntry as IntegerEntry

main :: Effect Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody
    appElement <- HA.selectElement $ QuerySelector "div#app"
    case appElement of
        Just something -> runUI (IntegerEntry.component 5) 0 something
        Nothing -> runUI (IntegerEntry.component 5) 0 body

type State = Workout

defaultState :: Workout
defaultState =
    [
        Exercise
        { movement: Movement "Barbell Squat"
        , sets: 5
        , reps: 5
        , weight: 0
        , success: false
        }
    ,
        Exercise
        { movement: Movement "Bench Press"
        , sets: 5
        , reps: 5
        , weight: 0
        , success: false
        }
    ,
        Exercise
        { movement: Movement "Bent Over Row"
        , sets: 3
        , reps: 8
        , weight: 0
        , success: false
        }
    ,
        Exercise
        { movement: Movement "Barbell Deadlift"
        , sets: 3
        , reps: 5
        , weight: 0
        , success: false
        }
    ,
        Exercise
        { movement: Movement "Overhead Press"
        , sets: 3
        , reps: 5
        , weight: 0
        , success: false
        }
    ,
        Exercise
        { movement: Movement "Lat Pulldown / Pullup"
        , sets: 3
        , reps: 8
        , weight: 0
        , success: false
        }
    ]