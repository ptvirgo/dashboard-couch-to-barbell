module Main where

import Prelude

import Data.Array
import Data.Maybe
import Data.Newtype (unwrap)

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML.Common ( ClassName (..) )

import Type.Proxy (Proxy(..))

import ExerciseEntry as ExerciseEntry

import Core
import Store

main :: Effect Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody
    appElement <- HA.selectElement $ QuerySelector "div#app"
    case appElement of
        Just something -> runUI component defaultState something
        Nothing -> runUI component defaultState body

{- Slots -}

type Slots =
    ( exerciseEntry :: forall query. H.Slot query ExerciseEntry.Output Int
    )

_exerciseEntry = Proxy :: Proxy "exerciseEntry"

{- State -}

type State =
    { workout :: Workout
    , editing :: Maybe Exercise
    }

defaultWorkout :: Workout
defaultWorkout =
    [
        Exercise
        { movement: Movement "Barbell Squat"
        , sets: 5
        , reps: 5
        , weight: 0
        , succeed: false
        }
    ,
        Exercise
        { movement: Movement "Bench Press"
        , sets: 5
        , reps: 5
        , weight: 0
        , succeed: false
        }
    ,
        Exercise
        { movement: Movement "Bent Over Row"
        , sets: 3
        , reps: 8
        , weight: 0
        , succeed: false
        }
    ,
        Exercise
        { movement: Movement "Barbell Deadlift"
        , sets: 3
        , reps: 5
        , weight: 0
        , succeed: false
        }
    ,
        Exercise
        { movement: Movement "Overhead Press"
        , sets: 3
        , reps: 5
        , weight: 0
        , succeed: false
        }
    ,
        Exercise
        { movement: Movement "Pulldown / Pullup"
        , sets: 3
        , reps: 8
        , weight: 0
        , succeed: false
        }
    ]

defaultState :: State
defaultState =
    { workout: []
    , editing: Nothing
    }

data Action = Initialize | HandleExerciseEntry ExerciseEntry.Output | Edit Exercise

{- Component -}


component :: forall query input output m. MonadEffect m => H.Component query input output m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
        }
    where

    initialState :: input -> State
    initialState _ = defaultState

    handleAction :: Action -> H.HalogenM State Action Slots output m Unit
    handleAction Initialize = do
       loaded <- H.liftEffect loadWorkout
       H.modify_ $ \state -> state { workout = fromMaybe defaultWorkout loaded }
        
    handleAction (Edit exercise) = H.modify_ $ \state -> state { editing = Just exercise }
    handleAction (HandleExerciseEntry output) = do
        H.modify_ $ handleExerciseEntry output
        state <- H.get
        H.liftEffect $ saveWorkout state.workout

    handleExerciseEntry :: ExerciseEntry.Output -> State -> State
    handleExerciseEntry (ExerciseEntry.UpdateExercise exercise) state =
        state
            { workout =
                map
                    (\x -> if (_.movement <<< unwrap $ x) == (_.movement <<< unwrap $ exercise)
                                then exercise
                                else x
                    )
                    state.workout
            , editing = Nothing 
            }

    render :: State -> H.ComponentHTML Action Slots m
    render state = case state.editing of
        Nothing -> renderWorkout state.workout
        Just exercise -> HH.slot _exerciseEntry 0 ExerciseEntry.component exercise HandleExerciseEntry

    renderWorkout :: Workout -> H.ComponentHTML Action Slots m
    renderWorkout workout =
        HH.table [ HP.classes [ ClassName "main" ] ]
            [ HH.tbody_ $
                ( cons ( HH.tr_ [ HH.th [ HP.colSpan 4 ] [ HH.h1_ [ HH.text "Day A" ]]] )
                     $ map renderExercise <<< take 3 $ workout )
                <>
                ( cons ( HH.tr_ [ HH.th [ HP.colSpan 4 ] [ HH.h1_ [ HH.text "Day B" ]]] )
                     $ map renderExercise <<< drop 3 $ workout )
            ]

    renderExercise :: Exercise -> H.ComponentHTML Action Slots m 
    renderExercise (Exercise record) =
        HH.tr
            [ HE.onClick (\_ -> Edit <<< Exercise $ record)
            , HP.classes [ ClassName "exercise" ]
            ]
            [ HH.td
                [ HP.classes [ ClassName "movement" ]]
                [ HH.text <<< show $ record.movement ]
            , HH.td_ [ HH.text $ show record.sets <> " sets of " <> show record.reps ]
            , HH.td_ [ HH.text $ show record.weight <> " lbs." ]
            , HH.td
                [ HP.classes [ ClassName "successCheck" ] ]
                [ HH.button
                    [ HP.classes [ ClassName "exercise", ClassName $ if record.succeed then "succeed" else "fail" ]
                    ]
                    [ HH.text $ if record.succeed then "✓"  else "✗" ]
                ]
            ]
