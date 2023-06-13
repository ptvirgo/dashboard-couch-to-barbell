module ExerciseEntry where

import Prelude

import Effect.Class (class MonadEffect)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Type.Proxy (Proxy(..))
import Web.HTML.Common ( ClassName (..) )

import Core
import IntegerEntry as IntEntry


{- Slots -}

type Slots =
    ( intEntry :: forall query. H.Slot query IntEntry.Output Int
    )

_intEntry = Proxy :: Proxy "intEntry"

{- I/O -}
type Input = Exercise
type State = Exercise

data Output = UpdateExercise Exercise

data Action = HandleIntEntry (IntEntry.Output) | Submit Exercise

{- Component -}

component :: forall query m. MonadEffect m => H.Component query Input Output m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval
            { handleAction = handleAction
            }
        }
    where

    initialState :: Input -> State
    initialState input = input

    handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
    handleAction (HandleIntEntry (IntEntry.UserEntered x)) = H.modify_ $ \state -> setWeight state x
    handleAction (Submit exercise) = H.raise $ UpdateExercise exercise
    handleAction _ = pure unit

    render :: State -> H.ComponentHTML Action Slots m
    render (Exercise record) =
        HH.div
            [ HP.classes [ ClassName "main", ClassName $ "updateExercise" ]]
            [ HH.h1_ [ HH.text <<< show $ record.movement ]
            , HH.p
                [ HP.classes [ ClassName "counts" ] ]
                [ HH.text $ show record.sets <> " sets of " <> show record.reps ]
            , HH.p_
                [ HH.slot _intEntry 0 (IntEntry.component "lbs" 5) record.weight HandleIntEntry
                , HH.label [ HP.for " lbs" ] [ HH.text "lbs " ]
                ]
            , HH.p_
                [ HH.button
                    [ HE.onClick (\_ -> Submit <<< flip setSuccess false <<< Exercise $ record)
                    , HP.classes [ ClassName "fail" ]
                    ]
                    [ HH.text "✗" ]
                , HH.button
                    [ HE.onClick (\_ -> Submit <<< flip setSuccess true <<< Exercise $ record)
                    , HP.classes [ ClassName "succeed" ]
                    ]
                    [ HH.text "✓" ]
                ]
            ]
