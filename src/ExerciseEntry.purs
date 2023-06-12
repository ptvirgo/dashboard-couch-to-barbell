module ExerciseEntry where

import Prelude

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

data Action = ToggleSuccess | HandleIntEntry (IntEntry.Output) | Submit Exercise

{- Component -}

component :: forall query m. H.Component query Input Output m
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

    handleAction :: forall state. Action -> H.HalogenM State Action Slots Output m Unit
    handleAction ToggleSuccess = H.modify_ toggleSuccess
    handleAction (HandleIntEntry (IntEntry.UserEntered x)) = H.modify_ $ \state -> setWeight state x
    handleAction (Submit exercise) = H.raise $ UpdateExercise exercise
    handleAction _ = pure unit

    render :: State -> H.ComponentHTML Action Slots m
    render (Exercise record) =
        HH.div
            [ HP.class_ <<< ClassName $ "updateExercise" ]
            [ HH.h2_ [ HH.text <<< show $ record.movement ]
            , HH.p_
                [ HH.text $ "Sets: " <> show record.sets <> " Reps: " <> show record.reps ]
            , HH.p_
                [ HH.slot _intEntry 0 (IntEntry.component "Weight" 5) record.weight HandleIntEntry
                , HH.label [ HP.for "successful" ] [ HH.text "Complete: " ]
                , HH.input
                    [ HP.type_ HP.InputCheckbox
                    , HP.checked record.success
                    , HE.onClick (\_ -> ToggleSuccess)
                    , HP.name "successful"
                    ]
                ]
            , HH.p_
                [ HH.button
                    [ HE.onClick (\_ -> Submit <<< Exercise $ record) ]
                    [ HH.text "Ok" ]
                ]
            ]
