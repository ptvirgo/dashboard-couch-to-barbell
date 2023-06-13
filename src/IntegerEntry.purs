module IntegerEntry where

import Prelude

import Data.Either
import Data.Int
import Data.Maybe

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Web.HTML.Common ( ClassName (..) )

type Input = Int
data Output = UserEntered Int

data Action = Receive Input | Typing String | Submit Int

type State = Either String Int

component :: forall query m. String -> Int -> H.Component query Input Output m
component label stepSize =
    H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }
    where

    validInt :: Int -> Int
    validInt x = max 0 $ x - (x `mod` stepSize)

    mkState :: String -> State
    mkState str =
        maybe (Left str) (Right <<< validInt) (fromString str)

    initialState :: Input -> State
    initialState x = Right (validInt x)

    handleAction :: Action -> H.HalogenM State Action () Output m Unit
    handleAction (Receive x) = H.modify_ \_ -> Right (validInt x)
    handleAction (Typing str) = H.modify_ \_ -> typing str
    handleAction (Submit x) = H.raise <<< UserEntered $ x

    {- If you try to consolidate 'mkState' and 'typing', the validation function will interfere with the user while they are preparing their input. -}

    typing :: String -> State
    typing str = Left str

    render :: State -> H.ComponentHTML Action () m
    render state = HH.div_
        [ HH.label [ HP.for label ] [ HH.text $ label <> ": " ]
        , HH.input
            [ HP.value $ either identity show state
            , HE.onValueChange (\txt -> either Typing Submit $ mkState txt)
            , HE.onValueInput $ (\txt -> Typing txt)
            , HE.onBlur $ (\_ -> either Typing Submit state)
            , HP.class_ <<< ClassName $ stateClass state
            , HP.name label
            ]
        , HH.button
            [ HP.disabled <<< isLeft $ state
            , HE.onClick (\_ -> either Typing Submit $ (_ - stepSize) <$> state)
            ]
            [ HH.text "↓" ]
        , HH.button
            [ HP.disabled <<< isLeft $ state
            , HE.onClick (\_ -> either Typing Submit $ (_ + stepSize) <$> state)
            ]
            [ HH.text "↑" ]
        ]

    stateClass :: State -> String
    stateClass (Right _) = "valid"
    stateClass (Left str) =
        if (isJust <<< fromString $ str)
            then "valid"
            else "invalid"
