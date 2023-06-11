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

data Action = Receive Input | Typing String | SetValue String | Increment | Decrement | Submit Int

type State = Either String Int

component :: forall query m. Int -> H.Component query Input Output m
component stepSize =
    H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }
    where

    lockStep :: Int -> Int
    lockStep x = x - (x `mod` stepSize)

    initialState :: Input -> State
    initialState x = Right (lockStep x)

    handleAction :: Action -> H.HalogenM State Action () Output m Unit
    handleAction (Receive x) = H.modify_ \_ -> Right (lockStep x)
    handleAction (SetValue str) = H.modify_ \_ -> setValue str
    handleAction (Typing str) = H.modify_ \_ -> typing str
    handleAction Increment = H.modify_ increment
    handleAction Decrement = H.modify_ decrement

    handleAction (Submit x) = H.raise <<< UserEntered $ x


    {- If you try to consolidate 'setValue' and 'typing', the lockStep function will interfere with the user's ability to type numbers. -}

    setValue :: String -> State
    setValue str = maybe (Left str) (Right <<< lockStep) (fromString str)

    typing :: String -> State
    typing str = Left str

    increment :: State -> State
    increment state = (_ + stepSize) <$> state

    decrement :: State -> State
    decrement state = (\x -> max 0 $ x - stepSize) <$> state

    render :: State -> H.ComponentHTML Action () m
    render state = HH.div_
        [ HH.input
            [ HP.value $ either identity show state
            , HE.onValueChange $ (\txt -> SetValue txt)
            , HE.onValueInput $ (\txt -> Typing txt)
            , HP.class_ <<< ClassName $ stateClass state
            ]
        , HH.button
            [ HP.disabled <<< isLeft $ state
            , HE.onClick (\_ -> Decrement)
            ]
            [ HH.text "↓" ]
        , HH.button
            [ HP.disabled <<< isLeft $ state
            , HE.onClick (\_ -> Increment)
            ]
            [ HH.text "↑" ]
        , HH.button
            [ HP.disabled <<< isLeft $ state
            , HE.onClick
                (\_ -> case state of
                    Left str -> Typing str
                    Right x -> Submit x
                )
            ]
            [ HH.text "✓" ]
        ]

    stateClass :: State -> String
    stateClass (Right _) = "valid"
    stateClass (Left str) =
        if (isJust <<< fromString $ str)
            then "valid"
            else "invalid"
