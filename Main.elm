module Main where

import Debug
import Random exposing (Generator, Seed)
import Signal exposing (Mailbox)
import String
import Time
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Mappings exposing (..)
import Words exposing (..)

type alias QuizState =
    { seed : Seed
    , word : String
    , currentGuess : String
    }

initialState =
    { seed = Random.initialSeed 0
    , word = ""
    , currentGuess = ""
    }

type QuizDelta
    = SetSeed Seed
    | Skip
    | Guess String

ix : Int -> List a -> Maybe a
ix n l = List.head (List.drop n l)

sample : List a -> Generator a
sample l =
    let
    n = List.length l
    genFunc s =
        let
        (i, s') = Random.generate (Random.int 0 (n - 1)) s
        in
        case ix i l of
            Nothing -> Debug.crash "Impossible"
            Just x -> (x, s')
    in
    Random.customGenerator genFunc

updateQuiz : QuizDelta -> QuizState -> QuizState
updateQuiz delta state = case delta of
    SetSeed s ->
        let
        (w, s') = Random.generate (sample words) s
        in
        { seed = s'
        , word = w
        , currentGuess = ""
        }
    Skip ->
        let
        (w, s') = Random.generate (sample words) state.seed
        in
        { seed = s'
        , word = w
        , currentGuess = ""
        }
    Guess g ->
        if g == state.word
        then
            let
            (w, s') = Random.generate (sample words) state.seed
            in
            { seed = s'
            , word = w
            , currentGuess = ""
            }
        else
            { seed = state.seed
            , word = state.word
            , currentGuess = g
            }

skipBox : Mailbox ()
skipBox = Signal.mailbox ()

guessBox : Mailbox String
guessBox = Signal.mailbox ""

displayQuiz : QuizState -> Html
displayQuiz state =
    div
        [ style
            [ ("margin", "auto")
            , ("text-align", "center")
            ]
        ]
        [ div
            [ style
                [ ("font-size", "48pt")
                , ("margin", "auto")
                , ("text-align", "center")
                ]
            ]
            [ text (mapForward morse state.word)
            ]
        , input
            [ type' "text"
            , on "keyup" targetValue (Signal.message guessBox.address)
            , style
                [ ("margin", "auto")
                , ("display", "block")
                ]
            , value state.currentGuess
            ]
            []
        , input
            [ type' "button"
            , value "Skip"
            , style
                [ ("margin", "auto")
                , ("display", "block")
                ]
            , onClick skipBox.address ()
            ]
            []
        ]

once : Signal ()
once =
    Signal.foldp (\_ _ -> True) False (Time.fps 1)
    |> Signal.dropRepeats
    |> Signal.map (\_ -> ())

seedSource : Signal Seed
seedSource =
    once
    |> Time.timestamp
    |> Signal.map fst
    |> Signal.map (\t -> Random.initialSeed (round (t / Time.millisecond)))

deltaSource : Signal QuizDelta
deltaSource = Signal.mergeMany
    [ Signal.map SetSeed seedSource
    , Signal.map (\_ -> Skip) skipBox.signal
    , Signal.map Guess guessBox.signal
    ]

quizState : Signal QuizState
quizState = Signal.foldp updateQuiz initialState deltaSource

main : Signal Html
main = Signal.map displayQuiz quizState
