module Mappings where

import Dict exposing (Dict)
import List
import Maybe
import String

type alias Mapping = Dict String String

mappings : List (String, Mapping)
mappings =
    [ ("Morse", morse) ]

morse : Mapping
morse = Dict.fromList
    [ ("a", ".-")
    , ("b", "-...")
    , ("c", "-.-.")
    , ("d", "-..")
    , ("e", ".")
    , ("f", "..-.")
    , ("g", "--.")
    , ("h", "....")
    , ("i", "..")
    , ("j", ".---")
    , ("k", "-.-")
    , ("l", ".-..")
    , ("m", "--")
    , ("n", "-.")
    , ("o", "---")
    , ("p", ".--.")
    , ("q", "--.-")
    , ("r", ".-.")
    , ("s", "...")
    , ("t", "-")
    , ("u", "..-")
    , ("v", "...-")
    , ("w", ".--")
    , ("x", "-..-")
    , ("y", "-.--")
    , ("z", "--..")
    , ("0", "-----")
    , ("1", ".----")
    , ("2", "..---")
    , ("3", "...--")
    , ("4", "....-")
    , ("5", ".....")
    , ("6", "-....")
    , ("7", "--...")
    , ("8", "---..")
    , ("9", "----.")
    ]

mapForward : Mapping -> String -> String
mapForward mapping s =
    s
    |> String.split ""
    |> List.map (\x -> Maybe.withDefault "?" (Dict.get x mapping))
    |> String.join " "
