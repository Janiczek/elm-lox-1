module Env exposing
    ( Env
    , EnvChain
    , assignToExisting
    , declareInCurrent
    , empty
    , get
    , member
    , popEnv
    , pushEmptyEnv
    )

import Dict exposing (Dict)
import NonemptyList exposing (NonemptyList)
import Value exposing (Value)


type alias Env =
    Dict String Value


type alias EnvChain =
    NonemptyList Env


empty : EnvChain
empty =
    NonemptyList.singleton Dict.empty


popEnv : EnvChain -> EnvChain
popEnv chain =
    chain
        |> NonemptyList.pop
        |> -- TODO this shouldn't happen but it's risky (depends on us)
           Maybe.withDefault chain


pushEmptyEnv : EnvChain -> EnvChain
pushEmptyEnv chain =
    NonemptyList.cons Dict.empty chain


declareInCurrent : String -> Value -> EnvChain -> EnvChain
declareInCurrent name value ( x, xs ) =
    ( Dict.insert name value x
    , xs
    )


assignToExisting : String -> Value -> EnvChain -> Maybe EnvChain
assignToExisting name value (( x, xs ) as chain) =
    let
        go : List Env -> List Env -> Maybe (List Env)
        go acc todo =
            case todo of
                [] ->
                    Nothing

                head :: tail ->
                    if Dict.member name head then
                        Just (List.reverse acc ++ (Dict.insert name value head :: tail))

                    else
                        go (head :: acc) tail
    in
    go [] (x :: xs)
        |> Maybe.map
            (NonemptyList.fromList
                >> -- TODO shouldn't happen:
                   Maybe.withDefault chain
            )


member : String -> EnvChain -> Bool
member name chain =
    NonemptyList.any (Dict.member name) chain


get : String -> EnvChain -> Maybe Value
get name ( x, xs ) =
    let
        go : List Env -> Maybe Value
        go todo =
            case todo of
                [] ->
                    Nothing

                head :: tail ->
                    case Dict.get name head of
                        Nothing ->
                            go tail

                        Just value ->
                            Just value
    in
    go (x :: xs)
