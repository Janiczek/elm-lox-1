module SteppingInterpreter exposing
    ( Error(..)
    , Expr(..)
    , Step(..)
    , example
    , step
    , stepUntilFixpoint
    )


type Expr
    = Int Int
    | Bool Bool
    | Plus Expr Expr
    | Lt Expr Expr
    | And Expr Expr
    | Not Expr


example : Expr
example =
    And
        (Not
            (Lt
                (Plus
                    (Plus (Int 1) (Int 2))
                    (Int 3)
                )
                (Plus (Int 2) (Int 5))
            )
        )
        (Lt
            (Plus
                (Int 3)
                (Plus (Int 5) (Int 6))
            )
            (Int 7)
        )


type Error
    = TypeMismatch


type Step
    = NoWork Expr
    | WorkDone Expr
    | Error Error


stepUntilFixpoint : Expr -> Result Error Expr
stepUntilFixpoint expr =
    let
        go : Step -> Result Error Expr
        go step_ =
            case Debug.log "go" step_ of
                NoWork expr_ ->
                    Ok expr_

                WorkDone expr_ ->
                    go (step expr_)

                Error error ->
                    Err error
    in
    go (step (Debug.log "init" expr))
        |> Debug.log "result"


step : Expr -> Step
step expr =
    case expr of
        Int _ ->
            NoWork expr

        Bool _ ->
            NoWork expr

        Plus e1 e2 ->
            case ( step e1, step e2 ) of
                ( Error err, _ ) ->
                    Error err

                ( _, Error err ) ->
                    Error err

                ( NoWork e1_, NoWork e2_ ) ->
                    case ( e1_, e2_ ) of
                        ( Int i1, Int i2 ) ->
                            WorkDone (Int (i1 + i2))

                        _ ->
                            Error TypeMismatch

                ( WorkDone e1_, _ ) ->
                    WorkDone (Plus e1_ e2)

                ( _, WorkDone e2_ ) ->
                    WorkDone (Plus e1 e2_)

        Lt e1 e2 ->
            case ( step e1, step e2 ) of
                ( Error err, _ ) ->
                    Error err

                ( _, Error err ) ->
                    Error err

                ( NoWork e1_, NoWork e2_ ) ->
                    case ( e1_, e2_ ) of
                        ( Int i1, Int i2 ) ->
                            WorkDone (Bool (i1 < i2))

                        _ ->
                            Error TypeMismatch

                ( WorkDone e1_, _ ) ->
                    WorkDone (Lt e1_ e2)

                ( _, WorkDone e2_ ) ->
                    WorkDone (Lt e1 e2_)

        And e1 e2 ->
            case ( step e1, step e2 ) of
                ( Error err, _ ) ->
                    Error err

                ( _, Error err ) ->
                    Error err

                ( NoWork e1_, NoWork e2_ ) ->
                    case ( e1_, e2_ ) of
                        ( Bool b1, Bool b2 ) ->
                            WorkDone (Bool (b1 && b2))

                        _ ->
                            Error TypeMismatch

                ( WorkDone e1_, _ ) ->
                    WorkDone (And e1_ e2)

                ( _, WorkDone e2_ ) ->
                    WorkDone (And e1 e2_)

        Not e1 ->
            case step e1 of
                NoWork e1_ ->
                    case e1_ of
                        Bool b ->
                            WorkDone (Bool (not b))

                        _ ->
                            Error TypeMismatch

                WorkDone e1_ ->
                    WorkDone (Not e1_)

                Error err ->
                    Error err
