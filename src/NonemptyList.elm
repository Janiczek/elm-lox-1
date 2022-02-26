module NonemptyList exposing (NonemptyList, any, cons, fromList, pop, singleton)


type alias NonemptyList a =
    ( a, List a )


any : (a -> Bool) -> NonemptyList a -> Bool
any pred ( x, xs ) =
    pred x || List.any pred xs


fromList : List a -> Maybe (NonemptyList a)
fromList list =
    case list of
        x :: xs ->
            Just ( x, xs )

        _ ->
            Nothing


cons : a -> NonemptyList a -> NonemptyList a
cons x ( y, ys ) =
    ( x, y :: ys )


pop : NonemptyList a -> Maybe (NonemptyList a)
pop ( x, xs ) =
    fromList xs


singleton : a -> NonemptyList a
singleton x =
    ( x, [] )
