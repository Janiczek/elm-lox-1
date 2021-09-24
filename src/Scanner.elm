module Scanner exposing (Token(..), scanTokens)


type Token
    = TodoToken


scanTokens : String -> List Token
scanTokens program =
    Debug.todo "scan tokens"
