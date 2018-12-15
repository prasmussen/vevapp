module Vevapp.Command exposing (chain)


chain : (a -> Cmd msg) -> ( a, Cmd msg ) -> ( a, Cmd msg )
chain f ( orgModel, orgCmd ) =
    ( orgModel, Cmd.batch [ orgCmd, f orgModel ] )
