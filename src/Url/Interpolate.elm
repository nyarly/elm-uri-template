module Url.Interpolate exposing
    ( interpolate
    , Context, Value(..), simpleContext
    )

{-| Url.Interpolate provides a single function, `interpolate`, which takes
a URI Template string and a Dict of variables, and expands
the input string according to the rules in IETF RFC 6570,
up to Level 3 (Level 4 compliance is not provided or planned).

@docs interpolate

[rfc6570]: https://tools.ietf.org/html/rfc6570

-}

import Dict exposing (Dict)
import Hex
import Maybe
import Regex exposing (Match, Regex)
import Set exposing (Set)


type alias Context =
    Dict String Value


type Value
    = Scalar String
    | Multi (List String)
    | Assoc (Dict String String)


type Modifier
    = NoMod
    | Explode
    | Prefix Int


{-| Works like Dict.fromList to produce a context of names to Scalar string values
-}
simpleContext : List ( String, String ) -> Context
simpleContext pairs =
    List.map (Tuple.mapSecond Scalar) pairs |> Dict.fromList


{-| Example URI template interpolation:

    interpolate "<http://example.com/{path}{?x,y,empty}"> <|
    simpleContext [("path", "hello"), ("x", "1024"), ("y", "768")]

    -- "<http://example.com/hello?x=1024&y=768&empty=">

Internal note: I was surprised to find that the baseline %-encode rules for URI
templates are _slightly different_ than the built-in `encodeURIComponent`. For
instance, '!' _is_ escaped for the template operations that use the
"unrestricted set" of unescaped characters, while the built-in does _not_
escape it. Thus, we rely on the `Hex` library rather than `Url.percentEncode`.

-}
interpolate : String -> Context -> String
interpolate string args =
    Regex.replace interpolationRegex (applyInterpolation args) string


interpolationRegex : Regex
interpolationRegex =
    "\\{([+#.\\/;?&]?)([A-Za-z0-9_,%:*0-9]+)\\}"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


modifiersRegex : Regex
modifiersRegex =
    "([A-Za-z0-9_%]+)(:([0-9]+)|[*])?(,?)"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


applyInterpolation : Context -> Match -> String
applyInterpolation replacements { match, submatches } =
    submatches
        |> getTemplateParts
        |> Maybe.map (\( operator, vars ) -> expand operator vars replacements)
        |> Maybe.withDefault match


getTemplateParts : List (Maybe String) -> Maybe ( String, List ( String, Modifier ) )
getTemplateParts submatches =
    case submatches of
        [ Just operator, Just expression ] ->
            Just ( operator, splitVars expression )

        [ Nothing, Just expression ] ->
            Just ( "", splitVars expression )

        _ ->
            Nothing


splitVars : String -> List ( String, Modifier )
splitVars string =
    Regex.find modifiersRegex string
        |> List.filterMap
            (\{ submatches } ->
                case submatches of
                    [ Just varname, Just "*", _, _ ] ->
                        Just ( varname, Explode )

                    [ Just varname, Just _, Just prefix, _ ] ->
                        Just ( varname, Prefix (String.toInt prefix |> Maybe.withDefault 0) )

                    [ Just varname, Nothing, _, _ ] ->
                        Just ( varname, NoMod )

                    _ ->
                        Nothing
            )


expand : String -> List ( String, Modifier ) -> Context -> String
expand operator vars replacements =
    case operator of
        "" ->
            expandSimple vars replacements

        "+" ->
            expandReservedString vars replacements

        "#" ->
            expandFragment vars replacements

        "." ->
            expandLabel vars replacements

        "/" ->
            expandPath vars replacements

        ";" ->
            expandPathParam vars replacements

        "?" ->
            expandQuery vars replacements

        "&" ->
            expandQueryContinuation vars replacements

        _ ->
            ""


expandSimple : List ( String, Modifier ) -> Context -> String
expandSimple vars replacements =
    unpackContext percentEncodeValue vars replacements
        |> separatedBy ","


expandReservedString : List ( String, Modifier ) -> Context -> String
expandReservedString vars replacements =
    unpackContext percentEncodeValueReserved vars replacements
        |> separatedBy ","


expandFragment : List ( String, Modifier ) -> Context -> String
expandFragment vars replacements =
    "#"
        ++ (unpackContext percentEncodeValueReserved vars replacements
                |> separatedBy ","
           )


expandLabel : List ( String, Modifier ) -> Context -> String
expandLabel vars replacements =
    "."
        ++ (unpackContext percentEncodeValue vars replacements
                |> separatedBy "."
           )


expandPath : List ( String, Modifier ) -> Context -> String
expandPath vars replacements =
    "/"
        ++ (unpackContext percentEncodeValue vars replacements
                |> separatedBy "/"
           )


expandPathParam : List ( String, Modifier ) -> Context -> String
expandPathParam vars replacements =
    unpackContext percentEncodeValue vars replacements
        |> expandPathParamHelp


expandQuery : List ( String, Modifier ) -> Context -> String
expandQuery vars replacements =
    unpackContext percentEncodeValue vars replacements
        |> expandQueryHelp "?"


expandQueryContinuation : List ( String, Modifier ) -> Context -> String
expandQueryContinuation vars replacements =
    unpackContext percentEncodeValue vars replacements
        |> expandQueryHelp "&"


addUnpacked : Dict String Value -> ( String, Modifier ) -> List ( String, String ) -> List ( String, String )
addUnpacked context ( var, mod ) l =
    case ( mod, Dict.get var context ) of
        ( Prefix pre, Just (Scalar s) ) ->
            ( var, String.left pre s ) :: l

        ( _, Just (Scalar s) ) ->
            ( var, s ) :: l

        ( Explode, Just (Multi ss) ) ->
            List.map (\s -> ( var, s )) ss ++ l

        ( _, Just (Multi ss) ) ->
            ( var, String.join "," ss ) :: l

        ( Explode, Just (Assoc d) ) ->
            Dict.toList d ++ l

        ( _, Just (Assoc d) ) ->
            List.map (\( k, v ) -> ( var, String.join "=" [ k, v ] )) (Dict.toList d) ++ l

        ( _, Nothing ) ->
            ( var, "" ) :: l


unpackContext : (Value -> Value) -> List ( String, Modifier ) -> Dict String Value -> List ( String, String )
unpackContext encoding vars context =
    let
        encode _ =
            encoding
    in
    List.foldr (addUnpacked (Debug.log "context" (Dict.map encode context))) [] (Debug.log "vars" vars)



-- HELPERS


expandPathParamHelp : List ( String, String ) -> String
expandPathParamHelp pairs =
    ";"
        ++ (pairs
                |> List.map
                    (\( var, val ) ->
                        if String.isEmpty val then
                            var

                        else
                            var ++ "=" ++ val
                    )
                |> String.join ";"
           )


expandQueryHelp : String -> List ( String, String ) -> String
expandQueryHelp prefix pairs =
    prefix
        ++ (pairs
                |> List.map (\( var, val ) -> var ++ "=" ++ val)
                |> String.join "&"
           )


separatedBy : String -> List ( String, String ) -> String
separatedBy sep pairs =
    pairs
        |> List.map
            Tuple.second
        |> String.join sep


alphanumChars : Set Char
alphanumChars =
    Set.fromList
        [ 'a'
        , 'b'
        , 'c'
        , 'd'
        , 'e'
        , 'f'
        , 'g'
        , 'h'
        , 'i'
        , 'j'
        , 'k'
        , 'l'
        , 'm'
        , 'n'
        , 'o'
        , 'p'
        , 'q'
        , 'r'
        , 's'
        , 't'
        , 'u'
        , 'v'
        , 'w'
        , 'x'
        , 'y'
        , 'z'
        , 'A'
        , 'B'
        , 'C'
        , 'D'
        , 'E'
        , 'F'
        , 'G'
        , 'H'
        , 'I'
        , 'J'
        , 'K'
        , 'L'
        , 'M'
        , 'N'
        , 'O'
        , 'P'
        , 'Q'
        , 'R'
        , 'S'
        , 'T'
        , 'U'
        , 'V'
        , 'W'
        , 'X'
        , 'Y'
        , 'Z'
        , '0'
        , '1'
        , '2'
        , '3'
        , '4'
        , '5'
        , '6'
        , '7'
        , '8'
        , '9'
        ]


unreservedChars : Set Char
unreservedChars =
    Set.fromList
        [ '-', '.', '_', '~' ]
        |> Set.union alphanumChars


reservedChars : Set Char
reservedChars =
    Set.fromList
        [ ':', '/', '?', '#', '[', ']', '@', '!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=' ]


percentEncodeValue : Value -> Value
percentEncodeValue =
    percentEncodeValueExcept unreservedChars


percentEncodeValueReserved : Value -> Value
percentEncodeValueReserved =
    percentEncodeValueExcept reservedAndUnreserved


percentEncodeValueExcept : Set Char -> Value -> Value
percentEncodeValueExcept chars val =
    case val of
        Scalar s ->
            Scalar (percentEncodeExcept chars s)

        Multi l ->
            Multi (List.map (percentEncodeExcept chars) l)

        Assoc d ->
            let
                encode _ =
                    percentEncodeExcept chars
            in
            Assoc (Dict.map encode d)


reservedAndUnreserved : Set Char
reservedAndUnreserved =
    Set.union unreservedChars reservedChars


percentEncodeExcept : Set Char -> String -> String
percentEncodeExcept exceptChars string =
    let
        encodeChar c strs =
            if Set.member c exceptChars then
                String.fromChar c :: strs

            else
                (percentEncodeChar c |> Maybe.withDefault (String.fromChar c)) :: strs
    in
    String.foldr encodeChar [] string
        |> String.join ""


percentEncodeChar : Char -> Maybe String
percentEncodeChar c =
    c
        |> Char.toCode
        |> (\i ->
                if i < 256 then
                    Just ("%" ++ (Hex.toString i |> String.toUpper))

                else
                    Nothing
           )
