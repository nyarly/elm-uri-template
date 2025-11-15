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


simpleContext : List ( String, String ) -> Context
simpleContext pairs =
    List.map (Tuple.mapSecond Scalar) pairs |> Dict.fromList


{-| Example URI template interpolation:

    interpolate "<http://example.com/{path}{?x,y,empty}"> <|
    Dict.fromList [("path", "hello"), ("x", "1024"), ("y", "768")]

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
    "\\{([+#.\\/;?&]{0,1})([A-Za-z0-9_,%]+)\\}"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


applyInterpolation : Context -> Match -> String
applyInterpolation replacements { match, submatches } =
    submatches
        |> getTemplateParts
        |> Maybe.map (\( operator, vars ) -> expand operator vars replacements)
        |> Maybe.withDefault match


getTemplateParts : List (Maybe String) -> Maybe ( String, List String )
getTemplateParts submatches =
    case submatches of
        [ Just operator, Just expression ] ->
            Just ( operator, String.split "," expression )

        [ Nothing, Just expression ] ->
            Just ( "", String.split "," expression )

        _ ->
            Nothing


expand : String -> List String -> Context -> String
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


expandSimple : List String -> Context -> String
expandSimple vars replacements =
    unpackContext vars replacements
        |> expandUnreservedStringSeparatedBy ","


expandReservedString : List String -> Context -> String
expandReservedString vars replacements =
    unpackContext vars replacements
        |> expandReservedStringSeparatedBy ","


expandFragment : List String -> Context -> String
expandFragment vars replacements =
    "#"
        ++ (unpackContext vars replacements
                |> expandReservedStringSeparatedBy ","
           )


expandLabel : List String -> Context -> String
expandLabel vars replacements =
    "."
        ++ (unpackContext vars replacements
                |> expandUnreservedStringSeparatedBy "."
           )


expandPath : List String -> Context -> String
expandPath vars replacements =
    "/"
        ++ (unpackContext vars replacements
                |> expandUnreservedStringSeparatedBy "/"
           )


expandPathParam : List String -> Context -> String
expandPathParam vars replacements =
    unpackContext vars replacements
        |> expandPathParamHelp


expandQuery : List String -> Context -> String
expandQuery vars replacements =
    unpackContext vars replacements
        |> expandQueryHelp "?"


expandQueryContinuation : List String -> Context -> String
expandQueryContinuation vars replacements =
    unpackContext vars replacements
        |> expandQueryHelp "&"


unpackContext : List String -> Dict String Value -> List ( String, String )
unpackContext vars context =
    let
        addUnpacked var l =
            case Dict.get var context of
                Just (Scalar s) ->
                    ( var, s ) :: l

                Just (Multi ss) ->
                    List.map (\s -> ( var, s )) ss ++ l

                Just (Assoc d) ->
                    Dict.toList d ++ l

                Nothing ->
                    ( var, "" ) :: l
    in
    List.foldr addUnpacked [] vars



-- HELPERS


expandPathParamHelp : List ( String, String ) -> String
expandPathParamHelp pairs =
    ";"
        ++ (pairs
                |> List.map (Tuple.mapSecond percentEncodeWithUnreserved)
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
                |> List.map (Tuple.mapSecond percentEncodeWithUnreserved)
                |> List.map (\( var, val ) -> var ++ "=" ++ val)
                |> String.join "&"
           )


expandUnreservedStringSeparatedBy : String -> List ( String, String ) -> String
expandUnreservedStringSeparatedBy sep pairs =
    pairs
        |> List.map
            (Tuple.second
                >> percentEncodeWithUnreserved
            )
        |> String.join sep


expandReservedStringSeparatedBy : String -> List ( String, String ) -> String
expandReservedStringSeparatedBy sep pairs =
    pairs
        |> List.map
            (Tuple.second
                >> percentEncodeWithReserved
            )
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


percentEncodeWithUnreserved : String -> String
percentEncodeWithUnreserved =
    percentEncodeExcept unreservedChars


percentEncodeWithReserved : String -> String
percentEncodeWithReserved =
    percentEncodeExcept (Set.union unreservedChars reservedChars)


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
