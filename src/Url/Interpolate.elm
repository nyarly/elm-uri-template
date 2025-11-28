module Url.Interpolate exposing
    ( interpolate
    , Context
    , simpleContext
    , addScalar
    , addList
    , addAssoc
    )

{-| Url.Interpolate provides a primary entrypoint function, `interpolate`, which takes
a URI Template string and a variable, and expands
the input string according to the rules in IETF RFC 6570,
up to Level 4.

@docs interpolate

@docs Context
@docs simpleContext
@docs addScalar
@docs addList
@docs addAssoc

[rfc6570]: https://tools.ietf.org/html/rfc6570

-}

import Dict exposing (Dict)
import Hex
import Maybe
import Regex exposing (Match, Regex)
import Set exposing (Set)


{-| the Context for a template interpolation
-}
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


{-| Works like Dict.fromList to produce a context of names to Scalar string values;
you could also use this to create an empty context like

    simpleContext []

-}
simpleContext : List ( String, String ) -> Context
simpleContext pairs =
    List.map (Tuple.mapSecond Scalar) pairs |> Dict.fromList


{-| add a scalar (in other words a simple string) value to the context
-}
addScalar : Context -> String -> String -> Context
addScalar context key scalar =
    Dict.insert key (Scalar scalar) context


{-| add a list of strings to the context
-}
addList : Context -> String -> List String -> Context
addList context key list =
    Dict.insert key (Multi list) context


{-| add an associated list (in other words a map) to a context
-}
addAssoc : Context -> String -> Dict String String -> Context
addAssoc context key assoc =
    Dict.insert key (Assoc assoc) context


{-| Example URI template interpolation:

    interpolate "<http://example.com/{path}{?x,y,empty}"> <|
      simpleContext [("path", "hello"), ("x", "1024"), ("y", "768")]

    -- "<http://example.com/hello?x=1024&y=768&empty=">

-}



{-

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
    unpackContext percentEncode vars replacements
        |> separatedBy ","


expandReservedString : List ( String, Modifier ) -> Context -> String
expandReservedString vars replacements =
    unpackContext percentEncodeReserved vars replacements
        |> separatedBy ","


expandFragment : List ( String, Modifier ) -> Context -> String
expandFragment vars replacements =
    "#"
        ++ (unpackContext percentEncodeReserved vars replacements
                |> separatedBy ","
           )


expandLabel : List ( String, Modifier ) -> Context -> String
expandLabel vars replacements =
    "."
        ++ (unpackContext percentEncode vars replacements
                |> separatedBy "."
           )


expandPath : List ( String, Modifier ) -> Context -> String
expandPath vars replacements =
    "/"
        ++ (unpackContext percentEncode vars replacements
                |> separatedBy "/"
           )


expandPathParam : List ( String, Modifier ) -> Context -> String
expandPathParam vars replacements =
    unpackQueryContext percentEncode vars replacements
        |> expandPathParamHelp


expandQuery : List ( String, Modifier ) -> Context -> String
expandQuery vars replacements =
    unpackQueryContext percentEncode vars replacements
        |> expandQueryHelp "?"


expandQueryContinuation : List ( String, Modifier ) -> Context -> String
expandQueryContinuation vars replacements =
    unpackQueryContext percentEncode vars replacements
        |> expandQueryHelp "&"


type alias AssocExploder =
    (String -> String) -> String -> Dict String String -> List ( String, String )


addUnpacked : (String -> String) -> AssocExploder -> Dict String Value -> ( String, Modifier ) -> List ( String, String ) -> List ( String, String )
addUnpacked encoding assocExplode context ( var, mod ) l =
    case ( mod, Dict.get var context ) of
        ( Prefix pre, Just (Scalar s) ) ->
            ( var, encoding (String.left pre s) ) :: l

        ( _, Just (Scalar s) ) ->
            ( var, encoding s ) :: l

        ( Explode, Just (Multi ss) ) ->
            List.map (\s -> ( var, encoding s )) ss ++ l

        ( _, Just (Multi ss) ) ->
            ( var, String.join "," (List.map encoding ss) ) :: l

        ( Explode, Just (Assoc d) ) ->
            assocExplode encoding var d ++ l

        ( _, Just (Assoc d) ) ->
            ( var, String.join "," (List.foldr (\( k, v ) -> \a -> k :: encoding v :: a) [] (Dict.toList d)) ) :: l

        ( _, Nothing ) ->
            ( var, "" ) :: l


explodeAssoc : AssocExploder
explodeAssoc encoding var d =
    List.map (\( k, v ) -> ( var, String.join "=" [ k, encoding v ] )) (Dict.toList d)


explodeAssocForQuery : AssocExploder
explodeAssocForQuery encoding _ d =
    Dict.toList d |> List.map (Tuple.mapSecond encoding)


unpackContext : (String -> String) -> List ( String, Modifier ) -> Dict String Value -> List ( String, String )
unpackContext encoding vars context =
    List.foldr (addUnpacked encoding explodeAssoc context) [] vars


unpackQueryContext : (String -> String) -> List ( String, Modifier ) -> Dict String Value -> List ( String, String )
unpackQueryContext encoding vars context =
    List.foldr (addUnpacked encoding explodeAssocForQuery context) [] vars



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


percentEncode : String -> String
percentEncode =
    percentEncodeExcept unreservedChars


percentEncodeReserved : String -> String
percentEncodeReserved =
    percentEncodeExcept reservedAndUnreserved


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
