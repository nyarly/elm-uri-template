module Tests exposing (..)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Json.Decode as D exposing (Decoder, int, string)
import Test exposing (..)
import Url.Interpolate exposing (Context, addAssoc, addList, addScalar, interpolate, simpleContext)


type alias Cases =
    { data : Context
    , expectations : List ( String, String )
    }


{-| Note: these are taken from the [test suite][test-suite], with a few extras.

[test-suite]: https://github.com/uri-templates/uritemplate-test/blob/master/spec-examples.json

-}
suite : Test
suite =
    case D.decodeString examplesDecoder specExamples of
        Ok exampleConfig ->
            describe "specExamples"
                (List.map
                    (\( name, ex ) ->
                        describe name <|
                            List.map
                                (\( template, exp ) ->
                                    test template
                                        (\() ->
                                            ex.variables
                                                |> interpolate template
                                                |> matchOutput exp
                                        )
                                )
                                ex.testcases
                    )
                    exampleConfig
                )

        Err e ->
            test "oops"
                (\() ->
                    Expect.fail (D.errorToString e)
                )


matchOutput : List String -> String -> Expectation
matchOutput acceptable actual =
    if List.any (\n -> n == actual) acceptable then
        Expect.pass

    else
        Expect.fail ("expected \"" ++ actual ++ "\" in " ++ Debug.toString acceptable)


type alias Examples =
    List ( String, Example )


type alias Example =
    { level : Int
    , variables : Context
    , testcases : List TestCase
    }


type alias TestCase =
    ( String, List String )


examplesDecoder : Decoder Examples
examplesDecoder =
    D.keyValuePairs
        (D.map3 Example
            (D.field "level" int)
            (D.field "variables" contextDecoder)
            (D.field "testcases" (D.list testCaseDecoder))
        )


type TestValue
    = Scalar String
    | Multi (List String)
    | Assoc (Dict String String)


contextDecoder : Decoder Context
contextDecoder =
    let
        remapValues : Dict String TestValue -> Context
        remapValues d =
            List.foldl
                (\k ->
                    \a ->
                        case Dict.get k d of
                            Just (Scalar s) ->
                                addScalar a k s

                            Just (Multi l) ->
                                addList a k l

                            Just (Assoc c) ->
                                addAssoc a k c

                            Nothing ->
                                addScalar a k "wtfbbq"
                )
                (simpleContext
                    []
                )
                (Dict.keys d)
    in
    D.map
        remapValues
        (D.dict
            (D.oneOf
                [ D.map Scalar string
                , D.map Multi (D.list string)
                , D.map Assoc (D.dict string)
                ]
            )
        )


testCaseDecoder : Decoder TestCase
testCaseDecoder =
    D.map2 Tuple.pair
        (D.index 0 string)
        (D.index 1
            (D.oneOf
                [ D.map (\i -> [ i ]) string
                , D.list string
                ]
            )
        )


specExamples : String
specExamples =
    """
{
  "Level 1 Examples" :
  {
    "level": 1,
    "variables": {
       "var"   : "value",
       "hello" : "Hello World!"
     },
     "testcases" : [
        ["{var}", "value"],
        ["'{var}'", "'value'"],
        ["{hello}", "Hello%20World%21"]
     ]
  },
  "Level 2 Examples" :
  {
    "level": 2,
    "variables": {
       "var"   : "value",
       "hello" : "Hello World!",
       "path"  : "/foo/bar"
     },
     "testcases" : [
        ["{+var}", "value"],
        ["{+hello}", "Hello%20World!"],
        ["{+path}/here", "/foo/bar/here"],
        ["here?ref={+path}", "here?ref=/foo/bar"]
     ]
  },
  "Level 3 Examples" :
  {
    "level": 3,
    "variables": {
       "var"   : "value",
       "hello" : "Hello World!",
       "empty" : "",
       "path"  : "/foo/bar",
       "x"     : "1024",
       "y"     : "768"
     },
     "testcases" : [
        ["map?{x,y}", "map?1024,768"],
        ["{x,hello,y}", "1024,Hello%20World%21,768"],
        ["{+x,hello,y}", "1024,Hello%20World!,768"],
        ["{+path,x}/here", "/foo/bar,1024/here"],
        ["{#x,hello,y}", "#1024,Hello%20World!,768"],
        ["{#path,x}/here", "#/foo/bar,1024/here"],
        ["X{.var}", "X.value"],
        ["X{.x,y}", "X.1024.768"],
        ["{/var}", "/value"],
        ["{/var,x}/here", "/value/1024/here"],
        ["{;x,y}", ";x=1024;y=768"],
        ["{;x,y,empty}", ";x=1024;y=768;empty"],
        ["{?x,y}", "?x=1024&y=768"],
        ["{?x,y,empty}", "?x=1024&y=768&empty="],
        ["?fixed=yes{&x}", "?fixed=yes&x=1024"],
        ["{&x,y,empty}", "&x=1024&y=768&empty="]
     ]
  },
  "Level 4 Examples" :
  {
    "level": 4,
    "variables": {
      "var": "value",
      "hello": "Hello World!",
      "path": "/foo/bar",
      "list": ["red", "green", "blue"],
      "keys": {"semi": ";", "dot": ".", "comma":","}
    },
    "testcases": [
      ["{var:3}", "val"],
      ["{var:30}", "value"],
      ["{list}", "red,green,blue"],
      ["{list*}", "red,green,blue"],
      ["{keys}", [
        "comma,%2C,dot,.,semi,%3B",
        "comma,%2C,semi,%3B,dot,.",
        "dot,.,comma,%2C,semi,%3B",
        "dot,.,semi,%3B,comma,%2C",
        "semi,%3B,comma,%2C,dot,.",
        "semi,%3B,dot,.,comma,%2C"
      ]],
      ["{keys*}", [
        "comma=%2C,dot=.,semi=%3B",
        "comma=%2C,semi=%3B,dot=.",
        "dot=.,comma=%2C,semi=%3B",
        "dot=.,semi=%3B,comma=%2C",
        "semi=%3B,comma=%2C,dot=.",
        "semi=%3B,dot=.,comma=%2C"
      ]],
      ["{+path:6}/here", "/foo/b/here"],
      ["{+list}", "red,green,blue"],
      ["{+list*}", "red,green,blue"],
      ["{+keys}", [
        "comma,,,dot,.,semi,;",
        "comma,,,semi,;,dot,.",
        "dot,.,comma,,,semi,;",
        "dot,.,semi,;,comma,,",
        "semi,;,comma,,,dot,.",
        "semi,;,dot,.,comma,,"
      ]],
      ["{+keys*}", [
        "comma=,,dot=.,semi=;",
        "comma=,,semi=;,dot=.",
        "dot=.,comma=,,semi=;",
        "dot=.,semi=;,comma=,",
        "semi=;,comma=,,dot=.",
        "semi=;,dot=.,comma=,"
      ]],
      ["{#path:6}/here", "#/foo/b/here"],
      ["{#list}", "#red,green,blue"],
      ["{#list*}", "#red,green,blue"],
      ["{#keys}", [
        "#comma,,,dot,.,semi,;",
        "#comma,,,semi,;,dot,.",
        "#dot,.,comma,,,semi,;",
        "#dot,.,semi,;,comma,,",
        "#semi,;,comma,,,dot,.",
        "#semi,;,dot,.,comma,,"
      ]],
      ["{#keys*}", [
        "#comma=,,dot=.,semi=;",
        "#comma=,,semi=;,dot=.",
        "#dot=.,comma=,,semi=;",
        "#dot=.,semi=;,comma=,",
        "#semi=;,comma=,,dot=.",
        "#semi=;,dot=.,comma=,"
      ]],
      ["X{.var:3}", "X.val"],
      ["X{.list}", "X.red,green,blue"],
      ["X{.list*}", "X.red.green.blue"],
      ["X{.keys}", [
        "X.comma,%2C,dot,.,semi,%3B",
        "X.comma,%2C,semi,%3B,dot,.",
        "X.dot,.,comma,%2C,semi,%3B",
        "X.dot,.,semi,%3B,comma,%2C",
        "X.semi,%3B,comma,%2C,dot,.",
        "X.semi,%3B,dot,.,comma,%2C"
      ]],
      ["{/var:1,var}", "/v/value"],
      ["{/list}", "/red,green,blue"],
      ["{/list*}", "/red/green/blue"],
      ["{/list*,path:4}", "/red/green/blue/%2Ffoo"],
      ["{/keys}", [
        "/comma,%2C,dot,.,semi,%3B",
        "/comma,%2C,semi,%3B,dot,.",
        "/dot,.,comma,%2C,semi,%3B",
        "/dot,.,semi,%3B,comma,%2C",
        "/semi,%3B,comma,%2C,dot,.",
        "/semi,%3B,dot,.,comma,%2C"
      ]],
      ["{/keys*}", [
        "/comma=%2C/dot=./semi=%3B",
        "/comma=%2C/semi=%3B/dot=.",
        "/dot=./comma=%2C/semi=%3B",
        "/dot=./semi=%3B/comma=%2C",
        "/semi=%3B/comma=%2C/dot=.",
        "/semi=%3B/dot=./comma=%2C"
      ]],
      ["{;hello:5}", ";hello=Hello"],
      ["{;list}", ";list=red,green,blue"],
      ["{;list*}", ";list=red;list=green;list=blue"],
      ["{;keys}", [
        ";keys=comma,%2C,dot,.,semi,%3B",
        ";keys=comma,%2C,semi,%3B,dot,.",
        ";keys=dot,.,comma,%2C,semi,%3B",
        ";keys=dot,.,semi,%3B,comma,%2C",
        ";keys=semi,%3B,comma,%2C,dot,.",
        ";keys=semi,%3B,dot,.,comma,%2C"
      ]],
      ["{;keys*}", [
        ";comma=%2C;dot=.;semi=%3B",
        ";comma=%2C;semi=%3B;dot=.",
        ";dot=.;comma=%2C;semi=%3B",
        ";dot=.;semi=%3B;comma=%2C",
        ";semi=%3B;comma=%2C;dot=.",
        ";semi=%3B;dot=.;comma=%2C"
      ]],
      ["{?var:3}", "?var=val"],
      ["{?list}", "?list=red,green,blue"],
      ["{?list*}", "?list=red&list=green&list=blue"],
      ["{?keys}", [
        "?keys=comma,%2C,dot,.,semi,%3B",
        "?keys=comma,%2C,semi,%3B,dot,.",
        "?keys=dot,.,comma,%2C,semi,%3B",
        "?keys=dot,.,semi,%3B,comma,%2C",
        "?keys=semi,%3B,comma,%2C,dot,.",
        "?keys=semi,%3B,dot,.,comma,%2C"
      ]],
      ["{?keys*}", [
        "?comma=%2C&dot=.&semi=%3B",
        "?comma=%2C&semi=%3B&dot=.",
        "?dot=.&comma=%2C&semi=%3B",
        "?dot=.&semi=%3B&comma=%2C",
        "?semi=%3B&comma=%2C&dot=.",
        "?semi=%3B&dot=.&comma=%2C"
      ]],
      ["{&var:3}", "&var=val"],
      ["{&list}", "&list=red,green,blue"],
      ["{&list*}", "&list=red&list=green&list=blue"],
      ["{&keys}", [
        "&keys=comma,%2C,dot,.,semi,%3B",
        "&keys=comma,%2C,semi,%3B,dot,.",
        "&keys=dot,.,comma,%2C,semi,%3B",
        "&keys=dot,.,semi,%3B,comma,%2C",
        "&keys=semi,%3B,comma,%2C,dot,.",
        "&keys=semi,%3B,dot,.,comma,%2C"
      ]],
      ["{&keys*}", [
        "&comma=%2C&dot=.&semi=%3B",
        "&comma=%2C&semi=%3B&dot=.",
        "&dot=.&comma=%2C&semi=%3B",
        "&dot=.&semi=%3B&comma=%2C",
        "&semi=%3B&comma=%2C&dot=.",
        "&semi=%3B&dot=.&comma=%2C"
      ]]
    ]
  }
}
"""
