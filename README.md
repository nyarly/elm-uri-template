# Url.Interpolate

Inject values from a Dict into a [URI Template][rfc6570].

## Documentation

`Url.Interpolate` provides a single function, `interpolate`, which takes
a [URI Template][rfc6570] string and a Dict of variables, and expands
the input string according to the rules in [IETF RFC 6570][rfc6570],
up to Level 4

This package was forked from [ericgj/elm-uri-template](https://github.com/ericgj/elm-uri-template)
This package was adapted from [lukewestby/elm-string-interpolate][lukewestby].


**Example:**
```elm
interpolate "http://example.com/{path}{?x,y,empty}" <|
    simpleContext [("path", "hello"), ("x", "1024"), ("y", "768")]

-- "http://example.com/hello?x=1024&y=768&empty="
```

[rfc6570]: https://tools.ietf.org/html/rfc6570
[lukewestby]: https://github.com/lukewestby/elm-string-interpolate
