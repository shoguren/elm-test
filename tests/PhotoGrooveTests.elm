module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import PhotoGroove exposing (..)
import Json.Decode exposing (decodeString, decodeValue)
import Json.Encode as Encode


suite : Test
suite =
    test "one plus one equals two" (\_ -> Expect.equal 2 (1 + 1))


decoderTest : Test
decoderTest =
    test "title defaults to (untitled)" <|
        \_ ->
            """
                {"url": "fruits.com", "size": 5}
            """
                |> decodeString photoDecoder
                |> Expect.equal
                    (Ok { url = "fruits.com", size = 5, title = "(Untitled)" })


decoderTest2 : Test
decoderTest2 =
    test "title only defaults to (untitled)" <|
        \_ ->
            """
                {"url": "fruits.com", "size": 5}
            """
                |> decodeString photoDecoder
                |> Result.map .title
                |> Expect.equal
                    (Ok "(Untitled)")


fuzzDecoderTest2 : Test
fuzzDecoderTest2 =
    fuzz2 string int "fuzztest: title only defaults to (untitled)" <|
        \url size ->
            Encode.object
                [ ( "url", Encode.string url )
                , ( "size", Encode.int size )
                ]
                |> decodeValue photoDecoder
                |> Result.map .title
                |> Expect.equal
                    (Ok "(Untitled)")
