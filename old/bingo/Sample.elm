module Sample where

import Stack exposing (..)
import String
import Html exposing (..)

reverseString : String -> String
reverseString str =
  String.split "" str
    |> Stack.fromList
    |> Stack.reverse
    |> Stack.toList
    |> String.join ""


main : Html.Html
main =
  reverseString "Hello World !" |> Html.text
