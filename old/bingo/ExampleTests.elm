import ElmTest exposing (..)
import String exposing (..)
import Graphics.Element exposing (..)


bmap : List BPair
bmap = 
  [
    newPair '(' ')',
    newPair '{' '}' 
  ]


type alias BPair = {
  opener: Char, 
  closer: Char,
  isEnabled: Bool,
  id: Int
}

type alias BMap = List BPair

newPair : Char -> Char -> BPair
newPair op cl =
  { opener = op,
    closer = cl,
    isEnabled = True,
    id = 0
  }




tests: Test
tests = suite "My Test Suite"
        [ test "Addition" (assertEqual (3 + 7) 10)
        , test "String.reverse" (assertEqual "ekiM" (String.reverse "Mike"))
        , test "This test should pass" (assert True)
        , test "This test should fail" (assert False)
        , test "Get enabled pair" (assertEqual 2 (List.length (bmap)) 
        ]
{-
main = 
  runDisplay tests

-}

main : Element
main = 
    elementRunner tests