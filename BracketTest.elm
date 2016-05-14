import ElmTest exposing (..)
import String exposing (..)
import Graphics.Element exposing (..)

import Bracket exposing (Model, validate, isValid, validateString, initialModel)

testm : Model
testm = {initialModel| expression = "()"} 

tests: Test
tests = suite "My Test Suite"
  [ test "Addition" (assertEqual (3 + 7) 10)
  , test "String.reverse" (assertEqual "ekiM" (String.reverse "Mike"))
  , test "This test should pass" (assert True)
  , test "This test should fail" (assert False)
  , test "Get enabled pair" (assertEqual 3 (List.length (initialModel.bmap)) )
  , test "Test simple expression" (assertEqual True (.isValid (validateString testm)))
  , test "Test invalid expression" (assertEqual False 
            (.isValid (validate {testm|expression = "({)"})))
  , test "Imbalanced expression must display as so" (assertEqual "is imbalanced"
            (String.trimLeft (isValid (validate {testm|expression = "((({{"} ))) )
  , test "Invalid expression must display as so" (assertEqual "is invalid"
            (String.trimLeft (isValid (validate {testm|expression = "(>)"} ))) )
  ]


main : Element
main = 
    elementRunner tests


{-
main = 
  runDisplay tests

-}
