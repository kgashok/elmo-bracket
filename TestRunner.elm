module Main where

import Task
import Console exposing (..)
import Graphics.Element exposing (Element)
import ElmTest exposing (..)

import ElmTestBDDStyle exposing (..)
import Dict exposing (..)

-- import BDD

--tests : Test
--tests =
--  BDD.tests

type alias BPair = {
  opener: Char, 
  closer: Char,
  isEnabled: Bool,
  id: Int
}   

newPair : Char -> Char -> BPair
newPair op cl =
    { opener = op,
      closer = cl,
      isEnabled = True,
      id = 0
    }


tests : ElmTestBDDStyle.Test
tests = 
    describe "Dictionary Testing"
        [ it "does math correctly " <| 
            expect (1+1) toBe 2, 

          it "does dictionary correctly" <|
            let 
                d = fromList
                    [("gates", "Seattle"),
                    ("gandhi", "gandhinagar"), 
                    ("steve", "mountain view")
                    ]
                v = get "gates" d 
            in
                expect (get "gates" d) toBe (Just "Seattle"),

          it "gets closer for corresponding opener" <|
            let 
                map = fromList 
                        [ ('(', ')'),
                          ('{', '}')
                        ]
            in 
                expect (get '(' map) toBe (Just ')'),

          it "returns Nothing for a non existent closer" <|
            let 
                map = fromList 
                        [ ('(', ')'),
                          ('{', '}')
                        ]
            in 
                expect (get '<' map) toBe Nothing ,


          it "returns membership in the Opener list" <|
            let 
                map = fromList 
                  [ ('(', ')'),
                    ('{', '}')
                  ]
            in 
                expect (member '(' map) toBeTruthy , 

          
          it "returns enabled and match bracket pair" <|
            let 
              bmap = 
                [
                    newPair '(' ')',
                    newPair '{' '}' 
                  ]
            in 
                expect (List.length bmap) toBe 3                
        ]



main : Element
main =
    elementRunner tests

port runner : Signal (Task.Task x ())
port runner =
    Console.run <| consoleRunner tests