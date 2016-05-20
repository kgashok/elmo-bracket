import ElmTestBDDStyle exposing (..)
import Dict exposing (..)


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



tests : Test
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
                expect (List.length bmap) toBe 2
                
        ]
