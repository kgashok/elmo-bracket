import Graphics.Element exposing (Element, leftAligned)
import Http
import Task exposing (Task, andThen)
import Text exposing (fromString)

fibMailbox : Signal.Mailbox String
fibMailbox = Signal.mailbox ""  

getFibTask : Task Http.Error ()
getFibTask = Http.getString "http://localhost:5000/fibonacci/5" `andThen` (Signal.send fibMailbox.address)

port getFib : Task Http.Error ()
port getFib = getFibTask

main : Signal Element
main =
  Signal.map (leftAligned << fromString) fibMailbox.signal