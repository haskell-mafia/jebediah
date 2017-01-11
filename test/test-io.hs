import           Disorder.Core.Main

import qualified Test.IO.Jebediah.Conduit
import qualified Test.IO.Jebediah.File
import qualified Test.IO.Jebediah.Structure


main :: IO ()
main =
  disorderMain [
      Test.IO.Jebediah.Conduit.tests
    , Test.IO.Jebediah.File.tests
    , Test.IO.Jebediah.Structure.tests
    ]
