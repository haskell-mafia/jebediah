import           Disorder.Core.Main

import qualified Test.Jebediah.Data

main :: IO ()
main =
  disorderMain [
      Test.Jebediah.Data.tests
    ]
