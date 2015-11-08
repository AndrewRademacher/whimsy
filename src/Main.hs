import           System.Environment

import qualified Impl.IORef1        as IORef1
import qualified Impl.IORef2        as IORef2

main :: IO ()
main = do
    [impl] <- getArgs
    case impl of
        "ioref-1" -> IORef1.runApp
        "ioref-2" -> IORef2.runApp
        v         ->
            putStrLn $ "Invalid implementation: " ++ v
