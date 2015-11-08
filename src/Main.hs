import           System.Environment

import qualified Impl.IORef1        as IORef1

main :: IO ()
main = do
    [impl] <- getArgs
    case impl of
        "ioref-1" -> IORef1.runApp
        v         ->
            putStrLn $ "Invalid implementation: " ++ v
