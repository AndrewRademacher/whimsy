import           Control.Monad
import           Criterion.Main
import           Data.Char               as Char
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Vector             (Vector)
import qualified Data.Vector             as V
import           System.Random.MWC.Monad as MWC

import           API

type Generator a = MWC.Rand IO a

type Players = Int
type Trades  = Int

data TestEnvironment
    = TestEnvironment
        { teFortunes :: Vector NewPlayer
        , teTrades   :: Vector (Email, Email)
        }

main :: IO ()
main = do
    te <- MWC.runWithSystemRandom (createOffsetTestEnv 1000000 100000)
    print $ V.length $ teFortunes te
    print $ V.length $ teTrades te

createOffsetTestEnv :: Players -> Trades -> Generator TestEnvironment
createOffsetTestEnv pcount tcount = do
    v <- V.replicateM pcount newplayerGenerator
    f <- V.replicateM tcount (MWC.uniformR (0, pcount))
    t <- V.replicateM tcount (MWC.uniformR (0, pcount))
    let ts = V.map (\(fix, tix) -> (npEmail (v V.! fix), npEmail (v V.! tix))) $ V.zip f t
    return $ TestEnvironment v ts

newplayerGenerator :: Generator NewPlayer
newplayerGenerator = NewPlayer <$> textGenerator <*> textGenerator

textGenerator :: Generator Text
textGenerator = do
  l <- length
  s <- replicateM l char
  return $! T.pack s
  where
    length = MWC.uniformR (7, 20)
    char = Char.chr <$> MWC.uniformR (Char.ord 'a', Char.ord 'z')
