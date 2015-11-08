{-# LANGUAGE DeriveGeneric #-}

import           Control.Concurrent.Async
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Criterion.Main
import           Data.Char                  as Char
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           GHC.Generics
import           Servant.Server
import           System.Random.MWC.Monad    as MWC

import           API
import qualified Impl.IORef1                as IORef1
import qualified Impl.IORef2                as IORef2
import qualified Impl.MVar1                 as MVar1
import qualified Impl.MVar2                 as MVar2
import qualified Impl.STM1                  as STM1
import qualified Impl.TVar1                 as TVar1

type Generator a = MWC.Rand IO a

type Players = Int
type Trades  = Int

data TestEnvironment
    = TestEnvironment
        { teFortunes  :: Vector NewPlayer
        , teTrades    :: Vector (Email, Email)
        , teIORef1Env :: IORef1.Environment
        , teIORef2Env :: IORef2.Environment
        , teMVar1Env  :: MVar1.Environment
        , teMVar2Env  :: MVar2.Environment
        , teTVar1Env  :: TVar1.Environment
        , teSTM1Env   :: STM1.Environment
        }

main :: IO ()
main = do
    te <- MWC.runWithSystemRandom (createOffsetTestEnv 1000000 100000)
    let threads = 8
    defaultMain
        [
            bgroup "create players"
                [ bench "ioref-1" $ whnfIO $ benchCreate threads (teFortunes te) $ IORef1.createPlayer (teIORef1Env te)
                , bench "ioref-2" $ whnfIO $ benchCreate threads (teFortunes te) $ IORef2.createPlayer (teIORef2Env te)
                , bench "mvar-1"  $ whnfIO $ benchCreate threads (teFortunes te) $ MVar1.createPlayer (teMVar1Env te)
                , bench "mvar-2"  $ whnfIO $ benchCreate threads (teFortunes te) $ MVar2.createPlayer (teMVar2Env te)
                , bench "tvar-1"  $ whnfIO $ benchCreate threads (teFortunes te) $ TVar1.createPlayer (teTVar1Env te)
                , bench "stm-1"   $ whnfIO $ benchCreate threads (teFortunes te) $ STM1.createPlayer (teSTM1Env te)
                ]
        ,   bgroup "make trades"
                [ bench "ioref-1" $ whnfIO $ benchTrade threads (teTrades te) $ IORef1.tradeFortunes (teIORef1Env te)
                , bench "ioref-2" $ whnfIO $ benchTrade threads (teTrades te) $ IORef2.tradeFortunes (teIORef2Env te)
                , bench "mvar-1"  $ whnfIO $ benchTrade threads (teTrades te) $ MVar1.tradeFortunes (teMVar1Env te)
                , bench "mvar-2"  $ whnfIO $ benchTrade threads (teTrades te) $ MVar2.tradeFortunes (teMVar2Env te)
                , bench "tvar-1"  $ whnfIO $ benchTrade threads (teTrades te) $ TVar1.tradeFortunes (teTVar1Env te)
                , bench "stm-1"   $ whnfIO $ benchTrade threads (teTrades te) $ STM1.tradeFortunes (teSTM1Env te)
                ]
        ]

---

benchCreate :: Word -> Vector NewPlayer -> (NewPlayer -> EitherT ServantErr IO ()) -> IO ()
benchCreate = undefined

benchTrade :: Word -> Vector (Email, Email) -> (Email -> Email -> EitherT ServantErr IO FortunePair) -> IO ()
benchTrade = undefined

---

createOffsetTestEnv :: Players -> Trades -> Generator TestEnvironment
createOffsetTestEnv pcount tcount = do
    v <- V.replicateM pcount newplayerGenerator
    f <- V.replicateM tcount (MWC.uniformR (0, pcount))
    t <- V.replicateM tcount (MWC.uniformR (0, pcount))
    let ts = V.map (\(fix, tix) -> (npEmail (v V.! fix), npEmail (v V.! tix)))
           $ V.zip f t
    TestEnvironment v ts
        <$> liftIO IORef1.createEnvironment
        <*> liftIO IORef2.createEnvironment
        <*> liftIO MVar1.createEnvironment
        <*> liftIO MVar2.createEnvironment
        <*> liftIO TVar1.createEnvironment
        <*> liftIO STM1.createEnvironment

newplayerGenerator :: Generator NewPlayer
newplayerGenerator = NewPlayer <$> textGenerator <*> textGenerator

textGenerator :: Generator Text
textGenerator = do
  l <- length'
  s <- replicateM l char
  return $! T.pack s
  where
    length' = MWC.uniformR (7, 20)
    char = Char.chr <$> MWC.uniformR (Char.ord 'a', Char.ord 'z')
