module VaporTrail.FileWatcher (transmitDirectory) where

import qualified Codec.Archive.Tar as Tar
import Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TQueue
import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as Lazy
import Data.List
import Data.Word
import qualified System.Directory as Directory
import qualified System.FSNotify as FSNotify
import qualified System.FilePath as FilePath
import qualified System.Process as Process
import VaporTrail.Codec
import qualified System.IO as IO

data Env = Env
  { envEventQueue :: TQueue FSNotify.Event
  , envBaseDir :: FilePath
  , envTransmitFrequency :: Int
  }

getNewQueueges :: ReaderT Env IO [FilePath]
getNewQueueges = do
  eventQueue <- asks envEventQueue
  let readEvents = do
        next <- tryReadTQueue eventQueue
        case next of
          Nothing -> return []
          Just evt -> fmap (FSNotify.eventPath evt :) readEvents
  liftIO $ do
    uniqueEvents <- fmap nub (STM.atomically readEvents)
    filterM Directory.doesFileExist uniqueEvents

handleEvents :: ReaderT Env IO ()
handleEvents = forever $ getNewQueueges >>= transmitFiles

rpitxTransmit :: [Word8] -> ReaderT Env IO ()
rpitxTransmit bytes = do
  freq <- asks envTransmitFrequency
  let cmd = "rpitx"
      args = ["-m", "RF", "-f", show freq, "-i", "/dev/stdin"]
      toneBytes = encodePacketsTone [bytes]
      createProc =
        (Process.proc cmd args)
        { Process.std_in = Process.CreatePipe
        , Process.std_out = Process.CreatePipe
        , Process.std_err = Process.CreatePipe
        }
      doProc (Just hin) (Just hout) (Just herr) procHandle =
        Async.race_
          (Async.race_
             (IO.hGetContents hout >>= putStr)
             (IO.hGetContents herr >>= IO.hPutStr IO.stderr))
          (do Lazy.putStr (Lazy.pack toneBytes)
              IO.hFlush hin
              IO.hClose hin
              void (Process.waitForProcess procHandle))
      doProc _ _ _ _ =
        IO.hPutStrLn
          IO.stderr
          "Error creating rpitx process for transmission - no STDIN pipe"
  liftIO (Process.withCreateProcess createProc doProc)

transmitFiles :: [FilePath] -> ReaderT Env IO ()
transmitFiles [] = return ()
transmitFiles paths = do
  liftIO (putStrLn ("Transmitting files: " ++ show paths))
  baseDir <- asks envBaseDir
  let relPaths = fmap (FilePath.makeRelative baseDir) paths
  bytes <- liftIO (fmap Tar.write (Tar.pack baseDir relPaths))
  rpitxTransmit (Lazy.unpack bytes)
  
eventFilter :: FSNotify.Event -> Bool
eventFilter (FSNotify.Added _ _) = True
eventFilter (FSNotify.Modified _ _) = True
eventFilter (FSNotify.Removed _ _ ) = False

watch :: ReaderT Env IO ()
watch = do
  env <- ask
  let doWatch mgr =
        FSNotify.watchTree
          mgr
          (envBaseDir env)
          eventFilter
          (\evt ->
             print evt *> (STM.atomically . writeTQueue (envEventQueue env)) evt)
  liftIO
    (FSNotify.withManager (\mgr -> doWatch mgr *> runReaderT handleEvents env))

transmitDirectory :: Int -> FilePath -> IO ()
transmitDirectory frequency baseDir = do
  eventQueue <- STM.atomically newTQueue
  let env =
        Env
        { envEventQueue = eventQueue
        , envBaseDir = baseDir
        , envTransmitFrequency = frequency
        }
  runReaderT watch env
