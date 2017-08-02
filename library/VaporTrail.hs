module VaporTrail where

import System.Environment
import System.IO
import VaporTrail.Codec
import VaporTrail.Codec.PCM
import VaporTrail.FileWatcher
import VaporTrail.Filter.Basic
import VaporTrail.Filter.Compressor
import Control.Lens
import Control.Monad
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.List

sampleRate :: Int
sampleRate = 48000

dataRate :: Int
dataRate = 2000

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["enc_pcm"] -> do
      output <- fmap (encodePacketsPcm . (: []) . Lazy.unpack) Lazy.getContents
      Lazy.putStr (Lazy.pack output)
    ["enc"] -> do
      output <- fmap (encodePacketsTone . (: []) . Lazy.unpack) Lazy.getContents
      Lazy.putStr (Lazy.pack output)
    ["dec"] -> do
      output <- fmap (decodePacketsPcm . Lazy.unpack) Lazy.getContents
      forM_ output $ \xs -> do
        Strict.putStr (Strict.pack xs)
        hFlush stdout
    {-["tr"] ->-}
      {-encodeThroughPipe-}
        {-(view pcms16le .-}
         {-((colimiter 1.0 (4000 / fromIntegral dataRate) 0 sampleRate) .-}
          {-lowPass12db (fromIntegral dataRate * 4) sampleRate) .-}
         {-view (from pcms16le))-}
        {-stdin-}
        {-stdout-}
    ["transmit", frequency, baseDir] ->
      transmitDirectory (read frequency) baseDir
    _ -> putStrLn "Usage: vaportrail <enc_pcm|enc|dec|transmit <frequency> <path>>"
      
