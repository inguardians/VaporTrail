module VaporTrail where

import System.Environment
import System.IO
import VaporTrail.Codec
import VaporTrail.FileWatcher


main :: IO ()
main = do
  args <- getArgs
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  case args of
    ["enc_pcm"] -> encodeThroughPipe encodePacketsPcm stdin stdout
    ["enc"] -> encodeThroughPipe encodePacketsTone stdin stdout
    ["dec"] -> encodeThroughPipe decodePacketsPcm stdin stdout
    ["transmit", frequency, baseDir] ->
      transmitDirectory (read frequency) baseDir
    _ -> putStrLn "Usage: fsk <enc_pcm|enc|dec|transmit <frequency> <path>>"
      
