module Myelin.Spikey(run) where

import Data.Aeson
import System.Process
import GHC.IO.Handle

import Myelin.SNN


run :: Task -> IO String
run model = do
  let pythonCode = show $ encode $ toJSON model
  (Just scriptHandle, Just runOut, _, _) <- createProcess (proc "python" ["python/spikey_pynn_executor.py"]) { std_in = CreatePipe, std_out = CreatePipe}
  hPutStr scriptHandle pythonCode
  hGetContents runOut
