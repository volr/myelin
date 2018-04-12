module Myelin.NEST(run) where

import Data.Aeson
import System.Process
import GHC.IO.Handle
import Data.ByteString.Lazy as B

import Myelin.SNN

run :: Task -> IO ByteString
run task = do
    let jsonDescription = encode $ toJSON task
    (Just scriptHandle, Just runOut, _, _) <- createProcess (proc "python" ["python/nest_pynn_executor.py"]) { std_in = CreatePipe, std_out = CreatePipe}
    B.hPutStr scriptHandle jsonDescription
    B.hGetContents runOut

