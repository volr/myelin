module Main where

import Data.Aeson
import Data.Text
import Data.Text.Lazy
import Data.Text.IO as T
import Data.ByteString.Lazy as B

import Myelin.SNN
import Control.Monad.Trans.State
import Data.Aeson.Encode.Pretty


main =
    -- let (_, net) = runState netTest initialBlockState
    -- let dot = renderNetwork net
    -- T.putStr $ toStrict dot
    B.putStrLn $ encodePretty $ toJSON exampleTask