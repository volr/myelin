module Main where

import Data.Text
import Data.Text.Lazy
import Data.Text.IO as T
import Myelin.SNN

main = do
    net <- netTest
    let dot = renderNetwork net
    T.putStr $ toStrict dot

