module Main where

import Data.Aeson
import Data.Aeson.Encode.Pretty

import Data.Text
import Data.Text.Lazy
import Data.Text.IO as T
import Data.ByteString.Lazy as B

import Myelin.Examples


main = B.putStrLn $ encodePretty $ toJSON exampleTask