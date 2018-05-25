{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Myelin.Graphviz(run) where

import Control.Monad    
import Control.Monad.Trans.State


import Data.GraphViz.Types.Generalised
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Attributes.Colors.Brewer
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Printing
import Data.Text
import Data.Text.Lazy (toStrict)

import Myelin.SNN

toGraph :: BlockState -> DotGraph String
toGraph b = digraph (Str "Network") $ do
    nodes <- forM (_nodes b) $ node' . nodeLabel
    forM_ (_edges b) $ \Projection{..} -> (nodeLabel _input) --> (nodeLabel _output)
    where nodeLabel x = case x of
                            Population{..} -> "population:id:" ++ show _id ++ ":" ++ _label
                            Input{..} -> "input:" ++ show _id ++ ":" ++ _fileName
                            Output{..} -> "output:" ++ show _id ++ ":" ++ _fileName
                            SpikeSourceArray{..} -> "spike_source_array:" ++ show _id
                            SpikeSourcePoisson{..} -> "spike_source_poisson:" ++ show _id

renderNetwork = renderDot . toDot . toGraph

run :: Monad m => Task -> m Text
run task =
    let dot = renderNetwork (Prelude.head (_blocks $ _network $ task)) in
    return $ toStrict dot