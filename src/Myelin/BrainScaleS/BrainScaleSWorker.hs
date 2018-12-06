{-# LANGUAGE OverloadedStrings #-}
module Myelin.BrainScaleS.BrainScaleSWorker where

{--
import System.ZMQ4.Monadic


taskBroker :: ZMQ z ()
taskBroker self = runZMQ $ do
  -- Prepare local frontend and backend
  frontEnd <- socket Router
  bind localEnd (connectString self "local_frontend")
  backEnd <- socket Router
  bind backEnd (connectString self "local_backend")
  



workerTask :: Show a => ByteString -> a -> ZMQ z ()
workerTask self i = do
    worker <- socket Req
    connect worker (connectString self "localbe")
    let ident = "Worker-" <> self <> C.pack (show i)
    setIdentity (restrict ident) worker

    -- Tell broker we're ready for work
    send worker [] "READY"

    -- Process messages as they arrive
    forever $ do
        msg <- receiveMulti worker
        -- Workers are busy for 0-1 seconds
        liftIO $ randomRIO (0,1000000) >>= threadDelay
        sendMulti worker (N.fromList msg)
--}
