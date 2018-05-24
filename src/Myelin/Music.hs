{-# Language TemplateHaskell, OverloadedStrings #-}
module Myelin.Music where

import qualified Data.Map as Map
import Data.Monoid

-- import qualified Language.C.Inline as C
-- import           Language.C.Inline.Context
import Foreign.Ptr (Ptr)
import Data.ByteString


import Myelin.Music.Internal

-- C.context context
-- C.include "<music-c.h>"

-- MUSIC_Setup *MUSIC_createSetup (int *argc, char ***argv);
createSetup :: IO (Ptr Setup)
createSetup = undefined

-- MUSIC_Setup *MUSIC_createSetupThread (int *argc,char ***argv,int required,int* provided);
createSetupThread = undefined

-- MUSIC_ContOutputPort *MUSIC_publishContOutput (MUSIC_Setup *setup, char *id);
publishContinuousOutput :: Ptr Setup -> ByteString -> IO (Ptr ContinuousOutputPort)
publishContinuousOutput = undefined
-- MUSIC_ContInputPort *MUSIC_publishContInput (MUSIC_Setup *setup, char *id);
publishContinuousInput :: Ptr Setup -> ByteString -> IO (Ptr ContinuousInputPort)
publishContinuousInput = undefined
-- MUSIC_EventOutputPort *MUSIC_publishEventOutput (MUSIC_Setup *setup, char *id);
publishEventOutputPort :: Ptr Setup -> ByteString -> IO (Ptr EventOutputPort)
publishEventOutputPort = undefined
-- MUSIC_EventInputPort *MUSIC_publishEventInput (MUSIC_Setup *setup, char *id);
publishEventInputPort :: Ptr Setup -> ByteString -> IO (Ptr EventInputPort)
publishEventInputPort = undefined
-- MUSIC_MessageOutputPort *MUSIC_publishMessageOutput (MUSIC_Setup *setup, char *id);
publishMessageOutputPort :: Ptr Setup -> ByteString -> IO (Ptr MessageOutputPort)
publishMessageOutputPort = undefined
-- MUSIC_MessageInputPort *MUSIC_publishMessageInput (MUSIC_Setup *setup, char *id);
publishMessageInputPort :: Ptr Setup -> ByteString -> IO (Ptr MessageInputPort)
publishMessageInputPort = undefined

-- int MUSIC_ContOutputPort_isConnected (MUSIC_ContOutputPort *port);
-- int MUSIC_ContInputPort_isConnected (MUSIC_ContInputPort *port);
-- int MUSIC_EventOutputPort_isConnected (MUSIC_EventOutputPort *port);
-- int MUSIC_EventInputPort_isConnected (MUSIC_EventInputPort *port);
-- int MUSIC_MessageOutputPort_isConnected (MUSIC_MessageOutputPort *port);
-- int MUSIC_MessageInputPort_isConnected (MUSIC_MessageInputPort *port);
-- int MUSIC_ContOutputPort_hasWidth (MUSIC_ContOutputPort *port);
-- int MUSIC_ContInputPort_hasWidth (MUSIC_ContInputPort *port);
-- int MUSIC_EventOutputPort_hasWidth (MUSIC_EventOutputPort *port);
-- int MUSIC_EventInputPort_hasWidth (MUSIC_EventInputPort *port);
-- int MUSIC_ContOutputPort_width (MUSIC_ContOutputPort *port);
-- int MUSIC_ContInputPort_width (MUSIC_ContInputPort *port);
-- int MUSIC_EventOutputPort_width (MUSIC_EventOutputPort *port);
-- int MUSIC_EventInputPort_width (MUSIC_EventInputPort *port);

-- void MUSIC_ContOutputPort_map (MUSIC_ContOutputPort *port, MUSIC_ContData *dmap, int maxBuffered);
-- void MUSIC_ContInputPort_map (MUSIC_ContInputPort *port, MUSIC_ContData *dmap, double delay, int maxBuffered, int interpolate);

-- void MUSIC_EventOutputPort_mapGlobalIndex (MUSIC_EventOutputPort *Port, MUSIC_IndexMap *indices, int maxBuffered);
-- void MUSIC_EventOutputPort_mapLocalIndex (MUSIC_EventOutputPort *Port, MUSIC_IndexMap *indices, int maxBuffered);



-- Runtime functions

-- MUSIC_Runtime *MUSIC_createRuntime (MUSIC_Setup *setup, double h);
createRuntime :: Ptr Setup -> Double -> IO (Ptr Runtime)
createRuntime = undefined
-- void MUSIC_tick (MUSIC_Runtime *runtime);
tick :: Ptr Runtime -> IO ()
tick = undefined
-- double MUSIC_time (MUSIC_Runtime *runtime);
time :: Ptr Runtime -> IO Double
time = undefined
-- void MUSIC_destroyRuntime (MUSIC_Runtime *runtime);
destroyRuntime :: Ptr Runtime -> IO ()
destroyRuntime = undefined

-- convenience function
withMusic setup time action = do
    runtime <- createRuntime setup time
    action runtime
    destroyRuntime runtime
