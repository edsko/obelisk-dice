{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend where

import Control.Monad.IO.Class
import System.Random

import qualified Data.ByteString.Char8 as BS.C8

import Common.Route

import Obelisk.Backend
import Obelisk.Route

import Snap

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve $ \case
       BackendRoute_Missing :/ () -> pure ()
       BackendRoute_Die     :/ () -> remoteDie
  , _backend_routeEncoder = fullRouteEncoder
  }

remoteDie :: Snap ()
remoteDie = do
    r :: Int <- liftIO $ randomRIO (1, 6)
    writeBS $ BS.C8.pack (show r)