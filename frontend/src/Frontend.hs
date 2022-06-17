{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Frontend (frontend) where

import Control.Monad
import Control.Lens
import Control.Monad.IO.Class
import Data.Proxy
import Data.Text (Text)
import Text.Read

import qualified Data.Text as T

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend

import Reflex.Dom.Core

import Common.Route

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Obelisk scratchpad"
  , _frontend_body = body
  }

-- The type of 'body' type is dictated by 'Frontend'
-- ObeliskWidget is a ton of stuff, but critically, also MonadIO (Performable m)
-- The call to 'prerender_' is necessary in order to be able to do
-- 'performRequestAsync' (which can be done in @Client m@ but not in @m@).
body :: forall t m.
     ObeliskWidget t (R FrontendRoute) m
  => RoutedT t (R FrontendRoute) m ()
body = prerender_ blank $ client (Proxy @m)

client ::
     ObeliskWidget t (R FrontendRoute) m
  => Proxy m -> RoutedT t (R FrontendRoute) (Client m) ()
client _ = do
    el "p" $ do
      die <- button "Roll local die"
      text " "
      output   <- performEvent (liftIO localDie <$ die)
      rendered <- holdDyn "die not yet rolled" (showLocalDie <$> output)
      dynText rendered
    el "p" $ do
      die <- button "Roll remote die"
      text " "
      output <- performRequestAsync (remoteDie <$ die)
      rendered <- holdDyn "die not yet rolled" (showRemoteDie <$> output)
      dynText rendered
  where
    showLocalDie :: Int -> Text
    showLocalDie = T.pack . show

    showRemoteDie :: XhrResponse -> Text
    showRemoteDie =
          maybe "failure" (T.pack . show)
        . ((^. xhrResponse_responseText) >=> parseRemote)

    parseRemote :: Text -> Maybe Int
    parseRemote = readMaybe . T.unpack

remoteDie :: XhrRequest ()
remoteDie =
    xhrRequest
      "GET"
      (renderBackendRoute encoder $ BackendRoute_Die :/ ())
      def
  where
    encoder :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
    encoder = case checkEncoder fullRouteEncoder of
                Right e -> e
                Left _  -> error "encoder failure"

localDie :: IO Int
localDie = return 4

