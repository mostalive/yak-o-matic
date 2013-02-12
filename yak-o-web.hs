{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty(scotty, get, text, middleware)
import Control.Monad.Trans (liftIO)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Data.Text.Lazy.Encoding as E

-- for testing
import Network.HTTP as H

import YakWeb
import YakCli

-- | Launch web server listening on port 3000 and outputting yak for current directory
-- main >> H.simpleHTTP (H.getRequest "http://localhost:3000/") >>= H.getResponseBody >> 
-- ""
main :: IO ()
main = do 
  scotty 3000 $ do
    middleware logStdoutDev
    middleware (staticPolicy $ addBase "static/")

    get "/yak" $ do
      yak <- liftIO $ outputCfdData emptyOptions
      text $ E.decodeUtf8 $ toJsonString yak
    
