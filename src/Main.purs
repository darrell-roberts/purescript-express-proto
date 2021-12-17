module Main where

import Prelude

import Effect (Effect)
import Framework.Arguments (parseOptions)
import Framework.Express (getAppContext, listenHttp, useRouter)
import Framework.Logger (logDebug, logInfo)
import Framework.MicroService.MSMiddleware (ReqFlow(..), loggerErrorHandler, loggerHandler, requestIdHandler, responseErrorHandler, responseHandler)
import Framework.Types (AppContext(..), ProgramOptions(..))
import Node.Express.App (App, get, use, useOnError)
import Node.Express.Response (send)
import Node.HTTP (Server)
import SampleService.Routes (someRoute, userRoute)

-- | Configures the express app and adds routes
app :: App
app = do
  getAppContext >>= logInfo <<< ((<>) "using app context: ") <<< show
  -- logInfo $ "using app context: " <> show appContext
  logDebug "Setting up express application"
  -- Pre middleware
  use requestIdHandler
  use $ loggerHandler ReqStart
  -- Routes
  get "/" $ send "hello!"
  useRouter "/some" someRoute
  useRouter "/user" userRoute
  -- Post middleware
  use responseHandler
  use $ loggerHandler ReqEnd
  -- Error handlers
  useOnError loggerErrorHandler
  useOnError responseErrorHandler

-- appNew :: ReaderT AppContext App
-- appNew = do
--   get "/" $ send "hello!"

main :: Effect Server
main = do
  p@(ProgramOptions opts) <- parseOptions
  let
    appContext = AppContext { options: p }
  listenHttp appContext app opts.port \_ ->
    logDebug $ "Server started on " <> show opts.port
