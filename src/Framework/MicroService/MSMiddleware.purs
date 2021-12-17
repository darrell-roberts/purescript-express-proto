module Framework.MicroService.MSMiddleware
  ( ReqFlow(..)
  , loggerHandler
  , responseHandler
  , loggerErrorHandler
  , responseErrorHandler
  , requestIdHandler
  ) where

import Prelude
import Data.DateTime (diff)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Time.Duration (Milliseconds(..), fromDuration)
import Data.UUID (genUUID)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error, message)
import Effect.Now (nowDateTime)
import Node.Express.Handler (Handler, next, nextThrow)
import Node.Express.Request (getOriginalUrl, getUserData, setUserData, getRequestHeader)
import Node.Express.Response (sendJson, setStatus, end)
import Framework.Logger (logDebug)

data ReqFlow
  = ReqStart
  | ReqEnd

-- | Handler that logs information about request on enter and exit.
loggerHandler :: ReqFlow -> Handler
loggerHandler flow = do
  url <- getOriginalUrl
  d <- liftEffect nowDateTime
  case flow of
    ReqStart -> do
      logDebug $ "Request Start " <> url
      setUserData "reqStart" d
    ReqEnd -> do
      startTime <- fromMaybe d <$> getUserData "reqStart"
      logDebug
        ( "Request End "
            <> url
            <> " "
            <> show (totalTime startTime d)
            <> "ms"
        )
  next
  where
  totalTime a b =
    let
      Milliseconds n = fromDuration ((b `diff` a) :: Milliseconds)
    in
      n

-- | Handler that sets up the requestId for each request.
requestIdHandler :: Handler
requestIdHandler = do
  requestId <- getRequestHeader "x-requestid"
  case requestId of
    Just s -> setUserData "requestId" s
    Nothing -> liftEffect genUUID >>= setUserData "requestId"
  next

-- | Handler that sends api response.
responseHandler :: Handler
responseHandler = do
  logDebug "Sending response"
  getUserData "result"
    >>= case _ of
      Just r -> sendJson r
      Nothing -> setStatus 204 *> end
  next

-- | Error handler that logs errors.
loggerErrorHandler :: Error -> Handler
loggerErrorHandler err = do
  liftEffect $ log ("Failed with " <> show err)
  nextThrow err

-- | Error handler that sends error response.
responseErrorHandler :: Error -> Handler
responseErrorHandler err = do
  url <- getOriginalUrl
  sendJson $ { message: message err, url }
