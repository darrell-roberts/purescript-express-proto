module Framework.Logger
  ( class HasLogger
  , logger
  , logWithTags
  , logInfo
  , logWarn
  , logError
  , logDebug
  ) where

import Prelude
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Maybe (Maybe(..))
import Data.Set (Set, fromFoldable, member)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Now (nowDateTime)
import Framework.Express (class HasAppContext, RouterM, getAppContext)
import Framework.Types (AppContext(..), LogLevel(..), Tags(..), ProgramOptions(..))
import Node.Express.App (AppM)
import Node.Express.Handler (HandlerM)
import Node.Express.Request (getUserData)

getTags :: Maybe String -> LogLevel -> String -> Effect Tags
getTags requestId level message = do
  time <- nowDateTime
  pure $ Tags { time, message, level, requestId }

logWithTags :: Maybe String -> LogLevel -> String -> Effect Unit
logWithTags requestId level message = do
  tags <- getTags requestId level message
  log $ stringify $ encodeJson tags

class HasLogger a where
  logger :: LogLevel -> String -> a Unit

logWithContext ∷ ∀ (m ∷ Type -> Type). HasAppContext m ⇒ MonadEffect m ⇒ LogLevel → String → m Unit
logWithContext level message = do
  logLevel <- getLogLevel <$> getAppContext
  when (isLevelEnabled level logLevel)
    (liftEffect $ logWithTags Nothing level message)

instance appMLogger :: HasLogger AppM where
  logger = logWithContext

instance routerMLogger :: HasLogger RouterM where
  logger = logWithContext

instance effectLoger :: HasLogger Effect where
  logger = logWithTags Nothing

{--
  A Logger instance for the HandlerM monad will be able
  to source additional tag information such as requestId.
-}
instance handlerMLogger :: HasLogger HandlerM where
  logger level message = do
    requestId <- getUserData "requestId"
    logLevel <- getLogLevel <$> getAppContext
    when (isLevelEnabled level logLevel)
      (liftEffect $ logWithTags requestId level message)

getLogLevel :: AppContext -> LogLevel
getLogLevel (AppContext { options: (ProgramOptions popts) }) = popts.logLevel

isLevelEnabled :: LogLevel -> LogLevel -> Boolean
isLevelEnabled requestedLevel enabledLevel = requestedLevel `member` levels enabledLevel
  where
  levels :: LogLevel -> Set LogLevel
  levels Debug = fromFoldable [ Error, Info, Warn, Debug ]
  levels Warn = fromFoldable [ Error, Info, Warn ]
  levels Info = fromFoldable [ Error, Info ]
  levels Error = fromFoldable [ Error ]

logDebug :: ∀ (a ∷ Type -> Type). HasLogger a ⇒ String → a Unit
logDebug = logger Debug

logInfo :: ∀ (a ∷ Type -> Type). HasLogger a ⇒ String → a Unit
logInfo = logger Info

logError :: ∀ (a ∷ Type -> Type). HasLogger a ⇒ String → a Unit
logError = logger Error

logWarn :: ∀ (a ∷ Type -> Type). HasLogger a ⇒ String → a Unit
logWarn = logger Warn
