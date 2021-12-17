module Framework.Types where

import Prelude
import Data.DateTime (DateTime)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode (class EncodeJson, (:=), (:=?), (~>), (~>?))
import Framework.Formatter (formatDate)
import Data.Maybe (Maybe)

data AppContext = AppContext
  { options :: ProgramOptions
  }

instance showAppContext :: Show AppContext where
  show (AppContext { options }) = "Options: " <> show options <> " "

data ProgramOptions = ProgramOptions
  { port :: Int
  , appName :: String
  , logLevel :: LogLevel
  }

instance showProgramOptions :: Show ProgramOptions where
  show (ProgramOptions { port, appName, logLevel }) =
    "port: " <> show port <> " "
      <> "appName: "
      <> show appName
      <> " "
      <> "loglevel: "
      <> show logLevel

data LogLevel
  = Info
  | Debug
  | Warn
  | Error

derive instance eqLogLevel :: Eq LogLevel

derive instance ordLogLevel :: Ord LogLevel

instance logLevelShow :: Show LogLevel where
  show Info = "info"
  show Debug = "debug"
  show Warn = "warn"
  show Error = "error"

data Tags = Tags
  { time :: DateTime
  , message :: String
  , level :: LogLevel
  , requestId :: Maybe String
  }

instance encodeJsonTags :: EncodeJson Tags where
  encodeJson (Tags { time, message, level, requestId }) =
    "message" := message
      ~> "time"
        := formatDate time
      ~> "level"
        := show level
      ~> "requestid"
        :=? requestId
      ~>? jsonEmptyObject
