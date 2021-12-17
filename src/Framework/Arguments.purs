module Framework.Arguments
  ( parseOptions
  ) where

import Prelude
import Options.Applicative
import Effect (Effect)
import Data.Either (Either(..))
import Framework.Types (LogLevel(..), ProgramOptions(..))

readLogLevel :: ReadM LogLevel
readLogLevel =
  eitherReader
    $ \s -> case s of
        "debug" -> Right Debug
        "info" -> Right Info
        "warn" -> Right Warn
        "error" -> Right Error
        _ -> Left $ "Invalid debug level: " <> s

parseLogLevel :: Parser LogLevel
parseLogLevel =
  option readLogLevel
    ( long "loglevel"
        <> metavar "LOGLEVEL"
        <> help "Log level"
    )

parsePort :: Parser Int
parsePort =
  option int
    ( long "port"
        <> short 'p'
        <> metavar "PORT"
        <> help "Server LISTEN port number"
        <> showDefault
        <> value 8081
    )

parseAppName :: Parser String
parseAppName =
  strOption
    ( long "appName"
        <> metavar "APPNAME"
        <> help "Application Name"
    )

progOptions :: Parser ProgramOptions
progOptions = ado
  port <- parsePort
  appName <- parseAppName
  logLevel <- parseLogLevel
  in ProgramOptions { port, appName, logLevel }

opts :: ParserInfo ProgramOptions
opts =
  info (progOptions <**> helper)
    ( fullDesc
        <> progDesc "Run sample micro-service"
        <> header "Sample micro-service"
    )

parseOptions :: Effect ProgramOptions
parseOptions = execParser opts
