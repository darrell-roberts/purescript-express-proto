module SampleService.Routes
  ( someRoute
  , userRoute
  ) where

import Prelude
import Effect.Class (liftEffect)
import Effect.Exception (error, throwException)
import Framework.Express (Router, get, use)
import Node.Express.Handler (Handler, next)
import Node.Express.Request (setUserData)
import Framework.Logger (logDebug)

-- | Builds an express router for some endpoint.
someRoute :: Router
someRoute =
  get "/router"
    $ do
        setUserData "result" { message: "it worked" }
        next

type User =
  { firstName :: String
  , lastName :: String
  , email :: String
  , id :: Int
  }

testUsers :: Array User
testUsers =
  [ { firstName: "Darrell", lastName: "Roberts", email: "droberts@nowhere.com", id: 1 }
  , { firstName: "Jane", lastName: "Doe", email: "jdoe@nowhere.com", id: 2 }
  , { firstName: "John", lastName: "Smith", email: "jsmith@nowhere.com", id: 3 }
  ]

failingHandler :: Handler
failingHandler = liftEffect $ throwException $ error "It failed"

-- | Builds an express router for a users endpoint.
userRoute :: Router
userRoute = do
  logDebug "Setting up user router"
  -- Middleware on route.
  use $ logDebug "I am middleware for user route" *> next
  -- Endpoints for this route.
  get "/users" $ setUserData "result" testUsers *> next
  get "/fail" failingHandler
