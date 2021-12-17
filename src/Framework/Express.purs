{--
  Enhancements to the base purescript-express providing the following.

  * Router support.
      - New RouterM monad used to build up a router in the same way we build up and AppM
      - useRouter function used in the AppM monad that takes a RouteM monad and attaches the
          route to the underlying express app with target path.

  * Application Context.
      - Ability to access an application context from within the AppM, RouteM, HandlerM monads.
-}
module Framework.Express
  ( setAppContext
  , class HasAppContext
  , getAppContext
  , RouterM
  , Router
  , ExpressRouter
  , useRouter
  , listenHttp
  -- * Router endpoints
  , get
  , post
  , put
  , delete
  , all
  , use
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2, runFn1, runFn4, Fn4, Fn3, runFn3)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign (Foreign, unsafeToForeign)
import Framework.Types (AppContext)
import Node.Express.App (AppM(..), App, HandlerFn, mkApplication, _listenHttp)
import Node.Express.Handler (Handler, HandlerM(..), runHandlerM)
import Node.Express.Types (class RoutePattern, Application, Method(..), Path, Request, Port, Event)
import Node.HTTP (Server)

class HasAppContext a where
  getAppContext :: a AppContext

-- | Store an application context in the express app.locals.context.
setAppContext :: AppContext -> App
setAppContext context = AppM \app -> runFn2 _setAppContext app context

-- | Retrieve the appliction context from the req.app.locals.context.
getHandlerMAppContext :: HandlerM AppContext
getHandlerMAppContext = HandlerM \req _ _ ->
  liftEffect $ runFn1 _getHandlerMAppContext req

-- | Retrieve the application context from the app.locals.context.
getAppMAppContext :: AppM AppContext
getAppMAppContext = AppM \app -> runFn1 _getAppMAppContext app

getRouteMAppContext :: RouterM AppContext
getRouteMAppContext = RouterM \app router -> runFn2 _getRouterMAppContext app router

instance HasAppContext HandlerM where
  getAppContext = getHandlerMAppContext

instance HasAppContext AppM where
  getAppContext = getAppMAppContext

instance HasAppContext RouterM where
  getAppContext = getRouteMAppContext

foreign import _getHandlerMAppContext :: Request -> Effect AppContext

foreign import _setAppContext :: Fn2 Application AppContext (Effect Unit)

foreign import _getAppMAppContext :: Application -> Effect AppContext

foreign import _getRouterMAppContext :: Fn2 Application ExpressRouter (Effect AppContext)

foreign import data ExpressRouter :: Type

foreign import _mkRouter :: Effect ExpressRouter

newtype RouterM a = RouterM (Application -> ExpressRouter -> Effect a)

type Router = RouterM Unit

instance functorRouterM :: Functor RouterM where
  map f (RouterM h) = RouterM \app router -> liftM1 f $ h app router

instance applyRouterM :: Apply RouterM where
  apply (RouterM f) (RouterM h) = RouterM \app router -> do
    routerA <- f app router
    routerB <- h app router
    pure $ routerA routerB

instance applicativeRouterM :: Applicative RouterM where
  pure x = RouterM \_ _ -> pure x

instance bindRouterM :: Bind RouterM where
  bind (RouterM h) f = RouterM \app router -> do
    res <- h app router
    case f res of
      RouterM g -> g app router

instance monadRouterM :: Monad RouterM

instance monadEffectRouterM :: MonadEffect RouterM where
  liftEffect act = RouterM \_ _ -> act

useRouter :: Path -> Router -> App
useRouter path (RouterM act) = AppM \app -> do
  router <- _mkRouter
  act app router
  runFn3 _useRouter app path router
  pure unit

http ∷ ∀ r. RoutePattern r ⇒ Method → r → Handler → Router
http method route handler = RouterM \_ router -> do
  runFn4 _http router (show method) (unsafeToForeign route) $ runHandlerM handler

get ∷ ∀ r. RoutePattern r ⇒ r → Handler → Router
get = http GET

post ∷ ∀ r. RoutePattern r ⇒ r → Handler → Router
post = http POST

put ∷ ∀ r. RoutePattern r ⇒ r → Handler → Router
put = http PUT

delete ∷ ∀ r. RoutePattern r ⇒ r → Handler → Router
delete = http DELETE

all ∷ ∀ r. RoutePattern r ⇒ r → Handler → Router
all = http ALL

use :: Handler -> Router
use middleware = RouterM \_ router ->
  runFn2 _use router $ runHandlerM middleware

foreign import _useRouter :: Fn3 Application Path ExpressRouter (Effect Unit)

foreign import _http :: Fn4 ExpressRouter String Foreign HandlerFn (Effect Unit)

foreign import _use :: Fn2 ExpressRouter HandlerFn (Effect Unit)

listenHttp :: AppContext -> App -> Port -> (Event -> Effect Unit) -> Effect Server
listenHttp context (AppM act) port cb = do
    app <- mkApplication
    runFn2 _setAppContext app context
    act app

    _listenHttp app port cb
