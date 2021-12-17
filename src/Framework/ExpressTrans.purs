module Framework.ExpressTrans

where

import Prelude

import Node.Express.App (AppM(..))
import Effect (Effect)
import Node.Express.Types (Application)

-- newtype ExpressAppT a = ExpressAppT (Application -> Effect a)
-- Monad Transformers
-- newtype ExpressAppT m a = ExpressAppT (m (AppM a))

-- runExpressT ∷ ∀ m a. ExpressAppT m a → m (AppM a)
-- runExpressT (ExpressAppT a) = a

-- mapExpressT ∷
--   ∀ (m1 ∷ Type -> Type) (m2 ∷ Type) (a ∷ Type -> Type) (b ∷ Type).
--   (m1 (AppM m2) →
--   a (AppM b)) →
--   ExpressAppT m1 m2 →
--   ExpressAppT a b
-- mapExpressT f (ExpressAppT m) = ExpressAppT (f m)

-- -- derive instance newtypeExpressAppT :: Newtype (ExpressAppT m a) _

-- instance functorExpressAppT :: Functor m => Functor (ExpressAppT m) where
--   map f (ExpressAppT ma) = ExpressAppT (map f <$> ma)

-- instance applyExpressAppT :: Monad m => Apply (ExpressAppT m) where
--   apply = ap

-- instance applicativeExpressAppT :: Monad m => Applicative (ExpressAppT m) where
--   pure = ExpressAppT <<< pure <<< AppM

-- instance bindExpressAppT :: Monad m => Bind (ExpressAppT m) where
--   bind (ExpressAppT m) f = ExpressAppT do
--     m >>= case _ of
--         AppM \app -> do
--             res <- h app
--             case f res of ExpressAppT g -> g app

--             -- case f res of ExpressAppT g -> g app


