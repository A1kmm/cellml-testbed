{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, RankNTypes, PatternGuards #-}
-- | Code in this module is not directly related to CellML, but it is rather convenience code used by the solver.
module Data.CellML12.SupportCode
where       

import Control.Monad.Trans.Class
import Control.Monad
import Data.Generics
import System.IO.Temp
import qualified Data.Map as M
import qualified Control.Exception
import System.IO.Temp
import Control.Monad.IO.Unwrappable
import Control.Monad.IO.Class
import Data.IORef

withSystemTempDirectory' :: MonadIOUnwrappable m => String -> (FilePath -> m a) -> m a
withSystemTempDirectory' pattern f = do
  s <- unwrapState
  ret <- liftIO $ withSystemTempDirectory pattern (\fp -> unwrapMonadIO s (f fp))
  rewrapMonadIO s ret

-- | Allows (!!) to be used on maps, reserving (!) for arrays.
a!!b 
  | Just x <- res = x
  | Nothing <- res = error "Attempt to lookup undefined key in a map"
  where res = M.lookup b a

-- | A new SYB scheme (proposed here: http://www.haskell.org/pipermail/generics/2012-May/000524.html).
-- | Summarise all nodes in top-down, left-to-right order, carrying some
-- | state down the tree during the computation, but not left-to-right to siblings.
everythingWithContext :: s -> (r -> r -> r) -> GenericQ (s -> (r, s)) -> GenericQ r
everythingWithContext s0 f q x =
  foldl f r (gmapQ (everythingWithContext s' f q) x)
    where (r, s') = q x s0
