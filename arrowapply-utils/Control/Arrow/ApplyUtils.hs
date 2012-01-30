{-# LANGUAGE FlexibleInstances #-}
module Control.Arrow.ApplyUtils
where
import Control.Arrow

-- | An instance that lets you work with Arrows that support ArrowApply as monads.
-- | Example:
-- |   myarrow :: ArrowApply a => a Int Int
-- |   myarrow = monadicA $ \v -> do
-- |               let vp1 = v + 1
-- |               v' <- unmonadicA anotherIntToIntArrow vp1
-- |               return (v' * 10)
instance ArrowApply a => Monad (a ())
  where
    x >>= y = (x >>^ (\v -> (y v, ()))) >>> app
    return = arr . const

-- | Embed a block of monadic code in an arrow.
monadicA :: ArrowApply a => (b -> a () c) -> a b c
monadicA f = arr (\v -> (f v, ())) >>> app

-- | Embed an arrow in a block of monadic code.
unmonadicA :: ArrowApply a => a b c -> b -> a () c
unmonadicA a b = arr (const b) >>> a

-- | Lift an Arrow operation.
liftA :: ArrowApply a => (c -> d) -> a b c -> a b d
liftA = flip (>>^)

-- | Lift an Arrow operation with two parameters
liftA2 :: ArrowApply a => (p1 -> p2 -> d) -> a b p1 -> a b p2 -> a b d
liftA2 f a1 a2 = monadicA $ \v -> do
  p1 <- unmonadicA a1 v
  p2 <- unmonadicA a2 v
  return (f p1 p2)

-- | Lift an Arrow operation with three parameters
liftA3 :: ArrowApply a => (p1 -> p2 -> p3 -> d) -> a b p1 -> a b p2 -> a b p3 -> a b d
liftA3 f a1 a2 a3 = monadicA $ \v -> do
  p1 <- unmonadicA a1 v
  p2 <- unmonadicA a2 v
  p3 <- unmonadicA a3 v
  return (f p1 p2 p3)

-- | Lift an Arrow operation with four parameters
liftA4 :: ArrowApply a => (p1 -> p2 -> p3 -> p4 -> d) -> a b p1 -> a b p2 -> a b p3 -> a b p4 -> a b d
liftA4 f a1 a2 a3 a4 = monadicA $ \v -> do
  p1 <- unmonadicA a1 v
  p2 <- unmonadicA a2 v
  p3 <- unmonadicA a3 v
  p4 <- unmonadicA a4 v
  return (f p1 p2 p3 p4)

-- | Lift an Arrow operation with five parameters
liftA5 :: ArrowApply a => (p1 -> p2 -> p3 -> p4 -> p5 -> d) -> a b p1 -> a b p2 -> a b p3 -> a b p4 -> a b p5 -> a b d
liftA5 f a1 a2 a3 a4 a5 = monadicA $ \v -> do
  p1 <- unmonadicA a1 v
  p2 <- unmonadicA a2 v
  p3 <- unmonadicA a3 v
  p4 <- unmonadicA a4 v
  p5 <- unmonadicA a5 v
  return (f p1 p2 p3 p4 p5)

-- | Lift an Arrow operation with six parameters
liftA6 :: ArrowApply a => (p1 -> p2 -> p3 -> p4 -> p5 -> p6 -> d) -> a b p1 -> a b p2 -> a b p3 -> a b p4 -> a b p5 -> a b p6 -> a b d
liftA6 f a1 a2 a3 a4 a5 a6 = monadicA $ \v -> do
  p1 <- unmonadicA a1 v
  p2 <- unmonadicA a2 v
  p3 <- unmonadicA a3 v
  p4 <- unmonadicA a4 v
  p5 <- unmonadicA a5 v
  p6 <- unmonadicA a6 v  
  return (f p1 p2 p3 p4 p5 p6)

-- | Lift an Arrow operation with seven parameters
liftA7 :: ArrowApply a => (p1 -> p2 -> p3 -> p4 -> p5 -> p6 -> p7 -> d) -> a b p1 -> a b p2 -> a b p3 -> a b p4 -> a b p5 -> a b p6 -> a b p7 -> a b d
liftA7 f a1 a2 a3 a4 a5 a6 a7 = monadicA $ \v -> do
  p1 <- unmonadicA a1 v
  p2 <- unmonadicA a2 v
  p3 <- unmonadicA a3 v
  p4 <- unmonadicA a4 v
  p5 <- unmonadicA a5 v
  p6 <- unmonadicA a6 v
  p7 <- unmonadicA a7 v
  return (f p1 p2 p3 p4 p5 p6 p7)

-- | Lift an Arrow operation with eight parameters
liftA8 :: ArrowApply a => (p1 -> p2 -> p3 -> p4 -> p5 -> p6 -> p7 -> p8 -> d) -> a b p1 -> a b p2 -> a b p3 -> a b p4 -> a b p5 -> a b p6 -> a b p7 -> a b p8 -> a b d
liftA8 f a1 a2 a3 a4 a5 a6 a7 a8 = monadicA $ \v -> do
  p1 <- unmonadicA a1 v
  p2 <- unmonadicA a2 v
  p3 <- unmonadicA a3 v
  p4 <- unmonadicA a4 v
  p5 <- unmonadicA a5 v
  p6 <- unmonadicA a6 v
  p7 <- unmonadicA a7 v
  p8 <- unmonadicA a8 v
  return (f p1 p2 p3 p4 p5 p6 p7 p8)

-- | Lift an Arrow operation with eight parameters
liftA9 :: ArrowApply a => (p1 -> p2 -> p3 -> p4 -> p5 -> p6 -> p7 -> p8 -> p9 -> d) -> a b p1 -> a b p2 -> a b p3 -> a b p4 -> a b p5 -> a b p6 -> a b p7 -> a b p8 -> a b p9 -> a b d
liftA9 f a1 a2 a3 a4 a5 a6 a7 a8 a9 = monadicA $ \v -> do
  p1 <- unmonadicA a1 v
  p2 <- unmonadicA a2 v
  p3 <- unmonadicA a3 v
  p4 <- unmonadicA a4 v
  p5 <- unmonadicA a5 v
  p6 <- unmonadicA a6 v
  p7 <- unmonadicA a7 v
  p8 <- unmonadicA a8 v
  p9 <- unmonadicA a9 v
  return (f p1 p2 p3 p4 p5 p6 p7 p8 p9)
