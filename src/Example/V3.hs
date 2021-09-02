module Example.V3 (
  V3 (..),
  dot,
  (^*),(*^),(^/),
) where

import Clash.Prelude

data V3 a = V3 a a a deriving (Generic,NFDataX,Show)

-- This code was quasi-lifted from the `linear` package, which fails to build on clash
instance Functor V3 where
  fmap f (V3 a b c) = V3 (f a) (f b) (f c)
  a <$ _ = V3 a a a

instance Applicative V3 where
  pure a = V3 a a a
  V3 a b c <*> V3 d e f = V3 (a d) (b e) (c f)

instance Foldable V3 where
  foldMap f (V3 a b c) = f a <> f b <> f c

instance Num a => Num (V3 a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

dot :: Num a => V3 a -> V3 a -> a
dot x y = sum $ (*) <$> x <*> y

infixl 7 ^*, *^, ^/

(^*) :: Num a => V3 a -> a -> V3 a
v ^* s = fmap (*s) v

(*^) :: Num a => a -> V3 a -> V3 a
s *^ v = fmap (s*) v

(^/) :: Fractional a => V3 a -> a -> V3 a
v ^/ s = fmap (/s) v

-- Could also implement this in terms of (Vec 3 a):

-- type V3 = Vec 3

-- instance (KnownNat n, Num a) => Num (Vec n a) where
--   (+) = zipWith (+)
--   (-) = zipWith (-)
--   (*) = zipWith (*)
--   negate = fmap negate
--   abs = fmap abs
--   signum = fmap signum
--   fromInteger = pure . fromInteger

-- dot :: Num a => V3 a -> V3 a -> a
-- dot x y = sum (zipWith (*) x y)

-- infixl 7 ^*, *^, ^/

-- (^*) :: Num a => V3 a -> a -> V3 a
-- v ^* s = fmap (*s) v

-- (*^) :: Num a => a -> V3 a -> V3 a
-- s *^ v = fmap (s*) v

-- (^/) :: Fractional a => V3 a -> a -> V3 a
-- v ^/ s = fmap (/s) v
