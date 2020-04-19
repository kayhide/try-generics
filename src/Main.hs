{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

-- import ClassyPrelude
import Prelude

import Data.Proxy
import Data.Kind
import GHC.Generics
import GHC.TypeLits
import Debug.Trace
import Unsafe.Coerce

-- |
-- >>> :kind! Rep Point
-- Rep Point :: * -> *
-- = D1
--     ('MetaData "Point" "Main" "main" 'False)
--     (C1
--        ('MetaCons "Point" 'PrefixI 'True)
--        (S1
--           ('MetaSel
--              ('Just "x") 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
--           (Rec0 Double)
--         :*: S1
--               ('MetaSel
--                  ('Just "y") 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
--               (Rec0 Double)))
data Point = Point { x :: Double, y :: Double }
   deriving (Show, Generic)

data Position = Position { x :: !Double, y :: !Double }
   deriving (Show, Generic)

-- |
-- >>> from pt
-- M1 {unM1 = M1 {unM1 = M1 {unM1 = K1 {unK1 = 1.2}} :*: M1 {unM1 = K1 {unK1 = 8.3}}}}
--
-- >>> rep = M1 $ M1 $ M1 (K1 1.2) :*: M1 (K1 8.3)
-- >>> to rep :: Point
-- Point {x = 1.2, y = 8.3}
-- >>> to rep :: Position
-- Position {x = 1.2, y = 8.3}
-- >>> to rep :: (Double, Double)
-- (1.2,8.3)
pt :: Point
pt = Point 1.2 8.3



-- |
-- >>> keys pt
-- ["x","y"]
class Keys a where
  keys :: a -> [String]

instance (Generic a, Keys' (Rep a)) => Keys a where
  keys x = keys' $ from x


class Keys' f where
  keys' :: f a -> [String]

instance Keys' a => Keys' (D1 f a) where
  keys' (M1 x) = keys' x

instance Keys' a => Keys' (C1 f a) where
  keys' (M1 x) = keys' x

instance (Keys' a, Keys' b) => Keys' (a :*: b) where
  keys' (x :*: y) = keys' x <> keys' y

instance KnownSymbol key => Keys' (S1 ('MetaSel ('Just key) su ss ds) a) where
  keys' _ = [symbolVal (Proxy @key)]

instance KnownSymbol key => Keys' (S1 ('MetaSel 'Nothing su ss ds) (Rec0 (Keyed key a))) where
  keys' _ = [symbolVal (Proxy @key)]

-- |
-- >>> :kind! Append '[Bool, Int] '[]
-- Append '[Bool, Int] '[] :: [*]
-- = '[Bool, Int]
-- >>> :kind! Append '[Bool, Int] '[Char, Word]
-- Append '[Bool, Int] '[Char, Word] :: [*]
-- = '[Bool, Int, Char, Word]
type family Append (a :: [k]) (b :: [k]) :: [k]
type instance Append '[] ys = ys
type instance Append (x ': xs) ys = x ': Append xs ys



-- |
-- >>> :kind! RowTy (Rep Point)
-- RowTy (Rep Point) :: [(Symbol, *)]
-- = '[ '("x", Double), '("y", Double)]
class Row f where
  type RowTy f :: [(Symbol, Type)]

instance Row f => Row (D1 i f) where
  type RowTy (D1 i f) = RowTy f

instance Row f => Row (C1 i f) where
  type RowTy (C1 i f) = RowTy f

instance (Row a, Row b) => Row (a :*: b) where
  type RowTy (a :*: b) = Append (RowTy a) (RowTy b)

instance KnownSymbol key => Row (S1 ('MetaSel ('Just (key :: Symbol)) su ss ds) (Rec0 (a :: Type))) where
  type RowTy (S1 ('MetaSel ('Just key) su ss ds) (Rec0 a)) = '[ '(key, a) ]

instance KnownSymbol key => Row (S1 ('MetaSel 'Nothing su ss ds) (Rec0 (Keyed key a))) where
  type RowTy (S1 ('MetaSel 'Nothing su ss ds) (Rec0 (Keyed key a))) = '[ '(key, a) ]



-- |
-- >>> x = Keyed @"x" @Double 3.5
-- >>> y = Keyed @"y" @Double 4.8
-- >>> :t RCons x (RCons y RNil)
-- RCons x (RCons y RNil) :: Rec '[ '("x", Double), '("y", Double)]
--
-- >>> (x, y)
-- (Keyed 3.5,Keyed 4.8)
--
-- >>> keys x
-- ["x"]
--
-- >>> keys (x, y)
-- ["x","y"]

newtype Keyed (k :: Symbol) (a :: Type) = Keyed a
  deriving (Eq, Show)

instance Generic (Keyed k a) where
  type Rep (Keyed k a) = S1 ('MetaSel ('Just k) 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy) (Rec0 a)
  from (Keyed x) = M1 (K1 x)
  to (M1 (K1 x)) = Keyed x

data Rec (row :: [(Symbol, Type)]) where
  RNil :: Rec '[]
  RCons :: Keyed k a -> Rec xs -> Rec ('(k, a) ': xs)



-- class Union l r u where
--   type UnionTy l r :: [(Symbol, k)]
--   union :: l -> r -> u

-- instance (Row l, Row r, Row u) => Union l r u where
--   type UnionTy l r = Append (RowTy l) (RowTy r)
--   union x y = _

main :: IO ()
main = do
  print $ from pt
  let M1 (M1 (rx@(M1 (K1 x)) :*: ry@(M1 (K1 y)))) = from pt
  print $ (x, y)
  print $ (to $ M1 $ M1 $ rx :*: ry :: Point)
  print $ keys pt
