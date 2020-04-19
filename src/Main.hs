{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

-- import ClassyPrelude
import Prelude

import Data.Proxy
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
--              ('Just "x") 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict)
--           (Rec0 Double)
--         :*: S1
--               ('MetaSel
--                  ('Just "y") 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict)
--               (Rec0 Double)))
data Point = Point { x :: !Double, y :: !Double }
   deriving (Show, Generic)

-- |
-- >>> from pt
-- M1 {unM1 = M1 {unM1 = M1 {unM1 = K1 {unK1 = 1.2}} :*: M1 {unM1 = K1 {unK1 = 8.3}}}}
--
-- >>> rep = M1 $ M1 $ M1 (K1 1.2) :*: M1 (K1 8.3)
-- >>> to rep :: Point
-- Point {x = 1.2, y = 8.3}
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
  keys' x = keys' (unsafeCoerce x :: a _b)

instance Keys' a => Keys' (C1 f a) where
  keys' x = keys' (unsafeCoerce x :: a _b)

instance (Keys' a, Keys' b) => Keys' (a :*: b) where
  keys' (x :*: y) = keys' x <> keys' y

instance KnownSymbol key => Keys' (S1 ('MetaSel ('Just key) x y z) a) where
  keys' _ = [symbolVal (Proxy @key)]


-- |
-- >>> :kind! Append '[Bool, Int] '[]
-- Append '[Bool, Int] '[] :: [*]
-- = '[Int, Bool]
-- >>> :kind! Append '[Bool, Int] '[Char, Word]
-- Append '[Bool, Int] '[Char, Word] :: [*]
-- = '[Int, Bool, Char, Word]
type family Append (a :: [k]) (b :: [k]) :: [k]
type instance Append '[] ys = ys
type instance Append (x ': xs) ys = Append xs (x ': ys)


-- |
-- >>> :kind RowTy (Rep Point)
-- RowTy (Rep Point) :: [(Symbol, k)]
class Row f where
  type RowTy f :: [(Symbol, k)]

instance Row f => Row (D1 i f) where
  type RowTy (D1 i f) = RowTy f

instance Row f => Row (C1 i f) where
  type RowTy (C1 i f) = RowTy f

instance (Row a, Row b) => Row (a :*: b) where
  type RowTy (a :*: b) = Append (RowTy a) (RowTy b)

instance KnownSymbol key => Row (S1 ('MetaSel ('Just (key :: Symbol)) x y z) a) where
  type RowTy (S1 ('MetaSel ('Just key) x y z) a) = '[ '(key, a) ]


data Rec (row :: [(Symbol, k)]) where
  RNil :: Rec '[]
  RCons :: f x -> Rec xs -> Rec (x ': xs)


-- class Union l r u where
--   type UnionTy l r :: [(Symbol, k)]
--   union :: l -> r -> u

-- instance (Row l, Row r, Row u) => Union l r u where
--   type UnionTy l r = Append (RowTy l) (RowTy r)
--   union x y = _

main :: IO ()
main = do
  print $ from pt
  let M1 (M1 (M1 (K1 x) :*: M1 (K1 y))) = from pt
  print $ (x, y)
  print $ keys pt
