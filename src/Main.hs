{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

-- import ClassyPrelude
import Prelude

import Control.Arrow ((***), first)
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


type FIELD = (Symbol, Type)

-- |
-- >>> :kind! RowTy (Rep Point)
-- RowTy (Rep Point) :: [(Symbol, *)]
-- = '[ '("x", Double), '("y", Double)]
class Row f where
  type RowTy f :: [FIELD]
  toRec :: f a -> Rec (RowTy f)
  fromRec :: Rec (RowTy f) -> f a

instance Row f => Row (D1 i f) where
  type RowTy (D1 i f) = RowTy f
  toRec (M1 x) = toRec x
  fromRec = M1 . fromRec

instance Row f => Row (C1 i f) where
  type RowTy (C1 i f) = RowTy f
  toRec (M1 x) = toRec x
  fromRec = M1 . fromRec

instance (Row a, Row b, l ~ RowTy a, r ~ RowTy b, u ~ Append l r, Union l r u) => Row (a :*: b) where
  type RowTy (a :*: b) = Append (RowTy a) (RowTy b)
  toRec (x :*: y) = union (toRec x) (toRec y)
  fromRec = uncurry (:*:) . (fromRec *** fromRec) . ununion

instance Row (S1 ('MetaSel ('Just (key :: Symbol)) su ss ds) (Rec0 (a :: Type))) where
  type RowTy (S1 ('MetaSel ('Just key) su ss ds) (Rec0 a)) = '[ '(key, a) ]
  toRec (M1 (K1 x)) = RCons (Keyed x) RNil
  fromRec (RCons (Keyed x) RNil) = M1 $ K1 x

instance Row (S1 ('MetaSel 'Nothing su ss ds) (Rec0 (Keyed key a))) where
  type RowTy (S1 ('MetaSel 'Nothing su ss ds) (Rec0 (Keyed key a))) = '[ '(key, a) ]
  toRec (M1 (K1 x)) = RCons x RNil
  fromRec (RCons x RNil) = M1 $ K1 x


-- * Value type tagged by @Symbol@

-- |
-- >>> x = Keyed @"x" @Double 3.5
-- >>> y = Keyed @"y" @Double 4.8
-- >>> :t RCons x (RCons y RNil)
-- RCons x (RCons y RNil) :: Rec '[ '("x", Double), '("y", Double)]
--
-- >>> (x, y)
-- (x: 3.5,y: 4.8)
--
-- >>> toRec $ from (x, y)
-- x: 3.5, y: 4.8, _
--
-- >>> keys x
-- ["x"]
--
-- >>> keys (x, y)
-- ["x","y"]
--

newtype Keyed (k :: Symbol) (a :: Type) = Keyed a
  deriving (Eq)

instance (KnownSymbol k, Show a) => Show (Keyed k a) where
  show (Keyed x) = symbolVal (Proxy @k) <> ": " <> show x

instance Generic (Keyed k a) where
  type Rep (Keyed k a) = S1 ('MetaSel ('Just k) 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy) (Rec0 a)
  from (Keyed x) = M1 (K1 x)
  to (M1 (K1 x)) = Keyed x


-- * Heterogeneous associated list

data Rec (row :: [FIELD]) where
  RNil :: Rec '[]
  RCons :: Keyed k a -> Rec xs -> Rec ('(k, a) ': xs)

instance Show (Rec '[]) where
  show _ = "_"

instance (KnownSymbol k, Show a, Show (Rec xs)) => Show (Rec ('(k, a) ': xs)) where
  show (RCons x xs) = show x <> ", " <> show xs


-- * Union typeclass

-- |
-- >>> union (toRec $ from pt) (toRec $ from (Keyed @"z" 42.0))
-- x: 1.2, y: 8.3, z: 42.0, _

-- |
-- >>> ununion (toRec $ from pt) :: (Rec '[ '("x", Double)], Rec '[ '("y", Double)])
-- (x: 1.2, _,y: 8.3, _)
-- >>> ununion (toRec $ from pt) :: (Rec '[], Rec '[ '("x", Double), '("y", Double)])
-- (_,x: 1.2, y: 8.3, _)
class Union l r u | l r -> u where
  union :: Rec l -> Rec r -> Rec u
  ununion :: Rec u -> (Rec l, Rec r)

instance Union '[] r r where
  union _ r = r
  ununion x = (RNil, x)

instance (Union l r u, u ~ Append l r) => Union (x ': l) r (x ': u) where
  union (RCons x xs) r = RCons x $ union xs r
  ununion (RCons x xs) = first (RCons x) $ ununion xs


-- | Round trip of Point to/from Rec
-- >>> to @Point $ fromRec $ toRec $ from pt
-- Point {x = 1.2, y = 8.3}

-- | Converting Point to Point3d by adding z field
-- >>> data Point3d = Point3d { x :: Double, y :: Double, z :: Double } deriving (Show, Generic)
-- >>> to @Point3d $ fromRec $ union (toRec $ from pt) (toRec $ from (Keyed @"z" 42.0))
-- Point3d {x = 1.2, y = 8.3, z = 42.0}


main :: IO ()
main = do
  putStrLn "OK"
