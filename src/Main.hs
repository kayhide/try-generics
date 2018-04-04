module Main where

import ClassyPrelude

import GHC.Generics

-- |
-- >>> :kind! Rep Point
-- Rep Point :: * -> *
-- = D1
--     ('MetaData "Point" "Main" "main" 'False)
--     (C1
--        ('MetaCons "Point" 'PrefixI 'True)
--        (S1
--           ('MetaSel
--              ('Just "_x") 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict)
--           (Rec0 Double)
--         :*: S1
--               ('MetaSel
--                  ('Just "_y") 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict)
--               (Rec0 Double)))
data Point = Point { _x :: !Double, _y :: !Double }
   deriving (Show, Generic)

-- |
-- >>> from pt
-- M1 {unM1 = M1 {unM1 = M1 {unM1 = K1 {unK1 = 1.2}} :*: M1 {unM1 = K1 {unK1 = 8.3}}}}
pt :: Point
pt = Point 1.2 8.3

main :: IO ()
main = do
  putStrLn "hello world"
