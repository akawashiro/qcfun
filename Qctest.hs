{-# LANGUAGE TemplateHaskell #-}
module QCFun where
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Function
import Test.QuickCheck.Poly

prop_mapLength :: Fun A B -> [A] -> Bool
prop_mapLength (Fun _ f) xs = length (map f xs) == length xs

main :: IO Bool
main = $(quickCheckAll)
