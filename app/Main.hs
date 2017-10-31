{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Main where

import Prog

func f = reverse . f
prop f = (func f [1,2,3]) == [1,2,3]

testData' = $( testData )

main = do
  putStrLn "Test results are following."
  print $ zip (map prop testData') testCaption

