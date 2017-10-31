{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Prog(testData,testCaption) where

import System.Random
import Data.List
import Language.Haskell.TH
import Control.Monad.State

data QType = QFun QType QType | QList QType | QInt deriving (Show,Eq)
data QProg = QMap1 QProg | QMap2 QProg QProg 
             | QTail1 QProg | QTail | QRev | QMult QProg | QAdd QProg | QRand Int deriving (Eq)

instance Show QProg where
  show (QMap1 p) = "(map " ++ show p ++ ")"
  show (QMap2 p1 p2) = "(map " ++ show p1 ++ " " ++ show p2 ++ ")"
  show (QTail1 p) = "(tail " ++ show p ++ ")"
  show (QTail) = "tail"
  show QRev = "reverse"
  show (QMult p) = "(* " ++ show p ++ ")"
  show (QAdd p) = "(* " ++ show p ++ ")"
  show (QRand i) = show i

getRandomInt = do
  i <- get
  put ((i + 1000) * 12345)
  return i

typeToProg :: QType -> State Int QProg
typeToProg QInt = getRandomInt >>= (return . QRand . (`mod` 100))
typeToProg (QList QInt) = do
  i <- getRandomInt
  case i`mod`2 of
    0 -> do 
      p <- typeToProg (QList QInt)
      return (QTail1 p)
    1 -> do
      p1 <- typeToProg (QFun QInt QInt)
      p2 <- typeToProg (QList QInt)
      return (QMap2 p1 p2)
typeToProg (QFun (QList QInt) (QList QInt)) = do
  i <- getRandomInt
  case i`mod`3 of
    0 -> do
      p <- typeToProg (QFun QInt QInt)
      return (QMap1 p)
    1 -> return QTail
    2 -> return QRev
typeToProg (QFun QInt QInt) = do
  i <- getRandomInt
  case i`mod`2 of
    0 -> do
      p <- typeToProg QInt
      return (QMult p)
    1 -> do
      p <- typeToProg QInt
      return (QAdd p)

progToQ :: QProg -> Q Exp
progToQ (QMap1 p) = appE [e| map |] (progToQ p)
progToQ (QMap2 p1 p2) = (appE (appE [e| map |] (progToQ p1)) (progToQ p2))
progToQ (QTail1 p) = (appE [e| tail |] (progToQ p))
progToQ QTail = [e| tail |]
progToQ QRev = [e| reverse |]
progToQ (QMult p) = appE [e| (*) |] (progToQ p)
progToQ (QAdd p) = appE [e| (+) |] (progToQ p)
progToQ (QRand i) = [e| i |]

listToQ [] = [e| [] |]
listToQ (a:as) = appE (appE [e| (:) |] a) (listToQ as)

testType = QFun (QList QInt) (QList QInt)
testData = listToQ (map (\x -> progToQ $ evalState (typeToProg testType) x) [1..5])
testCaption = map (\x -> show $ evalState (typeToProg testType) x) [1..5]
