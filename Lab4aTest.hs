{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Lab4aTest where

import Control.Monad
import Data.Array
import Data.List
import qualified Data.Map as Map
import Test.QuickCheck

import Mooc.Th
import Mooc.Test

import Lab4a

main = score tests

tests = [(1,"allEqual",[ex1])
        ,(2,"distinct",[ex2_int, ex2_char])
        ,(3,"middle",[ex3_char, ex3_int])
        ,(4,"rangeOf",[ex4_int, ex4_integer, ex4_double])
        ,(5,"longest",[ex5_int, ex5_char])
        ,(6,"incrementKey",[ex6_bool_float, ex6_char_int])
        ,(7,"average",[ex7])]

-- -- -- -- -- -- -- --

m_ex1_true example = forAll_ $ \(cnt,v) ->
  $(testing [|allEqual (replicate cnt (v `asTypeOf` example))|]) (?==True)
m_ex1_false example = forAll_ $ \(v,NonEmpty vs) ->
  not (elem (v `asTypeOf` example) vs) ==> $(testing [|allEqual (v:vs)|]) (?==False)

ex1 = conjoin [m_ex1_true True
              ,m_ex1_true 'a'
              ,m_ex1_false 'a'
              ,m_ex1_true (1::Int)
              ,m_ex1_false (1::Int)]

m_ex2 domain = forAllBlind (sublistOf domain) $ \xs ->
  conjoin [forAllBlind (shuffle xs) $ \xs' -> $(testing [|distinct xs'|]) (?==True)
          ,not (null xs) ==> forAllBlind (shuffle (head xs:xs)) $ \xs' -> $(testing [|distinct xs'|]) (?==False)]

ex2_int = m_ex2 [1::Int,2,3,4]

ex2_char = m_ex2 ['a'..'z']



m_ex3 c =
  let d = succ c
      e = succ d
  in forAllBlind (shuffle [c,d,e]) $ \[x,y,z] ->
    $(testing [|middle x y z|]) (?==d)

ex3_char = forAllBlind (choose ('a','y')) m_ex3
ex3_int = forAllBlind (choose (0::Int,20)) m_ex3

genRangeOf delta = do
  base <- arbitrary
  vs <- listOf1 (choose (base, base+delta))
  shuffle (base:base+delta:vs)

m_ex4 delta = forAllBlind (genRangeOf delta) $ \vs ->
  $(testing [|rangeOf vs|]) (?==delta)

ex4_int = forAllBlind (choose (1::Int,10)) m_ex4
ex4_integer = forAllBlind (choose (1::Integer,10)) m_ex4
ex4_double = property $ do
  delta <- elements [1::Int,3,5,7,9]
  vs <- genRangeOf delta
  return $ $(testing [|rangeOf (map conv vs)|]) (?~=conv delta)
    where conv x = fromIntegral x / 2

ex5_int =
  forAllBlind (choose (2,10)) $ \l ->
  forAllBlind (vectorOf l (arbitrary :: Gen Int)) $ \ans ->
  forAllBlind (listOf (choose (1,l - 1) >>= vector)) $ \rest ->
  forAllBlind (shuffle (ans:rest)) $ \input ->
  $(testing [|longest input|]) (?==ans)

ex5_char = property $ do
  c <- choose ('a','k')
  d <- choose ('a','z') `suchThat` (>c)
  len <- choose (1,3)
  crest <- vectorOf len (choose ('a','z'))
  drest <- vectorOf len (choose ('a','z'))
  wrong <- vectorOf len (choose ('a','z'))
  input <- shuffle [c:crest, d:drest, wrong]
  return $ $(testing [|longest input|]) (?==(c:crest))

ex6_bool_float =
    property $ do
      c <- choose (True,False)
      let f i = fromIntegral i + 0.5
      i <- f <$> choose (0,10::Int)
      j <- f <$> choose (0,10::Int)
      let inp = [(c,i),(c,j)]
      return $ $(testing [|incrementKey c inp|]) (?==[(c,i+1),(c,j+1)])

ex6_char_int =
    property $ do
      is <- vectorOf 5 $ choose (0,20::Int)
      ks <- vectorOf 5 $ choose ('a','g')
      c <- choose ('a','h')
      let inp = zip ks is
          out = zipWith (\k i -> if k==c then (k,succ i) else (k,i)) ks is
      return $ $(testing [|incrementKey c inp|]) (?== out)



ex7 = conjoin [forAll_ $ \(Small i) -> $(testing [|average [fromIntegral i :: Float]|]) (?==fromIntegral i)
              ,forAllBlind (choose (1,10)) $ \n -> forAll_ $ \(i::Rational) ->
                  $(testing [|average (replicate n i)|]) (?==i)
              ,forAll_ $ \(Small i) -> let f = fromIntegral i in $(testing [|average [f-1,f,f+1]|]) (?==f)
              ,forAll_ $ \(Small i,Small j) ->
                  let x = fromIntegral i :: Double
                      y = fromIntegral j :: Double
                  in $(testing [|average (replicate 10 x ++ replicate 10 y)|]) (?~=(x+y)/2)]

