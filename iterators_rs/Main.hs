{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
module Main (main) where

import Foreign
import Foreign.C.Types
import Criterion.Main
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad.Primitive (RealWorld)
import System.IO.Unsafe
import Data.IORef
import GHC.Prim
import GHC.Types

foreign import ccall "rust_iters"
  rust_iters :: Int -> Int

foreign import ccall "rust_loop"
  rust_loop :: Int -> Int

foreign import ccall "rust_cheating"
  rust_cheating :: Int -> Int

foreign import ccall "rust_stream"
  rust_stream :: Int -> Int

foreign import ccall "rust_stream_immut"
  rust_stream_immut :: Int -> Int

foreign import ccall "rust_no_trait"
  rust_no_trait :: Int -> Int

foreign import ccall "c_loop"
  c_loop :: Int -> Int

foreign import ccall "c_cheating"
  c_cheating :: Int -> Int

foreign import ccall "c_bits"
  c_bits :: Int -> Int

main :: IO ()
main = do
  let expected = boxedVector 4
  mapM_ (\(s, f) ->
           if f 4 == expected
             then return ()
             else error $ "Did not match: " ++ show (expected, f 4, s))
    funcs
  defaultMain $ map (\(s, f) -> bench s $ whnf f high) funcs
  where
    high :: Int
    high = 1000000

    funcs =
      [ ("C cheating", c_cheating)
      , ("Rust cheating", rust_cheating)
      , ("Haskell cheating", haskellCheating)
      , ("C bits", c_bits)
      , ("C loop", c_loop)
      , ("Rust stream", rust_stream)
      , ("Rust stream immutable", rust_stream_immut)
      , ("Rust no trait", rust_no_trait)
      , ("Rust iters", rust_iters)
      , ("Rust loop", rust_loop)
      , ("Haskell boxed vector", boxedVector)
      , ("Haskell unboxed vector", unboxedVector)
      , ("Haskell list", list)
      , ("Haskell recursion", recursion)
      , ("Haskell primitive", primitive)
      , ("Haskell iterator 1", iterator1)
      , ("Haskell iterator 1 unboxed", iterator1U)
      , ("Haskell iterator 2", iterator2)
      , ("Haskell iterator 3", iterator3)
      , ("Haskell iterator 4", iterator4)
      , ("Haskell iterator 5", iterator5)
      ]

haskellCheating :: Int -> Int
haskellCheating high' =
  loop 0 4
  where
    loop !total !i
      | i <= high = loop (total + i) (i + 4)
      | otherwise = total
    high = high' * 2

boxedVector :: Int -> Int
boxedVector high =
  VB.sum $ VB.map (* 2) $ VB.filter even $ VB.enumFromTo 1 high

unboxedVector :: Int -> Int
unboxedVector high =
  VU.sum $ VU.map (* 2) $ VU.filter even $ VU.enumFromTo 1 high

list :: Int -> Int
list high =
  sum $ map (* 2) $ filter even $ enumFromTo 1 high

recursion :: Int -> Int
recursion high =
  loop 1 0
  where
    loop !i !total
      | i > high = total
      | even i = loop (i + 1) (total + i * 2)
      | otherwise = loop (i + 1) total

primitive :: Int -> Int
primitive (I# high) =
  loop 1# 0#
  where
    loop i total
      | isTrue# (i ># high) = I# total
      | isTrue# (andI# i 1#) = loop (i +# 1#) total
      | otherwise = loop (i +# 1#) (total +# (i *# 2#))

iterator1 :: Int -> Int
iterator1 high =
  unsafePerformIO $
  enumFromTo1 1 high >>=
  filter1 even >>=
  map1 (* 2) >>=
  sum1

class Iterator1 iter where
  type Item1 iter
  next1 :: iter -> IO (Maybe (Item1 iter))

data EnumFromTo1 a = EnumFromTo1 !(IORef a) !a
enumFromTo1 :: a -> a -> IO (EnumFromTo1 a)
enumFromTo1 low high = do
  ref <- newIORef low
  return $ EnumFromTo1 ref high
instance (Ord a, Num a) => Iterator1 (EnumFromTo1 a) where
  type Item1 (EnumFromTo1 a) = a
  next1 (EnumFromTo1 ref high) = do
    i <- readIORef ref
    if i > high
      then return Nothing
      else do
        writeIORef ref $! i + 1
        return $ Just i

data Filter1 a iter = Filter1 !(a -> Bool) !iter
filter1 :: (a -> Bool) -> iter -> IO (Filter1 a iter)
filter1 predicate iter = return $ Filter1 predicate iter
instance (Iterator1 iter, Item1 iter ~ a) => Iterator1 (Filter1 a iter) where
  type Item1 (Filter1 a iter) = a
  next1 (Filter1 predicate iter) =
    loop
    where
      loop = do
        mx <- next1 iter
        case mx of
          Nothing -> return Nothing
          Just x
            | predicate x -> return $ Just x
            | otherwise   -> loop

data Map1 a b iter = Map1 !(a -> b) !iter
map1 :: (a -> b) -> iter -> IO (Map1 a b iter)
map1 f iter = return $ Map1 f iter
instance (Iterator1 iter, Item1 iter ~ a) => Iterator1 (Map1 a b iter) where
  type Item1 (Map1 a b iter) = b
  next1 (Map1 f iter) = (fmap.fmap) f (next1 iter)

sum1 :: (Num (Item1 iter), Iterator1 iter) => iter -> IO (Item1 iter)
sum1 iter =
  loop 0
  where
    loop !total = do
      mx <- next1 iter
      case mx of
        Nothing -> return total
        Just x -> loop (total + x)

iterator1U :: Int -> Int
iterator1U high =
  unsafePerformIO $
  enumFromTo1U 1 high >>=
  filter1 even >>=
  map1 (* 2) >>=
  sum1

data EnumFromTo1U a = EnumFromTo1U !(VUM.MVector RealWorld a) !a
enumFromTo1U :: VUM.Unbox a => a -> a -> IO (EnumFromTo1U a)
enumFromTo1U low high = do
  ref <- VUM.replicate 1 low
  return $ EnumFromTo1U ref high
instance (Ord a, Num a, VUM.Unbox a) => Iterator1 (EnumFromTo1U a) where
  type Item1 (EnumFromTo1U a) = a
  next1 (EnumFromTo1U ref high) = do
    i <- VUM.unsafeRead ref 0
    if i > high
      then return Nothing
      else do
        VUM.unsafeWrite ref 0 $! i + 1
        return $ Just i

iterator2 :: Int -> Int
iterator2 high =
  sum2 $
  map2 (* 2) $
  filter2 even $
  enumFromTo2 1 high

data Step2 iter
  = Done2
  | Yield2 !iter !(Item2 iter)

class Iterator2 iter where
  type Item2 iter
  next2 :: iter -> Step2 iter

data EnumFromTo2 a = EnumFromTo2 !a !a
enumFromTo2 :: a -> a -> EnumFromTo2 a
enumFromTo2 = EnumFromTo2
instance (Ord a, Num a) => Iterator2 (EnumFromTo2 a) where
  type Item2 (EnumFromTo2 a) = a
  next2 (EnumFromTo2 i high)
    | i > high = Done2
    | otherwise = Yield2 (EnumFromTo2 (i + 1) high) i

data Filter2 a iter = Filter2 !(a -> Bool) !iter
filter2 :: (a -> Bool) -> iter -> Filter2 a iter
filter2 = Filter2
instance (Iterator2 iter, Item2 iter ~ a) => Iterator2 (Filter2 a iter) where
  type Item2 (Filter2 a iter) = a
  next2 (Filter2 predicate iter0) =
    loop iter0
    where
      loop iter1 =
        case next2 iter1 of
          Done2 -> Done2
          Yield2 iter2 x
            | predicate x -> Yield2 (Filter2 predicate iter2) x
            | otherwise   -> loop iter2

data Map2 a b iter = Map2 !(a -> b) !iter
map2 :: (a -> b) -> iter -> Map2 a b iter
map2 = Map2
instance (Iterator2 iter, Item2 iter ~ a) => Iterator2 (Map2 a b iter) where
  type Item2 (Map2 a b iter) = b
  next2 (Map2 f iter1) =
    case next2 iter1 of
      Done2 -> Done2
      Yield2 iter2 x -> Yield2 (Map2 f iter2) (f x)

sum2 :: (Num (Item2 iter), Iterator2 iter) => iter -> Item2 iter
sum2 =
  loop 0
  where
    loop !total iter1 =
      case next2 iter1 of
        Done2 -> total
        Yield2 iter2 x -> loop (total + x) iter2

iterator3 :: Int -> Int
iterator3 high =
  sum3 $
  map3 (* 2) $
  filter3 even $
  enumFromTo3 1 high

data Step3 iter
  = Done3
  | Yield3 !iter !Int

class Iterator3 iter where
  next3 :: iter -> Step3 iter

data EnumFromTo3 = EnumFromTo3 !Int !Int
enumFromTo3 :: Int -> Int -> EnumFromTo3
enumFromTo3 = EnumFromTo3
instance Iterator3 EnumFromTo3 where
  next3 (EnumFromTo3 i high)
    | i > high = Done3
    | otherwise = Yield3 (EnumFromTo3 (i + 1) high) i

data Filter3 = Filter3 !(Int -> Bool) {-# UNPACK #-} !EnumFromTo3
filter3 :: (Int -> Bool) -> EnumFromTo3 -> Filter3
filter3 = Filter3
instance Iterator3 Filter3 where
  next3 (Filter3 predicate iter0) =
    loop iter0
    where
      loop iter1 =
        case next3 iter1 of
          Done3 -> Done3
          Yield3 iter3 x
            | predicate x -> Yield3 (Filter3 predicate iter3) x
            | otherwise   -> loop iter3

data Map3 = Map3 !(Int -> Int) {-# UNPACK #-} !Filter3
map3 :: (Int -> Int) -> Filter3 -> Map3
map3 = Map3
instance Iterator3 Map3 where
  next3 (Map3 f iter1) =
    case next3 iter1 of
      Done3 -> Done3
      Yield3 iter3 x -> Yield3 (Map3 f iter3) (f x)

sum3 :: Map3 -> Int
sum3 =
  loop 0
  where
    loop !total iter1 =
      case next3 iter1 of
        Done3 -> total
        Yield3 iter3 x -> loop (total + x) iter3

iterator4 :: Int -> Int
iterator4 high =
  sum4 $
  map4 (* 2) $
  filter4 even $
  enumFromTo4 1 high

data Step4 s a
  = Done4
  | Yield4 s a

data Iterator4 a = forall s. Iterator4 s (s -> Step4 s a)

enumFromTo4 :: (Ord a, Num a) => a -> a -> Iterator4 a
enumFromTo4 start high =
  Iterator4 start f
  where
    f i
      | i > high  = Done4
      | otherwise = Yield4 (i + 1) i

filter4 :: (a -> Bool) -> Iterator4 a -> Iterator4 a
filter4 predicate (Iterator4 s0 next) =
  Iterator4 s0 loop
  where
    loop s1 =
      case next s1 of
        Done4 -> Done4
        Yield4 s2 x
          | predicate x -> Yield4 s2 x
          | otherwise   -> loop s2

map4 :: (a -> b) -> Iterator4 a -> Iterator4 b
map4 f (Iterator4 s0 next) =
  Iterator4 s0 loop
  where
    loop s1 =
      case next s1 of
        Done4 -> Done4
        Yield4 s2 x -> Yield4 s2 (f x)

sum4 :: Num a => Iterator4 a -> a
sum4 (Iterator4 s0 next) =
  loop 0 s0
  where
    loop !total !s1 =
      case next s1 of
        Done4 -> total
        Yield4 s2 x -> loop (total + x) s2

iterator5 :: Int -> Int
iterator5 high =
  sum5 $
  map5 (* 2) $
  filter5 even $
  enumFromTo5 1 high

data Step5 s a
  = Done5
  | Skip5 s
  | Yield5 s a

data Iterator5 a = forall s. Iterator5 s (s -> Step5 s a)

enumFromTo5 :: (Ord a, Num a) => a -> a -> Iterator5 a
enumFromTo5 start high =
  Iterator5 start f
  where
    f i
      | i > high  = Done5
      | otherwise = Yield5 (i + 1) i

filter5 :: (a -> Bool) -> Iterator5 a -> Iterator5 a
filter5 predicate (Iterator5 s0 next) =
  Iterator5 s0 noloop
  where
    noloop s1 =
      case next s1 of
        Done5 -> Done5
        Skip5 s2 -> Skip5 s2
        Yield5 s2 x
          | predicate x -> Yield5 s2 x
          | otherwise   -> Skip5 s2

map5 :: (a -> b) -> Iterator5 a -> Iterator5 b
map5 f (Iterator5 s0 next) =
  Iterator5 s0 noloop
  where
    noloop s1 =
      case next s1 of
        Done5 -> Done5
        Skip5 s2 -> Skip5 s2
        Yield5 s2 x -> Yield5 s2 (f x)

sum5 :: Num a => Iterator5 a -> a
sum5 (Iterator5 s0 next) =
  loop 0 s0
  where
    loop !total !s1 =
      case next s1 of
        Done5 -> total
        Skip5 s2 -> loop total s2
        Yield5 s2 x -> loop (total + x) s2
