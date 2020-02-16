{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified AnalyticsLaop as LM
import qualified AnalyticsHMatrix as HM
import qualified AnalyticsMatrix as DM
import qualified LAoP.Matrix.Type as LM
import qualified Numeric.LinearAlgebra as HM
import qualified Numeric.LinearAlgebra.Data as HM
import qualified Numeric.LinearAlgebra.HMatrix as HM
import qualified Data.Matrix as DM
import GHC.TypeLits
import Data.Proxy
import Criterion.Main
import Test.QuickCheck

randomBDLaop ::
             forall jn en .
             ( KnownNat jn,
               KnownNat en,
               LM.FromLists Int (LM.FromNat (jn + 1)) (LM.FromNat 3),
               LM.FromLists Int (LM.FromNat (jn + 1)) (),
               LM.FromLists Int (LM.FromNat (en + 1)) (LM.FromNat 5),
               LM.FromLists Int (LM.FromNat (en + 1)) (LM.FromNat 3),
               LM.FromLists Int (LM.FromNat (en + 1)) (LM.FromNat 2)
             ) => Gen (LM.BD jn en)
randomBDLaop = do
  let jn' = fromInteger (natVal (Proxy :: Proxy jn)) + 1
      en' = fromInteger (natVal (Proxy :: Proxy en)) + 1
  descs <- vectorOf (jn' * 3) (elements [0, 1])
  salary <- vectorOf jn' (choose (0, 10000))
  code <- vectorOf (jn' * 3) (elements [0, 1])
  let jobs = LM.JT {
      LM.jDesc   = LM.fromLists (buildList descs jn'),
      LM.jSalary = LM.fromLists (buildList salary jn'),
      LM.jCode   = LM.fromLists (buildList code jn')
          }
  name <- vectorOf (en' * 5) (elements [0, 1])
  job <- vectorOf (en' * 3) (elements [0, 1])
  country <- vectorOf (en' * 2) (elements [0, 1])
  branch <- vectorOf (en' * 2) (elements [0, 1])
  let employees = LM.ET {
      LM.eName    = LM.fromLists (buildList name en'),
      LM.eJob     = LM.fromLists (buildList job en'),
      LM.eCountry = LM.fromLists (buildList country en'),
      LM.eBranch  = LM.fromLists (buildList branch en')
          }
  return LM.BD { LM.jobsTable = jobs, LM.employeesTable = employees }

randomBDHMatrix ::
             forall jn en .
             ( KnownNat jn,
               KnownNat en
             ) => Gen (HM.BD jn en)
randomBDHMatrix = do
  let jn' = fromInteger (natVal (Proxy :: Proxy jn)) + 1
      en' = fromInteger (natVal (Proxy :: Proxy en)) + 1
  descs <- vectorOf (jn' * 3) (elements [0, 1])
  salary <- vectorOf jn' (choose (0, 10000))
  code <- vectorOf (jn' * 3) (elements [0, 1])
  let jobs = HM.JT {
      HM.jDesc   = HM.fromLists (buildList descs jn'),
      HM.jSalary = HM.fromLists (buildList salary jn'),
      HM.jCode   = HM.fromLists (buildList code jn')
          }
  name <- vectorOf (en' * 5) (elements [0, 1])
  job <- vectorOf (en' * 3) (elements [0, 1])
  country <- vectorOf (en' * 2) (elements [0, 1])
  branch <- vectorOf (en' * 2) (elements [0, 1])
  let employees = HM.ET {
      HM.eName    = HM.fromLists (buildList name en'),
      HM.eJob     = HM.fromLists (buildList job en'),
      HM.eCountry = HM.fromLists (buildList country en'),
      HM.eBranch  = HM.fromLists (buildList branch en')
          }
  return HM.BD { HM.jobsTable = jobs, HM.employeesTable = employees }

randomBDMatrix ::
             forall jn en .
             ( KnownNat jn,
               KnownNat en
             ) => Gen (DM.BD jn en)
randomBDMatrix = do
  let jn' = fromInteger (natVal (Proxy :: Proxy jn)) + 1
      en' = fromInteger (natVal (Proxy :: Proxy en)) + 1
  descs <- vectorOf (jn' * 3) (elements [0, 1])
  salary <- vectorOf jn' (choose (0, 10000))
  code <- vectorOf (jn' * 3) (elements [0, 1])
  let jobs = DM.JT {
      DM.jDesc   = DM.fromLists (buildList descs jn'),
      DM.jSalary = DM.fromLists (buildList salary jn'),
      DM.jCode   = DM.fromLists (buildList code jn')
          }
  name <- vectorOf (en' * 5) (elements [0, 1])
  job <- vectorOf (en' * 3) (elements [0, 1])
  country <- vectorOf (en' * 2) (elements [0, 1])
  branch <- vectorOf (en' * 2) (elements [0, 1])
  let employees = DM.ET {
      DM.eName    = DM.fromLists (buildList name en'),
      DM.eJob     = DM.fromLists (buildList job en'),
      DM.eCountry = DM.fromLists (buildList country en'),
      DM.eBranch  = DM.fromLists (buildList branch en')
          }
  return DM.BD { DM.jobsTable = jobs, DM.employeesTable = employees }

buildList [] _ = []
buildList l r  = take r l : buildList (drop r l) r

setupEnv1 = do
  -- HMatrix
  bd1 <- generate (resize 1 (randomBDHMatrix @999 @999))
  -- Matrix
  bd2 <- generate (resize 1 (randomBDMatrix @999 @999))
  -- Laop
  bd3 <- generate (resize 1 (randomBDLaop @999 @999))
  return (bd1, bd2, bd3)

benchmark :: IO ()
benchmark = do
  print "Starting benchmarks..."
  defaultMain [
   env setupEnv1 $ \ ~(bd1, bd2, bd3) -> bgroup "DB query" [
   bgroup "1000 entries" [
     bench "hmatrix" $ nf HM.query bd1
   , bench "matrix" $ nf DM.query bd2
   , bench "laop" $ nf LM.query bd3
   ] ] ]

main :: IO ()
main = benchmark
