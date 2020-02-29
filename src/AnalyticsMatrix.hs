{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module AnalyticsMatrix
where

import qualified LAoP.Matrix.Type as L
import qualified LAoP.Matrix.Internal as I
import Data.Matrix
import GHC.TypeLits
import GHC.Generics
import Data.Proxy
import Data.Coerce
import Control.DeepSeq

data Description = GroupLeader | Programmer | SystemAnalyst -- ...
  deriving (Show, Eq, Enum, Bounded, Generic)

data Job = GL | Pr | SA -- ...
  deriving (Show, Eq, Enum, Bounded, Generic)

data Country = PT | UK -- ...
  deriving (Show, Eq, Enum, Bounded, Generic)

data Name = Mary | John | Charles | Ana | Manuel -- ...
  deriving (Show, Eq, Enum, Bounded, Generic)

data Branch = Mobile | Web -- ...
  deriving (Show, Eq, Enum, Bounded, Generic)

type Salary = Double

-- | @n@ represents the last id number (starting at 0)
type ID (n :: Nat) = n

-- | @n@ represents the last id number (starting at 0)
data Jobs (n :: Nat) = JT {
  jCode :: Matrix Double,
  jDesc :: Matrix Double,
  jSalary :: Matrix Double
                         }
  deriving (Show, Generic, NFData)

data Employees (n :: Nat) = ET {
  eName :: Matrix Double,
  eJob :: Matrix Double,
  eCountry :: Matrix Double,
  eBranch :: Matrix Double
                              }
  deriving (Show, Generic, NFData)

data BD (jn :: Nat) (en :: Nat) = BD {
  jobsTable :: Jobs jn,
  employeesTable :: Employees en
                                   }
  deriving (Show, Generic, NFData)

jobs :: Jobs 2
jobs = JT { 
  jDesc   = fromLists [[0, 0, 1], [1, 0, 0], [0, 1, 0]],
  jSalary = fromLists [[1000, 1100, 1333]],
  jCode   = fromLists [[0, 0, 1], [1, 0, 0], [0, 1, 0]]
          }

employees :: Employees 4
employees = ET {
  eName    = fromLists [[1, 0, 0, 0, 0], [0, 1, 0, 0, 0], [0, 0, 1, 0, 0], [0, 0, 0, 1, 0], [0, 0, 0, 0, 1]],
  eJob     = fromLists [[0, 0, 1, 0, 0], [1, 1, 0, 0, 1], [0, 0, 0, 1, 0]],
  eCountry = fromLists [[0, 0, 0, 1, 1], [1, 1, 1, 0, 0]],
  eBranch  = fromLists [[1, 0, 1, 0, 0], [0, 1, 0, 1, 1]]
               }

bd :: BD 2 4
bd = BD {
  jobsTable      = jobs,
  employeesTable = employees
        }

insertJob :: BD jn en -> Job -> Description -> Salary -> BD (jn + 1) en
insertJob bd j d s =
  let jobTb      = jobsTable bd
      dPoint     = point d
      jPoint     = point j
      newJDesc   = jDesc jobTb <|> dPoint
      newJSalary = jSalary jobTb <|> one s
      newJCode   = jCode jobTb <|> jPoint
      newJobTb   = JT { jDesc = newJDesc, jSalary = newJSalary, jCode = newJCode }
   in bd { jobsTable = newJobTb }
  where
    one s = fromLists [[s]]
    point x = fromLists (L.toLists (L.point x))

query ::
      forall jn en .
      ( KnownNat (I.Count (I.FromNat (en + 1))),
        I.FromLists Double (I.FromNat (en + 1)) (I.FromNat 2),
        I.FromLists Double (I.FromNat (en + 1)) ()
      ) => BD jn en -> Matrix Double
query bd =
  let x = transpose (eBranch employeesTb)
   in khatri @(I.FromNat (en + 1)) @(I.FromNat 1) @(I.FromNat 2) v (eCountry employeesTb) `multStd2` x
  where
    jobTb = jobsTable bd
    employeesTb = employeesTable bd
    v = jSalary jobTb `multStd2` transpose (jCode jobTb) `multStd2` eJob employeesTb

khatri :: forall a b c .
       ( L.CountableN (b, c),
         L.CountableDimensions a b,
         L.CountableDimensions a c,
         I.FromLists Double a b,
         I.FromLists Double a c,
         I.FromLists Double (I.Normalize (b, c)) b,
         I.FromLists Double (I.Normalize (b, c)) c
       ) => Matrix Double -> Matrix Double -> Matrix Double
khatri a b =
  let x = I.fromLists (toLists a) :: I.Matrix Double a b
      y = I.fromLists (toLists b) :: I.Matrix Double a c
   in fromLists (I.toLists (I.kr x y))
