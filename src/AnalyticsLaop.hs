{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module AnalyticsLaop where

import LAoP.Matrix.Type
import LAoP.Utils.Internal
import GHC.TypeLits
import GHC.Generics
import Data.Proxy
import Data.Coerce
import Control.DeepSeq
import Prelude hiding ((.))

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

type Salary = Int

-- | @n@ represents the last id number (starting at 0)
type ID (n :: Nat) = Natural 0 n

-- | @n@ represents the last id number (starting at 0)
data Jobs (n :: Nat) = JT {
  jCode :: Matrix Int (ID n) Job,
  jDesc :: Matrix Int (ID n) Description,
  jSalary :: Matrix Int (ID n) One
                         }
  deriving (Show, Generic, NFData)

data Employees (n :: Nat) = ET {
  eName :: Matrix Int (ID n) Name,
  eJob :: Matrix Int (ID n) Job,
  eCountry :: Matrix Int (ID n) Country,
  eBranch :: Matrix Int (ID n) Branch
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

insertJob ::
          forall jn en .
          ( KnownNat jn,
            KnownNat (jn + 1),
            KnownNat (Count (FromNat ((jn + 1) + 1))),
            KnownNat (Count (FromNat (jn + 1)) + 1),
            FromLists Int (FromNat ((jn + 1) + 1)) (Either (FromNat (jn + 1)) ())
          )
          => BD jn en -> Job -> Description -> Salary -> BD (jn + 1) en
insertJob bd j d s =
  let jobTb      = jobsTable bd
      dPoint     = point d
      jPoint     = point j
      converter  = fromF convert
      newJDesc   = (jDesc jobTb `join` dPoint) . tr converter
      newJSalary = (jSalary jobTb `join` one s) . tr converter
      newJCode   = (jCode jobTb `join` jPoint) . tr converter
      newJobTb   = JT { jDesc = newJDesc, jSalary = newJSalary, jCode = newJCode }
   in bd { jobsTable = newJobTb }
  where
    convert :: forall n . (KnownNat (n + 1)) => Either (Natural 0 n) One -> Natural 0 (n + 1)
    convert (Left (Nat n)) = Nat n
    convert (Right ()) = nat $ fromInteger (natVal (Proxy :: Proxy (n + 1)))

query :: BD jn en -> Matrix Int Branch Country
query bd = sndM . kr v (eCountry employeesTb) . tr (eBranch employeesTb)
  where
    jobTb = jobsTable bd
    employeesTb = employeesTable bd
    v = jSalary jobTb . tr (jCode jobTb) . eJob employeesTb
