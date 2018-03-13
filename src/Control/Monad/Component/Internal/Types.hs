{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.Component.Internal.Types where

import RIO
import RIO.Time (NominalDiffTime)

import Control.Monad.Operational (ProgramT, singleton)
import Control.Teardown (Teardown)

--------------------------------------------------------------------------------

type ComponentKey = Text

data ComponentError
  = ComponentBuildFailure !SomeException
  | ComponentInitFailure  !SomeException ![BuildInfo]
  deriving (Generic, Show)

instance Exception ComponentError

data BuildInfo
  = BuildInfo
  {
    buildKey      :: !ComponentKey
  , buildTime     :: !NominalDiffTime
  }
  deriving (Generic, Show)

instance NFData BuildInfo

data ComponentI a where
  BuildComponent :: Typeable a => ComponentKey -> IO (a, Teardown) -> ComponentI a
  FailBuilder :: Exception e => e -> ComponentI void

-- | Monad used to build components for an application
newtype ComponentM a
  = ComponentM (ProgramT ComponentI IO a)
  deriving (Functor, Applicative, Monad)

instance MonadThrow ComponentM where
  throwM err =
    ComponentM $ singleton $ FailBuilder (ComponentBuildFailure $ toException err)
