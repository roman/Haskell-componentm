{-# LANGUAGE NoImplicitPrelude #-}
{-|
Module      : Control.Monad.Component
Description : Build composable, idempotent & transparent application cleanup sub-routines for an application
Copyright   : (c) Roman Gonzalez, 2017
License     : MIT
Maintainer  : romanandreg@gmail.com
Stability   : experimental

Provides functions that help on the creation of Application teardown sub-routines
-}
module Control.Monad.Component
  (

  -- * 'ComponentM' monad and runner
    ComponentM
  , runComponentM
  , runComponentM1

  -- * 'Component' error record
  , ComponentError (..)
  , ComponentBuildError (..)

  -- * 'Teardown' functions
  , TeardownResult (..)

  -- * Re-exports
  , throwM

  -- * Functions to build 'ComponentM' sub-routines
  , buildComponent
  , buildComponent_
  ) where

import Control.Monad.Catch                    (throwM)
import Control.Monad.Component.Internal.Core
    (buildComponent, buildComponent_, runComponentM, runComponentM1)
import Control.Monad.Component.Internal.Types
    (ComponentBuildError (..), ComponentError (..), ComponentM)
import Control.Teardown
    (TeardownResult (..))
