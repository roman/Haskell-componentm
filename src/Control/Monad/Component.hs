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

  -- * 'Component' record and functions
  , Component
  , fromComponent

  -- * 'Component' error record
  , ComponentError (..)

  -- * 'Teardown' functions
  , Teardown
  , TeardownResult (..)
  , runTeardown
  , runTeardown_
  , newTeardown
  , getTeardown

  -- * Re-exports
  , throwM

  -- * Functions to build 'ComponentM' sub-routines
  , buildComponent
  , buildComponent_
  ) where

import Control.Monad.Catch                    (throwM)
import Control.Monad.Component.Internal.Core
    (buildComponent, buildComponent_, runComponentM)
import Control.Monad.Component.Internal.Types
    (Component, ComponentError (..), ComponentM, fromComponent)
import Control.Teardown
    (Teardown, TeardownResult (..), newTeardown, runTeardown, runTeardown_, getTeardown)
