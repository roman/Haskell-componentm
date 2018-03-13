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
  , withComponentM
  , withComponentM2


  -- * 'Component' error record
  , ComponentError (..)

  -- * 'Teardown' API
  , TeardownResult (..)
  , renderTeardownReport

  -- * Re-exports
  , throwM

  -- * Functions to build 'ComponentM' sub-routines
  , BuildInfo(..)
  , buildComponent
  , buildComponent_
  ) where

import Control.Monad.Catch                    (throwM)
import Control.Monad.Component.Internal.Core
    (buildComponent, buildComponent_, withComponentM, withComponentM2)
import Control.Monad.Component.Internal.Types
    (BuildInfo(..), ComponentError (..), ComponentM)
import Control.Teardown
    (TeardownResult (..), renderTeardownReport)
