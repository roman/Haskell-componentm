{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.Component.Internal.Types where

import RIO
import RIO.Time (NominalDiffTime)
import qualified RIO.Set as S
import qualified RIO.HashMap as M.Hash

import Data.Text.Prettyprint.Doc ((<+>), Pretty, pretty)
import qualified Data.Text.Prettyprint.Doc as Pretty

import           Data.Graph (graphFromEdges', topSort)
import           Control.Monad.Catch    (MonadThrow (..))

import Control.Teardown (Teardown, TeardownResult, newTeardown)

--------------------------------------------------------------------------------

data ComponentError
  = ComponentBuildFailure   !SomeException
  | ComponentStartupFailure !Text !SomeException
  deriving (Generic, Show)

instance Exception ComponentError

data DuplicatedComponentKeyDetected
  = DuplicatedComponentKeyDetected !Text
  deriving (Generic, Show)

instance Exception DuplicatedComponentKeyDetected

-- | Failure raised when the application callback throws an exception
data ComponentRuntimeFailed
  = ComponentRuntimeFailed !SomeException !TeardownResult
  deriving (Generic, Show)

instance Exception ComponentRuntimeFailed

-- | Failure raised when construction of ComponentM throws an exception
data ComponentBuildFailed
  = ComponentBuildFailed ![ComponentError] !TeardownResult
  deriving (Generic, Show)

instance Exception ComponentBuildFailed

type Description = Text

data Build
  = Build {
      componentDesc      :: !Description
    , componentTeardown  :: !Teardown
    , buildElapsedTime   :: !NominalDiffTime
    , buildFailure       :: !(Maybe SomeException)
    , buildDependencies  :: !(Set Description)
    }
  deriving (Generic)

instance Pretty Build where
  pretty Build {componentDesc, buildElapsedTime, buildFailure} =
    let
      statusSymbol :: Text
      statusSymbol = if isJust buildFailure then "✘" else "✓"

      errorInfo =
        if isJust buildFailure then
          [
            Pretty.hardline
          , Pretty.pipe <+> pretty (show buildFailure)
          ]
        else
          []

    in
      Pretty.hang 2
          $ Pretty.hsep
          $ [ pretty statusSymbol
            , pretty componentDesc
            , Pretty.parens (pretty $ show buildElapsedTime)
            ] <> errorInfo


instance Display Build where
  display = displayShow . pretty

type BuildTable = HashMap Description Build

newtype BuildResult
  = BuildResult [Build]

instance Pretty BuildResult where
  pretty (BuildResult builds) =
      pretty ("Application Initialized" :: Text)
      <> Pretty.hardline
      <> Pretty.vsep (map pretty builds)

instance Display BuildResult where
  display buildResult =
    displayShow $ pretty buildResult

data ComponentEvent
  = ComponentBuilt !BuildResult
  | ComponentReleased !TeardownResult

instance Pretty ComponentEvent where
  pretty ev =
    case ev of
      ComponentBuilt buildResult ->
        pretty buildResult
      ComponentReleased teardownResult ->
        "Application Teardown"
        <> Pretty.hardline
        <> pretty teardownResult

instance Display ComponentEvent where
  display = displayShow . pretty


-- newtype BuildError
--   = BuildError { fromBuildError :: [ComponentError] }
--   deriving (Generic, Show)

-- instance Exception BuildError

-- instance Pretty BuildError where
--   pretty (BuildError errList) =
--     Pretty.indent 2
--      $ Pretty.vsep
--      $ ["Your application failed with the following errors:"]
--      ++ map (\err -> Pretty.hsep ["-", pretty (show err)]) errList

-- instance Display BuildError where
--   display = displayShow . pretty

--------------------

newtype ComponentM a
  = ComponentM (IO (Either ([ComponentError], BuildTable)
                           (a, BuildTable)))

--------------------

instance Functor ComponentM where
  fmap f (ComponentM action) =
    ComponentM $ do
      result <- action
      return $! case result of
        Left err ->
          Left err
        Right (a, builds) ->
          Right (f a, builds)

--------------------

validateKeyDuplication
  :: Monad m
  => (HashMap Text v -> HashMap Text v -> HashMap Text v)
  -> HashMap Text v
  -> HashMap Text v
  -> m (Either ([ComponentError], HashMap Text v) (HashMap Text v))
validateKeyDuplication mergeFn a b =
  case M.Hash.keys $ M.Hash.intersection a b of
    [] -> return $ Right (mergeFn a b)
    keys -> do
      let
        errors = map (ComponentBuildFailure . toException . DuplicatedComponentKeyDetected)
                     keys
      return (Left (errors, M.Hash.union a b))

instance Applicative ComponentM where
  pure a = ComponentM $
    return $ Right (a, M.Hash.empty)

  (<*>) (ComponentM cf) (ComponentM ca) = ComponentM $ do
    -- NOTE: We do not handle IO errors here because they are being managed in
    -- the leafs; we don't expose the constructor of ComponentM
    let validateKeys =
          validateKeyDuplication M.Hash.union

    (rf, ra) <- concurrently cf ca
    case (rf, ra) of
      (Right (f, depsF), Right (a, depsA)) ->
        validateKeys depsF depsA >>= \case
          Right deps  -> return $ Right (f a, deps)
          Left (errors, deps) -> return $ Left (errors, deps)


      (Right (_, depsF), Left (errA, depsA)) ->
        validateKeys depsF depsA >>= \case
          Right deps -> return $ Left (errA, deps)
          Left (errors, deps) -> return $ Left (errA <> errors, deps)

      (Left (errF, depsF), Right (_, depsA)) ->
        validateKeys depsF depsA >>= \case
          Right deps -> return $ Left (errF, deps)
          Left (errors, deps) -> return $ Left (errF <> errors, deps)

      (Left (errF, depsF), Left (errA, depsA)) ->
        validateKeys depsF depsA >>= \case
          Right deps -> return $ Left (errF, deps)
          Left (errors, deps) -> return $ Left (errF <> errA <> errors, deps)

--------------------

appendDependency :: Description -> Build -> Build
appendDependency depDesc build =
  build {
    buildDependencies =
      S.insert depDesc (buildDependencies build)
  }

appendDependencies :: BuildTable -> BuildTable -> BuildTable
appendDependencies fromBuildTable toBuildTable =
  let
    appendDependenciesToBuild build =
      foldr appendDependency build (M.Hash.keys fromBuildTable)
  in
    -- First, we add all keys from fromBuildTable to all entries of toBuildTable
    M.Hash.map appendDependenciesToBuild toBuildTable
    -- Then, we join both fromBuildTable and toBuildTable
    & M.Hash.union fromBuildTable

instance Monad ComponentM where
  return = pure
  (>>=) (ComponentM ma) f = ComponentM $ do
    let validateKeys =
          validateKeyDuplication appendDependencies

    resultA <- ma
    case resultA of
      Left (errA, depsA) ->
        return $ Left (errA, depsA)

      Right (a, depsA) -> do
        let (ComponentM mb) = f a
        resultB <- mb

        case resultB of
          Left (errB, depsB) ->
            validateKeys depsA depsB >>= \case
              Right deps -> return $ Left (errB, deps)
              Left (errors, deps) -> return $ Left (errB <> errors, deps)

          Right (b, depsB) ->
            validateKeys depsA depsB >>= \case
              Right deps -> return $ Right (b, deps)
              Left (errors, deps) -> return $ Left (errors, deps)

instance MonadThrow ComponentM where
  throwM e =
    ComponentM
      $ return
      $ Left ([ComponentBuildFailure $ toException e], M.Hash.empty)

instance MonadIO ComponentM where
  liftIO action = ComponentM $ do
    eresult <- try action
    case eresult of
      Left err -> return $ Left ([ComponentBuildFailure err], M.Hash.empty)
      Right a -> return $ Right (a, M.Hash.empty)


--------------------------------------------------------------------------------

buildTableToOrderedList :: BuildTable -> [Build]
buildTableToOrderedList buildTable =
  let
    buildGraphEdges :: [(Build, Description, [Description])]
    buildGraphEdges =
      M.Hash.foldrWithKey
        (\k build acc -> (build, k, S.toList $ buildDependencies build):acc)
        []
        buildTable

    (componentGraph, lookupBuild) =
      graphFromEdges' buildGraphEdges

  in
     map (\buildIndex ->
            let (build, _, _) = lookupBuild buildIndex
            in build
         )
         (topSort componentGraph)

buildTableToTeardown :: Text -> BuildTable -> IO Teardown
buildTableToTeardown appName buildTable =
   newTeardown appName (map componentTeardown $ buildTableToOrderedList buildTable)
