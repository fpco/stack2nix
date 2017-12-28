{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Stack2nix
  ( Args(..)
  , stack2nix
  , version
  ) where

import           Control.Monad              (unless, void)
import           Data.Maybe                 (isJust)
import           Data.Monoid                ((<>))
import           Distribution.Text          (display)
import           Path                       (parseAbsFile)
import           Paths_stack2nix            (version)
import           Stack.Config
import           Stack.Prelude              (LogLevel (..), runRIO)
import           Stack.Types.Config
import           Stack.Types.Runner
import           Stack2nix.External.Stack
import           Stack2nix.External.Util    (runCmdFrom)
import           Stack2nix.External.VCS.Git (Command (..), ExternalCmd (..),
                                             InternalCmd (..), git)
import           Stack2nix.Types            (Args (..))
import           Stack2nix.Util
import           System.Directory           (canonicalizePath, doesFileExist,
                                             getCurrentDirectory, withCurrentDirectory)
import           System.Environment         (getEnv)
import           System.FilePath            ((</>))
import           System.IO.Temp             (withSystemTempDirectory)

stack2nix :: Args -> IO ()
stack2nix args@Args{..} = do
  checkRuntimeDeps
  updateCabalPackageIndex
  -- cwd <- getCurrentDirectory
  -- let projRoot = if isAbsolute argUri then argUri else cwd </> argUri
  let projRoot = argUri
  isLocalRepo <- doesFileExist $ projRoot </> "stack.yaml"
  logDebug args $ "stack2nix (isLocalRepo): " ++ show isLocalRepo
  logDebug args $ "stack2nix (projRoot): " ++ show projRoot
  logDebug args $ "stack2nix (argUri): " ++ show argUri
  if isLocalRepo
  then handleStackConfig Nothing projRoot
  else withSystemTempDirectory "s2n-" $ \tmpDir ->
    tryGit tmpDir >> handleStackConfig (Just argUri) tmpDir
  where
    checkRuntimeDeps :: IO ()
    checkRuntimeDeps = do
      assertMinVer "git" "2"
      assertMinVer "cabal" "2"

    updateCabalPackageIndex :: IO ()
    updateCabalPackageIndex =
      getEnv "HOME" >>= \home -> void $ runCmdFrom home "cabal" ["update"]

    tryGit :: FilePath -> IO ()
    tryGit tmpDir = do
      void $ git $ OutsideRepo $ Clone argUri tmpDir
      case argRev of
        Just r  -> void $ git $ InsideRepo tmpDir (Checkout r)
        Nothing -> return mempty

    handleStackConfig :: Maybe String -> FilePath -> IO ()
    handleStackConfig remoteUri localDir = do
      cwd <- getCurrentDirectory
      logDebug args $ "handleStackConfig (cwd): " ++ cwd
      logDebug args $ "handleStackConfig (localDir): " ++ localDir
      logDebug args $ "handleStackConfig (remoteUri): " ++ show remoteUri
      let stackFile = localDir </> "stack.yaml"
      alreadyExists <- doesFileExist stackFile
      unless alreadyExists $ error $ stackFile <> " does not exist. Use 'stack init' to create it."
      logDebug args $ "handleStackConfig (alreadyExists): " ++ show alreadyExists
      cp <- canonicalizePath stackFile
      fp <- parseAbsFile cp
      lc <- withRunner LevelError True False ColorAuto Nothing False $ \runner ->
        -- https://www.fpcomplete.com/blog/2017/07/the-rio-monad
        runRIO runner $ loadConfig mempty Nothing (SYLOverride fp)
      if isJust remoteUri then withCurrentDirectory localDir (toNix args localDir lc) else toNix args localDir lc


toNix :: Args -> FilePath -> LoadConfig -> IO ()
toNix args baseDir lc = runPlan baseDir lc args $ patchAndMerge args

patchAndMerge :: Args -> String -> IO ()
patchAndMerge Args{..} contents = do
  case argOutFile of
    Just fname -> writeFile fname defaultNix
    Nothing    -> putStrLn defaultNix
  where
    defaultNix = unlines
      [ "# Generated using stack2nix " ++ display version ++ "."
      , "#"
      , "# Only works with sufficiently recent nixpkgs, e.g. \"NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs/archive/21a8239452adae3a4717772f4e490575586b2755.tar.gz\"."
      , ""
      , "{ pkgs ? (import <nixpkgs> {})"
      , ", compiler ? pkgs.haskell.packages.ghc802"
      , ", ghc ? pkgs.haskell.compiler.ghc802"
      , "}:"
      , ""
      , "with pkgs.haskell.lib;"
      , ""
      , "let"
      , "  stackPackages = { callPackage, pkgs, stdenv }:"
      , "self: {"
      , contents
      , "};"
      , "in"
      , "compiler.override {"
      , "  initialPackages = stackPackages;"
      , "  configurationCommon = { ... }: self: super: {};"
      , "}"
      ]
