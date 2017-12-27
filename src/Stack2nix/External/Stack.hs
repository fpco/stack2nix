{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Stack2nix.External.Stack
  ( PackageRef(..), runPlan
  ) where

import           Data.List                     (isInfixOf)
import qualified Data.Map.Strict               as M
import           Data.Maybe                    (fromJust)
import qualified Data.Set                      as S
import           Data.Text                     (pack, unpack)
import           Data.Time                     (UTCTime)
import           Options.Applicative
import           Stack.Build.Source            (loadSourceMapFull)
import           Stack.Build.Target            (NeedTargets (..))
import           Stack.Options.BuildParser
import           Stack.Options.GlobalParser
import           Stack.Options.Utils           (GlobalOptsContext (..))
import           Stack.Prelude                 hiding (mapConcurrently, logDebug)
import           Stack.Runners                 (withBuildConfig)
import           Stack.Types.BuildPlan         (Repo (..), PackageLocation (..))
import           Stack.Types.Config
import           Stack.Types.Config.Build      (BuildCommand (..))
import           Stack.Types.Nix
import           Stack.Types.Package           (PackageSource (..), lpPackage,
                                                packageName,
                                                packageVersion, lpLocation)
import           Stack.Types.PackageName       (PackageName)
import           Stack.Types.PackageIdentifier (PackageIdentifier (..),
                                                packageIdentifierString,
                                                PackageIdentifierRevision (..))
import           Stack2nix.External.Cabal2nix  (cabal2nix)
import           Stack2nix.External.Util       (failHard, runCmd)
import           Stack2nix.Types               (Args (..))
import           Stack2nix.Util                (mapPool, logDebug)
import           System.Directory              (canonicalizePath,
                                                createDirectoryIfMissing,
                                                getCurrentDirectory,
                                                makeRelativeToCurrentDirectory)
import           System.FilePath               (makeRelative, (</>))
import           System.IO                     (hPutStrLn, stderr)
import qualified Distribution.Nixpkgs.Haskell.Hackage as DB
import Distribution.Nixpkgs.Haskell.PackageSourceSpec (loadHackageDB)

data PackageRef
  = HackagePackage PackageIdentifierRevision
  | NonHackagePackage PackageIdentifier (PackageLocation FilePath)
  deriving (Eq, Show)

genNixFile :: Args -> FilePath -> FilePath -> Maybe String -> Maybe String -> DB.HackageDB -> PackageRef -> IO ()
genNixFile args baseDir outDir uri argRev hackageDB pkgRef = do
  cwd <- getCurrentDirectory
  logDebug args $ "\nGenerating nix expression for " ++ show pkgRef
  logDebug args $ "genNixFile (cwd): " ++ cwd
  logDebug args $ "genNixFile (baseDir): " ++ baseDir
  logDebug args $ "genNixFile (outDir): " ++ outDir
  logDebug args $ "genNixFile (uri): " ++ show uri
  logDebug args $ "genNixFile (pkgRef): " ++ show pkgRef
  case pkgRef of
    HackagePackage (PackageIdentifierRevision pkg revision) -> -- FIXME use revision
      void $ cabal2nix ("cabal://" <> packageIdentifierString pkg) Nothing Nothing (Just outDir) hackageDB
    NonHackagePackage _ident (PLFilePath path) -> do
      relPath <- makeRelativeToCurrentDirectory path
      logDebug args $ "genNixFile (LocalPackage: relPath): " ++ relPath
      projRoot <- canonicalizePath $ cwd </> baseDir
      logDebug args $ "genNixFile (LocalPackage: projRoot): " ++ projRoot
      let defDir = baseDir </> makeRelative projRoot path
      logDebug args $ "genNixFile (LocalPackage: defDir): " ++ defDir
      unless (".s2n" `isInfixOf` path) $
        void $ cabal2nix (fromMaybe defDir uri) (pack <$> argRev) (const relPath <$> uri) (Just outDir) hackageDB
    NonHackagePackage _ident (PLRepo repo) ->
       cabal2nix (unpack $ repoUrl repo) (Just $ repoCommit repo) (Just (repoSubdirs repo)) (Just outDir) hackageDB
    NonHackagePackage _ident PLArchive {} -> error "genNixFile: No support for archive package locations"

sourceMapToPackages :: Map PackageName PackageSource -> [PackageRef]
sourceMapToPackages = map sourceToPackage . M.elems
  where
    sourceToPackage :: PackageSource -> PackageRef
    sourceToPackage (PSIndex _ _flags _options pir) = HackagePackage pir
    sourceToPackage (PSFiles lp _) =
      let pkg = lpPackage lp
          ident = PackageIdentifier (packageName pkg) (packageVersion pkg)
       in NonHackagePackage ident (lpLocation lp)

planAndGenerate :: HasEnvConfig env
                => Args
                -> BuildOptsCLI
                -> FilePath
                -> FilePath
                -> Maybe String
                -> Maybe String
                -> Maybe UTCTime
                -> Int
                -> IO ()
                -> RIO env ()
planAndGenerate args boptsCli baseDir outDir remoteUri argRev hSnapshot threads doAfter = do
  (_targets, _mbp, _locals, _extraToBuild, sourceMap) <- loadSourceMapFull NeedTargets boptsCli

  let pkgs = sourceMapToPackages sourceMap
  liftIO $ hPutStrLn stderr $ "plan:\n" ++ show pkgs

  hackageDB <- liftIO $ loadHackageDB Nothing hSnapshot
  void $ liftIO $ mapM_ (\pkg -> cabal2nix ("cabal://" ++ pkg) Nothing Nothing (Just outDir) hackageDB) $ words "hscolour stringbuilder"
  void $ liftIO $ mapPool threads (genNixFile args baseDir outDir remoteUri argRev hackageDB) pkgs
  liftIO doAfter

runPlan :: FilePath
        -> FilePath
        -> Maybe String
        -> LoadConfig
        -> Args
        -> IO ()
        -> IO ()
runPlan baseDir outDir remoteUri lc args@Args{..} doAfter = do
  let pkgsInConfig = nixPackages (configNix $ lcConfig lc)
  let pkgs = map unpack pkgsInConfig ++ ["ghc", "git"]
  let stackRoot = "/tmp/s2n"
  createDirectoryIfMissing True stackRoot
  globals <- queryNixPkgsPaths Include pkgs >>= \includes ->
             queryNixPkgsPaths Lib pkgs >>= \libs ->
             pure $ globalOpts baseDir stackRoot includes libs args
  logDebug args $ "stack global opts:\n" ++ show globals
  logDebug args $ "stack build opts:\n" ++ show buildOpts
  withBuildConfig globals $ planAndGenerate args buildOpts baseDir outDir remoteUri argRev argHackageSnapshot argThreads doAfter

{-
  TODO:
  - replace "ghc" in package list with value encoding compiler version
  - handle custom shell.nix  (see mshellFile in Stack.Nix.runShellAndExit)
  - remove baseDir arguments; due to withCurrentDirectory it should always be PWD.
-}

data NixPkgPath = Lib
                | Include

queryNixPkgsPaths :: NixPkgPath -> [String] -> IO (Set FilePath)
queryNixPkgsPaths kind pkgs = do
  (_, out, _) <- runCmd "nix-instantiate" [ "--eval"
                                          , "-E"
                                          , "with import <nixpkgs>{}; lib.concatMapStringsSep \" \" (pkg: ''" ++ query kind ++ "'') [" ++ unwords pkgs ++ "]"
                                          ]
                 >>= failHard
  pure . S.fromList . words . filter (/= '"') $ out
    where
      query Lib     = "${lib.getLib pkg}/lib"
      query Include = "${lib.getDev pkg}/include"

globalOpts :: FilePath -> FilePath -> Set FilePath -> Set FilePath -> Args -> GlobalOpts
globalOpts currentDir stackRoot extraIncludes extraLibs Args{..} =
  go { globalReExecVersion = Just "1.5.1" -- TODO: obtain from stack lib if exposed
     , globalConfigMonoid =
         (globalConfigMonoid go)
         { configMonoidExtraIncludeDirs = extraIncludes
         , configMonoidExtraLibDirs = extraLibs
         }
     , globalLogLevel = if argVerbose then LevelDebug else LevelInfo
     }
  where
    pinfo = info (globalOptsParser currentDir OuterGlobalOpts (Just LevelError)) briefDesc
    args = concat [ ["--work-dir", "./.s2n"]
                  , ["--stack-root", stackRoot]
                  , ["--jobs", show argThreads]
                  , ["--test" | argTest]
                  , ["--haddock" | argHaddock]
                  , ["--nix"]
                  ]
    go = globalOptsFromMonoid False . fromJust . getParseResult $ execParserPure defaultPrefs pinfo args

buildOpts :: BuildOptsCLI
buildOpts = fromJust . getParseResult $ execParserPure defaultPrefs (info (buildOptsParser Build) briefDesc) ["--dry-run"]
