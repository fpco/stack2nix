{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Stack2nix.External.Stack
  ( runPlan
  ) where

import           Data.List                     (intercalate)
import qualified Data.HashMap.Strict           as HashMap
import qualified Data.Map.Strict               as M
import           Data.Maybe                    (fromJust)
import qualified Data.Set                      as S
import           Data.Text                     (unpack)
import           Options.Applicative
import           Stack.Build.Source            (loadSourceMapFull)
import           Stack.Build.Target            (NeedTargets (..))
import           Stack.Fetch                   (withCabalLoader)
import           Stack.Options.BuildParser
import           Stack.Options.GlobalParser
import           Stack.Options.Utils           (GlobalOptsContext (..))
import           Stack.Package                 (PackageConfig (..), readPackageUnresolvedBS,
                                                resolvePackage)
import           Stack.PackageIndex            (getPackageCaches)
import           Stack.Prelude                 hiding (mapConcurrently, logDebug)
import           Stack.Runners                 (withBuildConfig)
import           Stack.Types.BuildPlan         (Repo (..), PackageLocation (..), PackageLocationIndex (..))
import           Stack.Types.Config
import           Stack.Types.Config.Build      (BuildCommand (..))
import           Stack.Types.FlagName          (FlagName, flagNameString)
import           Stack.Types.Nix
import           Stack.Types.Package           (PackageSource (..), lpPackage,
                                                packageVersion, lpLocation,
                                                packageFlags, packageDeps,
                                                packageGhcOptions)
import           Stack.Types.PackageName       (PackageName, packageNameString, parsePackageName)
import           Stack.Types.PackageIdentifier (PackageIdentifier (..),
                                                PackageIdentifierRevision (..),
                                                StaticSHA256,
                                                staticSHA256ToText,
                                                CabalFileInfo (..),
                                                showCabalHash)
import           Stack.Types.PackageIndex      (PackageCache (PackageCache), pdSHA256)
import           Stack.Types.Version           (Version, versionString)
import           Stack2nix.External.Util       (failHard, runCmd)
import           Stack2nix.Types               (Args (..))
import           Stack2nix.Util                (logDebug)
import           System.Directory              (createDirectoryIfMissing)

planAndGenerate :: HasEnvConfig env
                => BuildOptsCLI
                -> (String -> IO ())
                -> RIO env ()
planAndGenerate boptsCli doAfter = do
    (_targets, _mbp, _locals, _extraToBuild, sourceMap') <- loadSourceMapFull NeedTargets boptsCli

    if' <- parsePackageName "if"
    let sourceMap = M.delete if' sourceMap'

    res <- fmap concat $ forM (M.toList sourceMap) $ \(name, pkgSrc) -> withCabalLoader $ \loadFromIndex -> do
      (package, source) <-
        case pkgSrc of
          PSFiles lp _ -> do
            let source =
                  case lpLocation lp of
                    PLRepo repo -> SourceGit (repoUrl repo) (repoCommit repo) (repoSubdirs repo)
                    PLFilePath fp -> SourceFilePath fp
                    PLArchive _ -> error "Archives are not supported"
            return (lpPackage lp, source)
          PSIndex _ flags options pir -> do
            bs <- liftIO $ loadFromIndex pir
            (_warnings, gpd) <- readPackageUnresolvedBS (PLIndex pir) bs
            compiler <- view actualCompilerVersionL
            platform <- view platformL
            (sha256, revNum) <- lookupRevision pir
            let packageConfig = PackageConfig
                  { packageConfigEnableTests = False
                  , packageConfigEnableBenchmarks = False
                  , packageConfigFlags = flags
                  , packageConfigGhcOptions = options
                  , packageConfigCompilerVersion = compiler
                  , packageConfigPlatform = platform
                  }
                package = resolvePackage packageConfig gpd
            return (package, SourceHackage (staticSHA256ToText <$> sha256) revNum)
      return $ displayDecl Decl
        { declName = name
        , declDeps = S.fromList $ map packageNameString $ M.keys $ packageDeps package
        , declVersion = packageVersion package
        , declSource = source
        , declFlags = packageFlags package
        , declOptions = packageGhcOptions package
        }

    liftIO $ doAfter res

lookupRevision :: HasConfig env => PackageIdentifierRevision -> RIO env (Maybe StaticSHA256, Int)
lookupRevision pir@(PackageIdentifierRevision (PackageIdentifier name version) revision) = do
  let ensure Nothing = error $ "Could not look up revision info for: " ++ show pir
      ensure (Just x) = return x
  PackageCache m1 <- getPackageCaches
  m2 <- ensure $ HashMap.lookup name m1
  (_index, mpd, revisions) <- ensure $ HashMap.lookup version m2
  revNum <-
    case revision of
      CFILatest -> return $ length (toList revisions) - 1
      CFIHash _ cabalHash ->
        let loop _ [] = error $ "Could not find cabal hash: " ++ unpack (showCabalHash cabalHash)
            loop !idx ((hashes, _):xs) =
              if cabalHash `elem` hashes
                then return idx
                else loop (idx + 1) xs
         in loop 0 (toList revisions)
      CFIRevision w -> return $ fromIntegral w
  return (pdSHA256 <$> mpd, revNum)

data Decl = Decl
  { declName :: !PackageName
  , declDeps :: !(S.Set String)
  , declVersion :: !Version
  , declSource :: !Source
  , declFlags :: !(Map FlagName Bool)
  , declOptions :: ![Text]
  }

data Source
  = SourceHackage
      !(Maybe Text) -- sha256 of tarball, Maybe due to hackage-security bug
      !Int -- revision number
  | SourceGit
      !Text -- URL
      !Text -- commit
      !FilePath -- subdir
  | SourceFilePath !FilePath

displayDecl :: Decl -> String
displayDecl Decl {..} = unlines $ map ("  " ++) $ lines $ concat
  [ packageNameString declName
  , " = callPackage ({"
  , intercalate ", "
      $ S.toList
      $ S.fromList (words "fetchgit mkDerivation stdenv") <> declDeps
  , " }:\nmkDerivation {\n  pname = \""
  , packageNameString declName
  , "\";\n  version = \""
  , versionString declVersion
  , "\";\n"
  , "  license = stdenv.lib.licenses.bsd3;\n" -- FIXME get the real info
  , case declSource of
      SourceHackage msha256 rev -> concat
        [ case msha256 of
            Nothing -> ""
            Just sha256 -> concat
              [ "  sha256 = "
              , show sha256
              , ";\n"
              ]
        , "  revision = \""
        , show rev
        , "\";\n"
        ]
      SourceGit url commit _subdir -> concat -- FIXME use subdir
        [ "  src = fetchgit {\n    url = "
        , show url
        , ";\n    rev = "
        , show commit
        , ";\n  };\n"
        ]
      SourceFilePath fp -> concat
        [ "  src = ./"
        , fp
        , ";\n"
        ]
  , "  configureFlags = [\""
  , concatMap (\(name, active) -> concat
      [ "-f"
      , if active then "" else "-"
      , flagNameString name
      ]) (M.toList declFlags) -- FIXME declOptions
  , "\"];\n"
  , "  libraryHaskellDepends = [\n"
  , concatMap (\p -> "    " ++ p ++ "\n") (S.toList declDeps)
  , "  ];\n  executableHaskellDepends = [\n"
  , concatMap (\p -> "    " ++ p ++ "\n") (S.toList declDeps)
  , "  ];\n  doHaddock = false;\n  doCheck = false;\n}) {};\n"
  ]

runPlan :: FilePath
        -> LoadConfig
        -> Args
        -> (String -> IO ())
        -> IO ()
runPlan baseDir lc args@Args{..} doAfter = do
  let pkgsInConfig = nixPackages (configNix $ lcConfig lc)
  let pkgs = map unpack pkgsInConfig ++ ["ghc", "git"]
  let stackRoot = "/tmp/s2n"
  createDirectoryIfMissing True stackRoot
  globals <- queryNixPkgsPaths Include pkgs >>= \includes ->
             queryNixPkgsPaths Lib pkgs >>= \libs ->
             pure $ globalOpts baseDir stackRoot includes libs args
  logDebug args $ "stack global opts:\n" ++ show globals
  logDebug args $ "stack build opts:\n" ++ show buildOpts
  withBuildConfig globals $ planAndGenerate buildOpts doAfter

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
         , configMonoidNixOpts = mempty
             { nixMonoidEnable = First (Just True)
             }
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
