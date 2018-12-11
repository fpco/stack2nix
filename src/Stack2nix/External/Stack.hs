{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Stack2nix.External.Stack
  ( PackageRef(..), runPlan
  ) where

import           Control.Lens                                   (_Right, (<>~))
import           Data.List                                      (concat)
import qualified Data.Map.Strict                                as M
import           Data.Maybe                                     (fromJust)
import qualified Data.Set                                       as Set (fromList)
import           Data.Text                                      (pack, unpack)
import           Distribution.Nixpkgs.Haskell.Derivation        (Derivation,
                                                                 configureFlags)
import qualified Distribution.Nixpkgs.Haskell.Hackage           as DB
import qualified Distribution.Text                              as CT
import           Distribution.Types.PackageId
import           Options.Applicative
import           Pantry
import           Pantry.SHA256                                  (toHexText)
import           Path                                           (parseAbsFile)
import           Stack.Build.Target                             (NeedTargets (..))
import           Stack.Config
import           Stack.Options.GlobalParser
import           Stack.Options.Utils                            (GlobalOptsContext (..))
import           Stack.Prelude                                  hiding
                                                                 (logDebug)
import           Stack.Runners                                  (loadCompilerVersion,
                                                                 withBuildConfig)
import           Stack.Types.Compiler                           (getGhcVersion, wantedToActual)
import           Stack.Types.Config
import           Stack.Types.Nix
import           Stack.Types.SourceMap
import           Stack.Types.Runner
import           Stack.Types.Version                            (Version)
import           Stack2nix.External.Cabal2nix                   (cabal2nix)
import           Stack2nix.Hackage                              (loadHackageDB)
import           Stack2nix.Render                               (render)
import           Stack2nix.Types                                (Args (..), Flags)
import           Stack2nix.Util                                 (ensureExecutable,
                                                                 mapPool)
import           System.Directory                               (canonicalizePath,
                                                                 createDirectoryIfMissing,
                                                                 getCurrentDirectory,
                                                                 makeRelativeToCurrentDirectory
                                                                )
import           System.FilePath                                (makeRelative,
                                                                 (</>))
import           Text.PrettyPrint.HughesPJClass                 (Doc)

data PackageRef
  = HackagePackage Flags PackageIdentifierRevision
  | NonHackagePackage Flags PackageIdentifier FilePath
  deriving (Eq, Show)

genNixFile :: Args -> Version -> FilePath -> Maybe String -> Maybe String -> DB.HackageDB -> PackageLocation -> Flags -> IO (Either Doc Derivation)
genNixFile args ghcVersion baseDir uri argRev hackageDB pl flags = do
  cwd <- getCurrentDirectory
  case pl of
    PLMutable dir -> do
      let path = toFilePath $ resolvedAbsolute dir
      relPath <- makeRelativeToCurrentDirectory path
      projRoot <- canonicalizePath $ cwd </> baseDir
      let defDir = baseDir </> makeRelative projRoot path
      cabal2nix args ghcVersion (fromMaybe defDir uri) (pack <$> argRev) (const relPath <$> uri) flags hackageDB
    PLImmutable (PLIHackage pir _) ->
      cabal2nix args ghcVersion ("cabal://" <> packageIdentifierRevisionString pir) Nothing Nothing flags hackageDB
    PLImmutable (PLIRepo repo _pm) ->
      cabal2nix args ghcVersion (unpack $ repoUrl repo) (Just $ repoCommit repo) (Just (unpack $ repoSubdir repo)) flags hackageDB
    PLImmutable (PLIArchive _ _pm) ->
      error "genNixFile: No support for archive package locations"

packageIdentifierRevisionString :: PackageIdentifierRevision -> String
packageIdentifierRevisionString (PackageIdentifierRevision n v cfi) =
  concat $ packageIdentifierString (PackageIdentifier n v) : rest
  where
    rest =
      case cfi of
        CFILatest -> []
        CFIHash hash' msize ->
            "@sha256:"
          : unpack (toHexText hash')
          : showSize msize
        CFIRevision rev -> ["@rev:", show rev]

    showSize Nothing = []
    showSize (Just int) = [',' : show int]

planAndGenerate
  :: HasEnvConfig env
  => FilePath
  -> Maybe String
  -> Args
  -> Version
  -> RIO env ()
planAndGenerate baseDir remoteUri args@Args {..} ghcVersion = do
  sourceMap <- view $ envConfigL.to envConfigSourceMap
  let deps = smDeps sourceMap
      project = smProject sourceMap
      inputs =
        [ (dpLocation, M.toList (cpFlags dpCommon), cpGhcOptions dpCommon)
        | DepPackage{..} <- M.elems deps]
        ++
        [ (PLMutable ppResolvedDir, M.toList (cpFlags ppCommon), cpGhcOptions ppCommon)
        | ProjectPackage{..} <- M.elems project ]

  hackageDB <- liftIO $ loadHackageDB Nothing argHackageSnapshot
  drvs      <- liftIO $ mapPool
    argThreads
    (\(pl, flags, ghcOptions) -> do
      drv <- genNixFile args ghcVersion baseDir remoteUri argRev hackageDB pl flags
      return $ drv & _Right . configureFlags <>~ Set.fromList (map unpack ghcOptions)
    )
    inputs
  let locals = map packageNameString $ M.keys project
  liftIO . render drvs args locals $ nixVersion ghcVersion

runPlan :: FilePath
        -> Maybe String
        -> Args
        -> IO ()
runPlan baseDir remoteUri args@Args{..} = do
  let stackRoot = "/tmp/s2n"
  createDirectoryIfMissing True stackRoot
  globals <- globalOpts baseDir stackRoot args
  let stackFile = baseDir </> argStackYaml

  ghcVersion <- getGhcVersionIO globals stackFile
  ensureExecutable ("haskell.compiler.ghc" ++ nixVersion ghcVersion)
  withBuildConfig globals NeedTargets buildOptsCli $
    planAndGenerate baseDir remoteUri args ghcVersion

nixVersion :: Version -> String
nixVersion =
  filter (/= '.') . CT.display

getGhcVersionIO :: GlobalOpts -> FilePath -> IO Version
getGhcVersionIO go stackFile = do
  cp <- canonicalizePath stackFile
  fp <- parseAbsFile cp
  lc <- withRunner LevelError True False ColorAuto mempty Nothing False $ \runner ->
    -- https://www.fpcomplete.com/blog/2017/07/the-rio-monad
    runRIO runner $ loadConfig mempty Nothing (SYLOverride fp) return
  getGhcVersion . wantedToActual <$> loadCompilerVersion go lc

globalOpts :: MonadIO m => FilePath -> FilePath -> Args -> m GlobalOpts
globalOpts currentDir stackRoot Args{..} = do
  go <- globalOptsFromMonoid False . fromJust . getParseResult $
      execParserPure defaultPrefs pinfo args
  return $ go { globalReExecVersion = Just "1.5.1" -- TODO: obtain from stack lib if exposed
     , globalConfigMonoid =
         (globalConfigMonoid go)
         { configMonoidNixOpts = mempty
           { nixMonoidEnable = First (Just True)
           }
         }
     , globalStackYaml = SYLOverride (currentDir </> argStackYaml)
     , globalLogLevel = if argVerbose then LevelDebug else LevelInfo
     }
  where
    pinfo = info (globalOptsParser currentDir OuterGlobalOpts (Just LevelError)) briefDesc
    args = concat [ ["--stack-root", stackRoot]
                  , ["--jobs", show argThreads]
                  , ["--test" | argTest]
                  , ["--bench" | argBench]
                  , ["--haddock" | argHaddock]
                  , ["--no-install-ghc"]
                  ]

buildOptsCli :: BuildOptsCLI
buildOptsCli = defaultBuildOptsCLI { boptsCLIDryrun = False }
