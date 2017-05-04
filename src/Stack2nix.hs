{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Stack2nix
  ( Args(..)
  , Package(..)
  , RemotePkgConf(..)
  , StackConfig(..)
  , parseStackYaml
  , stack2nix
  ) where

import qualified Data.ByteString as BS
import Data.Foldable
import Data.List
import Data.Monoid
import Data.Text (Text, unpack)
import Data.Yaml (FromJSON(..), (.:), (.:?), (.!=))
import qualified Data.Yaml as Y
import System.Directory (getHomeDirectory)
import System.Exit
import System.FilePath
import System.FilePath.Glob
import System.Process

data Args = Args
  { argUri :: String
  }
  deriving (Show)

data StackConfig =
  StackConfig { resolver  :: Text
         , packages  :: [Package]
         , extraDeps :: [Text]
         }
  deriving (Show, Eq)

data Package = LocalPkg FilePath
             | RemotePkg RemotePkgConf
             deriving (Show, Eq)

data RemotePkgConf =
  RemotePkgConf { gitUrl :: Text
                , commit :: Text
                , extraDep :: Bool
                }
  deriving (Show, Eq)

instance FromJSON StackConfig where
  parseJSON (Y.Object v) =
    StackConfig <$>
    v .: "resolver" <*>
    v .: "packages" <*>
    v .: "extra-deps"
  parseJSON _ = fail "Expected Object for StackConfig value"

instance FromJSON Package where
  parseJSON (Y.String v) = return $ LocalPkg $ unpack v
  parseJSON obj@(Y.Object _) = RemotePkg <$> parseJSON obj
  parseJSON _ = fail "Expected String or Object for Package value"

instance FromJSON RemotePkgConf where
  parseJSON (Y.Object v) = do
    loc <- v .: "location"
    git <- loc .: "git"
    commit <- loc .: "commit"
    extra <- v .:? "extra-dep" .!= False
    return $ RemotePkgConf git commit extra
  parseJSON _ = fail "Expected Object for RemotePkgConf value"

parseStackYaml :: BS.ByteString -> Maybe StackConfig
parseStackYaml = Y.decode

{-
  Unused parts of sample input stack.yaml

  * resolver
  * extraDep and extraDeps
-}

-- TODO: Factor out pure parts.
stack2nix :: Args -> IO ()
stack2nix Args{..} = do
  -- TODO: Support URIs from version control systems
  yaml <- BS.readFile $ argUri </> "stack.yaml"
  case parseStackYaml yaml of
    Just config -> toNix argUri config
    Nothing -> error $ "Failed to parse " <> argUri

toNix :: FilePath -> StackConfig -> IO ()
toNix baseDir StackConfig{..} = do
  traverse_ genNixFile packages
  nixFiles <- glob "*.nix"
  writeFile "default.nix" $ defaultNix $ map overrideFor nixFiles
    where
      genNixFile :: Package -> IO ()
      genNixFile (LocalPkg relPath) = runCabal2nix dir Nothing Nothing
        where
          dir = if relPath == "." then baseDir else baseDir </> relPath
      genNixFile (RemotePkg RemotePkgConf{..}) = runCabal2nix (unpack gitUrl) (Just commit) Nothing

      overrideFor :: FilePath -> String
      overrideFor nixFile = "    " <> name <> " = super.callPackage " <> nixFile <> " { };"
        where
          name = dropExtension $ takeFileName nixFile

      defaultNix overrides = unlines $
        [ "{ pkgs ? (import <nixpkgs> {})"
        , ", compiler ? pkgs.haskell.packages.ghc802"
        , "}:"
        , ""
        , "with (import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs; });"
        , ""
        , "compiler.override {"
        , "  overrides = self: super: {"
        ] ++ overrides ++
        [ "  };"
        , "}"
        ]

runCabal2nix :: FilePath -> Maybe Text -> Maybe FilePath -> IO ()
runCabal2nix dir commit subpath = do
  result <- cabal2nix dir commit subpath
  case result of
    Just nix -> writeFile (pname nix <> ".nix") nix
    Nothing -> error $ "cabal2nix failed on directory: " <> dir
  where
    pname :: String -> String
    pname = pname' . lines

    pname' :: [String] -> String
    pname' [] = error "nix expression generated by cabal2nix is missing the 'pname' attr"
    pname' (x:xs) =
      case stripPrefix "  pname = \"" x of
        Just x' -> takeWhile (/= '"') x'
        Nothing -> pname' xs

-- TODO: Avoid calling process; use cabal2nix as lib. Would be simpler
-- after upstream changes.
cabal2nix :: FilePath -> Maybe Text -> Maybe FilePath -> IO (Maybe String)
cabal2nix dir commit subpath = do
  homeDir <- getHomeDirectory
  -- TODO: Don't assume cabal2nix >=2.2 binary is installed to ~/.local/bin.
  (exitCode, stdout, _stderr) <- readProcessWithExitCode (homeDir </> ".local/bin/cabal2nix") args ""
  case exitCode of
    ExitSuccess -> return $ Just stdout
    _ -> return Nothing
  where
    args :: [String]
    args = concat
      [ maybe [] (\c -> ["--revision", unpack c]) commit
      , maybe [] (\d -> ["--subpath", d]) subpath
      , [dir]
      ]