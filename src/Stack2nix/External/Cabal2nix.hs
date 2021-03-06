{-# LANGUAGE RecordWildCards   #-}

module Stack2nix.External.Cabal2nix (
  cabal2nix
  ) where

import           Cabal2nix                   (cabal2nixWithDB)
import           Data.List                   (stripPrefix, takeWhile)
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text, unpack)
import           Distribution.System         (Platform(..), Arch(..), OS(..))
import           Language.Nix.PrettyPrinting (pPrint)
import           System.FilePath             ((</>))
import           System.IO                   (hPutStrLn, stderr)
import           Stack2nix.Types             (Args (..))
import qualified Distribution.Nixpkgs.Haskell.Hackage as DB

cabal2nix :: Args -> FilePath -> Maybe Text -> Maybe FilePath -> Maybe FilePath -> DB.HackageDB -> IO ()
cabal2nix Args{..} uri commit subpath outDir hackageDB = do
  let runCmdArgs = args $ fromMaybe "." subpath
  hPutStrLn stderr $ unwords ("+ cabal2nix":runCmdArgs)
  result <- cabal2nixWithDB hackageDB runCmdArgs
  case result of
    Right deriv ->
      let out = show $ pPrint deriv
          basename = pname out <> ".nix"
          fname = maybe basename (</> basename) outDir
      in
      writeFile fname out
    Left err ->
      hPutStrLn stderr $ unwords [ "ERROR: cabal2nix failed on"
                                 , uri
                                 , "(rev: " ++ show commit ++ ";"
                                 , "subpath: " ++ show subpath ++ "):"
                                 , "\n" ++ show err
                                 ]
  where
    args :: FilePath -> [String]
    args dir = concat
      [ maybe [] (\c -> ["--revision", unpack c]) commit
      , ["--subpath", dir]
      , ["--system", fromCabalPlatform argPlatform]
      , ["--no-check", "--no-haddock"]
      , [uri]
      ]

    pname :: String -> String
    pname = pname' . lines

    pname' :: [String] -> String
    pname' [] = error "nix expression generated by cabal2nix is missing the 'pname' attr"
    pname' (x:xs) =
      case stripPrefix "  pname = \"" x of
        Just x' -> takeWhile (/= '"') x'
        Nothing -> pname' xs

-- | Copied (and modified) from src/Distribution/Nixpkgs/Meta.hs
fromCabalPlatform :: Platform -> String
fromCabalPlatform (Platform I386 Linux)   = "i686-linux"
fromCabalPlatform (Platform X86_64 Linux) = "x86_64-linux"
fromCabalPlatform (Platform X86_64 OSX)   = "x86_64-darwin"
fromCabalPlatform p                       = error ("fromCabalPlatform: invalid Nix platform" ++ show p)
