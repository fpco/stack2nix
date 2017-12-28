module Stack2nix.Util
  ( assertMinVer
  , extractVersion
  , logDebug
  ) where

import           Control.Exception            (onException)
import           Control.Monad                (unless)
import           Data.Maybe                   (listToMaybe)
import           Data.Version                 (Version (..), parseVersion,
                                               showVersion)
import           GHC.Exts                     (sortWith)
import           Stack2nix.External.Util      (runCmd)
import           Stack2nix.Types              (Args, argVerbose)
import           System.Exit                  (ExitCode (..))
import           System.IO                    (hPutStrLn, stderr)
import           Text.ParserCombinators.ReadP (readP_to_S)
import           Text.Regex.PCRE              (AllTextMatches (..),
                                               getAllTextMatches, (=~))

-- heuristic for parsing version from stdout
extractVersion :: String -> Maybe Version
extractVersion str = ver
  where
    firstLine = head . lines $ str
    candidateVers = getAllTextMatches (firstLine =~ "[\\d\\.]+" :: AllTextMatches [] String)
    bestMatch = head . reverse . sortWith length $ candidateVers
    ver = fmap fst . listToMaybe . reverse . readP_to_S parseVersion $ bestMatch

assertMinVer :: String -> String -> IO ()
assertMinVer prog minVer = do
  hPutStrLn stderr $ unwords ["Ensuring", prog, "version is >=", minVer, "..."]
  result <- runCmd prog ["--version"] `onException` error ("Failed to run " ++ prog ++ ". Not found in PATH.")
  case result of
    (ExitSuccess, out, _) ->
      let ver = extractVersion out in
        unless (ver >= extractVersion minVer) $ error $ unwords ["ERROR:", prog, "version must be", minVer, "or higher. Current version:", maybe "[parse failure]" showVersion ver]
    (ExitFailure _, _, err)  -> error err

logDebug :: Args -> String -> IO ()
logDebug args msg
  | argVerbose args = hPutStrLn stderr msg
  | otherwise = return ()
