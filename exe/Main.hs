module Main where

import Prelude.Extended

import System.Directory
import System.FilePath ((</>))
import System.Process
import qualified Data.Text as T

import qualified Fay.Builder as Builder

import qualified Paths_pickler as Paths


main :: IO ()
main = do
  pkgDb       <- getPackageDb
  print pkgDb
  packageDesc <- Paths.getDataFileName "pickler.cabal" >>= Builder.readPackageDescription
  Builder.build packageDesc pkgDb

getPackageDb :: IO (Maybe FilePath)
getPackageDb = readProcess "cabal-dev-sandbox-print" [] ""
           >>= return . T.unpack . T.strip . T.pack
           >>= (\e -> map (e </>) <$> getDirectoryContents e)
           >>= return . headMay . filter (\d -> ".conf" `isSuffixOf` d || ".conf.d" `isSuffixOf` d)
