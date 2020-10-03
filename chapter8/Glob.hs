module Glob (namesMatching) where

import System.Directory (soesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getDirectoryContent)
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))
import Control.OldExeption (handle)
import Control.Monad (forM)
import GlobRegex (matchesGlob)

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")
namesMatching pat
    | not (isPattern pat) = do
     exists <- doesNameExist pat
     return (if exists then [pat] else [])
    | otherwise = do
        case splitFileName pat of
            ("", baseName) -> do
                curDir <- getCurrentDirectory
                listMatches curDir baseName
            (dirName, baseName) -> do
                dirs <- if isPattern dirName
                        then namesMatching (dropTrailingPathSeparator dirName)
                        else return [dirName]
                let listDir = if isPattern baseName
                              then listMatches
                              else return [dirName]
                pathNames <- forM dirs $ \dir -> do
                    baseNames <- listDir dir baseName
                    return (map (dir </>) baseName)
                return (concat pathNames)
