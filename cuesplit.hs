#!/usr/bin/env stack
-- stack --resolver nightly-2018-01-15 script

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (filterM)
import Data.Foldable (for_)

import Filesystem.Path (replaceExtension)
import Turtle

scanDirs :: [Turtle.FilePath] -> IO ()
scanDirs []   = scanDir "."
scanDirs dirs = for_ dirs scanDir

scanDir :: Turtle.FilePath -> IO ()
scanDir dir = do
    printf ("Scanning directory "%fp%"\n") dir
    cueFiles <- sort $ find (suffix ".cue") dir
    for_ cueFiles processCueFile

processCueFile :: Turtle.FilePath -> IO ()
processCueFile cueFile = do
    printf ("Found CUE file "%fp%"\n") cueFile
    audioFiles <- filterM testfile $ map (replaceExtension cueFile) ["ape", "flac", "wv"]
    case audioFiles of
        []            -> echo "Warning: audio file not found, skipping"
        (audioFile:_) -> splitAudioFile cueFile audioFile

splitAudioFile :: Turtle.FilePath -> Turtle.FilePath -> IO ()
splitAudioFile cueFile audioFile = do
    printf ("Found audio file "%fp%"\n") audioFile
    let splitDir = replaceExtension cueFile "tmp"
    mktree splitDir

    proc "shnsplit" ["-d", format fp splitDir, "-f", format fp cueFile, "-o", "flac flac -V --best -o %f -", "-t", "%n - %t", format fp audioFile] empty
        .||. die (format ("Error: failed to split audio file "%fp) audioFile)

    splitFiles <- sort $ find (invert (suffix "00 - pregap.flac")) splitDir
    proc "cuetag.sh" (map (format fp) (cueFile : splitFiles)) empty
        .||. die "Error: failed to tag split files"

    for_ splitFiles $ \file ->
        mv file (parent splitDir </> filename file)
    rmtree splitDir
    rm cueFile
    rm audioFile

    printf ("Successfully split audio file "%fp%"\n") audioFile

main :: IO ()
main = scanDirs =<< map fromText <$> arguments
