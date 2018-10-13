#!/usr/bin/env stack
-- stack --resolver lts-12.11 script

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (filterM)
import Data.Foldable (for_)

import Control.Foldl (list)
import Filesystem.Path (replaceExtension)
import Turtle

scanDirs :: [Turtle.FilePath] -> IO ()
scanDirs []   = scanDir "."
scanDirs dirs = for_ dirs scanDir

scanDir :: Turtle.FilePath -> IO ()
scanDir dir = do
    printf ("Scanning directory "%fp%"\n") dir
    cueSheets <- fold (find (suffix ".cue") dir) list
    for_ cueSheets processCueSheet

processCueSheet :: Turtle.FilePath -> IO ()
processCueSheet cueSheet = do
    printf ("Found cue sheet "%fp%"\n") cueSheet
    let extensions = ["ape", "flac", "wv"]
    audioFiles <- filterM testfile $ replaceExtension cueSheet <$> extensions
    case audioFiles of
        []            -> echo "Warning: audio file not found, skipping"
        (audioFile:_) -> splitAudioFile cueSheet audioFile

splitAudioFile :: Turtle.FilePath -> Turtle.FilePath -> IO ()
splitAudioFile cueSheet audioFile = do
    printf ("Found audio file "%fp%"\n") audioFile
    let splitDir = replaceExtension cueSheet "tmp"
    mktree splitDir

    proc "shnsplit" [ "-d", format fp splitDir
                    , "-f", format fp cueSheet
                    , "-o", "flac flac -V --best -o %f -"
                    , "-t", "%n - %t"
                    , format fp audioFile
                    ] empty
        .||. die (format ("Error: failed to split audio file "%fp) audioFile)

    proc "rm" ["-f", format fp (splitDir </> "00 - pregap.flac")] empty
    splitFiles <- sort $ ls splitDir
    proc "cuetag.sh" (format fp <$> (cueSheet : splitFiles)) empty
        .||. die "Error: failed to tag split files"

    for_ splitFiles $ \file ->
        mv file (parent splitDir </> filename file)
    rmtree splitDir
    rm cueSheet
    rm audioFile

    printf ("Successfully split audio file "%fp%"\n") audioFile

parser :: Parser [Turtle.FilePath]
parser = many $ argPath "dir" "A directory to recursively scan for cue sheets"

main :: IO ()
main = scanDirs =<< options "Split image+cue into separate flac tracks" parser
