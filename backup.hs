#!/usr/bin/env stack
-- stack --resolver lts-12.2 script

{-# LANGUAGE OverloadedStrings #-}

import System.Posix.User (getEffectiveUserID)
import Turtle

checkIfRoot :: IO ()
checkIfRoot = do
    userID <- getEffectiveUserID
    when (userID /= 0) $ die "Please run me as root"

mountBackupVolume :: Turtle.FilePath -> IO ()
mountBackupVolume mountPoint = do
    printf ("Mounting backup volume to "%fp%"\n") mountPoint
    execute "mount" [format fp mountPoint]
          $ format ("Failed to mount "%fp) mountPoint

unmountBackupVolume :: Turtle.FilePath -> IO ()
unmountBackupVolume mountPoint = do
    printf ("Unmounting backup volume from "%fp%"\n") mountPoint
    execute "umount" [format fp mountPoint]
          $ format ("Failed to unmount "%fp) mountPoint

mountLvmSnapshot :: Turtle.FilePath -> Turtle.FilePath -> IO ()
mountLvmSnapshot snapshot mountPoint = do
    printf ("Mounting LVM snapshot "%fp%" to "%fp%"\n") snapshot mountPoint
    mktree mountPoint
    execute "mount" ["-o", "ro", format fp snapshot, format fp mountPoint]
          $ format ("Failed to mount "%fp%" to "%fp) snapshot mountPoint

unmountLvmSnapshot :: Turtle.FilePath -> IO ()
unmountLvmSnapshot mountPoint = do
    printf ("Unmounting LVM snapshot from "%fp%"\n") mountPoint
    execute "umount" [format fp mountPoint]
          $ format ("Failed to unmount "%fp) mountPoint
    rmdir mountPoint

removeLvmSnapshot :: Turtle.FilePath -> IO ()
removeLvmSnapshot snapshot = do
    printf ("Removing LVM snapshot "%fp%"\n") snapshot
    execute "lvremove" ["-f", format fp snapshot]
          $ format ("Failed to remove "%fp) snapshot

backupEspPartition :: Turtle.FilePath -> Turtle.FilePath -> IO ()
backupEspPartition mountPoint backupDir = do
    printf ("Backing up ESP partition "%fp%" to "%fp%"\n") mountPoint backupDir
    proc "rm" ["-rf", format fp backupDir] empty
    mktree backupDir
    files <- sort (ls mountPoint)
    execute "cp" ("-rp" : (format fp <$> files) <> [format fp backupDir])
          $ format ("Failed to back up "%fp%" to "%fp) mountPoint backupDir

backupRootLvmSnapshot :: Turtle.FilePath -> Turtle.FilePath -> IO ()
backupRootLvmSnapshot snapshot backupFile = do
    printf ("Backing up LVM snapshot "%fp%" to "%fp%"\n") snapshot backupFile
    mktree (parent backupFile)
    execute "fsarchiver" [ "-o"
                         , "-z", "1"
                         , "-j", "4"
                         , "savefs"
                         , format fp backupFile
                         , format fp snapshot
                         ]
          $ format ("Failed to back up "%fp%" to "%fp) snapshot backupFile

backupStorageLvmSnapshot :: Turtle.FilePath -> Turtle.FilePath -> IO ()
backupStorageLvmSnapshot mountPoint backupDir = do
    printf ("Backing up LVM snapshot "%fp%" to "%fp%"\n") mountPoint backupDir
    mktree backupDir
    execute "rsync" [ "-aAX"
                    , "--delete"
                    , "--info=progress2,stats2"
                    , "--exclude=/lost+found"
                    , format fp mountPoint <> "/"
                    , format fp backupDir
                    ]
          $ format ("Failed to back up "%fp%" to "%fp) mountPoint backupDir

showSuccessMessage :: IO ()
showSuccessMessage = echo "Yay! Backup has been successfully completed!"

execute :: Text -> [Text] -> Text -> IO ()
execute command arguments errorText = do
    proc command arguments empty .||. die errorText
    pure ()

main :: IO ()
main = do
    let backupMountPoint  = "/media/backup"

        espMountPoint     = "/media/esp"
        espBackupDir      = backupMountPoint </> "esp"

        rootSnapshot      = "/dev/vg_root/snap_root"
        rootBackupFile    = backupMountPoint </> "root/root.fsa"

        storageSnapshot   = "/dev/vg_storage/snap_storage"
        storageMountPoint = "/mnt/snap_storage"
        storageBackupDir  = backupMountPoint </> "storage"

    checkIfRoot
    mountBackupVolume backupMountPoint
    mountLvmSnapshot storageSnapshot storageMountPoint
    backupEspPartition espMountPoint espBackupDir
    backupRootLvmSnapshot rootSnapshot rootBackupFile
    backupStorageLvmSnapshot storageMountPoint storageBackupDir
    unmountLvmSnapshot storageMountPoint
    unmountBackupVolume backupMountPoint
    removeLvmSnapshot rootSnapshot
    removeLvmSnapshot storageSnapshot
    showSuccessMessage
