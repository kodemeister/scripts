{-# LANGUAGE OverloadedStrings #-}

import Control.Foldl (list)
import System.Posix.User (getEffectiveUserID)
import Turtle

checkIfRoot :: IO ()
checkIfRoot = do
    userID <- getEffectiveUserID
    when (userID /= 0) $ die "Please run me as root"

mountBackupVolume :: Turtle.FilePath -> Turtle.FilePath -> IO ()
mountBackupVolume volume mountPoint = do
    printf ("Mounting backup volume "%fp%" to "%fp%"\n") volume mountPoint
    mktree mountPoint
    execute "mount" [format fp volume, format fp mountPoint]
          $ format ("Failed to mount "%fp%" to "%fp) volume mountPoint

unmountBackupVolume :: Turtle.FilePath -> IO ()
unmountBackupVolume mountPoint = do
    printf ("Unmounting backup volume from "%fp%"\n") mountPoint
    execute "umount" [format fp mountPoint]
          $ format ("Failed to unmount "%fp) mountPoint
    rmdir mountPoint

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

backupBootPartition :: Turtle.FilePath -> Turtle.FilePath -> IO ()
backupBootPartition mountPoint backupDir = do
    printf ("Backing up boot partition "%fp%" to "%fp%"\n") mountPoint backupDir
    proc "rm" ["-rf", format fp backupDir] empty
    mktree backupDir
    files <- fold (ls mountPoint) list
    execute "cp" ("-rp" : (format fp <$> files) <> [format fp backupDir])
          $ format ("Failed to back up "%fp%" to "%fp) mountPoint backupDir

backupRootLvmSnapshot :: Turtle.FilePath -> Turtle.FilePath -> IO ()
backupRootLvmSnapshot snapshot backupFile = do
    printf ("Backing up LVM snapshot "%fp%" to "%fp%"\n") snapshot backupFile
    mktree (parent backupFile)
    execute "fsarchiver" [ "-o"
                         , "-Z", "1"
                         , "-j", "4"
                         , "savefs"
                         , format fp backupFile
                         , format fp snapshot
                         ]
          $ format ("Failed to back up "%fp%" to "%fp) snapshot backupFile

backupHomeLvmSnapshot :: Turtle.FilePath -> Turtle.FilePath -> IO ()
backupHomeLvmSnapshot mountPoint backupDir = do
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
    let backupVolume     = "/dev/vg_backup/lv_backup"
        backupMountPoint = "/mnt/backup"

        bootMountPoint   = "/boot"
        bootBackupDir    = backupMountPoint </> "boot"

        rootSnapshot     = "/dev/vg_root/snap_root"
        rootBackupFile   = backupMountPoint </> "root.fsa"

        homeSnapshot     = "/dev/vg_home/snap_home"
        homeMountPoint   = "/mnt/snap_home"
        homeBackupDir    = backupMountPoint </> "home"

    checkIfRoot
    mountBackupVolume backupVolume backupMountPoint
    mountLvmSnapshot homeSnapshot homeMountPoint
    backupBootPartition bootMountPoint bootBackupDir
    backupRootLvmSnapshot rootSnapshot rootBackupFile
    backupHomeLvmSnapshot homeMountPoint homeBackupDir
    unmountLvmSnapshot homeMountPoint
    unmountBackupVolume backupMountPoint
    removeLvmSnapshot rootSnapshot
    removeLvmSnapshot homeSnapshot
    showSuccessMessage
