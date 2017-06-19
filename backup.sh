#!/bin/bash

BACKUP_MOUNT_POINT="/media/backup"

ESP_MOUNT_POINT="/media/esp"
ESP_BACKUP_DIR="$BACKUP_MOUNT_POINT/esp"

ROOT_SNAPSHOT="/dev/vg_root/snap_root"
ROOT_BACKUP_FILE="$BACKUP_MOUNT_POINT/root/root.fsa"

STORAGE_SNAPSHOT="/dev/vg_storage/snap_storage"
STORAGE_MOUNT_POINT="/mnt/snap_storage"
STORAGE_BACKUP_DIR="$BACKUP_MOUNT_POINT/storage"

check_if_root()
{
	if [ "$EUID" -ne "0" ]; then
		echo "Please run me as root"
		exit 1
	fi
}

mount_backup_volume()
{
	echo "Mounting backup volume to $1..."

	if ! mount "$1"; then
		echo "ERROR: Failed to mount backup volume to $1"
		exit 1
	fi
}

unmount_backup_volume()
{
	echo "Unmounting backup volume from $1..."

	if ! umount "$1"; then
		echo "ERROR: Failed to unmount backup volume from $1"
		exit 1
	fi
}

mount_lvm_snapshot()
{
	echo "Mounting LVM snapshot $1 to $2..."

	mkdir -p "$2"

	if ! mount -o ro "$1" "$2"; then
		echo "ERROR: Failed to mount LVM snapshot $1 to $2"
		exit 1
	fi
}

unmount_lvm_snapshot()
{
	echo "Unmounting LVM snapshot from $1..."

	if ! umount "$1"; then
		echo "ERROR: Failed to unmount LVM snapshot from $1"
		exit 1
	fi

	rmdir "$1"
}

remove_lvm_snapshot()
{
	echo "Removing LVM snapshot $1..."

	if ! lvremove -f "$1"; then
		echo "ERROR: Failed to remove LVM snapshot $1..."
		exit 1
	fi
}

backup_esp_partition()
{
	echo "Backing up ESP partition $1 to $2..."

	rm -rf "$2"
	mkdir -p "$2"

	if ! cp -rp "$1"/* "$2"; then
		echo "ERROR: Failed to back up ESP partition $1 to $2"
		exit 1
	fi
}

backup_root_lvm_snapshot()
{
	echo "Backing up root LVM snapshot $1 to $2..."

	mkdir -p "$(dirname "$2")"

	if ! fsarchiver -o -z 1 -j 4 savefs "$2" "$1"; then
		echo "ERROR: Failed to back up root LVM snapshot $1 to $2"
		exit 1
	fi
}

backup_storage_lvm_snapshot()
{
	echo "Backing up storage LVM snapshot $1 to $2..."

	mkdir -p "$2"

	if ! rsync -aAX --delete --info=progress2,stats2 --exclude="/lost+found" "$1/" "$2"; then
		echo "ERROR: Failed to back up storage LVM snapshot $1 to $2"
		exit 1
	fi
}

show_success_message()
{
	echo "Yay! Backup has been successfully completed!"
}

main()
{
	check_if_root
	mount_backup_volume "$BACKUP_MOUNT_POINT"
	mount_lvm_snapshot "$STORAGE_SNAPSHOT" "$STORAGE_MOUNT_POINT"
	backup_esp_partition "$ESP_MOUNT_POINT" "$ESP_BACKUP_DIR"
	backup_root_lvm_snapshot "$ROOT_SNAPSHOT" "$ROOT_BACKUP_FILE"
	backup_storage_lvm_snapshot "$STORAGE_MOUNT_POINT" "$STORAGE_BACKUP_DIR"
	unmount_lvm_snapshot "$STORAGE_MOUNT_POINT"
	unmount_backup_volume "$BACKUP_MOUNT_POINT"
	remove_lvm_snapshot "$ROOT_SNAPSHOT"
	remove_lvm_snapshot "$STORAGE_SNAPSHOT"
	show_success_message
}

main
