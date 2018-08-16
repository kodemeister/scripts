#!/bin/bash

declare -r KERNEL_PARAMETERS="root=/dev/mapper/vg_root-lv_root rw initrd=/intel-ucode.img initrd=/initramfs-linux.img intel_iommu=on ipv6.disable=1"

efibootmgr -c -d /dev/sda -p 1 -L "Arch Linux" -l /vmlinuz-linux -u "$KERNEL_PARAMETERS"
efibootmgr -c -d /dev/sda -p 1 -L "Arch Linux (LVM Snapshots)" -l /vmlinuz-linux -u "$KERNEL_PARAMETERS systemd.unit=lvm-snapshots.target"
