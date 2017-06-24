#!/bin/bash

KERNEL_PARAMETERS="root=/dev/mapper/vg_root-lv_root rw initrd=/EFI/arch/intel-ucode.img initrd=/EFI/arch/initramfs-linux.img intel_iommu=on nvidia-drm.modeset=1 ipv6.disable=1"

efibootmgr -c -d /dev/sda -p 1 -L "Arch Linux" -l /EFI/arch/vmlinuz-linux -u "$KERNEL_PARAMETERS"
efibootmgr -c -d /dev/sda -p 1 -L "Arch Linux (LVM Snapshots)" -l /EFI/arch/vmlinuz-linux -u "$KERNEL_PARAMETERS systemd.unit=lvm-snapshots.target"
