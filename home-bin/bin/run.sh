#!/bin/bash

# run.sh: run a virtual machine
# usage: run.sh [--init] disk-img [iso-img]

set -eo pipefail

REQUIRE=(
    "qemu"
    "qemu-system-x86_64"
)

PAS=$'[ \033[32;1mPASS\033[0m ] '
ERR=$'[ \033[31;1mFAIL\033[0m ] '
WAR=$'[ \033[33;1mWARN\033[0m ] '
INF="[ INFO ] "

# -----------------------------------------------------------------------------
# Utilities
# -----------------------------------------------------------------------------

_err()
{ # All errors go to stderr.
    printf "[%s]: %s\n" "$(date +%s.%3N)" "$1"
}

_msg()
{ # Default message to stdout.
    printf "[%s]: %s\n" "$(date +%s.%3N)" "$1"
}

_debug()
{
    if [ "${DEBUG}" = '1' ]; then
        printf "[%s]: %s\n" "$(date +%s.%3N)" "$1"
    fi
}



die() {
    # **
    # Prints a message to stderr & exits script with non-successful code "1"
    # *

    _err "${ERR}$*"
    exit 1
}

chk_require()
{ # Check that every required command is available.
    declare -a warn
    local c

    _debug "--- [ ${FUNCNAME[0]} ] ---"

    for c in "$@"; do
        command -v "$c" &>/dev/null || warn+=("$c")
    done

    [ "${#warn}" -ne 0 ] && die "Missing commands: ${warn[*]}."

    _msg "${PAS}verification of required commands completed"
}



# -----------------------------------------------------------------------------
# Main
# -----------------------------------------------------------------------------

create_disk_img () {
    local disk_img="$1"
    qemu-img create -f qcow2 "$disk_img" 16G || die "Error creating disk img"
}


main_init () {
    _msg "Starting initialization process ($(date))"

    chk_require "${REQUIRE[@]}"

}

main_run () {
    local init_flag=NULL
    local disk_img=NULL
    local install_iso=NULL

    # parse args
    if [ "--init" = "$1" ]; then
        init_flag="$1"
        disk_img=./data/"$2"
        install_iso="$3"
    else
        disk_img=./data/"$1"
    fi


    # Basic virtual machine properties: a recent i440fx machine type, KVM acceleration,
    # 2048 MB RAM, two VCPUs
    OPTS="-machine pc-i440fx-8.2 -enable-kvm -m 4096 -smp 8"
    
    # bios / firmware (UEFI emulation)
    OPTS="$OPTS -bios $(guix build ovmf)/share/firmware/ovmf_x64.bin"
    
    # The hard disk is exposed to the guest as a virtio-block device. OVMF has a
    # driver stack that supports such a disk. We specify this disk as first boot
    # option. OVMF recognizes the boot order specification
    OPTS="$OPTS -drive id=disk0,if=none,format=qcow2,file=${disk_img}"
    OPTS="$OPTS -device virtio-blk-pci,drive=disk0,bootindex=0"

    
    if [[ "--init" = "$init_flag" ]]; then
        if ! [[ -f "$disk_img" ]]; then
            create_disk_img "$disk_img"
        fi
        # The installed disk appears as an IDE CD-ROM in the guest. This is the
        # second boot option
        OPTS="$OPTS -drive id=cd0,if=none,format=raw,readonly=on"
        OPTS="$OPTS,file=${install_iso}"
        OPTS="$OPTS -device ide-cd,bus=ide.1,drive=cd0,bootindex=1"
    fi


    # QEMU accepts various commands and queries from the user on the monitor
    # interface. Connect the monitor with the qemu process's standard input
    # and output.
    OPTS="$OPTS -monitor stdio"
    
    # A USB tablet device in the guest allows for accurate pointer tracking
    # between the host and the guest.
    OPTS="$OPTS -device piix3-usb-uhci -device usb-tablet"
    
    # Provide the guest with a virtual network card (virtio-net).
    #
    # Normally, qemu provides the guest with UEFI-conformant network driver
    # from the iPXE project, in the form of a PCI expansion ROM. For this test,
    # we disable the expansion ROM and allow OVMF's built-in virtio-net driver to
    # take effect.
    #
    # On the host side, we use the SLIRP ("user") network backend, which has
    # relatively low performance, but it doesn't require extra privileges from
    # the user executing qemu.
    OPTS="$OPTS -netdev id=net0,type=user"
    OPTS="$OPTS -device virtio-net-pci,netdev=net0,romfile="

    # set up port fowarding to host so we can use ssh
    OPTS="$OPTS -nic user,hostfwd=tcp::10022-:22"

    # A Spice QXL GPU is recommended as the primary VGA-compatible display
    # device. It is a full-featured virtual video card, with great operating
    # system driver support. OVMF supports it too.
    OPTS="$OPTS -device qxl-vga"
    
    OPTS="$OPTS -display spice-app"
    
    qemu-system-x86_64 $OPTS ||
        die 'qemu-system failed'    
}


main () {

    if [ 0 -eq $# ]; then
        die "run.sh [--init] disk-img [iso-img]"
    fi
    
    local init_flag=NULL
    local disk_img=NULL
    local install_iso=NULL
    if [ '--init' = "$1" ]; then
        init_flag="$1"
        disk_img="$2"
        install_iso="$3"
        echo "Initializing"
        main_run "$init_flag"
    else
        disk_img="$1"
        main_run "$disk_img"
    fi
}

main "$@"



