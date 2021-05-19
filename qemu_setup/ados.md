# os hacking


## Simple build and test with KVM

```
# Switch to the QEMU root directory.
cd qemu
# Prepare a native debug build.
mkdir -p bin/debug/native
cd bin/debug/native
# Configure QEMU and start the build.
../../../configure --tareget=x86_64-softmmu --enable-debug
make
# Return to the QEMU root directory.
cd ../../..
```

# Create a disk for the VM
```
./qemu-img create -f qcow2 test.qcow2 16G
# Download an install ISO - I have Fedora 20
ls -la Fedora-Live-Desktop-x86_64-20-1.iso
-rwxr-xr-x. 1 xxxxx xxxxx 999292928 May  4 16:32
```

# Getting ready to install a guest OS in a VM:

```
# Create a disk for the VM
./qemu-img create -f qcow2 test.qcow2 16G
# Download an install ISO - I have Fedora 20
ls -la ~/Images/ubuntu-20.04.2.0-desktop-amd64.iso
-rw-r--r-- 1 adam adam 2877227008 May 19 13:30 /home/adam/Images/ubuntu-20.04.2.0-desktop-amd64.iso
```

# Run QEMU with KVM enabled (w/o VNC):
```
x86_64-softmmu/qemu-system-x86_64 -m 1024 -enable-kvm \
-drive if=virtio,file=test.qcow2,cache=none \
-cdrom /home/adam/Images/ubuntu-20.04.2.0-desktop-amd64.iso
```
