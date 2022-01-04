#!/bin/sh

if [ "$1" == "post" ]; then
    echo "Reloading i2c_hid module to fix touchpad behavior"
    modprobe -r i2c_hid_acpi && modprobe i2c_hid_acpi
fi
