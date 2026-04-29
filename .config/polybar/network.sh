#!/bin/bash

WIRED_INTERFACE=enp0s31f6
WIFI_INTERFACE=wlp0s20f3

wired=$(ip addr show "${WIRED_INTERFACE}" | grep 'inet ' | awk '{print $2}' | cut -d/ -f1)
essid=$(iw dev "${WIFI_INTERFACE}" link | grep 'SSID' | awk '{print $2}')
wifi=$(ip addr show "${WIFI_INTERFACE}" | grep 'inet ' | awk '{print $2}' | cut -d/ -f1)

if [ -n "${wired}" ]; then
  echo "󰲝   ${wired}"
elif [ -n "${essid}" ]; then
  echo "󰖩   ${essid}: ${wifi}"
else
  # Catppuccin yellow
  echo "%{F#f9e2af}󰲜  󰖪%{F-}"
fi
