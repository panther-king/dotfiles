#!/usr/bin/env bash
set -euo pipefail

if [ $# -eq 0 ];then
  echo "Usage: ${0} <hostname>" >&2
  exit 1
fi

HOST="${1}"

nix shell nixpkgs#ghq nixpkgs#git -c ghq get github.com/panther-king/dotfiles
cd "$(ghq root)/github.com/panther-king/dotfiles"
nh os switch "${HOST}"
