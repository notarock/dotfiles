#!/usr/bin/env bash
set -euo pipefail

VOLUMESINK=$(pacmd stat | awk -F": " '/^Default sink name: /{print $2}')
