#!/usr/bin/env bash
set -euo pipefail

source ./volume-getsink.sh
pactl -- set-sink-volume $VOLUMESINK +5%
