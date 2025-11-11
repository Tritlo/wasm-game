#!/usr/bin/env bash

set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"

export ELECTRON_ENABLE_LOGGING=1
export ELECTRON_DISABLE_SANDBOX=1

# Wayland is fine via XWayland; enable Ozone if you've tested it:
# export OZONE_PLATFORM=wayland

exec "$HERE/gwp" "$@"
