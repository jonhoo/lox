#!/bin/sh
#
# This script is used to run your program on CodeCrafters
#
# This runs after .codecrafters/compile.sh
#
# Learn more: https://codecrafters.io/program-interface

exec /tmp/codecrafters-build-interpreter-rust/release/codecrafters-interpreter "$@"
