#!/bin/bash

set -euo pipefail

cabal build --enable-static --enable-executable-static
echo "=== Transferring to ee-beholder0 === "
rsync -r -h --info=progress2 "$(cabal list-bin verismith)" experiments/ ee-beholder0:verismith/
ssh -q ee-beholder0 "cd ~/verismith && ./verismith $@"
