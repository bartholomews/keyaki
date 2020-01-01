#! /bin/bash

set -e
echo "Building keyaki..."
stack build
strip `stack exec -- which keyaki`
echo "Creating bundle..."
cp `stack exec -- which keyaki` keyaki
tar -czvf keyaki.keter keyaki config ql-ui/assets
rm keyaki
# scp ./keyaki.keter user@host:/opt/keter/incoming/keyaki.keter
rm keyaki.keter
