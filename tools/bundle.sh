#!/bin/sh

# Get the directory into which the binaries have been installed
LINDIR="`stack path --local-install-root`/bin"

# Remove all the static temporary files
rm -rf static/tmp

# Create our deployment directory
mkdir -p deploy/config

# Copy in the static files
cp -rL static deploy

# Copy over the executable, strip and compress
cp $LINDIR/meadowstalk deploy
strip deploy/meadowstalk
upx deploy/meadowstalk

# Copy over our Keter configuration
cp config/keter.yaml deploy/config/keter.yaml

# Remove the old deployment archive
rm -f meadowstalk.keter
# Create the Keter bundle
cd deploy && tar -czvf ../meadowstalk.keter *
