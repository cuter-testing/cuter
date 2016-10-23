#! /bin/bash

set -e

DL="lib"
VER="3.1.0"
PROTOC_DIR="$DL/protoc-$VER"
rm -rf $PROTOC_DIR

ME=`basename "$0"`
MACHINE_TYPE=`uname -m`
if [ ${MACHINE_TYPE} == 'x86_64' ]; then
  # 64-bit
  echo "[$ME] Detected 64-bit installation ..."
  ZIP_FILE="protoc-$VER-linux-x86_64.zip"
else
  # 32-bit
  echo "[$ME] Detected 32-bit installation ..."
  ZIP_FILE="protoc-$VER-linux-x86_32.zip"
fi
wget -P $DL https://github.com/google/protobuf/releases/download/v$VER/$ZIP_FILE
mkdir -p $PROTOC_DIR
unzip $DL/$ZIP_FILE -d $PROTOC_DIR
rm -f $DL/$ZIP_FILE
echo "[$ME] protoc installed at $PWD/$PROTOC_DIR/bin"
echo "[$ME] Run the configure script:"
echo "[$ME]   autoconf"
echo "[$ME]   ./configure --with-protoc=$PWD/$PROTOC_DIR/bin/protoc"
