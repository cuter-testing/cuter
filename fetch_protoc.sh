#! /bin/bash

set -e

DL="lib"
VER="3.11.3"
PROTOC_DIR="$DL/protoc-$VER"
rm -rf $PROTOC_DIR

ME=`basename "$0"`

SYSTEM_TYPE=`uname -s`
case ${SYSTEM_TYPE} in
  'Darwin') OS=osx;;
  'Linux')  OS=linux;;
  *) echo "Sorry, but there is no automatic support for ${SYSTEM_TYPE}" >&2
     exit 1
     ;;
esac

MACHINE_TYPE=`uname -m`
case ${MACHINE_TYPE} in
  'i686')   ARCH='x86_32';;	# 32-bit
  'x86_64') ARCH='x86_64';;	# 64-bit
  *) echo "Sorry, but there is no automatic support for the ${MACHINE_TYPE} architecture" >&2
     exit 1
     ;;
esac

ZIP_FILE="protoc-$VER-$OS-$ARCH.zip"

wget -P $DL https://github.com/google/protobuf/releases/download/v$VER/$ZIP_FILE
mkdir -p $PROTOC_DIR
unzip $DL/$ZIP_FILE -d $PROTOC_DIR
rm -f $DL/$ZIP_FILE
echo "[$ME] protoc installed at $PWD/$PROTOC_DIR/bin"
echo "[$ME] Run the configure script:"
echo "[$ME]   autoconf"
echo "[$ME]   ./configure --with-protoc=$PWD/$PROTOC_DIR/bin/protoc"
