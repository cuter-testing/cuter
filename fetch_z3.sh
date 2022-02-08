#! /bin/bash

set -e

if [[ -z "${Z3_RELEASE}" ]]; then
  echo "The Z3_RELEASE environment variable is not set. Please specify the Z3 release."
  exit 1
fi

wget -O /tmp/z3.zip "https://github.com/Z3Prover/z3/releases/download/${Z3_RELEASE}.zip"
unzip /tmp/z3.zip -d /tmp
rm /tmp/z3.zip
mv /tmp/z3-* /opt/z3
ln -s /opt/z3/bin/z3 /usr/bin/z3
uname -a
ldd --version
strings /usr/lib/x86_64-linux-gnu/libstdc++.so.6 | grep GLIBCXX
which z3
z3 -version
