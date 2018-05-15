#!/bin/bash

wget https://github.com/INCF/MUSIC/archive/$1.tar.gz

tar xf $1.tar.gz

pushd .
cd MUSIC-${1//v}
./autogen.sh
./configure --prefix=$PWD/../music-${1//v}/ --disable-isend
make -j8
make install
popd

rm $1.tar.gz
rm -rf MUSIC-${1//v}

