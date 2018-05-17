#!/bin/bash

wget https://github.com/singularityware/singularity/archive/$1.tar.gz
tar xf $1.tar.gz
mv singularity-$1 singularity-sources
pushd .
cd singularity-sources
./autogen.sh
./configure --prefix=/usr/local/
make -j8
sudo make install

popd

# remove intermediate files
rm $1.tar.gz
rm -rf singularity-sources
