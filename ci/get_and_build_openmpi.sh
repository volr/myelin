#!/bin/bash

wget https://www.open-mpi.org/software/ompi/v3.0/downloads/openmpi-3.0.1.tar.gz
tar xf openmpi-3.0.1.tar.gz
pushd .
cd openmpi-3.0.1
./configure --prefix=$PWD/../openmpi-3.0.1/
make -j8
make install
