#!/bin/sh

cd $1/TimeSeries/Compression/bzip2-1.0.5/ 
make clean libbz2.a
cd $1 
