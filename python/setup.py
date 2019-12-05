#!/usr/bin/env python

from distutils.core import setup, Extension

python = " /wrk/hdstaff/paolod/perforce/RDI_paolod_Dev_work/temp/anaconda2/envs/ml-suite-py3/include/python3.7m/"
#numpy = "/usr/local/lib/python2.7/dist-packages/numpy/core/include/numpy/"

setup (name = "anomaly",
    version = "0.4",
    ext_modules = [
        Extension (
            "anomaly",
            [ "anomalymodule.c" ],
            library_dirs = ["../lib", "../TimeSeries/Compression/bzip2-1.0.5"],
            libraries = ['cdl', 'bz2'],
            include_dirs = [ python,
                             #numpy,
                             "../NonParametric/DistanceFunctions", "../Summation", "../Sort", "../TimeSeries/HoltWinters", "../NonParametric", "../TimeSeries/MovingAverage/","../Window", "../TimeSeries",
                             "../TimeSeries/Martingale", "../TimeSeries/Compression", "../TimeSeries/Kernels",
                             "../NonParametric/PValue","../Poset" ]
       )
    ]
)



