Virtual Laboratory Environment 2.0
==================================

A R binding for the VFL.

See AUTHORS and COPYRIGHT for the list of contributors.

## Requirements

* R (≥ 2.10)
* vle (≥ 2.0)
* boost (≥ 1.47)
* cmake (≥ 3.0)
* make (≥ 1.8)
* c++ compiler (gcc ≥ 4.9, clang ≥ 3.3, intel icc (≥ 11.0)

## Getting the code

The source tree is currently hosted on Github and Sourceforge. To view the
repository online: https://github.com/vle-forge/rvle The URL to clone it:

    git clone git://github.com/vle-forge/rvle.git

Once you have met requirements, compiling and installing is simple:


    cd rvle
    ./autogen.sh
    cd ..
    R CMD INSTALL rvle

## License

This software in GPLv3 or later. See the file COPYING. Some files are under a
different license. Check the headers for the copyright info.

## Usage

    f <- rvle.open(file="test_simulation.vpz", pkg="test_port")
    result <- rvle.run(f)
    checkEquals(class(result$view), "data.frame")
