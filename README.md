Virtual Laboratory Environment 2.1
==================================

| Linux | Windows |
| :---- | :---- |
| [![Linux](https://github.com/vle-forge/rvle/actions/workflows/Linux.yml/badge.svg?branch=master)](https://github.com/vle-forge/rvle/actions/workflows/Linux.yml) | [![Windows build status][3]][4] |

[3]: https://ci.appveyor.com/api/projects/status/github/vle-forge/rvle?branch=master&svg=true
[4]: https://ci.appveyor.com/project/vle-forge/rvle?branch=master

A R binding for the VFL (*Virtual Laboratory Environment Foundation Library*) and VLE (*Virtual Laboratory Environment*).

This package allows:
- read and write VPZ file from VLE
- launch simulation and launch experimental frames
- update simulation parameters
- update observation

## Requirements

* [R](http://www.r-project.org) (≥ 2.10)
* [Rtools](https://cran.r-project.org/bin/windows/Rtools/) (≥ 2.10)
* [VLE](http://www.vle-project.org) (≥ 2.0)

## Getting the code

First, we need to build VLE for both `i386` and `x86_64`. First, we clone the VLE repository:

    git clone git://github.com/vle-forge/vle.git

Using [http://www.msys2.org/](msys2), we install VLE's dependencies both for
`i386` and `x86_64`:

    pacman -S mingw64/mingw-w64-i686-cmake
    pacman -S mingw64/mingw-w64-i686-gcc
    pacman -S mingw64/mingw-w64-i686-make
    pacman -S mingw64/mingw-w64-i686-qt5
    pacman -S mingw64/mingw-w64-i686-boost
    pacman -S mingw64/mingw-w64-i686-libxml2
    pacman -S mingw64/mingw-w64-i686-gdb
    pacman -S mingw64/mingw-w64-x86_64-cmake
    pacman -S mingw64/mingw-w64-x86_64-gcc
    pacman -S mingw64/mingw-w64-x86_64-make
    pacman -S mingw64/mingw-w64-x86_64-boost
    pacman -S mingw64/mingw-w64-x86_64-libxml2

Then we build for `i386` (adjust the Rtools and R in `PATH` variable with your
installation directory):

    set PATH=C:\Rtools\mingw_32\bin;C:\Program Files\R\R-3.3.2\bin\i386;D:\msys64\mingw32\bin
    cd vle
    mkdir buildvle-rvle-i386
    cd buildvle-rvle-i386
    cmake.exe -G "MinGW Makefiles" -DWITH_MVLE=OFF -DWITH_CVLE=OFF -DWITH_DOXYGEN=OFF -DWITH_GVLE=OFF -DWITH_WIN32_INSTALLER=OFF -DCMAKE_INSTALL_PREFIX=d:/rvle-bin/32 -DCMAKE_BUILD_TYPE=RelWithDebInfo ..
    mingw32-make -j2
    mingw32-make install

And for `x86_64` (adjust the Rtools and R in `PATH` variable with your
installation directory):

    set PATH=C:\Rtools\mingw_64\bin;C:\Program Files\R\R-3.3.2\bin\x86_64;d:\msys64\mingw64\bin
    cd vle
    mkdir buildvle-rvle-x86_64
    cd buildvle-rvle-x86_64
    cmake.exe -G "MinGW Makefiles" -DWITH_MVLE=OFF -DWITH_CVLE=OFF -DWITH_DOXYGEN=OFF -DWITH_GVLE=OFF -DWITH_WIN32_INSTALLER=OFF -DCMAKE_INSTALL_PREFIX=d:/rvle-bin/64 -DCMAKE_BUILD_TYPE=RelWithDebInfo ..
    mingw32-make -j2
    mingw32-make install

Once you have met requirements, compiling and installing is simple:

    git clone git://github.com/vle-forge/rvle.git
    set PATH=C:\Program Files\R\R-3.3.2\bin;d:\msys64\usr\bin;C:\Rtools\bin
    set MINGW_PATH=d:/msys64
    set VLE_PATH=d:/rvle-bin
    R CMD build rvle
    R CMD INSTALL --build rvle

## Usage

Under R, use `help(rvle)` to get help. For example:

    library(rvle)
    f <- rvle.open(file="test_simulation.vpz", pkg="test_port")
    result <- rvle.run(f)
    checkEquals(class(result$view), "data.frame")

## License

This software in GPLv3 or later. See the file COPYING. Some files are under a
different license. Check the headers for the copyright info.
