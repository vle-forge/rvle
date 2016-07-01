AC_INIT([rvle],[1.3.0-0],[quesnel@users.sourceforge.net])
dnl AC_CONFIG_SRCDIR([configure.ac])
AC_CONFIG_MACRO_DIR([m4])

dnl Now find the compiler and compiler flags to use
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi

CXX1X=`"${R_HOME}/bin/R" CMD config CXX1X`
CXX1XSTD=`"${R_HOME}/bin/R" CMD config CXX1XSTD`
CXX="${CXX1X} ${CXX1XSTD}"
CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXX1XFLAGS`
AC_LANG(C++)

AC_CONFIG_HEADERS([src/config.h])
AC_CONFIG_FILES([src/Makevars])

AC_OUTPUT
