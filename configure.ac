AC_INIT([rvle],[1.1.1-0],[quesnel@users.sourceforge.net])
AC_CONFIG_SRCDIR([configure.ac])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE

dnl Now find the compiler and compiler flags to use
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
	echo "could not determine R_HOME"
	exit 1
fi

CC=`"${R_HOME}/bin/R" CMD config CC`
CPP=`"${R_HOME}/bin/R" CMD config CPP`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
AC_LANG(C++)
#AC_USE_SYSTEM_EXTENSIONS
#AC_PROG_CC
#AC_PROG_CXX

BOOST_REQUIRE

VLE_CFLAGS=`pkg-config --cflags vle-1.1`
VLE_LIBS=`pkg-config --libs vle-1.1`
PKG_CPPFLAGS="${PKG_CPPFLAGS} ${VLE_CFLAGS} ${BOOST_CPPFLAGS}"
PKG_LIBS="${PKG_LIBS} ${VLE_LIBS}"
AC_SUBST(PKG_CPPFLAGS)
AC_SUBST(PKG_LIBS)

AC_CONFIG_HEADERS([src/config.h])
AC_CONFIG_FILES([src/Makevars Makefile])

AC_OUTPUT
