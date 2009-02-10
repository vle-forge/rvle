AC_INIT([rvle],[0.6.200-0],[quesnel@users.sourceforge.net])

: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
	echo "could not determine R_HOME"
	exit 1
fi
CC=`"${R_HOME}/bin/R" CMD config CC`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`

AC_PROG_CXX
AC_PROG_CC

VLE_CFLAGS=`pkg-config --cflags vle-0.6`
VLE_LIBS=`pkg-config --libs vle-0.6`
PKG_CPPFLAGS="${PKG_CPPFLAGS} ${VLE_CFLAGS}"
PKG_LIBS="${PKG_LIBS} ${VLE_LIBS}"
AC_SUBST(PKG_CPPFLAGS)
AC_SUBST(PKG_LIBS)

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
