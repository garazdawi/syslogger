AC_INIT([syslogger], [1.0], [lukas@erlang.org])
AM_SILENT_RULES([yes])
AM_PROG_AR
LT_INIT
AM_INIT_AUTOMAKE([-Wall -Wno-portability foreign])

dnl Figure out which erlang nif header to use
AC_CHECK_HEADERS([erl_nif.h],[HAVE_ERL_NIF_H=true])
if test "x${HAVE_ERL_NIF_H}" != "xtrue"; then
   AC_ERLANG_NEED_ERL()
   AC_MSG_CHECKING(erts include dir)
   ERTS_ROOT_DIR=`$ERL -noshell -eval "io:format(\"~ts\", [[code:root_dir()]])" -s init stop`
   ERTS_VSN=`$ERL -noshell -eval "io:format(\"~ts\", [[erlang:system_info(version)]])" -s init stop`
   if test -d $ERTS_ROOT_DIR/erts-$ERTS_VSN/; then
   dnl Normal installed Erlang/OTP
      ERTS_INCLUDE_DIR=$ERTS_ROOT_DIR/erts-$ERTS_VSN/include
   elif test -d $ERTS_ROOT_DIR/usr/include; then
   dnl Normal installed Erlang/OTP if erts-vsn does not exist
      ERTS_INCLUDE_DIR=$ERTS_ROOT_DIR/usr/include
   elif test -d $ERTS_ROOT_DIR/erts/include -a \
             -d $ERTS_ROOT_DIR/erts/include/$build -a \
             -d $ERTS_ROOT_DIR/erts/emulator/beam; then
   dnl OTP source tree
      ERTS_INCLUDE_DIR="$ERTS_ROOT_DIR/erts/include \
                        $ERTS_ROOT_DIR/erts/include/$build \
                        $ERTS_ROOT_DIR/erts/emulator/beam"
   fi
   if test "x${ERTS_INCLUDE_DIR}" != "x"; then
      AC_MSG_RESULT([found $ERTS_INCLUDE_DIR])
      for dir in $ERTS_INCLUDE_DIR; do
          CPPFLAGS="$CPPFLAGS -I $dir"
      done
   else
      AC_MSG_ERROR([cannot find the erts include files])
   fi
fi

dnl Check specific to this project
AC_CHECK_HEADERS([syslog.h],[],[AC_MSG_FAILURE([syslog.h has to be present])])

AC_CONFIG_FILES([
 Makefile
])
AC_OUTPUT
