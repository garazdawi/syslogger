lib_LTLIBRARIES = libsyslogger.la
libsyslogger_la_SOURCES = syslogger.c
libsyslogger_la_LDFLAGS = -version-info 0:0:0

## libtool on OS X creates .dylib files, but we need .so files
## so we just coopy the .dylib to .so
%.so: %.dylib
	$(AM_V_GEN) cp $< $@

## copy the .so into .priv
../priv/%.so: .libs/%.so ../priv
	$(AM_V_GEN) cp $< $@

../priv:
	$(AM_V_GEN) $(MKDIR_P) $@

all-local: ../priv/libsyslogger.so
