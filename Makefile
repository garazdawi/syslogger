
all:
	rebar3 compile

clean:
	rebar3 clean

## These targets are called by rebar3 to make and clean the nif
nif: c_src/Makefile
	make -C c_src install
	## On OS X libtool creates a .dylib, but Erlang requires
	## it to be call .so. So make a copy of dylib if it exists.
	if [ -f priv/lib/libsyslogger.dylib ]; then \
		cp priv/lib/libsyslogger.dylib priv/lib/libsyslogger.so; \
	fi

c_src/configure:
	autoreconf --install c_src

c_src/Makefile: c_src/configure
	(cd c_src && ./configure --prefix=$(shell pwd)/priv ${CONFIGURE_FLAGS})

clean_nif:
	- make -C c_src clean uninstall
