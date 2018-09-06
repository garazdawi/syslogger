
all:
	rebar3 compile

clean:
	rebar3 clean

## These targets are called by rebar3 to make and clean the nif
nif: c_src/Makefile
	make -C c_src

c_src/configure:
	autoreconf --install c_src

c_src/Makefile: c_src/configure
	(cd c_src && ./configure ${CONFIGURE_FLAGS})

clean_nif:
	make -C c_src clean uninstall
