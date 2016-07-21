PKG = async,yojson
ADDITIONAL=-package sexplib,pa_sexp_conv -syntax camlp4o

all:
	corebuild -pkg $(PKG) $(ADDITIONAL) src/main.native

install:
	scp main.native ubuntu@54.208.53.6:

clean:
	corebuild -clean
