INCLUDE = network,logic,message,utils,state
PKG = async,yojson
ADDITIONAL=-package sexplib,pa_sexp_conv -syntax camlp4o

all:
	corebuild -pkg $(PKG) $(ADDITIONAL) main.native -Is $(INCLUDE)

install:
	scp main.native ubuntu@54.194.72.28:

clean:
	corebuild -clean
