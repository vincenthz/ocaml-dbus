CC = gcc
CFLAGS = -Wall -fPIC -O2 `pkg-config --cflags dbus-1`
OCAMLC = ocamlc
OCAMLOPT = ocamlopt

LDFLAGS = -cclib -L./

OCAMLOPTFLAGS =
OCAML_PKG_NAME = dbus

OCAMLABI := $(shell ocamlc -version)
OCAMLLIBDIR := $(shell ocamlc -where)
OCAMLDESTDIR ?= $(OCAMLLIBDIR)

OCAML_TEST_INC = -I `ocamlfind query oUnit`
OCAML_TEST_LIB = `ocamlfind query oUnit`/oUnit.cmxa

CHECK_PKGS = dbus-1

INTERFACES = dBus.cmi dBus.mli
LIBS = dBus.cmxa dBus.cma
PROGRAMS = test

all: $(INTERFACES) $(LIBS) $(PROGRAMS)

bins: $(PROGRAMS)

libs: $(LIBS)

dBus.cmxa: libdbus_stubs.a dbus_stubs.a dBus.cmx
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -cclib -ldbus-1 -cclib -ldbus_stubs -o $@ dBus.cmx

dBus.cma: libdbus_stubs.a dBus.cmi dBus.cmo
	$(OCAMLC) -a -dllib dlldbus_stubs.so -cclib -ldbus-1 -cclib -ldbus_stubs -o $@ -custom dBus.cmo

dbus_stubs.a: dbus_stubs.o
	ocamlmklib -o dbus_stubs $+

libdbus_stubs.a: dbus_stubs.o
	ar rcs $@ $+
	ocamlmklib -o dbus_stubs $+

%.cmo: %.ml
	$(OCAMLC) -c -o $@ $<

%.cmi: %.mli
	$(OCAMLC) -c -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

.PHONY: check
check:
	$(foreach pkg, $(CHECK_PKGS), \
		@pkg-config --modversion $(pkg) > /dev/null 2>&1 || \
			(echo "$(pkg) package not found" > /dev/stderr && exit 1))

.PHONY: install
install: $(LIBS)
	ocamlfind install -destdir $(OCAMLDESTDIR) -ldconf ignore $(OCAML_PKG_NAME) META $(INTERFACES) $(LIBS) *.a *.so *.cmx

uninstall:
	ocamlfind remove -destdir $(OCAMLDESTDIR) $(OCAML_PKG_NAME)

test: dBus.cmxa test.ml
	$(OCAMLOPT) -o $@ $(LDFLAGS) unix.cmxa $+

clean:
	rm -f *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa $(LIBS) $(PROGRAMS)
