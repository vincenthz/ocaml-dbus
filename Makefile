DBUS_CFLAGS = -ccopt "$(shell pkg-config --cflags dbus-1)"
OCAMLC ?= ocamlc
OCAMLOPT ?= ocamlopt
OCAMLMKLIB ?= ocamlmklib

DBUS_LDFLAGS = -cclib "" $(shell pkg-config --libs dbus-1)

OCAMLOPTFLAGS =
OCAML_PKG_NAME = dbus

OCAMLABI := $(shell $(OCAMLC) -version)
OCAMLLIBDIR := $(shell $(OCAMLC) -where)
OCAMLDESTDIR ?= $(OCAMLLIBDIR)

OCAML_TEST_INC = -I `ocamlfind query oUnit`
OCAML_TEST_LIB = `ocamlfind query oUnit`/oUnit.cmxa

CHECK_PKGS = dbus-1

INTERFACES = dBus.cmi dBus.mli
LIBS_NAT = dBus.cmxa
LIBS_BYTE = dBus.cma
LIBS = $(LIBS_BYTE) $(LIBS_NAT)
PROGRAMS = test

all: $(INTERFACES) $(LIBS_NAT) $(LIBS_BYTE)

all-opt: all

all-byte: $(INTERFACES) $(LIBS_BYTE)

bins: $(PROGRAMS)

libs: $(LIBS)

dBus.cmxa: libdbus_stubs.a dbus_stubs.a dBus.cmx
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -cclib -ldbus_stubs -cclib -ldbus-1 -o $@ dBus.cmx

dBus.cma: libdbus_stubs.a dBus.cmi dBus.cmo
	$(OCAMLC) -a -dllib dlldbus_stubs.so -cclib -ldbus_stubs -cclib -ldbus-1 -o $@ dBus.cmo

dbus_stubs.a: libdbus_stubs.a

libdbus_stubs.a: dbus_stubs.o
	$(OCAMLMKLIB) -o dbus_stubs $(DBUS_LDFLAGS) $+

%.cmo: %.ml
	$(OCAMLC) -c -o $@ $<

%.cmi: %.mli
	$(OCAMLC) -c -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c -o $@ $<

%.o: %.c
	$(OCAMLC) $(DBUS_CFLAGS) -c -o $@ $<

.PHONY: check
check:
	$(foreach pkg, $(CHECK_PKGS), \
		@pkg-config --modversion $(pkg) > /dev/null 2>&1 || \
			(echo "$(pkg) package not found" > /dev/stderr && exit 1))

.PHONY: install
install: $(LIBS)
	ocamlfind install -destdir $(OCAMLDESTDIR) -ldconf ignore $(OCAML_PKG_NAME) META $(INTERFACES) $(LIBS) *.a *.so *.cmx

install-opt: install

install-byte: all-byte
	ocamlfind install -destdir $(OCAMLDESTDIR) -ldconf ignore $(OCAML_PKG_NAME) META $(INTERFACES) $(LIBS_BYTE) *.a *.so

uninstall:
	ocamlfind remove -destdir $(OCAMLDESTDIR) $(OCAML_PKG_NAME)

test: dBus.cmxa test.ml
	$(OCAMLOPT) -o $@ -cclib -L. unix.cmxa $+

.PHONY: example
example: dBus.cmxa example.ml
	$(OCAMLOPT) -o $@ -cclib -L. $+

example_avahi: dBus.cmxa example_avahi.ml
	$(OCAMLOPT) -o $@ -cclib -L. $+

clean:
	rm -f *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa $(LIBS) $(PROGRAMS)
