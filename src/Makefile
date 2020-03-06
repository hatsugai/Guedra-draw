ifndef OSTYPE
OSTYPE=$(shell uname -s)
endif

ifeq ($(OSTYPE), Darwin)

#
#  macOS
#
CC=gcc
CXX=g++
WINBASE=win-gtk
EXE=
CFLAGS=
INCLUDE=`pkg-config --cflags gtk+-3.0`
LDFLAGS=`pkg-config --libs gtk+-3.0` -lasmrun -lunix -ldl -lm

else
ifeq ($(OSTYPE), Linux)

#
#  Linux
#
CC=gcc
CXX=g++
WINBASE=win-gtk
EXE=
CFLAGS=
INCLUDE=`pkg-config --cflags gtk+-3.0`
LDFLAGS=`pkg-config --libs gtk+-3.0` -lasmrun -lunix -ldl -lm

else

#
#  MS-Windows
#
CC=x86_64-w64-mingw32-gcc
CXX=x86_64-w64-mingw32-g++
WINBASE=win-d2d
EXE=.exe
CFLAGS=-D_UNICODE -DUNICODE
LDFLAGS=-lasmrun -lunix -lm -ld2d1 -ldwrite

endif
endif

CSP_PATH=../../ocaml-csp/src
GUEDRA_PATH=../../Guedra/src
OCAML_INCLUDE=-I `ocamlc -where` -I $(CSP_PATH) -I $(GUEDRA_PATH)
OBJECTS=$(GUEDRA_PATH)/win.o $(GUEDRA_PATH)/$(WINBASE).o
CSP_LIB=$(CSP_PATH)/csp.cmx
GUEDRA_LIB=$(GUEDRA_PATH)/guedra.cmxa $(OBJECTS)

OCAML_SRC=\
	setx.ml \
	mapx.ml \
	drawModel.ml \
	draw.ml \
	figRect.ml \
	figConn.ml \
	figConnLabel.ml \
	test_draw.ml

OCAML_CMO=\
	setx.cmo \
	mapx.cmo \
	drawModel.cmo \
	draw.cmo

OCAML_CMX=\
	setx.cmx \
	mapx.cmx \
	drawModel.cmx \
	draw.cmx

FIGS=\
	figRect.cmx \
	figConn.cmx \
	figConnLabel.cmx

DRAW_LIB=draw.cmxa
TESTS=test_draw.x
TARGETS=draw.cma draw.cmxa $(TESTS)

%.cmi:	%.mli
	ocamlc $(OCAML_INCLUDE) -bin-annot -c $<

%.cmx:	%.ml
	ocamlopt $(OCAML_INCLUDE) -bin-annot -c $<

%.cmo:	%.ml
	ocamlc $(OCAML_INCLUDE) -bin-annot -c $<

%.x:	%.cmx
	ocamlopt -o $@ $(CSP_LIB) $(GUEDRA_LIB) $(DRAW_LIB) $(FIGS) $< \
			-cclib "-L `ocamlc -where`" -cclib "$(LDFLAGS)"

all:	$(TARGETS)

draw.cma:	$(OCAML_CMO)
	ocamlc -a -o $@ $^

draw.cmxa:	$(OCAML_CMX)
	ocamlopt -a -o $@ $^

$(OCAML_CMO) $(OCAML_CMX) $(FIGS):	$(CSP_LIB) $(GUEDRA_LIB)

clean:
	rm *.cmi *.cmo *.cmx *.cmt *.cmti *.o *.cma *.cmxa *.a *.x *.exe

dep:
	ocamldep $(OCAML_SRC) > .depend

-include .depend
