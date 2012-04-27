.PHONY: all clean dcpuc-all dcpuc-clean dcpucaml-all dcpucaml-clean

CFLAGS = -Wall -W -Os

all: dcpuc-all dcpucaml-all
clean: dcpuc-clean dcpucaml-clean

############################################################

dcpuc-all: dcpu dcpuopt

dcpuc-clean:
	@rm -f dcpu dcpuopt dcpuopt-cases.h

dcpu: dcpu.c
dcpuopt: dcpuopt.c dcpuopt-cases.h

dcpuopt-cases.h: dcpuopt-gen.py
	python dcpuopt-gen.py > $@

%: %.c
	$(CC) $(CFLAGS) $(LDFLAGS) $< -o $@

############################################################

OCAMLC = ocamlc
OCAMLOPT = ocamlopt

DCPUASM = DcpuAsm.cmo
PA_DCPUASM = pa_dcpuasm.cmo

dcpucaml-all: $(DCPUASM) dcpuasmexample

dcpucaml-clean:
	@rm -f *.cmo *.cmi dcpuasmexample

pa_dcpuasm.cmo: pa_dcpuasm.ml
	$(OCAMLC) -c dynlink.cma -I +camlp4 camlp4lib.cma -pp camlp4of.opt $<

# huh, homebrew ocaml kettle does not contain dynlink.cmxa?
#pa_dcpuasm.cmx: pa_dcpuasm.ml
#	$(OCAMLOPT) dynlink.cmxa -I +camlp4 camlp4lib.cmxa -pp camlp4of.opt $<

dcpuasmexample: DcpuAsmExample.cmo DcpuAsm.cmo pa_dcpuasm.cmo
	$(OCAMLC) $(DCPUASM) $< -o $@

DcpuAsm.cmo: DcpuAsm.ml DcpuAsm.cmi pa_dcpuasm.cmo
	$(OCAMLC) -c $<
DcpuAsm.cmi: DcpuAsm.mli pa_dcpuasm.cmo
	$(OCAMLC) -c $<

%.cmo: %.ml
	$(OCAMLC) -c -pp 'camlp4o -parser $(PA_DCPUASM)' $(DCPUASM) $<
%.cmi: %.mli
	$(OCAMLC) -c -pp 'camlp4o -parser $(PA_DCPUASM)' $(DCPUASM) $<

