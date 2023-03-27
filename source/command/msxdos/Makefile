# Makefile for NEXTOR.SYS

# The generated NEXTOR.SYS and NEXTOR.SYS.japanese files are copied to the bin/tools directory.

ifeq ($(strip $(N80)),)
N80=N80
endif

export N80_ARGS=--no-string-escapes --no-show-banner --build-type rel --verbosity 0 --output-file-case lower --include-directory ../../kernel

ifeq ($(strip $(LK80)),)
LK80=LK80
endif

export LK80_ARGS=--no-show-banner --verbosity 0 --output-file-case lower --output-format bin

define copy_to_bin
	cp $(1) ../../../bin/tools/$(2)
endef

define assemble
	@printf "\n\033[0;36mAssembling %s\033[0m\n\n" $(1)
	@$(N80) $(1) $(2)
endef

nextor-sys: NEXTOR.SYS NEXTORK.SYS

.phony: prerequisites

TOOLS := N80 LK80

prerequisites:
	@mkdir -p ../../../bin/tools
	$(foreach exec,$(TOOLS),\
		$(if $(shell which $(exec)),,$(error "ERROR: can't execute $(exec), is it installed/in PATH?")))

-include prerequisites

INCS := ../../kernel/macros.inc ../../kernel/const.inc ../../kernel/condasm.inc
RELS := messages.rel real.rel ref.rel reloc.rel ver.rel end.rel
JRELS := $(RELS) kmsg.rel

NEXTOR.SYS: \
	$(RELS) \
	codes.rel \
	data.rel \
	sys.mac

	$(call assemble,sys.mac,--define-symbols NO_J_MSG)
	@$(LK80) --output-file NEXTOR.SYS --code 0100h codes.rel data.rel reloc.rel ver.rel ref.rel sys.rel real.rel sys.rel messages.rel end.rel
	$(call copy_to_bin,NEXTOR.SYS)

NEXTORK.SYS: \
	$(JRELS) \
	codes.rel \
	data.rel \
	sys.mac

	$(call assemble,sys.mac)
	@$(LK80) --output-file NEXTOR.SYS.japanese --code 0100h codes.rel data.rel reloc.rel ver.rel ref.rel sys.rel real.rel sys.rel messages.rel kmsg.rel end.rel
	$(call copy_to_bin,NEXTOR.SYS.japanese)

.SECONDEXPANSION:
$(JRELS): \
	$$(patsubst %.rel,%.mac,$$@) \
	$(INCS) \

	$(call assemble,$(patsubst %.rel,%.mac,$@))

codes.rel: \
	../../kernel/codes.mac \
	$(INCS)

	$(call assemble,../../kernel/codes.mac)

data.rel: \
	../../kernel/data.mac \
	$(INCS)

	$(call assemble,../../kernel/data.mac)

.PHONY: clean

clean:
	for ext in BIN HEX REL rel SYM; do find . -type f -name "*.$$ext" -delete; done
