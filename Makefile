# This makefile probably only works the DMD compiler because it uses some curious command-line switches
# that are of a different style than what GNU and LLVM compilers use.
D_COMPILER = dmd
D_INCDIRS  = -I$$(pwd)/src -I$(CAPSTONE_D_INCDIRS)
D_DEBUG    = -g -debug -vcolumns
D_UNITTEST = -unittest
TOOLS_SOURCE =

all: libmunstead-unittests readelf readpe

########################################################################################################################
# A tool to read ELF files and print their contents

TOOLS_SOURCE += tools/readelf.d
readelf: tools/readelf.o libmunstead.a
	$(D_COMPILER) $(D_COMPILE_FLAGS) $(D_LINK_FLAGS) -of=$@ -Isrc $^

########################################################################################################################
# A tool to read PE files and print their contents
TOOLS_SOURCE += tools/readpe.d
readpe: tools/readpe.o libmunstead.a
	$(D_COMPILER) $(D_COMPILE_FLAGS) $(D_LINK_FLAGS) -of=$@ -Isrc $^

########################################################################################################################

# The Munstead library
MUNSTEAD_SOURCE =				\
	src/munstead/ast/base.d			\
	src/munstead/ast/files.d		\
	src/munstead/ast/sections.d		\
	src/munstead/ast/tabulate.d		\
	src/munstead/core/bitflags.d		\
	src/munstead/core/byteorder.d		\
	src/munstead/core/exception.d		\
	src/munstead/core/hexdump.d		\
	src/munstead/core/imap.d		\
	src/munstead/core/interval.d		\
	src/munstead/core/iobuf.d		\
	src/munstead/core/iset.d		\
	src/munstead/core/itree.d		\
	src/munstead/core/mmap.d		\
	src/munstead/core/registers.d		\
	src/munstead/core/util.d		\
	src/munstead/core/wordtypes.d		\
	src/munstead/dos/files.d		\
	src/munstead/elf/array.d		\
	src/munstead/elf/dynamic.d		\
	src/munstead/elf/files.d		\
	src/munstead/elf/interp.d		\
	src/munstead/elf/loader.d		\
	src/munstead/elf/notes.d		\
	src/munstead/elf/reloc.d		\
	src/munstead/elf/sections.d		\
	src/munstead/elf/segments.d		\
	src/munstead/elf/strings.d		\
	src/munstead/elf/symbols.d		\
	src/munstead/elf/types.d		\
	src/munstead/loader.d			\
	src/munstead/pe/exports.d		\
	src/munstead/pe/files.d			\
	src/munstead/pe/imports.d		\
	src/munstead/pe/loader.d		\
	src/munstead/pe/resources.d		\
	src/munstead/pe/sections.d		\
	src/munstead/x86/registers.d

MUNSTEAD_OBJECTS = $(MUNSTEAD_SOURCE:.d=.o)

# I'm not sure if D_DEBUG and D_UNITTEST flags need to be present for linking, so am including them.
libmunstead.a: $(MUNSTEAD_OBJECTS)
	$(D_COMPILER) $(D_DEBUG) $(D_UNITTEST) -lib -of=$@ $^

# D won't run unittest blocks inside a library. The library must be recompiled as an executable. See
# [https://forum.dlang.org/thread/bakrxbzhexzuowiolfya@forum.dlang.org] and
# [https://issues.dlang.org/show_bug.cgi?id=4669]. As a new D programmer, it took me a while to realize
# that none of my unit tests were actually ever running -- IMHO, this bug either needs to be fixed or
# the compiler should complain loudly.
libmunstead-unittests: $(MUNSTEAD_OBJECTS)
	$(D_COMPILER) $(D_DEBUG) $(D_UNITTEST) -main -of=$@ $^


########################################################################################################################
# Boilerplate

dependencies: $(MUNSTEAD_SOURCE) $(TOOLS_SOURCE)
	$(D_COMPILER) $(D_INCDIRS) -deps=$@.tmp -o- -c $^
	sed -n '/([^\/].*([^\/]/ {s/([^)]*)//g; p}' $@.tmp |\
	    awk -f scripts/dmd-dependencies.awk |\
	    sort -u >$@.tmp2
	rm $@.tmp
	mv $@.tmp2 $@

-include dependencies

%.o: %.d
	$(D_COMPILER) $(D_INCDIRS) $(D_DEBUG) $(D_UNITTEST) -of=$@ -c -D -Df$<.html $<

clean:
	rm -f libmunstead.a
	rm -f libmunstead-unittests libmunstead-unittests.o
	rm -f readelf tools/readelf.o
	rm -f readpe tools/readpe.o
	rm -f $(MUNSTEAD_SOURCE:.d=.o)
	rm -f a.out a.o demo demo.o
	rm -f dependencies
