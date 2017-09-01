D_COMPILER = dmd
D_COMPILE_FLAGS = -c -g -unittest -debug -I$$(pwd)/src
D_LINK_FLAGS = -g -unittest -debug

#D_COMPILER = dmd
#D_COMPILE_FLAGS = -c -g -release -O -I$$(pwd)/src
#D_LINK_FLAGS = -g -release -O

#D_COMPILER = ldc2
#D_COMPILE_FLAGS = -c -g -I=$$(pwd)/src -enable-asserts -unittest
#D_LINK_FLAGS = -g -unittest

#D_COMPILER = gdc
#D_COMPILE_FLAGS = -c -g -funittest -I$$(pwd)/src
#D_LINK_FLAGS = -g -funittest



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
	src/munstead/core/imap.d		\
	src/munstead/core/interval.d		\
	src/munstead/core/iobuf.d		\
	src/munstead/core/iset.d		\
	src/munstead/core/itree.d		\
	src/munstead/core/mmap.d		\
	src/munstead/core/util.d		\
	src/munstead/core/wordtypes.d		\
	src/munstead/elf/dynamic.d		\
	src/munstead/elf/files.d		\
	src/munstead/elf/interp.d		\
	src/munstead/elf/notes.d		\
	src/munstead/elf/sections.d		\
	src/munstead/elf/segments.d		\
	src/munstead/elf/strings.d		\
	src/munstead/elf/symbols.d		\
	src/munstead/elf/types.d

MUNSTEAD_OBJECTS = $(MUNSTEAD_SOURCE:.d=.o)

########################################################################################################################
# Programs

a.out: src/main.o $(MUNSTEAD_OBJECTS)
	$(D_COMPILER) $(D_LINK_FLAGS) -of=$@ $^

tests1: src/tests1.o $(MUNSTEAD_OBJECTS)
	$(D_COMPILER) $(D_LINK_FLAGS) -of=$@ $^

########################################################################################################################
# Standard boiler plate

%.o: %.d
	$(D_COMPILER) $(D_COMPILE_FLAGS) -of=$@ $<

clean:
	rm -f a.out tests1 *.o src/*.o src/munstead/*.o src/munstead/ast/*.o src/munstead/core/*.o
