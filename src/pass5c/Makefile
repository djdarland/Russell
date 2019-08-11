HOME= /mnt/boehm
CC= $(HOME)/bin/CDCC
CFLAGS= -w
LIBC= -lc
OBJS= codegen.o allocate.o codeutil.o decl_sort.o find_inline.o Fcodegen.o accessible.o
SRCS= codegen.c allocate.c codeutil.c decl_sort.c find_inline.c Fcodegen.c accessible.c

pass5c.o: $(OBJS)
	-ld -r -o pass5c.o $(OBJS)

codegen.o: ../pass4/sigs.h ../pass3/is_local.h

$(OBJS): codeutil.h ../pass4/sigs.h ../stree/streedefs.h ../stree/ststructs.m ../datatypes/consnodes.h

codegen.o allocate.o Fcodegen.o accessible.o: ../../runtime/runtime.h

.s.o:
	-$(AS) -o $@ $<

