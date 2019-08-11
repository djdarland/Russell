RROOT = <path to grandparent directory, normally defined by top level Makefile>
RR_TEMPLATE= \"$(RROOT)\\0XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\"
CFLAGS= -O -DRROOT=$(RR_TEMPLATE)
OBJS= main.o loc_tab.o tmp_tab.o args.o strings.o

to_C: $(OBJS)
	cc $(CFLAGS) -o to_C $(OBJS) ../gc/gc.a /lib/libc.a

$(OBJS): tables.h strings.h
