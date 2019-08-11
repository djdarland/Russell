CFLAGS = -O

OBJECTS= alias.o bool.o chstr.o long.o long_mult.o \
	 short.o void.o comb_float.o array.o call_russell.o \
	 type_constr.o misc.o ref.o list.o lazy_list.o file.o callcc.o \
	 startup.o BigNum/BigNum.a ../src/gc/gc.a 

rrt0.o: main.o russell_lib.o
	-ld -r -o rrt0.o main.o russell_lib.o

russell_lib.o: $(OBJECTS)
	-ld -r -o russell_lib.o $(OBJECTS)

comb_float.o: float.o sfloat.o
	ld -r float.o sfloat.o -lm
	mv a.out comb_float.o

BigNum/BigNum.a:
	cd BigNum; 	make C32
	
$(OBJECTS): runtime.h ../src/gc/gc.h

chstr.o misc.o array.o alias.o bool.o short.o long.o void.o ref.o list.o lazy_list.o file.o alloc.o float.o call_russell.o: types.h

startup.o main.o: initenv.cpp types.h

../src/gc/gc.a:
	cd ../src/gc; make gc.a

