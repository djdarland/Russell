RFLAGS= -O
calc: calc.r cr/fusion.o primes/prime_test.o xlong.o window.o curses.o \
      term.o to_hex.o
	rc -pL $(RFLAGS) calc.r term.o curses.o to_hex.o -lcurses -ltermlib -lm
	mv a.out calc

xlong.o: xlong.r
	rc -cpN $(RFLAGS) xlong.r

window.o: window.r
	rc -cpNL $(RFLAGS) window.r

cr/fusion.o: cr/fusion.r cr/rat.o cr/cr.o cr/exp.o xlong.o
	rc -cpNL $(RFLAGS) cr/fusion.r

cr/cr.o: cr/cr.r cr/interval.o xlong.o
	rc -cpNL $(RFLAGS) cr/cr.r

cr/interval.o: cr/interval.r
	rc -cpN $(RFLAGS) cr/interval.r

cr/rat.o: cr/rat.r xlong.o
	rc -cpN $(RFLAGS) cr/rat.r

cr/exp.o: cr/exp.r
	rc -cpNL $(RFLAGS) cr/exp.r

primes/Zmod.o: primes/Zmod.r xlong.o
	rc -cpN $(RFLAGS) primes/Zmod.r

primes/prime_test.o: primes/prime_test.r primes/Zmod.o xlong.o
	rc -cpN $(RFLAGS) primes/prime_test.r

clean:
	rm -f *.[oscG] cr/*.[oscG] primes/*.[oscG] calc

