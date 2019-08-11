	program crtest
	implicit logical (a-z)
	integer a,b,c
	integer crpi, crmult, crsqrt, crexp
	call xcr
	a = crpi()
	call crput(a,20)
	b = crsqrt(crin(163))
	call crput(a,30)
	call crput(b,30)
	c = crexp(crmult(a,b))
	call crput(b,30)
	call crput(c,90)
	stop
	end
