
# Net effect of this is to compute r3*r4+(r5).
# The high half of the result is returned in r3.
# The low half is returned in (r5).
    .csect
    .set   r0,0
    .set   r1,1
    .set   r2,2
    .set   r3,3
    .set   r4,4
    .set   r5,5
    .set   r6,6
    .set   r7,7
    .set   r8,8
    .set   r9,9
	
    .globl	.mult_step
.mult_step:
    mul    r8,r3,r4
    l      r7,0(r5)
    mfmq   r6
#   Least signifcant word of SIGNED product is in r6.  Msw is in r8.
#   Adjust to UNSIGNED product
    	cmpi	0,r3,0
    	bge	r3pos
    	a	r8,r8,r4
r3pos:  cmpi	0,r4,0
	bge	r4pos
	a	r8,r8,r3
r4pos:
#   Add in (r5)
    a   r6,r6,r7
    aze r3,r8
    st  r6,0(r5)
    br
