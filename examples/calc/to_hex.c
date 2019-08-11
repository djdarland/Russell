/* A very simple routine to quickly convert nonnnegative Russell Long */
/* integers to a sequence of hexadecimal digits.  The result may      */
/* contain leading zeros.                                             */
/* This knows about the representation of Long integers.              */
char * to_hex(num)
register unsigned long * num;    /* Russell val Long */
{
    register unsigned long len = *num;  /* length of number */
    unsigned long buf[3];               /* actually holds characters */
    unsigned long * result = (unsigned long *) ralloc(2*len+1);
    register unsigned long *rp = &result[2*len];
    register unsigned long i;

    *rp-- = 0;  /* Trailing null */
    for (i = 1; i <= len; i++) {
	sprintf((char *)buf, "%08X", num[i]);
	*rp-- = buf[1];
	*rp-- = buf[0];
    }
    return((char *)result);
}

# ifdef UNDEFINED

unsigned long dummy[] = {3, 0x55555555, 0x2, 0x1};

main ()
{
    printf("%s\n", to_hex(dummy));
}

# endif
