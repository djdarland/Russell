/*
 * bitcnt(n) returns the number of bits set in n.
 */
int bitcnt(n)
register int n;
{
    register int count = 0;

    while (n != 0) {
        if (n < 0) count++;
        n <<= 1;
    }
    return(count);
}
