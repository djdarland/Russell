int
inenv(s)
	char	*s;
{
	extern char	*getenv();

	return getenv(s) != (char *) 0;
}
