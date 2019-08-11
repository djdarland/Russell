extern end;

# ifdef RT
#   define is_ptr(x) ( ((unsigned) (x)) == 0 || (((unsigned) (x)) >= 0x10000000 \
						 && ((unsigned) (x)) <= 0x20000000\
						 && (((unsigned)(x)) & 1) == 0 ) )
# else
#   define is_ptr(x) ( ((unsigned) (x)) == 0 || (((unsigned) (x)) >= ((unsigned) &end) \
						 && ((unsigned) (x)) <= 0x10000000\
						 && (((unsigned)(x)) & 1) == 0 ) )
# endif


