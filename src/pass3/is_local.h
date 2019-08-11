/* test whether the identifier p is declared inside the tree with root */
/* r.                                                                  */
# define is_descendant(p,r)  \
	((((p) -> post_num) < ((r) -> post_num)) \
	&& (((p) -> pre_num) > ((r) -> pre_num)))

#define is_local(p,r)  \
	is_descendant(p -> id_last_definition, r)

