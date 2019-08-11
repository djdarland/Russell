/*
 * declarations and macros for manipulating the list decl_list of pairs of
 * declaration pointers
 */

struct p_pair {
    NODE * pp_1;
    NODE * pp_2;
};

ConsNode * decl_list;

# define clr_dlist while (!is_null_cn(decl_list)) \
              { \
		vfree(unlock(((struct p_pair *)cn_head(decl_list)) -> pp_1)); \
		vfree(unlock(((struct p_pair *)cn_head(decl_list)) -> pp_2)); \
                decl_list = cn_del_hd(decl_list); \
              }

# define add_dlist(p,q) { struct p_pair * pp_o0p /* unreproducible id */; \
                          pp_o0p = (struct p_pair *) \
                          		alloc(sizeof(struct p_pair)); \
                          pp_o0p -> pp_1 = lock(p); \
                          pp_o0p -> pp_2 = lock(q); \
                          decl_list = cn_cons((ANYTHING *)pp_o0p, decl_list); \
                        }

boolean dl_match();

NODE * dl_new_decl();
