/* Success indication returned by findsig */
#   define SUCCESS  0

/* Special signature field values */
#   define ERR_SIG      ((NODE *) 2)

#   define has_sig(p) ((unsigned)((p)->signature) > (unsigned)ERR_SIG)

#   define initsig(p,q) p -> signature = (q)

#   define chgsig(p,q)  p -> signature = (q)

NODE * coerce();

NODE * infer_args();

NODE * declsig();

NODE * fixhints();

NODE * prefix();

NODE * mkcompnm();

NODE * findsig();

NODE * findidsig();

NODE * findapplsig();

NODE * findmpsig();

NODE * finddecl();

NODE * tsubst();

NODE * subst();

NODE * getcomp();

NODE * sig_structure();

NODE * findstdecl();

NODE * on_dontsubst();

NODE * expand_str();

NODE * delcomp();


boolean is_const();

boolean trivial();  /* expression is safe to substitute, even if signature */
                    /* Can't be found.                                     */

char * (* spcl_to_inline)();
