# include "parm.h"
# include <stdio.h>
# include "stree/ststructs.mh"
# include "codegen.h"
# include "op_codes.h"

void Ggen_special();

# ifdef DEBUG
#   define IFDEBUG(x) x
# else
#   define IFDEBUG(x)
# endif

extern int yydebug;
extern int yynerrs;

extern char str_code_buf[];

extern int avail_loc;

extern int Glevel;

extern FILE * Goutfile;

/* Generate code for the type construction p.  The type value is to
 * be left in rloc.
 */
void type_constr(p, rloc)
NODE *p;
int rloc;
{
    int env_loc;  /* pointer to pseudo environment for records */
    int i;

    if (p -> kind == EXTENSION) {
        int new_tp_ptr = avail_loc++;
        int old_tp_ptr = avail_loc++;
        int identity_loc = avail_loc++;
        int sz_loc = avail_loc++;
        int len = tsig_length(p -> signature);

        /* allocate an object of the right size for new type */
          gen2(DCL, sz_loc, DCL_INT);
          gen2(LDN, len, sz_loc);
          gen2(DCL, new_tp_ptr, DCL_ADDR);
          gen2(ALH, sz_loc, new_tp_ptr);
	  gen1(UDC, sz_loc);
          gen2(MOV, new_tp_ptr, rloc);
        /* put "argument" value into old_tp_ptr */
          gen2(DCL, old_tp_ptr, DCL_ADDR);
	  Gexpression(p -> ext_denotation, old_tp_ptr, FALSE);
        /* put identity function value in identity_loc    */
          gen2(DCL, identity_loc, DCL_ADDR);
	  Ggen_special(special(IDENTITY, 0), identity_loc, 0);
        /* Fill in individual fields. new_tp_ptr points to */
        /* next field to be filled in.                     */
        /* old_tp_ptr points to next unused field in arg   */
          ASSERT(p -> In_index < p -> Out_index,
                 "type_constr: bad In, Out indicees\n");
          /* Copy up to In function */
	    for (i = 0; i < p -> In_index; i++) {
	      gen3(LDI, old_tp_ptr, i, T1);
	      gen3(STI, new_tp_ptr, i, T1);
	    }
          /* Put In function in place */
	    gen3(STI, new_tp_ptr, p->In_index, identity_loc);
	  /* Copy up to Out function */
	    for (i = p -> In_index + 1; i < p -> Out_index; i++) {
	      gen3(LDI, old_tp_ptr, i-1, T1); 
	      gen3(STI, new_tp_ptr, i, T1);
	    }
          /* Put Out function in place */
	    gen3(STI, new_tp_ptr, p -> Out_index, identity_loc);
	  /* Copy the rest */
	    for (i = p -> Out_index + 1; i < len; i++) {
	      gen3(LDI, old_tp_ptr, i-2, T1); 
	      gen3(STI, new_tp_ptr, i, T1);
	    }
        /* new type value is already in rloc */
        /* undeclare temporaries */
            gen1(UDC, new_tp_ptr);
            gen1(UDC, old_tp_ptr);
            gen1(UDC, identity_loc);
        return;
    }
    if (p -> kind == RECORDCONSTRUCTION) {
        int n_components = length(p -> rec_component_list);
        int i;
        int sz_loc = avail_loc++;
        int tp_loc = avail_loc++;  /* pseudo-environment */
        int tmp_loc = avail_loc++;

        env_loc = avail_loc++;
        /* Allocate "environment" object for New, := and V */
        /* This is a vector of these 3 functions for each  */
        /* component.                                      */
          gen2(DCL, sz_loc, DCL_INT);
          gen2(DCL, env_loc, DCL_ADDR);
          gen2(LDN, 3*n_components, sz_loc);
          gen2(ALH, sz_loc, env_loc);
          gen1(UDC, sz_loc);
        /* Fill in fields in "environment"       */
          i = 0;  /* Position in p. e. */
          maplist(s, p -> rec_component_list, {
            /* evaluate field type into tmp_loc */
              gen2(DCL, tp_loc, DCL_ADDR);
	      Gexpression(s -> re_denotation, tp_loc, FALSE);
            /* copy assignment operator to p. e. */
              gen2(DCL, tmp_loc, DCL_INT);
              gen3(LDI, tp_loc, s -> re_assign_index, tmp_loc);
              gen3(STI, env_loc, i, tmp_loc);
              gen1(UDC, tmp_loc);
            /* copy New operator */
              gen2(DCL, tmp_loc, DCL_INT);
              gen3(LDI, tp_loc, s -> re_New_index, tmp_loc);
              gen3(STI, env_loc, i+1, tmp_loc);
              gen1(UDC, tmp_loc);
            /* copy ValueOf operator */
              gen2(DCL, tmp_loc, DCL_INT);
              gen3(LDI, tp_loc, s -> re_ValueOf_index, tmp_loc);
              gen3(STI, env_loc, i+2, tmp_loc);
              gen1(UDC, tmp_loc);
            i += 3;
            gen1(UDC, tp_loc);
          });
        /* env_loc points to pseudo-environment */
        /* Proceed as with other constructions: */
    }
    {
        NODE * clist = p -> signature -> ts_clist;
        int len = tsig_length(p -> signature);
        int sz_loc = avail_loc++;
        int tmp_loc = avail_loc++;

        /* allocate a new type object */
          gen2(DCL, sz_loc, DCL_INT);
          gen2(LDN, len, sz_loc);
          gen2(ALH, sz_loc, rloc);
          gen1(UDC, sz_loc);
        /* pointer to new type object is in tp_loc  */
        /* Fill in individual fields:               */
	  i = 0; /* Position to be filled in next; */
	  /* First take care of 1 character constants in enumerations */
	    {
		NODE * dcs = first(clist);

		ASSERT(dcs -> kind == DEFCHARSIGS, "type_const: bad DCS node\n");
		if (dcs -> dcs_exceptions != NIL) {
		  maplist(s, dcs -> dcs_exceptions, {
                    gen2(DCL, tmp_loc, DCL_INT);
		    Ggen_special(s -> dcse_special,
                                tmp_loc, env_loc);
		    gen3(STI, rloc, i++, tmp_loc);
                    gen1(UDC, tmp_loc);
		  });
		}
	    }
          maplist(s, clist, {
            switch(s -> kind) {
                case TSCOMPONENT:
                    gen2(DCL, tmp_loc, DCL_INT);
		    Ggen_special(s -> tsc_signature -> fsig_special,
                                tmp_loc, env_loc);
		    gen3(STI, rloc, i++, tmp_loc);
                    gen1(UDC, tmp_loc);
                    break;
	      IFDEBUG(
                case DEFCHARSIGS:
                    /* no constants */
                    break;
                default:
		    dbgmsg("type_constr: bad type constr. sig\n");
	      )
	    }
	  });
    }
    if (p -> kind == RECORDCONSTRUCTION) {
      /* Undeclare pseudo-environment location */
        gen1(UDC, env_loc);
    }
}

/* Compute the function value associated with the given special value */
/* Leave the result in rloc.                                          */
/* If a nontrivial pseudo-environment is needed, it is presumed to be */
/* in pe_loc.                                                         */
void Ggen_special(spcl, rloc, pe_loc)
unsigned spcl;
int rloc;
int pe_loc;
{
    char * routine_name;  /* name of routine for each operation */
    int n_args;           /* number of arguments to routine     */
    boolean explicit_ep = FALSE;  /* pseudo-env to be obtained from pe_loc */
    int tmp_loc = avail_loc++;

    /* Find routine name and n_args */
    /* Note that some of the runtime routines need gross hacks */
    /* in order to look at environment information             */
	switch(special_tp(spcl)) {
	    case PROD_PROJ:
	    case RECORD_VAL_FIELD:
	    case RECORD_VAR_FIELD:
		routine_name = "_P_R_ith";
		n_args = 1;
		break;

            case RECORD_MK:
	    case PROD_MK:
		routine_name = "_P_R_Make";
		    /* Needs hack in runtime code to deal with */
		    /* varying numbers of arguments.           */
		n_args = special_val(spcl);
		break;

	    case PROD_NEW:
	    case UNION_NEW:
		routine_name = "_P_U_New";
		n_args = 0;
                break;

            case RECORD_NEW:
                routine_name = "_Record_New";
                n_args = 0;
                explicit_ep = TRUE;
                break;

	    case ENUM_NEW:
		routine_name = "_E_New";
		n_args = 0;
		break;

	    case PROD_ASSIGN:
	    case UNION_ASSIGN:
            case ENUM_ASSIGN:
		routine_name = "_CF_PUE_Assign";
		n_args = 2;
                break;

            case RECORD_ASSIGN:
		routine_name = "_Record_Assign";
                n_args = 2;
                explicit_ep = TRUE;
                break;
    
	    case PROD_VALUEOF:
	    case UNION_VALUEOF:
	    case ENUM_VALUEOF:
		routine_name = "_CF_PUE_ValueOf";
		n_args = 1;
		break;
      
            case RECORD_VALUEOF:
		routine_name = "_Record_ValueOf";
                n_args = 1;
                explicit_ep = TRUE;
                break;

	    case UNION_PROJ:
		routine_name = "_Union_Proj";
		n_args = 1;
		break;
    
	    case UNION_INJ:
		routine_name = "_Union_Inj";
		n_args = 1;
		break;

	    case UNION_INQ:
		routine_name = "_Union_Inq";
		n_args = 1;
		break;

	    case ENUM_EQ:
		routine_name = "_CF_Enum_eq";
		n_args = 2;
		break;

	    case ENUM_NE:
		routine_name = "_CF_Enum_ne";
		n_args = 2;
		break;

	    case ENUM_ELEMENT:
		routine_name = "_Enum_Element";
		n_args = 0;
		break;

	    case ENUM_CARD:
		routine_name = "_Enum_Card";
		n_args = 0;
		break;

	    case ENUM_PRED:
		routine_name = "_CF_Enum_Pred";
		n_args = 1;
		break;

	    case ENUM_SUCC:
		routine_name = "_Enum_Succ";
		n_args = 1;
		break;

	    case IDENTITY:
		routine_name = "_CF_Identity";
		n_args = 1;
		break;

#         ifdef DEBUG
	    default:
	       dbgmsg("Ggen_special: Unknown special function\n");
#         endif
	}
    /* allocate new function object */
        ALLOC_FO(rloc);
        if (!explicit_ep) {
          /* Use special value as ep */
            gen2(DCL, tmp_loc, DCL_INT);
	    gen2(LDN, special_val(spcl), tmp_loc);
            gen3(STI, rloc, FO_EP, tmp_loc);
            gen1(UDC, tmp_loc);
        } else {
            gen3(STI, rloc, FO_EP, pe_loc);
        }
    /* Set up ip */
        genl(EXT, routine_name);
        gen2(DCL, tmp_loc, DCL_ADDR);
        genl(LBA, routine_name);
        gen1(LDL, tmp_loc);
        gen3(STI, rloc, FO_IP, tmp_loc);
        gen1(UDC, tmp_loc);
    /* set activation record size to number of arguments plus 1 */
        gen2(DCL, tmp_loc, DCL_INT);
        gen2(LDN, n_args + 1, tmp_loc);
	gen3(STI, rloc, FO_SIZE, tmp_loc);
        gen1(UDC, tmp_loc);
}

