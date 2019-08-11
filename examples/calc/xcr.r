(* A Fortran and C interface for the constructive real type 	*)
(* Note that Fortran function names end with an underscore. 	*)
(* Russell representations of machine integers (Short) are  	*)
(* compatible with C integers.  Similarly ChStr corresponds 	*)
(* to an immutable (char * ).  All storage allocation is    	*)
(* implicit.  The garbage collector will normally be 	    	*)
(* invoked implicitly. Constructive real numbers should be	*)
(* treated as opaque 32 bit quantities by the client.  They	*)
(* should not be stored in malloc'ed data areas.  If they are 	*)
(* to be stored in dynamically allocated memory, then that 	*)
(* memory should be allocated with gc_malloc, so that the	*)
(* collector will see it.					*)
(
  Expand_Hp[Short$200];
  let
    cr == extern {"cr/fusion"}[];
  in use Long in
    cr with C {
	crmult == C$*;
	crmult_ == func[x,y: var C] { x*y };
	cradd == C$+;
	cradd_ == func[x,y: var C] { x+y };
	crsub == C$- <<func[x,y:val C]val C>>;
	crsub_ == func[x,y: var C] { x-y };
	crneg == C$- <<func[x:val C]val C>>;
	crneg_ == func[x: var C] { -x };
	crdiv == C$/;
	crdiv_ == func[x,y: var C] { x/y };
	crsqrt == C$sqrt;
	crsqrt_ == func[x: var C] { sqrt x };
	crsin == C$sin;
	crsin_ == func[x: var C; impure] { sin x };
	crcos == C$cos;
	crcos_ == func[x: var C; impure] { cos x };
	cratan == C$atan;
	cratan_ == func[x: var C; impure] { atan x };
	crexp == C$exp;
	crexp_ == func[x: var C; impure] { exp x };
	crln == C$ln;
	crln_ == func[x: var C; impure] { ln x };
	crput == func[x: val C; prec: val Short; impure]
		{ put[x, Long$In[prec]] };
		(* Print on standard out to given precision 	*)
		(* prec is in units of decimal digits to the	*)
		(* right of the decimal.			*)
	crput_ == func[x: var C; prec: var Short; impure]
		      { put[x, Long$In[prec]]; put "\n" };
	crputs == func[x: val C; prec: val Short; impure]
		      { puts[x, Long$In[prec]] };
		(* Convert to character string *)
	crin == func[x: val Short] {C$In[Long$In[x]]};
	crin_ == func[x: var Short] {C$In[Long$In[x]]};
	crpi == C$pi;
	crpi_ == C$pi;
	crcomp == func[x,y:val C; r,a: val Short; impure] val Short {
		    compare[x,y,Long$In[r],Long$In[a]]
		  };
	    (* Return 0 if x = y to within the indicated tolerance, *)
	    (* -1 if x < y, and +1 if x > y.  If x and y are indeed *)
	    (* equal, it is guaranteed that 0 will be returned.  If *)
	    (* they differ by less than the tolerance, anything     *)
	    (* may happen.  The tolerance is specified in terms     *)
	    (* of a relative tolerance r (in bits) and an absolute  *)
	    (* tolerance a (in bits).  The tolerance allowed is     *)
	    (* the maximum of (abs(x)+abs(y))*(2**r) and 2**a       *)
	crcomp_ == func[x,y:var C; r,a: var Short; impure] val Short {
		    compare[x,y,Long$In[r],Long$In[a]]
		  };
	    (* Convert to double.  Accurate to +-1 digit in the last 	*)
	    (* place.  Note that with C conventions, this returns a 	*)
	    (* pointer to a double precision value.			*)
	    (* Assumes 32 bit Short, <= 60 bit mantissa double 		*)
	    (* precision floating point, with at most IEEE exponent	*)
	    (* range.							*)
	crtodouble == func[x: val C; impure] val Float {
	  		use Short in let
			   msb_pos == most_significant_bit_pos[x,-1100];
			   offset == msb_pos-61;
			   scaled == shift[x, -offset];
			   appr == Long$New[];
			   lsp == Long$New[];
			   msp == Long$New[];
			   fp_appr == Float$New[];
			in
			   appr := to_Long[scaled, -Long$2];
			   msp := shift[appr, -Long$31];
			   lsp := appr - shift[msp, Long$31];
			   fp_appr := shift[Float$In[Out[msp]], 31]
			   	      + Float$In[Out[lsp]];
			   shift[fp_appr, offset]
			ni ni
		      };
		(* Convert the other way.  The argument is a ptr to a 	*)
		(* double.						*)
	crfromdouble == func[x: val Float; impure] val C {
			    use Short in let
			    	e == exponent[x] - 70;
			    	scaled == to_Long[shift[x, -e]];
			    in
			        shift[C$In[scaled], e]
			    ni ni
			};
    } export {
	cradd; cradd_;
	crsub; crsub_;
	crmult; crmult_;
	crdiv; crdiv_;
	crneg; crneg_;
	crsqrt; crsqrt_;
	crsin; crsin_;
	crcos; crcos_;
	cratan; cratan_;
	crexp; crexp_;
	crln; crln_;
	crput; crput_;
	crputs;
	crin; crin_;
	crpi; crpi_;
	crcomp; crcomp_;
	crtodouble; crfromdouble;
    }
  ni ni
)
