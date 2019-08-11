# include "parm.h"
# include <stdio.h>
# include <string.h>

# include "stree/ststructs.mh"

extern char str_code_buf[];

extern boolean Tflag;  /* Generate trace code */

# define MAXINLINELEN 400

/* CURRENTLY RATHER PRIMITIVE */

int number_of_c();

/* Fill in the fsig_inline_code field in the function construction fc */
/* if possible.	 Assumes that signatures inside body are known.       */

void find_inline(fc)
NODE * fc;
{
    NODE * fsig = fc -> signature;
    NODE * body = fc -> fc_body;
    int code_len = 0;
    char * code;
    int n_percents = 0;   /* number of percent signs in inline code */
    NODE * p;
    int i;

#   ifdef DEBUG
	if (fc -> kind != FUNCCONSTR) {
	    dbgmsg("find_inline: bad function construction\n");
	}
#   endif
#   ifdef VERBOSE
	printf("find_inline: %s\n", fc -> fc_code_label);
#   endif
    if (fsig -> fsig_inline_code != NIL) return;
    if (body -> kind == EXTERNDEF) {
	sprintf(str_code_buf,
		"\tmovl\t$0x%%X,r11\n\tcalls\t$%d,%s\n\tpushl\tr0",
                length(fsig -> fsig_param_list),
		body -> ext_name);
	fsig -> fsig_inline_code = (char *) malloc(strlen(str_code_buf) + 1);
	strcpy(fsig -> fsig_inline_code, str_code_buf);
#       ifdef VERBOSE
	    printf("Generated inline code for external function\n");
#       endif
	return;
    }
    if (Tflag) {
        /* In-line expansion would hinder tracing */
        return;
    }
    /* find out if the body is a simple composition of functions */
        p = body;
	while (p -> kind == APPLICATION &&
	       length(p -> ap_args) == 1 &&
               first(p -> ap_args) -> kind == APPLICATION) {
	    if ((code = p -> ap_operator -> signature -> fsig_inline_code)
                 == NIL) {
	       return;	
	    }
	    code_len += strlen(code);
	    n_percents += number_of_c(code, '%');
	    p = first(p -> ap_args);
	}
	if (p -> kind == APPLICATION &&
	    length(p -> ap_args) == length(fsig -> fsig_param_list) &&
	    (code = p -> ap_operator -> signature -> fsig_inline_code) != NIL &&
	    code_len + strlen(code) <= MAXINLINELEN &&
	    n_percents + number_of_c(code,'%') <= 2) {
		map2lists(r, p -> ap_args, s, 
			  fsig -> fsig_param_list, {
                    if (r -> kind != LETTERID || !is_declared_by(r, s)) {
                        return;
		    }
		}); 
        } else {
            return;
	}
    /* function is a simple composition.  In-line code can be obtained by */
    /* concatenation.							  */
	str_code_buf[0] = '\0';
	for (p = body; p != NIL && p -> kind != LETTERID;
             p = is_empty(p -> ap_args)? NIL : first(p -> ap_args)) {
	    int len;
	    int old_len = strlen(str_code_buf);

	    code = p -> ap_operator -> signature -> fsig_inline_code;
	    len = strlen(code) + 1;
	    /* Copy old code to the right */
	        str_code_buf[len + old_len] = '\0';
		for (i = old_len - 1; i >= 0; i--) {
		    str_code_buf[i+len] = str_code_buf[i];
		}
	    /* Copy new code into place */
	        strcpy(str_code_buf, code);
		str_code_buf[len-1] = '\n';
	}
        fsig -> fsig_inline_code = (char *) malloc(strlen(str_code_buf) + 1);
        /* printf("inline code: \n%s\n",str_code_buf); */
	strcpy(fsig -> fsig_inline_code, str_code_buf);
	return;
}


/* Return the number of occurrences of the character c in the string s */
number_of_c(s,c)
char *s;
char c;
{
    char *p = s;
    char *q;
    int result = 0;

    while ((q = (char *)index(p, c)) != 0) {
	p = q + 1;
	result ++;
    }
    return(result);
}
