/*
 * this program is supposed to scan a RIC file and change uses
 * of the UNIX assembler's local labels (i.e. the 1f and 2b things)
 * to globally unique labels.
 *
 * Note: all labels generated herein are of the form "L_XnnX_" where nn
 * is a number.
 *
 * The way it works:
 *	case plain ol' instruction:
 *	    if no unresolved forward refs then
 *		output instr
 *	    else
 *		save instr
 *	case "n:"
 *	    replace current definition of n with next global name;
 *	    for each forward reference to "n" do
 *		change the reference to n's global name;
 *		remove reference from list;
 *	    if no more forward refs then
 *		output saved instr list;
 *	case forward reference to "n":
 *	    put the reference on list of references to "n";
 *	case backward reference to "n":
 *	    replace the reference with n's global name;
 *
 * While this is a nice little algorithm, the implementation is
 * greatly complicated by the way LBA is defined.  
 */

# include <stdio.h>
# include <strings.h>
# include "codegen.h"
# include "op_codes.h"

# define TRUE 1
# define FALSE 0
# define MAX_TEMP_LAB 20	/* max index used for temp labels */

int last_type;			/* type of last numerical label seen */
# define REF 0			/* 	reference to a numerical label */
# define DEF 1			/* 	definition of a numerical label */

int last_num;			/* number of last numerical label */

int last_dir;			/* last numerical reference direction */
# define FWD 0			/* 	fwd ref */
# define BCK 1			/* 	back ref */

/*
 * used to save lists of instructions which can't be output yet
 * because of unresolved forward references.
 */
struct instr {
    int opc, arg[3];		/* opcode and args */
    char *label;		/* label pointer */
    struct instr *next;		/* next instruction */
};

struct instr *instr_list = (struct instr *) 0;
				/* list of saved instructions */
struct instr *tail_il = (struct instr *) 0;
				/* last elt of instr_list */

/*
 * lists of unresolved forward references for each index number
 */
struct fwd_ref {
    struct instr *inst;
    struct fwd_ref *next;
};

struct fwd_ref *fwd_refs[MAX_TEMP_LAB+1] =
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };


extern char *op_code_table[];	/* names of RIC opcodes */

char label_buf[MAXLABELSZ+1];	/* temp buffer to store labels in */
char save_label[MAXLABELSZ+1];	/* buffer to save labels in for future use */

char * crud[20];		/* used to calculate next globally unique lbl*/

/* global label names for each local label index number */
char * cur_name[MAX_TEMP_LAB+1] = 
	{ "..............", "..............", "..............",
	  "..............", "..............", "..............",
	  "..............", "..............", "..............",
	  "..............", "..............", "..............",
	  "..............", "..............", "..............",
	  "..............", "..............", "..............",
	  "..............", ".............." };

int num_fwd_refs = 0;		/* total number of unresolved fwd refs */
int lbl_num = 0;		/* used to generate labels */
int read_ahead = FALSE;		/* => already read next opcode */
int saved_opc;			/* opcode which was read */

FILE *in_file, *out_file;	/* input file, output file */

/*#define DEBUG		generate voluminous output */

#ifdef DEBUG
#   define dbg_msg(s) 		printf(s); 		fflush(stdout)
#   define dbg_msg2(s,x) 	printf(s,x);		fflush(stdout)
#   define dbg_msg3(s,x,y) 	printf(s,x,y);		fflush(stdout)
#   define dbg_msg4(s,x,y,z)	printf(s,x,y,z);	fflush(stdout)
#else
#   define dbg_msg(x)
#   define dbg_msg2(s,x)
#   define dbg_msg3(s,x,y)
#   define dbg_msg4(s,x,y,z)
#endif




main(argc, argv)
int argc;
char ** argv;
{
    int opc,			/* current opcode */
	arg[3];			/* args */
    char * op_mnem;		/* mnemonic for current opcode */
    struct instr *newone;	/* used to allocate new instr structs */
    struct fwd_ref *cur_ref;	/* used to resolve fwd refs */

    int last_was_def= FALSE,	/* => last instr defined a local label */
	last_was_lba= FALSE;	/* => last instr was LBA */
    int input_index = 1,
	i;
    char *temp;			/* used to free heap space */

  /* process command-line arguments */
    if (argc == 1) {
        in_file = stdin;
	out_file = stdout;
    } else if (argc <= 3) {
	if (argv[1][0] == '-') {
	    int res = 0;

	    for (i = 1; i < strlen(argv[1])-1; i++) {
		if (argv[1][i] < '0' || argv[1][i] > '9') {
		    fprintf(stderr,"invalid label num argument\n");
		    usage(argv[0]);
		} else {
		    res = (10 * res) + (argv[1][i] - '0');
		}
	    }
	    lbl_num = res;
	}
        in_file = fopen(argv[1], "r");
	out_file =
	    (argc == 2) ? stdout : fopen(argv[2], "w");

        if (in_file == NULL || out_file == NULL) {
            fprintf(stderr, "Can't open %s\n", argv[1]);
	    usage(argv[0]);
        }
    } else {
        fprintf(stderr, "Usage: %s [%s]\n", argv[0], argv[1]);
    }

  /* read through file and translate */
    while (!feof(in_file)) {
	int i;

	if (read_ahead) {
	    opc = saved_opc;
	    read_ahead = FALSE;
	} else {
 	    opc = getw(in_file);		/* get opcode */
	}
        if (feof(in_file)) {
            /* compensate for strange feof semantics */
            exit(0);
        }

      /* get name of opcode */
	if (opc >= 0 && opc <= N_OP_CODES) op_mnem = op_code_table[opc];

      /* init potential arguments */
	*label_buf = '\0';
	for (i = 0; i < 3; i++) arg[i] = 0;

      /* output preceding LBA, changing label if necessary */
	if (last_was_lba) {
	    char *lba_lbl;

	    if (last_was_def) {
		lba_lbl = cur_name[last_num];
	    } else {
		lba_lbl = save_label;
	    }
	    if (num_fwd_refs == 0) {
		put_lbl_op(LBA,lba_lbl);
		dbg_msg2("LBA %s\n",lba_lbl);
	    } else {
		newone = (struct instr *) malloc(sizeof(struct instr));
		save_it(newone,LBA,arg,lba_lbl);
	    }
	}

      /* opcode has one operand which is a label */
	last_was_lba = FALSE;
	if (opc <= MAX_LABEL_OP) {
	    char *p = label_buf;
	    int c;
	    int i = 0;
	    
	    while ((c = getc(in_file)) != '\0' && c != EOF) {
		*p++ = c;
		if (++i >= MAXLABELSZ) {
		    fprintf(stderr, "Label too long\n");
		    p = label_buf;
		}
	    }
	    *p = '\0';

	    if (opc == LBA) {
		last_was_lba = TRUE;
		dbg_msg("opc == LBA\n");
		dbg_msg2("label_buf = %s\n",label_buf);
		strcpy(save_label,label_buf);
	    }

	    if (normal_label(opc,label_buf)) {
		if (opc != LBA) {
		    if (num_fwd_refs == 0) {
			put_lbl_op(opc,label_buf);
			dbg_msg3("%s %s\t(normal)\n",op_mnem,label_buf);
		    } else {
			newone = (struct instr *) malloc(sizeof(struct instr));
			save_it(newone,opc,arg,label_buf);
		    }
		} else {
		    last_was_def = FALSE;
		}
	    } else {
		if (last_type == DEF) {
		    last_was_def = (opc == LBA);

		    dbg_msg3("assigned L_X%dX_ to %d\n",lbl_num,last_num);
		    sprintf(crud,"L_X%dX_",lbl_num++);
		    strcpy(cur_name[last_num],crud);
		    cur_ref = fwd_refs[last_num];

		  /* output labelling op unless == LBA */
		    if (opc != LBA) {
			if (num_fwd_refs == 0) {
			    put_lbl_op(opc,cur_name[last_num]);
			} else {
			    newone = 
				(struct instr *) malloc(sizeof(struct instr));
			    save_it(newone,opc,arg,cur_name[last_num]);
			}
		    }
		    dbg_msg3("resolving fwd refs %d to %s\n",
					last_num,cur_name[last_num]);

		  /* resolve earlier references to this (now known) label */
		    while (cur_ref != (struct fwd_ref *) NULL) {
			strcpy( (cur_ref->inst)->label, cur_name[last_num]);
			temp = (char *) cur_ref;
			cur_ref = cur_ref->next;
			free(temp);
			num_fwd_refs--;
		    }
		    fwd_refs[last_num] = (struct fwd_ref *) NULL;

		    if (num_fwd_refs == 0) {
			struct instr *cur = instr_list;
			while (cur != (struct instr *) NULL) {
			    if (cur->opc < MAX_LABEL_OP) {
				put_lbl_op(cur->opc,cur->label);
				dbg_msg3("%s %s\t(saved)\n",
					op_code_table[cur->opc],cur->label);
			    } else if (cur->opc < N_OP_CODES) {
				put_reg_op(cur->opc,cur->arg);
				dbg_msg2("%s ",op_code_table[cur->opc]);
				dbg_msg4("%d %d %d\t(saved)\n",
					cur->arg[0],cur->arg[1],cur->arg[2]);
			    } else {
				fprintf(stderr,
					"Bad op code in instr_list = %d\n",
					cur->opc);
			    }
			    instr_list = tail_il = (struct instr *) NULL;
			    temp = (char *) cur;
			    cur = cur->next;
			    free(temp);
			}
		    } else {
			dbg_msg2("now num_fwd_refs = %d\n",num_fwd_refs);
		    }
		} else if (opc != LBA) {
		    if (last_dir == FWD) {
			newone =
			    (struct instr *) malloc(sizeof(struct instr));
			i = (int) malloc(sizeof(struct fwd_ref));
			cur_ref = (struct fwd_ref *) i;

			cur_ref->inst = newone;
			cur_ref->next = fwd_refs[last_num];
			fwd_refs[last_num] = cur_ref;
			save_it(newone,opc,arg,label_buf);
			num_fwd_refs++;
		    } else {
			dbg_msg3("resolved back ref to %d = %s\n",last_num,
						cur_name[last_num]);
			strcpy(label_buf, cur_name[last_num]);
			if (num_fwd_refs == 0) {
			    put_lbl_op(opc,label_buf);
			    dbg_msg3("%s %s\t(back ref)\n",op_mnem,label_buf);
			} else {
			    newone =
				(struct instr *) malloc(sizeof(struct instr));
			    save_it(newone,opc,arg,label_buf);
			}
		    }
		}
	    } /* end if (normal...) */

      /* opcode has registers as arguments */
        } else {
            for ( i = 0; i < 3; i++ ) {
                arg[i] = getw(in_file);
            }
	    if (num_fwd_refs == 0) {
		put_reg_op(opc,arg);
		dbg_msg2("%s ",op_mnem);
		dbg_msg4("%d %d %d\t(plain)\n",arg[0],arg[1],arg[2]);
	    } else {
		newone = (struct instr *) malloc(sizeof(struct instr));
		save_it(newone,opc,arg,label_buf);
	    }
        }

    }
}


/* save instruction in instr_list */

save_it(newone,opc,arg,label_buf)
    struct instr *newone;
    int opc,arg[3];
    char *label_buf;
{
    struct instr *temp;

  /* save instruction at head of instr_list */
    if (newone == (struct instr *) NULL) {
	fprintf(stderr,"Internal: (nil) newone passed to save_it\n");
	fflush(stderr);
	exit(1);
    } else {
	newone->next = (struct instr *) NULL;
	newone->opc = opc;
	newone->arg[0] = arg[0];
	newone->arg[1] = arg[1];
	newone->arg[2] = arg[2];
	newone->label = (char *) malloc(MAXLABELSZ+1);  /*will change if fwd*/
	strcpy(newone->label,label_buf);

	if (tail_il != (struct instr *) NULL) tail_il->next = newone;
	tail_il = newone;
	if (instr_list == (struct instr *) NULL) instr_list = tail_il;
    }

    dbg_msg("il = ");
    for(temp = instr_list; temp != NULL; temp = temp->next) {
	dbg_msg2("(%s",op_code_table[temp->opc]);
	dbg_msg4(" %d %d %d ",temp->arg[0],temp->arg[1],temp->arg[2]);
	dbg_msg2("%s), ",temp->label);
    }
    dbg_msg("\n");
}

/*
 * return next opcode in input
 * used for lookahead by normal_label
 */
int next_opc() {
    if (read_ahead) {
	fprintf(stderr,"tried to read ahead when read_ahead already set!\n");
	exit(1);
    }
    read_ahead = TRUE;
    saved_opc = getw(in_file);
    return(saved_opc);
}

/*
 * return TRUE if label is not a "local reference" label (i.e. we should
 * leave it alone).
 */
int normal_label(opc,p)
    int opc;
    char *p;
{
    char *s;

  /* if this is an LBA for an LDS, then it's data, not a label */
    if (opc == LBA && next_opc() == LDS) return(TRUE);
    
    last_num = 0;
    for (s = p; *s != '\0'; s++) {
	if (*s >= '0' && *s <= '9') {
	    last_num = 10 * last_num + *s - '0';
	} else if (s != p && (*s == 'f' || *s == 'b') && (*(s+1) == '\0')) {
	    last_type = REF;
	    last_dir = (*s == 'f') ? FWD : BCK;
	    dbg_msg3("found a %s ref to %d\n",
			(last_dir==FWD)?"fwd":"back",last_num);
	    return(FALSE);
	} else {
	    return(TRUE);	/* any non-numerical char => normal label */
	}
    }
    if (opc == LBA || opc == LBL) {
	last_type = DEF;
	dbg_msg2("found a def of %d\n",last_num);
	dbg_msg3("%s %s\n",op_code_table[opc],p);
    	return(FALSE);
    } else {
	return(TRUE);
    }
}

/*
 * put a RIC statement with registers args
 */
put_reg_op(opc,arg)
    int opc, arg[];
{
    putw(opc,out_file);
    putw(arg[0],out_file);
    putw(arg[1],out_file);
    putw(arg[2],out_file);
}

/*
 * put a RIC statement with label arg
 *
 * because the RT adb chokes on "_.", I substitute "__" for it everywhere
 */
put_lbl_op(opc,label)
    int opc;
    char *label;
{
    char *fixit();

    putw(opc,out_file);
    fputs(fixit(label),out_file);
    putc('\0',out_file);
}


/* replace "_." by "__" everywhere in str */
char *fixit(str)
char *str;
{
    char *p;
    char *q = (char *) malloc(strlen(str)+1);
    int state = 0, offset=0;

    for (p = str; *p != '\0'; p++, offset++) {
	if (state == 0) {
	    if (*p == '_') state = 1;
	    *(q+offset) = *p;
	} else {
	    if (*p == '.') {
		*(q+offset) = '_';
	    } else
		*(q+offset) = *p;
	    if (*p != '_') state = 0;
	}
    }
    *(q+offset) = '\0';
/*    fprintf(stderr,"changed %s to %s\n",str,q);*/
    return(q);
}


usage(name)
char *name;
{
    fprintf(stderr,"usage: %s [-nnn] infile [outfile]\n",name);
    exit(1);
}
