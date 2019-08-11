# include <stdio.h>
# include "../parm.h"
# include "codegen.h"
# include "op_codes.h"

# define NONE ((char *) 0)

FILE * in_file;

char label_buf[MAXLABELSZ+1];

extern char * op_code_table[];

main(argc, argv)
int argc;
char ** argv;
{
    int opc;
    char * op_mnem;
    int i;
    int arg;

    if (argc == 1) {
        in_file = stdin;
    } else if (argc == 2) {
        in_file = fopen(argv[1], "r");
        if (in_file == NULL) {
            fprintf(stderr, "Can't open %s\n", argv[1]);
            exit(1);
        }
    } else {
        fprintf(stderr, "Usage: %s [%s]\n", argv[0], argv[1]);
    }
    while (!feof(in_file)) {
        opc = getw(in_file);
        if (feof(in_file)) {
            /* compensate for strange feof semantics */
            exit(0);
        }
        if (opc >= 0 && opc < N_OP_CODES) {
            op_mnem = op_code_table[opc];
        } else {
            op_mnem = NONE;
        }
        if (op_mnem == NONE) {
            printf("(%d)", opc);
        } else {
            printf("%s", op_mnem);
        }
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
	    printf("\t%s", label_buf);
        } else {
            for ( i = 0; i < 3; i++ ) {
                arg = getw(in_file);
                switch (arg) {
                    case AR:
                        printf("\t%d(AR)", AR);
                        break;
                    case SP:
                        printf("\t%d(SP)", SP);
                        break;
                    case GF:
                        printf("\t%d(GF)", GF);
                        break;
                    case UN:
                        printf("\t%d(UN)", UN);
                        break;
                    case SK:
                        printf("\t%d(SK)", SK);
                        break;
		    case TL:
			printf("\t%d(TL)", TL);
			break;
		    case RL:
                        printf("\t%d(RL)", RL);
                        break;
                    case C0:
                        printf("\t%d(C0)", C0);
                        break;
                    case C1:
                        printf("\t%d(C1)", C1);
                        break;
                    case C2:
                        printf("\t%d(C2)", C2);
                        break;
                    case C3:
                        printf("\t%d(C3)", C3);
			break;
		    case C4:
			printf("\t%d(C4)", C4);
			break;
                    default:
                        printf("\t%d", arg);
                }
            }
        }
	printf("\n");
    }
}
