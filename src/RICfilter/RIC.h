#define MAXLABELSZ 512
/* Internal representation of intermediate code sequences */
struct RIC_instr {
    int op_code;
    int arg[3];                /* Only used if label_arg = FALSE */
    struct RIC_instr * next_instr;
    struct RIC_instr * prev_instr;
    /* Fields set by set_regs */
	int result_reg;     /* The result register */
	int op1_reg;        /* Positions of operand registers, i.e. 0,1,2 */
	int op2_reg;        /* or NONE                                    */
#       define NONE -1
	boolean side_effect;
    boolean label_arg;
    boolean second_decl;    /* This is a redeclaration of a previously used */
			    /* location.                                    */
    char label[1];             /* Only used if label_arg = TRUE  */
} RIC_instr;

# define RIC_nil ((struct RIC_instr *)0)

