func [
    argc: val Short;
    trace: var Boolean;
    argv: func[val Short] val ChStr;
    eof: func[var Void] val Boolean;
    abort: func [] val Void;
    Void: type (simple) {};
    Null: func (inline "\tpushl\t$0") [] val Void;
    FS: var Void;

    Boolean: type (simple) B {
      (* constants *)
	False:  func (inline "\tpushl\t$0") [] val B;
	True:   func (inline "\tpushl\t$1") [] val B;
      (* operations *)
	:= :    func (standard := 1)
		     (inline "\tmovl\t4(sp),*(sp)+")
		     [var B; val B] val B;
	<> :    func (inline "\txorl2\t(sp)+,(sp)")
		     [x,y: val B] val B;
	= :     func (inline "\txorl2\t(sp)+,(sp)\n\tmcoml\t(sp),(sp)")
		     [x,y: val B] val B;
	New:    func (standard New 1)
		     (inline "\tmovl\t_objfreelist+4,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$1\n\tcalls\t$1,_allocobj\n1:\tmovl\t(r0),_objfreelist+4\n\tmovl\t$0,(r0)\n\tpushl\tr0")
                     [] var B;
	New:    func (standard init_New 1)
		     (inline "\tmovl\t_objfreelist+4,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$1\n\tcalls\t$1,_allocobj\n1:\tmovl\t(r0),_objfreelist+4\n\tmovl\t(sp)+,(r0)\n\tpushl\tr0")
		     [val B] var B;
	V:      func (standard V 1)
		     (inline "\tmovl\t*(sp),(sp)")
		     [var B] val B;
	and:    func (inline "\tmcoml\t(sp),(sp)\n\tbicl2\t(sp)+,(sp)")
		     [x,y:val B] val B;
        not:    func (inline "\tbicl3\t(sp),$1,(sp)")
		     [val B] val B;
	or:     func (inline "\tbisl2\t(sp)+,(sp)")
                     [x,y:val B] val B;
	put :   func (standard put 0)
		     (inline "\t.globl\t_Bool_Put\n\tcalls\t$1,_Bool_Put\n\tpushl\tr0")
                     [val B] val B;
        puts :  func (inline "\t.globl\t_Bool_Puts\n\tcalls\t$1,_Bool_Puts\n\tpushl\tr0")
		     [val B] val ChStr;
    };
    
    Short: type (simple
		 constants "\tpushl\t$%c"
			   "\tmovzwl\t$%s,-(sp)"
			   "%c") S {
      (* constants *)
	'0' :   func [] val S;
	'1' :   func [] val S;
	'2' :   func [] val S;
	'3' :   func [] val S;
	'4' :   func [] val S;
	'5' :   func [] val S;
	'6' :   func [] val S;
	'7' :   func [] val S;
	'8' :   func [] val S;
	'9' :   func [] val S;
      (* operations *)
        % :     func (inline "\tdivw3\t4(sp),(sp),r0\n\tmulw2\t4(sp),r0\n\tsubw3\tr0,(sp),4(sp)\t\naddl2\t$4,sp")
                     [x,y:val S] val S;
	* :     func (inline "\tmulw2\t(sp),4(sp)\n\taddl2\t$4,sp")
                     [x,y:val S] val S;
        ** :    func (inline "\t.globl\t_Short_Exp\n\tcalls\t$2,_Short_Exp\n\tpushl\tr0")
                     [x,y:val S] val S;
	+ :     func (inline "\taddw2\t(sp),4(sp) #ADD\n\taddl2\t$4,sp")
                     [x,y:val S] val S;
	+= :    func (inline "\taddw2\t4(sp),*(sp)\n\tmovl*(sp),4(sp)\n\taddl2\t$4,sp")
                     [var S; val S] val S;
        - :     func (inline "\tmnegw\t(sp),(sp)")
                     [val S] val S;
	- :     func (inline "\tsubw3\t4(sp),(sp),4(sp) #SUB\n\taddl2\t$4,sp")
                     [x,y:val S] val S;
	-= :    func (inline "\tsubw2\t4(sp),*(sp)\n\tmovl*(sp),4(sp)\n\taddl2\t$4,sp")
                     [var S; val S] val S;
	/ :     func (inline "\tdivw3\t4(sp),(sp),4(sp)\n\taddl2\t$4,sp")
		     [x,y:val S] val S;
	:= :    func (standard := 1)
		     (inline "\tmovl\t4(sp),*(sp)+")
		     [var S; val S] val S;
	< :     func (inline "\taddl2 $8,sp; cmpw\t-8(sp),-4(sp) #COMP jgeq 4\n\tmovpsl\tr0\n\trotl\t$-3,r0,r0\n\tbicl3\t$0xfffffffe,r0,-(sp)")
		     [x,y:val S] val Boolean;
	<= :    func (inline "\taddl2 $8,sp; cmpw\t-4(sp),-8(sp) #COMP jlss 4\n\tmovpsl\tr0\n\trotl\t$-3,r0,r0\n\tbicl3\tr0,$0x1,-(sp)")
		     [x,y:val S] val Boolean;
	<> :    func (inline "\tcmpl\t(sp)+,(sp)+ #COMP jeql 4\n\tmovpsl\tr0\n\trotl\t$-2,r0,r0\n\tbicl3\tr0,$0x1,-(sp)")
		     [x,y:val S] val Boolean;
	= :     func (inline "\tcmpl\t(sp)+,(sp)+ #COMP jneq 4\n\tmovpsl\tr0\n\trotl\t$-2,r0,r0\n\tbicl3\t$0xfffffffe,r0,-(sp)")
		     [x,y:val S] val Boolean;
	> :     func (inline "\taddl2 $8,sp; cmpw\t-4(sp),-8(sp) #COMP jgeq 4\n\tmovpsl\tr0\n\trotl\t$-3,r0,r0\n\tbicl3\t$0xfffffffe,r0,-(sp)")
		     [x,y:val S] val Boolean;
	>= :    func (inline "\taddl2 $8,sp; cmpw\t-8(sp),-4(sp) #COMP jlss 4\n\tmovpsl\tr0\n\trotl\t$-3,r0,r0\n\tbicl3\tr0,$0x1,-(sp)")
		     [x,y:val S] val Boolean;
	New:    func (standard New 1)
		     (inline "\tmovl\t_objfreelist+4,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$1\n\tcalls\t$1,_allocobj\n1:\tmovl\t(r0),_objfreelist+4\n\tmovl\t$0,(r0)\n\tpushl\tr0")
		     [] var S;
	New:    func (standard init_New 1)
		     (inline "\tmovl\t_objfreelist+4,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$1\n\tcalls\t$1,_allocobj\n1:\tmovl\t(r0),_objfreelist+4\n\tmovl\t(sp)+,(r0)\n\tpushl\tr0")
		     [val S] var S;
	V:      func (standard V 1)
		     (inline "\tmovl\t*(sp),(sp)")
		     [var S] val S;
        ^+ :    func (inline "\tmulw3\t(sp),$10,r0\n\taddw2\tr0,4(sp)\n\taddl2\t$4,sp")
                     [x,y: val S] val S;
	get :   func (inline "\t.globl\t_Short_Get\n\tcalls\t$1,_Short_Get\n\tpushl\tr0")
		     [var Void] val S;
	put :   func (standard put 0)
		     (inline "\t.globl\t_Short_Put\n\tcalls\t$1,_Short_Put\n\tpushl\tr0")
                     [val S] val S;
        puts:   func (inline "\t.globl\t_Short_Puts\n\tmovl\t$0x%X,r11\n\tcalls\t$1,_Short_Puts\n\tpushl\tr0")
                     [val S] val ChStr;
        shift : func (inline "\tcvtwl\t(sp),(sp)\n\tashl\t4(sp),(sp),4(sp)\n\taddl2\t$4,sp\n\tclrw\t2(sp)")
                     [x,y: val S] val S;
    };

    (* The following two types are essentially identical on the Vax. *)
    (* Their implementation differs on other machines.               *)
    SFloat: type (simple) F {
      (* operations *)
        * :     func (inline "\tmovl\t_aobjfreelist+8,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$2\n\tcalls\t$1,_allocaobj\n1:\tmovl\t(r0),_aobjfreelist+8\n\tmuld3\t*(sp)+,*(sp),(r0)\n\tmovl\tr0,(sp)")
                     [x,y:val F] val F;
        + :     func (inline "\tmovl\t_aobjfreelist+8,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$2\n\tcalls\t$1,_allocaobj\n1:\tmovl\t(r0),_aobjfreelist+8\n\taddd3\t*(sp)+,*(sp),(r0)\n\tmovl\tr0,(sp)")
                     [x,y:val F] val F;
        - :     func (inline "\tmovl\t_aobjfreelist+8,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$2\n\tcalls\t$1,_allocaobj\n1:\tmovl\t(r0),_aobjfreelist+8\n\tmnegd\t*(sp),(r0)\n\tmovl\tr0,(sp)")
                     [val F] val F;
        - :     func (inline "\tmovl\t_aobjfreelist+8,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$2\n\tcalls\t$1,_allocaobj\n1:\tmovl\t(r0),_aobjfreelist+8\n\tmovl\t4(sp),r1\n\tsubd3\t(r1),*(sp)+,(r0)\n\tmovl\tr0,(sp)")
                     [x,y:val F] val F;
        . :     func (inline "\t.globl\t_Float_Dot\n\tmovl\t$0x%X,r11\n\tcalls\t$3,_Float_Dot\n\tpushl\tr0")
                     [whole, fraction, fraction_length: val Short] val F;
        / :     func (inline "\tmovl\t_aobjfreelist+8,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$2\n\tcalls\t$1,_allocaobj\n1:\tmovl\t(r0),_aobjfreelist+8\n\tmovl\t4(sp),r1\n\tdivd3\t(r1),*(sp)+,(r0)\n\tmovl\tr0,(sp)")
                     [x,y:val F] val F;
        := :    func (inline "\tmovd\t*4(sp),*(sp)+")
                     [var F; val F] val F;
        < :     func [x,y:val F] val Boolean;
        <= :    func [x,y:val F] val Boolean;
        <> :    func [x,y:val F] val Boolean;
        = :     func [x,y:val F] val Boolean;
        > :     func [x,y:val F] val Boolean;
        >= :    func [x,y:val F] val Boolean;
        In:     func (inline "\tmovl\t_aobjfreelist+8,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$2\n\tcalls\t$1,_allocaobj\n1:\tmovl\t(r0),_aobjfreelist+8\n\tcvtwd\t(sp),(r0)\n\tmovl\tr0,(sp)")
                     [val Short] val F;
	New:    func (inline "\tmovl\t_aobjfreelist+8,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$2\n\tcalls\t$1,_allocaobj\n1:\tmovl\t(r0),_aobjfreelist+8\n\tmovl\t$0,(r0)\n\tpushl\tr0")
		     [] var F;
	New:    func [val F] var F;
	Out:    func (inline "\t.globl\t_Float_Out\n\tcalls\t$1,_Float_Out\n\tpushl\tr0")
		     [val F] val Short;
        V:      func (inline "\tmovl\t_aobjfreelist+8,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$2\n\tcalls\t$1,_allocaobj\n1:\tmovl\t(r0),_aobjfreelist+8\n\tmovd\t*(sp),(r0)\n\tmovl\tr0,(sp)")
		     [var F] val F;
	atan:   func [val F] val F;
	cos:    func [val F] val F;
	exp:    func [val F] val F;
	exponent: func (inline "\t.globl\t_Float_Exponent\n\tcalls\t$1,_Float_Exponent\n\tpushl\tr0")
		       [val F] val Short;
	get :   func [var Void] val F;
	ln:     func [val F] val F;
	put :   func (standard put 0)
		     (inline "\t.globl\t_Float_Put\n\tcalls\t$1,_Float_Put\n\tpushl\tr0")
		     [val F] val F;
	puts:   func [val F] val ChStr;
	shift:  func [val F; val Short] val F;
	sin:    func [val F] val F;
	sqrt:    func [val F] val F;
	to_Float: func (inline " ")
		       [val F] val Float;
	to_Long: func (inline "\t.globl\t_Float_to_Long\n\tmovl\t$0x%X,r11\n\tcalls\t$1,_Float_to_Long\n\tpushl\tr0")
		      [val F] val Long;
    };

    Float: type (simple) F {
      (* operations *)
        * :     func (inline "\tmovl\t_aobjfreelist+8,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$2\n\tcalls\t$1,_allocaobj\n1:\tmovl\t(r0),_aobjfreelist+8\n\tmuld3\t*(sp)+,*(sp),(r0)\n\tmovl\tr0,(sp)")
                     [x,y:val F] val F;
        + :     func (inline "\tmovl\t_aobjfreelist+8,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$2\n\tcalls\t$1,_allocaobj\n1:\tmovl\t(r0),_aobjfreelist+8\n\taddd3\t*(sp)+,*(sp),(r0)\n\tmovl\tr0,(sp)")
                     [x,y:val F] val F;
        - :     func (inline "\tmovl\t_aobjfreelist+8,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$2\n\tcalls\t$1,_allocaobj\n1:\tmovl\t(r0),_aobjfreelist+8\n\tmnegd\t*(sp),(r0)\n\tmovl\tr0,(sp)")
                     [val F] val F;
        - :     func (inline "\tmovl\t_aobjfreelist+8,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$2\n\tcalls\t$1,_allocaobj\n1:\tmovl\t(r0),_aobjfreelist+8\n\tmovl\t4(sp),r1\n\tsubd3\t(r1),*(sp)+,(r0)\n\tmovl\tr0,(sp)")
                     [x,y:val F] val F;
        . :     func (inline "\t.globl\t_Float_Dot\n\tmovl\t$0x%X,r11\n\tcalls\t$3,_Float_Dot\n\tpushl\tr0")
                     [whole, fraction, fraction_length: val Short] val F;
        / :     func (inline "\tmovl\t_aobjfreelist+8,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$2\n\tcalls\t$1,_allocaobj\n1:\tmovl\t(r0),_aobjfreelist+8\n\tmovl\t4(sp),r1\n\tdivd3\t(r1),*(sp)+,(r0)\n\tmovl\tr0,(sp)")
                     [x,y:val F] val F;
        := :    func (inline "\tmovd\t*4(sp),*(sp)+")
                     [var F; val F] val F;
        < :     func [x,y:val F] val Boolean;
        <= :    func [x,y:val F] val Boolean;
        <> :    func [x,y:val F] val Boolean;
        = :     func [x,y:val F] val Boolean;
        > :     func [x,y:val F] val Boolean;
        >= :    func [x,y:val F] val Boolean;
        In:     func (inline "\tmovl\t_aobjfreelist+8,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$2\n\tcalls\t$1,_allocaobj\n1:\tmovl\t(r0),_aobjfreelist+8\n\tcvtwd\t(sp),(r0)\n\tmovl\tr0,(sp)")
                     [val Short] val F;
	New:    func (inline "\tmovl\t_aobjfreelist+8,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$2\n\tcalls\t$1,_allocaobj\n1:\tmovl\t(r0),_aobjfreelist+8\n\tmovl\t$0,(r0)\n\tpushl\tr0")
		     [] var F;
	New:    func [val F] var F;
	Out:    func (inline "\t.globl\t_Float_Out\n\tcalls\t$1,_Float_Out\n\tpushl\tr0")
		     [val F] val Short;
        V:      func (inline "\tmovl\t_aobjfreelist+8,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$2\n\tcalls\t$1,_allocaobj\n1:\tmovl\t(r0),_aobjfreelist+8\n\tmovd\t*(sp),(r0)\n\tmovl\tr0,(sp)")
		     [var F] val F;
	atan:   func [val F] val F;
	cos:    func [val F] val F;
	exp:    func [val F] val F;
	exponent: func (inline "\t.globl\t_Float_Exponent\n\tcalls\t$1,_Float_Exponent\n\tpushl\tr0")
		       [val F] val Short;
	get :   func [var Void] val F;
	ln:     func [val F] val F;
	put :   func (standard put 0)
		     (inline "\t.globl\t_Float_Put\n\tcalls\t$1,_Float_Put\n\tpushl\tr0")
		     [val F] val F;
	puts:   func [val F] val ChStr;
	shift:  func [val F; val Short] val F;
	sin:    func [val F] val F;
	sqrt:    func [val F] val F;
	to_Long: func (inline "\t.globl\t_Float_to_Long\n\tmovl\t$0x%X,r11\n\tcalls\t$1,_Float_to_Long\n\tpushl\tr0")
		      [val F] val Long;
	to_SFloat: func (inline " ")
			[val F] val SFloat;
    };

    ChStr: type (simple
		 constants "\t.globl\t_ChStr_v_%d\n\tpushl\t$_ChStr_v_%d"
			   "\tjbr\t1f\n2:%s\t.byte\t0\n1:\tpushl\t$2b"
                           "\t.byte\t%d\n")
		CS {
	characters;
	'' :     func (inline "\t.globl\t_empty_ChStr\tpushl\t$_empty_ChStr")
		      [] val CS;
	. :      func (inline "\t.globl\t_ChStr_Dot\n\tmovl\t0x%X,r11\n\tcalls\t$2,_ChStr_Dot\n\tpushl\tr0")
		      [val CS; val Short] val Short;
	:= :     func (inline "\tmovl\t4(sp),*(sp)+")
                      [var CS; val CS] val CS;
        <  :     func (inline "\t.globl\t_ChStr_lt\n\tmovl\t$0x%X,r11\n\tcalls\t$2,_ChStr_lt\n\tpushl\tr0")
                      [val CS; val CS] val Boolean;
        <= :     func (inline "\t.globl\t_ChStr_le\n\tmovl\t$0x%X,r11\n\tcalls\t$2,_ChStr_le\n\tpushl\tr0")
                      [val CS; val CS] val Boolean;
        <> :     func (inline "\t.globl\t_ChStr_ne\n\tmovl\t$0x%X,r11\n\tcalls\t$2,_ChStr_ne\n\tpushl\tr0")
                      [val CS; val CS] val Boolean;
        =  :     func (inline "\t.globl\t_ChStr_eq\n\tmovl\t$0x%X,r11\n\tcalls\t$2,_ChStr_eq\n\tpushl\tr0")
                      [val CS; val CS] val Boolean;
        >  :     func (inline "\t.globl\t_ChStr_gt\n\tmovl\t$0x%X,r11\n\tcalls\t$2,_ChStr_gt\n\tpushl\tr0")
                      [val CS; val CS] val Boolean;
        >= :     func (inline "\t.globl\t_ChStr_ge\n\tmovl\t$0x%X,r11\n\tcalls\t$2,_ChStr_ge\n\tpushl\tr0")
                      [val CS; val CS] val Boolean;
        In:      func (inline "\tmovl\t_aobjfreelist+4,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$1\n\tcalls\t$1,_allocaobj\n1:\tmovl\t(r0),_aobjfreelist+4\n\tmovzbl\t(sp),(r0)\n\tmovl\tr0,(sp)")
                      [val Short] val CS;
	New:     func (standard ptr_New 1)
		      (inline "\tmovl\t_objfreelist+4,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$1\n\tcalls\t$1,_allocobj\n1:\tmovl\t(r0),_objfreelist+4\n\t.globl\t_empty_ChStr\n\tmovl\t$_empty_ChStr,(r0)\n\tpushl\tr0")
		      [] var CS;
	New:     func (standard init_New 1)
		      (inline "\tmovl\t_objfreelist+4,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$1\n\tcalls\t$1,_allocobj\n1:\tmovl\t(r0),_objfreelist+4\n\t.globl\t_empty_ChStr\n\tmovl\t(sp)+,(r0)\n\tpushl\tr0")
		      [val CS] var CS;
        Out:     func (inline "\tmovzbl\t*(sp),(sp)")
                      [val CS] val Short;
        V:       func (inline "\tmovl\t*(sp),(sp)")
		      [var CS] val CS;
	^* :     func (inline "\t.globl\t_ChStr_Concat\n\tmovl\t$0x%X,r11\n\tcalls\t$2,_ChStr_Concat\n\tpushl\tr0")
		      [x,y: val CS] val CS;
        getchar: func (inline "\t.globl\t_ChStr_Getchar\n\tmovl\t$0x%X,r11\n\tcalls\t$1,_ChStr_Getchar\n\tpushl\tr0")
                      [var Void] val CS;
        len:     func (inline "\t.globl\t_strlen\n\tcalls\t$1,_strlen\n\tpushl\tr0")
                      [val CS] val Short;
	put:     func (standard put 0)
		      (inline "\t.globl\t_ChStr_Put\n\tcalls\t$1,_ChStr_Put\n\tpushl\tr0")
                      [val CS] val CS;
        substr:  func (inline "\t.globl\t_ChStr_Sub\n\tmovl\t$0x%X,r11\n\tcalls\t$3,_ChStr_Sub\n\tpushl\tr0")
                      [val CS; i,j: val Short] val CS;
    };

    Alias: func (inline "\tcmpl\t(sp)+,(sp)+; popr\t$1 #COMP jneq 4\n\tmovpsl\tr0\n\trotl\t$-2,r0,r0\n\tbicl3\t$0xfffffffe,r0,-(sp)")
                [x,y: var T; T: type {}] val Boolean;

    Array: func (standard Array 0)
		[size: val Short; comptype: type {New; V}]
		type A {. : func(inline "\tcmpl\t4(sp),*(sp) #---\n\tbgeq\t1f #---\n\ttstl\t4(sp) #---\n\tbgeq\t2f #---\n1:\taddl2\t$4,sp #---\n\t.globl\t_Array_error #---\n\tcalls\t$1,_Array_error #---\n2:\taddl3\t$1,4(sp),r0\n\tmovl\t*(sp)+[r0],(sp)")
				[var A; val Short] var comptype;
			. : func(inline "\tcmpl\t4(sp),*(sp) #---\n\tbgeq\t1f #---\n\ttstl\t4(sp) #---\n\tbgeq\t2f #---\n1:\taddl2\t$4,sp #---\n\t.globl\t_Array_error #---\n\tcalls\t$1,_Array_error #---\n2:\taddl3\t$1,4(sp),r0\n\tmovl\t*(sp)+[r0],(sp)")
				[val A; val Short] val comptype;
			New;
			V;
			size: func [] val Short;
                       };

    Ref: func [T: type {}]
              type t { = :  func (inline "\tcmpl\t(sp)+,(sp)+ #COMP jneq 4\n\tmovpsl\tr0\n\trotl\t$-2,r0,r0\n\tbicl3\t$0xfffffffe,r0,-(sp)")
                                 [val t; val t] val Boolean;
                       := : func (inline "\tmovl\t4(sp),*(sp)+")
                                 [var t; val t] val t;
                       New: func (inline "\tmovl\t_objfreelist+4,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$1\n\tcalls\t$1,_allocobj\n1:\tmovl\t(r0),_objfreelist+4\n\tmovl\t$0X40000001,(r0)\n\tpushl\tr0")
                                 [] var t;
                       Nil: func (inline "\tpushl\t$0x40000003")
                                 [] val t;
                       V:   func (inline "\tmovl\t*(sp),(sp)")
                                 [var t] val t;
                       In:  func (inline "# ref$In")
                                 [var T] val t;
                       Out: func (inline "\tmovl\t(sp)+,(sp)")
                                 [val t; var Void] var T;
                       ^ : func (inline "\tmovl\t(sp)+,(sp)")
                                [val t; var Void] var T;
                     };

    File: type f { = :  func (inline "\tcmpl\t(sp)+,(sp)+ #COMP jneq 4\n\tmovpsl\tr0\n\trotl\t$-2,r0,r0\n\tbicl3\t$0xfffffffe,r0,-(sp)")
                             [val f; val f] val Boolean;
                   := : func (inline "\tmovl\t4(sp),*(sp)+")
                             [var f; val f] val f;
                   Null: func (inline "pushl\t$0")
                              [] val f;
                   New: func (inline "\tmovl\t_objfreelist+4,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$1\n\tcalls\t$1,_allocobj\n1:\tmovl\t(r0),_objfreelist+4\n\tmovl\t$0X0,(r0)\n\tpushl\tr0")
                             [] var f;
                   V:   func (inline "\tmovl\t*(sp),(sp)")
                             [var f] val f;
                   close: func (inline "\t.globl\t_File_Close\n\tcalls\t$1,_File_Close\n\tpushl\tr0")
                               [val f] val Void;
                   eof:   func (inline "\t.globl\t_File_Eof\n\tcalls\t$2,_File_Eof\n\tpushl\tr0")
                               [val f; var Void] val Boolean;
                   flush: func (inline "\t.globl\t_File_Flush\n\tcalls\t$1,_File_Flush\n\tpushl\tr0")
                               [val f] val Void;
                   open:  func (inline "\t.globl\t_File_Open\n\tcalls\t$2,_File_Open\n\tpushl\tr0")
                               [fname,mode: val ChStr] val f;
		   readb: func (inline "\t.globl\t_File_Readb\n\tcalls\t$2,_File_Readb\n\tpushl\tr0")
			       [val f; var Void] val Short;
                   readc: func (inline "\t.globl\t_File_Readc\n\tmovl\t$0x%X,r11\n\tcalls\t$2,_File_Readc\n\tpushl\tr0")
                               [val f; var Void] val ChStr;
                   seek:  func (inline "\t.globl\t_File_Seek\n\tcalls\t$2,_File_Seek\n\tpushl\tr0")
                               [val f; val Long] val Void;
                   stderr: func (inline "\t.globl\t__iob\n\tpushl\t$__iob+40")
                                [] val f;
                   stdin:  func (inline "\t.globl\t__iob\n\tpushl\t$__iob")
                                [] val f;
                   stdout: func (inline "\t.globl\t__iob\n\tpushl\t$__iob+20")
                                [] val f;
                   write: func (inline "\t.globl\t_File_Write\n\tcalls\t$2,_File_Write\n\tpushl\tr0")
                               [val f; val ChStr] val ChStr;
		   writeb: func (inline "\t.globl\t_File_Writeb\n\tcalls\t$2,_File_Writeb\n\tpushl\tr0")
				[val f; val Short] val Short;
                 };

    List: func [t:type{}] type l {
                := :     func (inline "\tmovl\t4(sp),*(sp)+")
                              [var l; val l] val l;
                New:     func (inline "\tmovl\t_objfreelist+4,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$1\n\tcalls\t$1,_allocobj\n1:\tmovl\t(r0),_objfreelist+4\n\tmovl\t$0,(r0)\n\tpushl\tr0")
                              [] var l;
                V:       func (inline "\tmovl\t*(sp),(sp)")
                              [var l] val l;
                '' :     func (inline "\tpushl\t$0")
                              [] val l (*empty list*);
                is_nil:  func (inline "\ttstl\t(sp)+  #COMP jneq 4\n\tmovpsl\tr0\n\trotl\t$-2,r0,r0\n\tbicl3\t$0xfffffffe,r0,-(sp)")
                              [val l] val Boolean (* is empty *);
                cons:    func (inline "\tmovl\t_objfreelist+8,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$2\n\tcalls\t$1,_allocobj\n1:\tmovl\t(r0),_objfreelist+8\n\tmovl\t(sp)+,4(r0)\n\tmovl\t(sp)+,(r0)\n\tpushl\tr0")
                              [val t; val l] val l; (* add at left end *);
                ^* :     func (inline "\tmovl\t_objfreelist+8,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$2\n\tcalls\t$1,_allocobj\n1:\tmovl\t(r0),_objfreelist+8\n\tmovl\t(sp)+,(r0)\n\tmovl\t(sp)+,4(r0)\n\tpushl\tr0")
                              [val l; val t] val l; (* cons, with args reversed *);
                head:    func (inline "\ttstl\t(sp) #---\n\tbneq\t1f #---\n\tcalls\t$0,_list_error #---\n1:\tmovl\t(sp),r0\n\tmovl\t4(r0),(sp)")
			      [val l] val t (* first element, error if empty *);
                tail:    func (inline "\ttstl\t(sp) #---\n\tbneq\t1f #---\n\tcalls\t$0,_list_error #---\n1:\tmovl\t*(sp),(sp)")
                              [val l] val l (* all but first, error if empty *);
           };

    LList: func [t:type{}] type l {
                := :     func (inline "\tmovl\t4(sp),*(sp)+")
                              [var l; val l] val l;
                New:     func (inline "\tmovl\t_objfreelist+4,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$1\n\tcalls\t$1,_allocobj\n1:\tmovl\t(r0),_objfreelist+4\n\tmovl\t$0,(r0)\n\tpushl\tr0")
                              [] var l;
                V:       func (inline "\tmovl\t*(sp),(sp)")
                              [var l] val l;
                '' :     func (inline "\tpushl\t$0")
                              [] val l (*empty list*);
                is_nil:  func (inline "\ttstl\t(sp)+  #COMP jneq 4\n\tmovpsl\tr0\n\trotl\t$-2,r0,r0\n\tbicl3\t$0xfffffffe,r0,-(sp)")
                              [val l] val Boolean (* is empty *);
                cons:    func (inline "\tmovl\t_objfreelist+12,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$3\n\tcalls\t$1,_allocobj\n1:\tmovl\t(r0),_objfreelist+12\n\tmovl\t(sp)+,4(r0)\n\tmovl\t(sp)+,(r0)\n\tclrl\t8(r0)\n\tpushl\tr0")
                              [val t; val l] val l; (* add at left end *);
                cons:    func (inline "\tmovl\t_objfreelist+12,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$3\n\tcalls\t$1,_allocobj\n1:\tmovl\t(r0),_objfreelist+12\n\tmovl\t(sp)+,4(r0)\n\tmovl\t(sp)+,(r0)\n\tmovl\t$1,8(r0)\n\tpushl\tr0")
                              [val t; func [] val l] val l;
                head:    func (inline "\ttstl\t(sp) #---\n\tbneq\t1f #---\n\tcalls\t$0,_list_error #---\n1:\tmovl\t(sp),r0\n\tmovl\t4(r0),(sp)")
                              [val l] val t (* first element, error if empty *);
                tail:    func (inline "\ttstl\t(sp) #---\n\tbneq\t1f #---\n\tcalls\t$0,_list_error #---\n1:\tmovl\t(sp),r0\n\ttstl\t8(r0)\t\n\tbeql\t2f\n\tmovl\t$0x%X,r11\n\tcalls\t$1,_LList_Tail\n\tpushl\tr0\n\tjbr\t3f\n2:\tmovl\t(r0),(sp)\n3:\n")
                              [val l] val l (* all but first, error if empty *);
           };

    Long: type (simple
                constants "\t.globl\t_Long_%c\n\tpushl\t$_Long_%c"
               ) L {
      (* constants *)
	'0' :   func [] val L;
	'1' :   func [] val L;
	'2' :   func [] val L;
	'3' :   func [] val L;
	'4' :   func [] val L;
	'5' :   func [] val L;
	'6' :   func [] val L;
	'7' :   func [] val L;
	'8' :   func [] val L;
	'9' :   func [] val L;
      (* operations *)
        % :     func (inline "\t.globl\t_Long_Mod\n\tmovl\t$0x%X,r11\n\tcalls\t$2,_Long_Mod\n\tpushl\tr0")
                     [val L; val Short] val Short;
        * :     func (inline "\t.globl\t_FLong_Mult\n\tmovl\t$0x%X,r11\n\tjsb\t_FLong_Mult\n\tmovl\t(sp)+,r0")
                     [x,y:val L] val L;
        + :     func (inline "\t.globl\t_FLong_Add\n\tmovl\t$0x%X,r11\n\tjsb\t_FLong_Add\n\tmovl\t(sp)+,r0")
                     [x,y:val L] val L;
        - :     func (inline "\t.globl\t_FLong_Neg\n\tmovl\t$0x%X,r11\n\tjsb\t_FLong_Neg")
                     [x:val L] val L;
        - :     func (inline "\t.globl\t_FLong_Sub\n\tmovl\t$0x%X,r11\n\tjsb\t_FLong_Sub\n\tmovl\t(sp)+,r0")
                     [x,y:val L] val L;
        / :     func (inline "\t.globl\t_Long_Div\n\tmovl\t$0x%X,r11\n\tcalls\t$2,_Long_Div\n\tpushl\tr0")
                     [val L; val Short] val L;
	:= :    func (standard := 1)
		     (inline "\tmovl\t4(sp),*(sp)+")
                     [var L; val L] val L;
	< :     func (inline "\t.globl\t_Long_cmp; calls $2,_Long_cmp; tstw r0 #COMP jgeq 4\n\tmovpsl\tr0\n\trotl\t$-3,r0,r0\n\tbicl3\t$0xfffffffe,r0,-(sp)")
		     [x,y:val L] val Boolean;
	<= :    func (inline "\t.globl\t_Long_cmp; calls $2,_Long_cmp; tstw r0 #COMP jgtr 6\n\tbgtr\t1f\n\tpushl\t$1\n\tjbr\t2f\n1:\tpushl\t$0\n2:")
		     [x,y:val L] val Boolean;
	<> :    func (inline "\t.globl\t_Long_cmp; calls $2,_Long_cmp; tstw r0 #COMP jeql 4\n\tmovpsl\tr0\n\trotl\t$-2,r0,r0\n\tbicl3\tr0,$0x1,-(sp)")
		     [x,y:val L] val Boolean;
	= :     func (inline "\t.globl\t_Long_cmp; calls $2,_Long_cmp; tstw r0 #COMP jneq 4\n\tmovpsl\tr0\n\trotl\t$-2,r0,r0\n\tbicl3\t$0xfffffffe,r0,-(sp)")
		     [x,y:val L] val Boolean;
	> :     func (inline "\t.globl\t_Long_cmp; calls $2,_Long_cmp; tstw r0 #COMP jleq 6\n\tbleq\t1f\n\tpushl\t$1\n\tjbr\t2f\n1:\tpushl\t$0\n2:")
		     [x,y:val L] val Boolean;
	>= :    func (inline "\t.globl\t_Long_cmp; calls $2,_Long_cmp; tstw r0 #COMP jlss 4\n\tmovpsl\tr0\n\trotl\t$-3,r0,r0\n\tbicl3\tr0,$0x1,-(sp)")
		     [x,y:val L] val Boolean;
        In :    func (inline "\t.globl\t_Long_In\n\tmovl\t$0x%X,r11\n\tcalls\t$1,_Long_In\n\tpushl\tr0")
                     [val Short] val L;
	New:    func (standard ptr_New 1)
		     (inline "\t.globl\t_Long_0\n\tmovl\t_objfreelist+4,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$1\n\tcalls\t$1,_allocobj\n1:\tmovl\t(r0),_objfreelist+4\n\tmovl\t$_Long_0,(r0)\n\tpushl\tr0")
                     [] var L;
	New:    func (standard init_New 1)
		     (inline "\t.globl\t_Long_0\n\tmovl\t_objfreelist+4,r0\n\tjneq\t1f\n\tmovl\t$0x%X,r11\n\tpushl\t$1\n\tcalls\t$1,_allocobj\n1:\tmovl\t(r0),_objfreelist+4\n\tmovl\t(sp)+,(r0)\n\tpushl\tr0")
		     [val L] var L;
	Out:    func (inline "\t.globl\t_Long_Out\n\tmovl\t(sp),r0\n\tcmpl\t(r0),$1 #---\n\tbneq\t1f #---\n\tcmpl\t4(r0),$0x7fff #---\n\tbgtr\t1f #---\n\tcmpl\t4(r0),$0xffff8000 #---\n\tbgtr\t2f #---\n1:\tcalls\t$1,_Long_Out #---\n\tpushl\tr0 #---\n\tjbr\t3f #---\n2:\tmovzwl\t4(r0),(sp)\n3:")
		     [val L] val Short;
	V:      func (standard V 1)
		     (inline "\tmovl\t*(sp),(sp)")
                     [var L] val L;
        ^+ :    func (inline "\t.globl\t_Long_Concat\n\tmovl\t$0x%X,r11\n\tcalls\t$2,_Long_Concat\n\tpushl\tr0")
                     [x,y: val L] val L;
        nbits : func (inline "\t.globl\t_Long_Nbits\n\tmovl\t$0x%X,r11\n\tcalls\t$1,_Long_Nbits\n\tpushl\tr0")
		     [val L] val L;
	nwords: func [val L] val L;
	odd:    func [val L] val Boolean;
	put :   func (standard put 0)
		     (inline "\t.globl\t_Long_Put\n\tmovl\t$0x%X,r11\n\tcalls\t$1,_Long_Put\n\tpushl\tr0")
                     [val L] val L;
	puts:   func (inline "\t.globl\t_Long_Puts\n\tmovl\t$0x%X,r11\n\tcalls\t$1,_Long_Puts\n\tpushl\tr0")
		     [val L] val ChStr;
        shift : func (inline "\t.globl\t_Long_Shift\n\tmovl\t$0x%X,r11\n\tcalls\t$2,_Long_Shift\n\tpushl\tr0")
                     [x,amount: val L] val L;
    };
    Callcc: func (standard Callcc 0)
		 [ body: func[ cc: func[ val T ] val Void; var Void ] val T;
		   T: type {};
		   var Void
		 ] val T;
    Signal: func (standard Callcc 0)
		 [sig_num: val Short; var Void] val Boolean;
    Unsignal: func[ sig_num: val Short; var Void] val Void;
    Expand_Hp: func[ val Short ] val Void;
] 
{   let
	impure === var Void;
	^ == func[x: val T; y: val List[T]; T: type{}] {
		cons[x,y];
	     };
	^ == func[x: val T; y: val T; T: type{}] {
		cons[x, cons[y, (List[T])$'']];
	     };
    in
	use Float in use ChStr in use Boolean in use Short in
	    
	ni ni ni ni;
	Null[]
    ni
}
