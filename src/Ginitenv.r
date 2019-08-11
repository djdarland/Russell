func [
    argc: val Short;
    trace: var Boolean;
    argv: func[val Short] val ChStr;
    eof: func[var Void] val Boolean;
    abort: func [] val Void;
    Void: type (simple) {};
    Null: func (inline "LDN 0,RS") [] val Void;
    FS: var Void;

    Boolean: type (simple) B {
      (* constants *)
	False:  func (inline "LDN 0,RS") [] val B;
	True:   func (inline "LDN 1,RS") [] val B;
      (* operations *)
	:= :    func (standard := 1)
		     (inline "STI $1,0,$2; MOV $2,RS")
		     [var B; val B] val B;
	<> :    func (inline "NEI $1,$2,RS")
		     [x,y: val B] val B;
	= :     func (inline "EQI $1,$2,RS")
		     [x,y: val B] val B;
	New:    func (standard New 1)
		     (inline "ALA C1,RS; STI RS,0,C0")
		     [] var B;
	New:    func (standard init_New 1)
		     (inline "ALA C1,RS; STI RS,0,$1")
		     [val B] var B;
	V:      func (standard V 1)
		     (inline "LDI $1,0,RS")
		     [var B] val B;
	and:    func (inline "AND $1,$2,RS")
		     [x,y:val B] val B;
	not:    func (inline "NOT $1,RS")
		     [val B] val B;
	or:     func (inline "OR $1,$2,RS")
		     [x,y:val B] val B;
	put :   func (standard put 0)
		     (inline "ARG 1,$1; EXT \"_Bool_Put\"; LBA \"_Bool_Put\"; CLC 1; MOV RL,RS")
		     [val B] val B;
	puts :  func (inline "ARG 1,$1; EXT \"_Bool_Puts\"; LBA \"_Bool_Puts\"; CLC 1; MOV RL,RS")
		     [val B] val ChStr;
    };
    
    Short: type (simple
		 constants "LDN %c,RS"
			   "LDN %s,RS"
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
	% :     func (inline "DVI $1,$2,RS; MLI RS,$2,RS; SBI $1,RS,RS")
		     [x,y:val S] val S;
	* :     func (inline "MLI $1,$2,RS")
                     [x,y:val S] val S;
	** :    func (inline "ARG 2,$2; ARG 1,$1; EXT \"_Short_Exp\"; LBA \"_Short_Exp\"; CLC 2; MOV RL,RS")
		     [x,y:val S] val S;
	+ :     func (inline "ADI $1,$2,RS")
                     [x,y:val S] val S;
	+= :    func (standard += 1)
		     (inline "LDI $1,0,T1; ADI T1,$2,T1; STI $1,0,T1; MOV T1,RS; UDC T1")
		     [var S; val S] val S;
	- :     func (inline "NGI $1,RS")
                     [val S] val S;
	- :     func (inline "SBI $1,$2,RS")
                     [x,y:val S] val S;
	-= :    func (standard -= 1)
		     (inline "LDI $1,0,T1; SBI T1,$2,T1; STI $1,0,T1; MOV T1,RS; UDC T1")
		     [var S; val S] val S;
	/ :     func (inline "DVI $1,$2,RS")
		     [x,y:val S] val S;
	:= :    func (standard := 1)
		     (inline "STI $1,0,$2; MOV $2,RS")
		     [var S; val S] val S;
	< :     func (inline "LTI $1,$2,RS")
		     [x,y:val S] val Boolean;
	<= :    func (inline "LEI $1,$2,RS")
		     [x,y:val S] val Boolean;
	<> :    func (inline "NEI $1,$2,RS")
		     [x,y:val S] val Boolean;
	= :     func (inline "EQI $1,$2,RS")
		     [x,y:val S] val Boolean;
	> :     func (inline "GTI $1,$2,RS")
		     [x,y:val S] val Boolean;
	>= :    func (inline "GEI $1,$2,RS")
		     [x,y:val S] val Boolean;
	New:    func (standard New 1)
		     (inline "ALA C1,RS; STI RS,0,C0")
		     [] var S;
	New:    func (standard init_New 1)
		     (inline "ALA C1,RS; STI RS,0,$1")
		     [val S] var S;
	V:      func (standard V 1)
		     (inline "LDI $1,0,RS")
		     [var S] val S;
	^+ :    func (inline "LDN 10,RS; MLI RS,$1,RS; ADI RS,$2,RS")
		     [x,y: val S] val S;
	get :   func (inline "EXT \"_Short_Get\"; LBA \"_Short_Get\"; CLC 0; MOV RL,RS")
		     [var Void] val S;
	put :   func (standard put 0)
		     (inline "ARG 1,$1; EXT \"_Short_Put\"; LBA \"_Short_Put\"; CLC 1; MOV RL,RS")
		     [val S] val S;
	puts:   func (inline "ARG 1,$1; EXT \"_Short_Puts\"; LBA \"_Short_Puts\"; CLC 1; MOV RL,RS;")
		     [val S] val ChStr;
	shift : func (inline "ARG 2,$2; ARG 1,$1; EXT \"_Short_Shift\"; LBA \"_Short_Shift\"; CLC 2; MOV RL,RS")
		     [x,y: val S] val S;
    };

    SFloat: type (simple) F {
      (* operations *)
	* :     func (inline "ARG 2,$2; ARG 1,$1; EXT \"_SFloat_Mult\"; LBA \"_SFloat_Mult\"; CLC 2; MOV RL,RS")
		     [x,y:val F] val F;
	+ :     func (inline "ARG 2,$2; ARG 1,$1; EXT \"_SFloat_Add\"; LBA \"_SFloat_Add\"; CLC 2; MOV RL,RS")
		     [x,y:val F] val F;
	- :     func (inline "ARG 1,$1; EXT \"_SFloat_Neg\"; LBA \"_SFloat_Neg\"; CLC 1; MOV RL,RS")
		     [val F] val F;
	- :     func (inline "ARG 2,$2; ARG 1,$1; EXT \"_SFloat_Sub\"; LBA \"_SFloat_Sub\"; CLC 2; MOV RL,RS")
		     [x,y:val F] val F;
	. :     func (inline "ARG 3,$3; ARG 2,$2; ARG 1,$1; EXT \"_SFloat_Dot\"; HINT AL,2,1; LBA \"_SFloat_Dot\"; CLC 3; MOV RL,RS")
		     [whole, fraction, fraction_length: val Short] val F;
	/ :     func (inline "ARG 2,$2; ARG 1,$1; EXT \"_SFloat_Div\"; LBA \"_SFloat_Div\"; CLC 2; MOV RL,RS")
		     [x,y:val F] val F;
	:= :    func (standard := 1)
		     (inline "STI $1,0,$2; MOV $2,RS")
		     [var F; val F] val F;
	< :     func (inline "ARG 2,$2; ARG 1,$1; EXT \"_SFloat_Lt\"; LBA \"_SFloat_Lt\"; CLC 2; MOV RL,RS")
		     [x,y:val F] val Boolean;
	<= :    func (inline "ARG 2,$2; ARG 1,$1; EXT \"_SFloat_Le\"; LBA \"_SFloat_Le\"; CLC 2; MOV RL,RS")
		     [x,y:val F] val Boolean;
	<> :    func (inline "ARG 2,$2; ARG 1,$1; EXT \"_SFloat_Ne\"; LBA \"_SFloat_Ne\"; CLC 2; MOV RL,RS")
		     [x,y:val F] val Boolean;
	= :     func (inline "ARG 2,$2; ARG 1,$1; EXT \"_SFloat_Eq\"; LBA \"_SFloat_Eq\"; CLC 2; MOV RL,RS")
		     [x,y:val F] val Boolean;
	> :     func (inline "ARG 2,$2; ARG 1,$1; EXT \"_SFloat_Gt\"; LBA \"_SFloat_Gt\"; CLC 2; MOV RL,RS")
		     [x,y:val F] val Boolean;
	>= :    func (inline "ARG 2,$2; ARG 1,$1; EXT \"_SFloat_Ge\"; LBA \"_SFloat_Ge\"; CLC 2; MOV RL,RS")
		     [x,y:val F] val Boolean;
	In:     func (inline "ARG 1,$1; EXT \"_SFloat_In\"; LBA \"_SFloat_In\"; CLC 1; MOV RL,RS")
		     [val Short] val F;
	New:    func (standard New 1)
		     (inline "ALA C1,RS; STI RS,0,C0")
		     [] var F;
	New:    func (standard init_New 1)
		     (inline "ALA C1,RS; STI RS,0,$1")
		     [val F] var F;
	Out:    func (inline "ARG 1,$1; EXT \"_SFloat_Out\"; LBA \"_SFloat_Out\"; CLC 1; MOV RL,RS")
		     [val F] val Short;
	V:      func (standard V 1)
		     (inline "LDI $1,0,RS")
		     [var F] val F;
	atan:   func (inline "ARG 1,$1; EXT \"_SFloat_Atan\"; LBA \"_SFloat_Atan\"; CLC 1; MOV RL,RS")
		     [val F] val F;
	cos:    func (inline "ARG 1,$1; EXT \"_SFloat_Cos\"; LBA \"_SFloat_Cos\"; CLC 1; MOV RL,RS")
		     [val F] val F;
	exp:    func (inline "ARG 1,$1; EXT \"_SFloat_Exp\"; LBA \"_SFloat_Exp\"; CLC 1; MOV RL,RS")
		     [val F] val F;
	exponent:  func (inline "ARG 1,$1; EXT \"_SFloat_Exponent\"; LBA \"_SFloat_Exponent\"; CLC 1; MOV RL,RS")
			[val F] val Short;
	get :   func (inline "EXT \"_SFloat_Get\"; LBA \"_SFloat_Get\"; CLC 0; MOV RL,RS")
		     [var Void] val F;
	ln:     func (inline "ARG 1,$1; EXT \"_SFloat_Ln\"; LBA \"_SFloat_Ln\"; CLC 1; MOV RL,RS")
		     [val F] val F;
	put :   func (standard put 0)
		     (inline "ARG 1,$1; EXT \"_SFloat_Put\"; LBA \"_SFloat_Put\"; CLC 1; MOV RL,RS")
		     [val F] val F;
	puts:   func (inline "ARG 1,$1; EXT \"_SFloat_Puts\"; HINT AL,0,1; LBA \"_SFloat_Puts\"; CLC 1; MOV RL,RS")
		     [val F] val ChStr;
	shift:  func (inline "ARG 2,$2; ARG 1,$1; EXT \"_SFloat_Shift\"; LBA \"_SFloat_Shift\"; CLC 2; MOV RL,RS")
		     [val F; val Short] val F;
	sin:    func (inline "ARG 1,$1; EXT \"_SFloat_Sin\"; LBA \"_SFloat_Sin\"; CLC 1; MOV RL,RS")
		     [val F] val F;
	sqrt:   func (inline "ARG 1,$1; EXT \"_SFloat_Sqrt\"; LBA \"_SFloat_Sqrt\"; CLC 1; MOV RL,RS")
		     [val F] val F;
	to_Float: func (inline "ARG 1,$1; EXT \"_SFloat_to_Float\"; HINT AL,2,1; LBA \"_SFloat_to_Float\"; CLC 1; MOV RL,RS")
		      [val F] val Float;
	to_Long: func (inline "ARG 1,$1; EXT \"_SFloat_to_Long\"; HINT AL,0,1; LBA \"_SFloat_to_Long\"; CLC 1; MOV RL,RS")
		      [val F] val Long;
    };

    Float: type (simple) F {
      (* operations *)
	* :     func (inline "HINT NP; ARG 2,$2; HINT NP; ARG 1,$1; EXT \"_Float_Mult\"; HINT AL,2,1; LBA \"_Float_Mult\"; CLC 2; MOV RL,RS")
		     [x,y:val F] val F;
	+ :     func (inline "HINT NP; ARG 2,$2; HINT NP; ARG 1,$1; EXT \"_Float_Add\"; HINT AL,2,1; LBA \"_Float_Add\"; CLC 2; MOV RL,RS")
		     [x,y:val F] val F;
	- :     func (inline "HINT NP; ARG 1,$1; EXT \"_Float_Neg\"; HINT AL,2,1; LBA \"_Float_Neg\"; CLC 1; MOV RL,RS")
		     [val F] val F;
	- :     func (inline "HINT NP; ARG 2,$2; HINT NP; ARG 1,$1; EXT \"_Float_Sub\"; HINT AL,2,1; LBA \"_Float_Sub\"; CLC 2; MOV RL,RS")
		     [x,y:val F] val F;
	. :     func (inline "ARG 3,$3; ARG 2,$2; ARG 1,$1; EXT \"_Float_Dot\"; HINT AL,2,1; LBA \"_Float_Dot\"; CLC 3; MOV RL,RS")
		     [whole, fraction, fraction_length: val Short] val F;
	/ :     func (inline "HINT NP; ARG 2,$2; HINT NP; ARG 1,$1; EXT \"_Float_Div\"; HINT AL,2,1; LBA \"_Float_Div\"; CLC 2; MOV RL,RS")
		     [x,y:val F] val F;
	:= :    func (inline "LDI $2,0,T1; STI $1,0,T1; LDI $2,1,T1; STI $1,1,T1; UDC T1; MOV $2,RS")
		     [var F; val F] val F;
	< :     func (inline "HINT NP; ARG 2,$2; HINT NP; ARG 1,$1; EXT \"_Float_Lt\"; LBA \"_Float_Lt\"; CLC 2; MOV RL,RS")
		     [x,y:val F] val Boolean;
	<= :    func (inline "HINT NP; ARG 2,$2; HINT NP; ARG 1,$1; EXT \"_Float_Le\"; LBA \"_Float_Le\"; CLC 2; MOV RL,RS")
		     [x,y:val F] val Boolean;
	<> :    func (inline "HINT NP; ARG 2,$2; HINT NP; ARG 1,$1; EXT \"_Float_Ne\"; LBA \"_Float_Ne\"; CLC 2; MOV RL,RS")
		     [x,y:val F] val Boolean;
	= :     func (inline "HINT NP; ARG 2,$2; HINT NP; ARG 1,$1; EXT \"_Float_Eq\"; LBA \"_Float_Eq\"; CLC 2; MOV RL,RS")
		     [x,y:val F] val Boolean;
	> :     func (inline "HINT NP; ARG 2,$2; HINT NP; ARG 1,$1; EXT \"_Float_Gt\"; LBA \"_Float_Gt\"; CLC 2; MOV RL,RS")
		     [x,y:val F] val Boolean;
	>= :    func (inline "HINT NP; ARG 2,$2; HINT NP; ARG 1,$1; EXT \"_Float_Ge\"; LBA \"_Float_Ge\"; CLC 2; MOV RL,RS")
		     [x,y:val F] val Boolean;
	In:     func (inline "ARG 1,$1; EXT \"_Float_In\"; HINT AL,2,1; LBA \"_Float_In\"; CLC 1; MOV RL,RS")
		     [val Short] val F;
	New:    func (inline "ALA C2,RS; STI RS,0,C0; STI RS,1,C0")
		     [] var F;
	New:    func (inline "DCL T2,ADDR; ALA C2,T2; LDI $1,0,T1; STI T2,0,T1; LDI $1,1,T1; STI T2,1,T1; UDC T1; MOV T2,RS; UDC T2")
		     [val F] var F;
	Out:    func (inline "HINT NP; ARG 1,$1; EXT \"_Float_Out\"; LBA \"_Float_Out\"; CLC 1; MOV RL,RS")
		     [val F] val Short;
	V:      func (inline "DCL T2,ADDR; ALA C2,T2; LDI $1,0,T1; STI T2,0,T1; LDI $1,1,T1; STI T2,1,T1; UDC T1; MOV T2,RS; UDC T2")
		     [var F] val F;
	atan:   func (inline "HINT NP; ARG 1,$1; EXT \"_Float_Atan\"; HINT AL,2,1; LBA \"_Float_Atan\"; CLC 1; MOV RL,RS")
		     [val F] val F;
	cos:    func (inline "HINT NP; ARG 1,$1; EXT \"_Float_Cos\"; HINT AL,2,1; LBA \"_Float_Cos\"; CLC 1; MOV RL,RS")
		     [val F] val F;
	exp:    func (inline "HINT NP; ARG 1,$1; EXT \"_Float_Exp\"; HINT AL,2,1; LBA \"_Float_Exp\"; CLC 1; MOV RL,RS")
		     [val F] val F;
	exponent:  func (inline "HINT NP; ARG 1,$1; EXT \"_Float_Exponent\"; LBA \"_Float_Exponent\"; CLC 1; MOV RL,RS")
			[val F] val Short;
	get :   func (inline "EXT \"_Float_Get\"; HINT AL,2,1; LBA \"_Float_Get\"; CLC 0; MOV RL,RS")
		     [var Void] val F;
	ln:     func (inline "HINT NP; ARG 1,$1; EXT \"_Float_Ln\"; HINT AL,2,1; LBA \"_Float_Ln\"; CLC 1; MOV RL,RS")
		     [val F] val F;
	put :   func (standard put 0)
		     (inline "HINT PT; ARG 1,$1; EXT \"_Float_Put\"; LBA \"_Float_Put\"; CLC 1; MOV RL,RS")
		     [val F] val F;
	puts:   func (inline "HINT NP; ARG 1,$1; EXT \"_Float_Puts\"; HINT AL,0,1; LBA \"_Float_Puts\"; CLC 1; MOV RL,RS")
		     [val F] val ChStr;
	shift:  func (inline "ARG 2,$2; HINT NP; ARG 1,$1; EXT \"_Float_Shift\"; HINT AL,2,1; LBA \"_Float_Shift\"; CLC 2; MOV RL,RS")
		     [val F; val Short] val F;
	sin:    func (inline "HINT NP; ARG 1,$1; EXT \"_Float_Sin\"; HINT AL,2,1; LBA \"_Float_Sin\"; CLC 1; MOV RL,RS")
		     [val F] val F;
	sqrt:   func (inline "HINT NP; ARG 1,$1; EXT \"_Float_Sqrt\"; HINT AL,2,1; LBA \"_Float_Sqrt\"; CLC 1; MOV RL,RS")
		     [val F] val F;
	to_Long: func (inline "HINT NP; ARG 1,$1; EXT \"_Float_to_Long\"; HINT AL,0,1; LBA \"_Float_to_Long\"; CLC 1; MOV RL,RS")
		      [val F] val Long;
	to_SFloat: func (inline "HINT NP; ARG 1,$1; EXT \"_Float_to_SFloat\"; LBA \"_Float_to_SFloat\"; CLC 1; MOV RL,RS")
		      [val F] val SFloat;
    };

    ChStr: type (simple
		 constants "LBA \"\\%03o\"; LDS RS"
			   "LBA \"%s\"; LDS RS"
			   "\\%03o")
		CS {
	characters;
	'' :     func (inline "EXT \"_empty_ChStr\"; LBA \"_empty_ChStr\"; LDL RS")
		      [] val CS;
	. :      func (inline "HINT OPT,5; ARG 2,$2; ARG 1,$1; EXT \"_ChStr_check_Dot\"; LBA \"_ChStr_check_Dot\"; CLC 2; ARG 2,$2; HINT NP; ARG 1,$1; EXT \"_ChStr_unchecked_Dot\"; LBA \"_ChStr_unchecked_Dot\"; CLC 2; MOV RL,RS")
		      [val CS; val Short] val Short;
	:= :     func (standard := 1)
		      (inline "STI $1,0,$2; MOV $2,RS")
		      [var CS; val CS] val CS;
	< :     func (inline "HINT NP; ARG 2,$2; HINT NP; ARG 1,$1; EXT \"_strcmp\"; LBA \"_strcmp\"; CLC 2; LTI RL,C0,RS")
		     [x,y:val CS] val Boolean;
	<= :    func (inline "HINT NP; ARG 2,$2; HINT NP; ARG 1,$1; EXT \"_strcmp\"; LBA \"_strcmp\"; CLC 2; LEI RL,C0,RS")
		     [x,y:val CS] val Boolean;
	<> :    func (inline "HINT NP; ARG 2,$2; HINT NP; ARG 1,$1; EXT \"_strcmp\"; LBA \"_strcmp\"; CLC 2; NEI RL,C0,RS")
		     [x,y:val CS] val Boolean;
	= :     func (inline "HINT NP; ARG 2,$2; HINT NP; ARG 1,$1; EXT \"_strcmp\"; LBA \"_strcmp\"; CLC 2; EQI RL,C0,RS")
		     [x,y:val CS] val Boolean;
	> :     func (inline "HINT NP; ARG 2,$2; HINT NP; ARG 1,$1; EXT \"_strcmp\"; LBA \"_strcmp\"; CLC 2; GTI RL,C0,RS")
		     [x,y:val CS] val Boolean;
	>= :    func (inline "HINT NP; ARG 2,$2; HINT NP; ARG 1,$1; EXT \"_strcmp\"; LBA \"_strcmp\"; CLC 2; GEI RL,C0,RS")
		     [x,y:val CS] val Boolean;
	In:      func (inline "ARG 1,$1; EXT \"_Short_to_ChStr\"; HINT AL,1,1; LBA \"_Short_to_ChStr\"; CLC 1; MOV RL,RS")
		      [val Short] val CS;
	New:     func (standard ptr_New 1)
		      (inline "ALH C1,RS; STI RS,0,UN")
		      [] var CS;
	New:     func (standard init_New 1)
		      (inline "ALH C1,RS; STI RS,0,$1")
		      [val CS] var CS;
	Out:     func (inline "HINT NP; ARG 1,$1; EXT \"_ChStr_to_Short\"; LBA \"_ChStr_to_Short\"; CLC 1; MOV RL,RS")
		      [val CS] val Short;
	V:       func (standard V 1)
		      (inline "LDI $1,0,RS")
		      [var CS] val CS;
	^* :     func (inline "HINT NP; ARG 2,$2; HINT NP; ARG 1,$1; EXT \"_ChStr_Concat\"; HINT AL,0,1; LBA \"_ChStr_Concat\"; CLC 2; MOV RL,RS")
		      [x,y: val CS] val CS;
	getchar: func (inline "EXT \"_ChStr_Getchar\"; HINT AL,1,1; LBA \"_ChStr_Getchar\"; CLC 0; MOV RL,RS")
		      [var Void] val CS;
	len:     func (inline "HINT NP; ARG 1,$1; EXT \"_ChStr_Length\"; LBA \"_ChStr_Length\"; CLC 1; MOV RL,RS")
		      [val CS] val Short;
	put:     func (standard put 0)
		      (inline "HINT PT; ARG 1,$1; EXT \"_ChStr_Put\"; LBA \"_ChStr_Put\"; CLC 1; MOV RL,RS")
		      [val CS] val CS;
	substr:  func (inline "ARG 3,$3; ARG 2,$2; HINT NP; ARG 1,$1; EXT \"_ChStr_Sub\"; HINT AL,0,1; LBA \"_ChStr_Sub\"; CLC 3; MOV RL,RS")
		      [val CS; i,j: val Short] val CS;
    };

    Alias: func (inline "EQI $1,$2,RS")
		[x,y: var T; T: type {}] val Boolean;

    Array: func (standard Array 0)
		(inline "ARG 2,$2; ARG 1,$1; EXT \"_Array\"; HINT AL,0,0; LBA \"_Array\"; CLC 2; MOV RL,RS")
		[size: val Short; comptype: type {New; V}]
		type A {. : func (inline "HINT OPT,10; LTI $2,C0,TL; BRT \"1f\"; LDI $1,0,T1; LTI $2,T1,TL; BRT \"2f\"; LBL \"1\"; ARG 1,$2; EXT \"_Array_error\"; ERR \"_Array_error\"; LBL \"2\"; ADP $1,$2,T1; LDI T1,1,RS")
				 [var A; val Short] var comptype;
			. : func (inline "HINT OPT,10; LTI $2,C0,TL; BRT \"1f\"; LDI $1,0,T1; LTI $2,T1,TL; BRT \"2f\"; LBL \"1\"; ARG 1,$2; EXT \"_Array_error\"; ERR \"_Array_error\"; LBL \"2\"; ADP $1,$2,T1; LDI T1,1,RS")
				 [val A; val Short] val comptype;
			New;
			V;
			size: func [] val Short;
                       };

    Ref: func [T: type {}]
	      type t { = :  func (inline "EQI $1,$2,RS")
				 [val t; val t] val Boolean;
		       := : func (standard := 1)
				 (inline "STI $1,0,$2; MOV $2,RS")
				 [var t; val t] val t;
		       New: func (standard ptr_New 1)
				 (inline "ALH C1,RS; STI RS,0,UN")
				 [] var t;
		       Nil: func (inline "MOV UN,RS")
				 [] val t;
		       V:   func (standard V 1)
				 (inline "LDI $1,0,RS")
				 [var t] val t;
		       In:  func (inline "MOV $1,RS")
				 [var T] val t;
		       Out: func (inline "MOV $1,RS")
				 [val t; var Void] var T;
		       ^ : func (inline "MOV $1,RS")
				[val t; var Void] var T;
                     };

    File: type (simple) f {
		   := : func (standard := 1)
			     (inline "STI $1,0,$2; MOV $2,RS")
			     [var f; val f] val f;
		   = :  func (inline "EQI $1,$2,RS")
			     [val f; val f] val Boolean;
		   New: func (standard ptr_New 1)
			     (inline "ALH C1,RS; STI RS,0,UN")
			     [] var f;
		   Null: func (inline "LDN 0,RS")
			      [] val f;
		   V:   func (standard V 1)
			     (inline "LDI $1,0,RS")
			     [var f] val f;
		   close: func (inline "ARG 1,$1; EXT \"_File_Close\"; LBA \"_File_Close\"; CLC 1; MOV C0,RS")
			       [val f] val Void;
		   eof:   func (inline "ARG 1,$1; EXT \"_File_Eof\"; LBA \"_File_Eof\"; CLC 1; MOV RL,RS")
			       [val f; var Void] val Boolean;
		   flush: func (inline "ARG 1,$1; EXT \"_File_Flush\"; LBA \"_File_Flush\"; CLC 1; MOV C0,RS")
			       [val f] val Void;
		   open:  func (inline "ARG 2,$2; ARG 1,$1; EXT \"_File_Open\"; LBA \"_File_Open\"; CLC 2; MOV RL,RS")
			       [fname,mode: val ChStr] val f;
		   readb: func (inline "ARG 1,$1; EXT \"_File_Readb\"; LBA \"_File_Readb\"; CLC 1; MOV RL,RS")
			       [val f; var Void] val Short;
		   readc: func (inline "ARG 1,$1; EXT \"_File_Readc\"; LBA \"_File_Readc\"; CLC 1; MOV RL,RS")
			       [val f; var Void] val ChStr;
		   seek:  func (inline "ARG 2,$2; ARG 1,$1; EXT \"_File_Seek\"; LBA \"_File_Seek\"; CLC 2; MOV C0,RS")
			       [val f; val Long] val Void;
		   stderr: func (inline "EXT \"_File_Stderr\"; LBA \"_File_Stderr\"; CLC 0; MOV RL,RS")
				[] val f;
		   stdin:  func (inline "EXT \"_File_Stdin\"; LBA \"_File_Stdin\"; CLC 0; MOV RL,RS")
				[] val f;
		   stdout: func (inline "EXT \"_File_Stdout\"; LBA \"_File_Stdout\"; CLC 0; MOV RL,RS")
				[] val f;
		   write: func  (inline "ARG 2,$2; ARG 1,$1; EXT \"_File_Write\"; LBA \"_File_Write\"; CLC 2; MOV RL,RS")
				[val f; val ChStr] val ChStr;
		   writeb: func (inline "ARG 2,$2; ARG 1,$1; EXT \"_File_Writeb\"; LBA \"_File_Writeb\"; CLC 2; MOV RL,RS")
				[val f; val Short] val Short;
                 };

    List: func [t:type{}] type (simple) l {
		:= :     func (standard := 1)
			      (inline "STI $1,0,$2; MOV $2,RS")
			      [var l; val l] val l;
		New:     func (standard ptr_New 1)
			      (inline "ALH C1,RS; STI RS,0,UN")
			      [] var l;
		V:       func (standard V 1)
			      (inline "LDI $1,0,RS")
			      [var l] val l;
		'' :     func (inline "MOV UN,RS")
			      [] val l (*empty list*);
		is_nil:  func (inline "EQI UN,$1,RS")
			      [val l] val Boolean (* is empty *);
		cons:    func (inline "ALH C2,T1; STI T1,1,$1; STI T1,0,$2; MOV T1,RS")
			      [val t; val l] val l; (* add at left end *);
		^* :     func (inline "ALH C2,T1; STI T1,1,$2; STI T1,0,$1; MOV T1,RS")
			      [val l; val t] val l; (* cons, with args reversed *);
		head:    func (inline "HINT OPT,5; EQI $1,UN,TL; BRF \"1f\"; EXT \"_list_error\"; ERR \"_list_error\"; LBL \"1\"; LDI $1,1,RS")
			      [val l] val t (* first element, error if empty *);
		tail:    func (inline "HINT OPT,5; EQI $1,UN,TL; BRF \"1f\"; EXT \"_list_error\"; ERR \"_list_error\"; LBL \"1\"; LDI $1,0,RS")
			      [val l] val l (* all but first, error if empty *);
           };

    LList: func [t:type{}] type l {
		:= :     func (standard := 1)
			      (inline "STI $1,0,$2; MOV $2,RS")
			      [var l; val l] val l;
		New:     func (standard ptr_New 1)
			      (inline "ALH C1,RS; STI RS,0,UN")
			      [] var l;
		V:       func (standard V 1)
			      (inline "LDI $1,0,RS")
			      [var l] val l;
		'' :     func (inline "MOV UN,RS")
			      [] val l (*empty list*);
		is_nil:  func (inline "EQI UN,$1,RS")
			      [val l] val Boolean (* is empty *);
		cons:    func (inline "ALH C3,T1; DCL T2,INT; MOV C1,T2; NGI T2,T2; STI T1,2,T2; UDC T2; STI T1,1,$1; STI T1,0,$2; MOV T1,RS")
			      [val t; val l] val l; (* add at left end *);
		cons:    func (inline "ALH C3,T1; HINT STSZ,$2; STI T1,2,C0; STI T1,1,$1; STI T1,0,$2; MOV T1,RS")
			      [val t; func [] val l] val l;
		head:    func (inline "HINT OPT,5; EQI $1,UN,TL; BRF \"1f\"; EXT \"_list_error\"; ERR \"_list_error\"; LBL \"1\"; LDI $1,1,RS")
			      [val l] val t (* first element, error if empty *);
		tail:    func (inline "HINT OPT,5; EQI $1,UN,TL; BRF \"1f\"; EXT \"_list_error\"; ERR \"_list_error\"; LBL \"1\"; LDI $1,2,T1; LTI T1,C0,TL; BRT \"1f\"; HINT NP; ARG 1,$1; EXT \"_LList_Tail1\"; LBA \"_LList_Tail1\"; CLC 1; LBL \"1\"; LDI $1,0,RS")
			      [val l] val l (* all but first, error if empty *);
	   };

    Long: type (simple
		constants
		    "EXT \"_Long_%c\"; LBA \"_Long_%c\"; LDL RS"
		    "LBA \"1\"; IDT 1; IDT %s; LBA \"1b\"; LDL RS"
		    "%c"
		    9  (* Max number of digits understood by assembler *)
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
	% :     func (inline "ARG 2,$2; HINT NP; ARG 1,$1; EXT \"_Long_Mod\"; HINT AL,0,1; LBA \"_Long_Mod\"; CLC 2; MOV RL,RS")
		     [val L; val Short] val Short;
	% :     func (inline "HINT NP; ARG 2,$2; HINT NP; ARG 1,$1; EXT \"_Long_LMod\"; HINT AL,0,1; LBA \"_Long_LMod\"; CLC 2; MOV RL,RS")
		     [val L; val L] val L;
	* :     func (inline "HINT NP; ARG 2,$2; HINT NP; ARG 1,$1; EXT \"_Long_Mult\"; HINT AL,0,1; LBA \"_Long_Mult\"; CLC 2; MOV RL,RS")
		     [x,y:val L] val L;
	+ :     func (inline "HINT NP; ARG 2,$2; UDC $2; HINT NP; ARG 1,$1; UDC $1; EXT \"_Long_Add\"; HINT AL,0,1; LBA \"_Long_Add\"; CLC 2; MOV RL,RS")
		     [x,y:val L] val L;
	- :     func (inline "HINT NP; ARG 1,$1; UDC $1; EXT \"_Long_Neg\"; HINT AL,0,1; LBA \"_Long_Neg\"; CLC 1; MOV RL,RS")
		     [x:val L] val L;
	- :     func (inline "HINT NP; ARG 2,$2; UDC $2; HINT NP; ARG 1,$1; UDC $1; EXT \"_Long_Sub\"; HINT AL,0,1; LBA \"_Long_Sub\"; CLC 2; MOV RL,RS")
		     [x,y:val L] val L;
	/ :     func (inline "ARG 2,$2; HINT NP; ARG 1,$1; EXT \"_Long_Div\"; HINT AL,0,1; LBA \"_Long_Div\"; CLC 2; MOV RL,RS")
		     [val L; val Short] val L;
	/ :     func (inline "HINT NP; ARG 2,$2; HINT NP; ARG 1,$1; EXT \"_Long_LDiv\"; HINT AL,0,1; LBA \"_Long_LDiv\"; CLC 2; MOV RL,RS")
		     [val L; val L] val L;
	:= :    func (standard := 1)
		     (inline "STI $1,0,$2; MOV $2,RS")
		     [var L; val L] val L;
	< :     func (inline "HINT NP; ARG 2,$2; HINT NP; ARG 1,$1; UDC $1; UDC $2; EXT \"_Long_cmp\"; LBA \"_Long_cmp\"; CLC 2; LTI RL,C0,RS")
		     [x,y:val L] val Boolean;
	<= :    func (inline "HINT NP; ARG 2,$2; HINT NP; ARG 1,$1; UDC $1; UDC $2; EXT \"_Long_cmp\"; LBA \"_Long_cmp\"; CLC 2; LEI RL,C0,RS")
		     [x,y:val L] val Boolean;
	<> :    func (inline "HINT NP; ARG 2,$2; HINT NP; ARG 1,$1; UDC $1; UDC $2; EXT \"_Long_cmp\"; LBA \"_Long_cmp\"; CLC 2; NEI RL,C0,RS")
		     [x,y:val L] val Boolean;
	= :     func (inline "HINT NP; ARG 2,$2; HINT NP; ARG 1,$1; UDC $1; UDC $2; EXT \"_Long_cmp\"; LBA \"_Long_cmp\"; CLC 2; EQI RL,C0,RS")
		     [x,y:val L] val Boolean;
	> :     func (inline "HINT NP; ARG 2,$2; HINT NP; ARG 1,$1; UDC $1; UDC $2; EXT \"_Long_cmp\"; LBA \"_Long_cmp\"; CLC 2; GTI RL,C0,RS")
		     [x,y:val L] val Boolean;
	>= :    func (inline "HINT NP; ARG 2,$2; HINT NP; ARG 1,$1; UDC $1; UDC $2; EXT \"_Long_cmp\"; LBA \"_Long_cmp\"; CLC 2; GEI RL,C0,RS")
		     [x,y:val L] val Boolean;
	In :    func (inline "ALA C2,RS; STI RS,0,C1; STI RS,1,$1")
		     [val Short] val L;
	New:    func (standard ptr_New 1)
		     (inline "ALH C1,RS; STI RS,0,C0")
		     [] var L;
	New:    func (standard init_New 1)
		     (inline "ALH C1,RS; STI RS,0,$1")
		     [val L] var L;
	Out:    func (inline "HINT OPT,8; LDI $1,0,T1; EQI T1,C1,TL; BRT \"1f\"; ARG 1,$1; EXT \"_Long_Out\"; LBA \"_Long_Out\"; CLC 1; LBL \"1\"; LDI $1,1,RS")
		     [val L] val Short;
	V:      func (standard V 1)
		     (inline "LDI $1,0,RS")
		     [var L] val L;
	^+ :    func (inline "HINT NP; ARG 2,$2; HINT NP; ARG 1,$1; EXT \"_Long_Concat\"; HINT AL,0,1; LBA \"_Long_Concat\"; CLC 2; MOV RL,RS")
		     [x,y: val L] val L;
	nbits : func (inline "HINT NP; ARG 1,$1; EXT \"_Long_Nbits\"; HINT AL,2,1; LBA \"_Long_Nbits\"; CLC 1; MOV RL,RS")
		     [val L] val L;
	nwords: func (inline "DCL T2,INT; ALA C2,T2; LDI $1,0,T1; STI T2,0,C1; STI T2,1,T1; MOV T2,RS; UDC T2")
		     [val L] val L;
	odd:    func (inline "LDI $1,1,T1; AND T1,C1,RS")
		     [val L] val Boolean;
	put :   func (standard put 0)
		     (inline "HINT PT; ARG 1,$1; EXT \"_Long_Put\"; LBA \"_Long_Put\"; CLC 1; MOV RL,RS")
		     [val L] val L;
	puts:   func (inline "HINT NP; ARG 1,$1; EXT \"_Long_Puts\"; HINT AL,0,1; LBA \"_Long_Puts\"; CLC 1; MOV RL,RS")
		     [val L] val ChStr;
	shift : func (inline "HINT NP; ARG 2,$2; UDC $2; HINT NP; ARG 1,$1; UDC $1; EXT \"_Long_Shift\"; HINT AL,0,1; LBA \"_Long_Shift\"; CLC 2; MOV RL,RS")
		     [x,amount: val L] val L;
    };
    Callcc: func (standard Callcc 0)
		 [ body: func[ cc: func[ val T ] val Void; var Void ] val T;
		   T: type {};
		   var Void
		 ] val T;
    Signal: func (standard Callcc 0) (* Saves a continuation *)
		 [ sig_num: val Short; var Void] val Boolean;
    Unsignal: func[ sig_num: val Short; var Void] val Void;
    Expand_Hp: func[ val Short ] val Void;
] 
{
    let
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
	ni ni ni ni; Null[]
    ni
}
