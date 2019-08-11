		let bq == basics.q;
		    op == opc [bq]
		in if op = DCL ==> 
				gen [to_arg [arg1 [bq]], vars]

		    # op = UDC ==> 
				kill [to_arg [arg1 [bq]], vars]

		    # op = LDN ==> (* ignoring negative consts - not a size *)
				   (* can't handle negs as is *)
				   let signed_val == to_arg [arg1 [bq]]
				   in

					if signed_val >= 0 then
					 constant [arg2 [bq], signed_val, vars]
					fi
				   ni
		    # op = MOV ==> (* arg2 var := arg1 value *)
				   constant [arg2 [bq], 
					      value [to_arg [arg1 [bq]], vars],
						   vars]

		    # (op = ALH) cor (op = ALA) ==>
				   let size_var == to_arg [arg1 [bq]]
				   in
					if is_constant [size_var, vars] then
OPEN
put q;put " ALLOC size (determined by varstuff.i):";
CLOSE
					     let sz == value [size_var, vars]
					     in
OPEN
put [
CLOSE
					     (size [more.q])^ :=
						if (op = ALH) then sz
						  else - sz fi
OPEN
];put "\n"
CLOSE
					     ni

				   	fi
				   ni;
				   not_constant [arg2 [bq], vars]

		    (* other instructions that could mess up a const loc *)
		    # (op = LDI) cor (op = ADP) ==>
					not_constant [arg3 [bq], vars]
		    # (op = LDL) cor (op = LDS) ==>
					not_constant [to_arg [arg1 [bq]], vars]
		    # (op = TRU) cor (op = FLS) ==>
					not_constant [to_arg [arg1 [bq]], vars]
		    # else ==> Null []
		    fi
		ni;
