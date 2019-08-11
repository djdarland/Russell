(* Generic exponential with Long exponent.  Uses O(log n) multiplications *)

# define IMPURE
# undef IMPURE

# ifndef IMPURE
#   define impure
# endif

(* Compute y^x, x >= 0 *)
func[y: val T; x: val Long;
     T : type t {New; := ; V;
		 * : func[x,y: val t; impure] val t;
		 '1': func[] val t};
     impure] val T {
   let
     (* curr_exp == Long$New[]; *)
     curr_power == T$New[];
     curr_result == T$New[];
     rem_x == Long$New[];
   in use Long in
     (* curr_exp := 1; *)
     curr_power := y;
     curr_result := T$'1'[];
     rem_x := x;
     do rem_x > 0 ==>
       (* curr_result * y^(rem_x*curr_exp)  =  y^x *)
       (* curr_power = y^curr_exp *)
       if odd[rem_x] then
	   (* rem_x -= 1; *)
	   curr_result := curr_result * curr_power
       fi;
       (* curr_exp *= 2; *)
       rem_x := shift[rem_x, -1];
       curr_power := curr_power * curr_power;
     od;
     curr_result
   ni ni
}

