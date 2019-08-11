(* Integer modular arithmetic.  Returns a type that represents the  *)
(* ring of integers mod n.  We always represent an equivalence      *)
(* class by a number between 0 and n-1.                             *)
func[N: val Long] {
    let
	xl == extern {"xlong"} [];
	n == xl$from_Long[N];
    in use xl in
	extend { xl } hide {to_Long} with M {
	    times == func[x,y: val M] { M$In[Out[x * y] % n] };
	    plus == func[x,y: val M] { M$In[Out[x + y] % n] };
	    minus == func[x,y: val M] { M$In[n + Out[x - y] % n] };
	    neg == func[x: val M] { M$In[(n - Out[x]) % n] };
	    one == func[] {M$In[1]};
	    zero == func[] {M$In[0]};
	} with M {
	    * == M$times;
	    + == M$plus;
	    - == M$minus;
	    - == M$neg;
	    '1' == M$one;
	    '0' == M$zero;
	    from_Long == func[x: val Long] {
			      if x >= Long$0 then
				  M$In[from_Long[x]%n]
			      else
				  let
				      rem == (-from_Long[x])%n
				  in
				    M$In[
				      if rem = 0 then
					0
				      else
					n - rem
				      fi
				    ]
				  ni
			      fi
			 };
	    to_Long == func[x: val M] { to_Long[M$Out[x]] };
	} export M {+; *; put; puts; :=; V; from_Long; to_Long; =; <>;
		    New <<func[val M] var M>>; New <<func[]var M>>;
		    - <<func[x,y: val M]val M>>;
		    - <<func[val M]val M>>; '0'; '1';
		   }   
    ni ni
}
