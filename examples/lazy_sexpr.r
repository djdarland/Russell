# define impure var Void
# define skip Null[]

(* Compile with rc -cpL *)

(* Lazy S-expression implementation.  A SEdelay primitive is added.  *)
(* Evaluation is automatically forced where appropriate.             *)
(* The SEdelay function is intended to be sugared as follows:        *)
# define delay(se) SEdelay[func[impure] {se}]

func[atype : type {}] {
  let
    UN == union U { atom : val atype;
                    cn: val CN;
                    lazy: func[impure] val Ref[UN];
                    empty_list: val Void };
    CN == prod { hd: val Ref[UN]; tl : val Ref[UN] };
    SE ==
      extend {Ref[UN]}
      hide { Out <<func[val extend {Ref[UN]}; impure] var UN>> }
      with SE {
        create_ref == func[x:val UN] val SE {
                        let
                            y == UN$New[]
                        in
                            y := x;
                            SE$In[y]
                        ni
                      };
        force == func[x: val SE; impure] val SE {
                    if
                        is_lazy[Out[x]^] ==>
                            (x^) := ((to_lazy[x^][])^); x
                    #   else        ==>
                            x
                    fi
                 };
        SEdelay == func[f: func[impure] val SE] val SE {
                     SE$create_ref[
                       UN$from_lazy[
                         func[impure] val Ref[UN] {
                           SE$Out[f[]]
                         }
                       ]
                     ]
                   };
        car == func[x: val SE; impure] val SE {
                  SE$In[hd[to_cn[force[x]^]]]
               };
        cdr == func[x: val SE; impure] val SE {
                  SE$In[tl[to_cn[force[x]^]]]
               };
        cons == func[x,y: val SE] val SE {
                  SE$create_ref[
                    UN$from_cn[ CN$Mk [SE$Out[x], SE$Out[y]] ]
                  ]
                };
        In == func[x: val atype] val SE {SE$create_ref[UN $from_atom[x]]};
        Out == func[x: val SE; impure] val atype {to_atom[force[x]^]};
        atom == func [x: val SE; impure] val Boolean {is_atom[force[x]^]};
        null == func [x: val SE; impure] val Boolean {is_empty_list[force[x]^]};
        nil == func[] val SE { SE$create_ref[UN$from_empty_list[Null]] }
      }
    in
      SE export { New; := ; V; car; cdr; cons; force; SEdelay;
                  In <<func[val atype] val SE>>;
                  Out <<func[val SE; impure] val atype>>;
                  atom; null; nil }
    ni
}

