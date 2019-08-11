(* Binary trees parametrized with respect to type of data stored in leaves *)
(* Other variants are in sexpr.r, tree.r                                   *)
func[atype : type {}] {
     union U { atom : val atype;
               cn: val prod { hd: val U; tl : val U };
               empty_list: val Void }
     with SE {
        car == func[x: val SE] val SE {
                  hd[to_cn[x]]
               };
        cdr == func[x: val SE] val SE {
                  tl[to_cn[x]]
               };
        cons == func[x,y: val SE] val SE {
                  SE$from_cn[
                    prod { hd: val SE; tl: val SE } $ Mk [x, y]
                  ]
               };
        In == SE$from_atom;
        Out == SE$to_atom;
        atom == SE$is_atom;
        null == SE$is_empty_list;
        nil == func[] { SE$from_empty_list[Null] }  }
        export { New; := ; V; car; cdr; cons; In; Out; atom; null; nil }
}

