(* Compile with -p *)
#define L val Short
#define R val Short
#define L_to_R func[L]R

(*
 * Applicative Call-by-Value Least Fixed Point
 *)

let
    Y == 
            func[ h: func[ L_to_R ] L_to_R ] L_to_R {
               let
                  T == prod{ fn: func[ val T ] L_to_R } ;

                  f == func[ v: val T ] L_to_R {
                          h [
                             func[ x: L ] R {
                                ((fn[v])[v])[x]
                             }
                          ] 
                       }
               in
                  f[T$Mk[f]]
               ni
            } ;

         G == 
            func[ f: func[val Short] val Short ] {
               func[ n: val Short ] {
                   if
                     n > 0 ==>
                       n * f[n - 1]
                   # n = 0 ==>
                       1
                   fi
               }
            } ;

         ! == Y[G]

in

      put[6!]; put["\n"]

ni

