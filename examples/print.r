let
    to_ChStr == func[x: val Short] {ChStr$In[x + Out["0"]]};
    base == Short$10;
    print1 == func[x: val Long] val Void {
                  use Long in
                    if 
                      x > 0 ==> print1[x/base]; put[to_ChStr[x%base]]
                    # x = 0 ==> Null
                    # x < 0 ==> put["-"]; print1[x]
                    fi
                  ni
              };
    print == func[x: val Long] val Void {
                use Long in
                  if
                    x = 0 ==> put["0"];
                  # else  ==> print1[x];
                  fi
                ni
             };
    ** == func[x,y: val Long] {
            use Long in
                if
                    y = 0 ==> 1
                #   else  ==> x**(y-1) * x
                fi
            ni
          }
in
    use Long in
        print[12345];
        put["\n"];
        print[2**32];
        put["\n"];
    ni
ni
