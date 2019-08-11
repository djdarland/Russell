
let
    ! == func [ n : val Short ]
                {  
                    if 
                        n > 0 ==> Long$In[n] * ((n - 1)!)
                    #   n = 0 ==> Long$1
                    fi
                 };
    x == Short$New[];
in
    do
        (put["Factorial of?"]; x := get[FS]) >= 0  ==>  put[x!]; put["\n"]
    od
ni
