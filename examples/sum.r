let
    size == 30000;
    A == (Array[size,Short])$New[];
    n == Short$New[];
    sum == Long$New[];
in
    n := 0;
    do
        n < size  ==>  A.n := n; n += 1
    od;
    n := 0;
    sum := Long$0;
    do
        n < size  ==>  sum := sum + Long$In[A.n]; n += 1
    od;
    put[sum]; put["\n"]
ni
