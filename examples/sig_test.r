let
    SIGINT == 2;
    i == Short$New[];
    intr_received == Signal[SIGINT];
in
    if
        intr_received ==> put "Caught one\n"; Unsignal[SIGINT]
    #   else ==> Null[];
    fi;
    do True ==>
        i := 0;
        do i < 1000 ==>
            Short$New[]; i += 1;
        od;
        put "x\n";
    od 
ni
