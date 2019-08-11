let
    j == Long$17
in
  trace := True;
  put[
      Callcc[
        func[ cc: func[ val Short] val Void; var Void ] val Short {
            let
                i == Short$New[];
                j == Short$New[];
            in
                put "Got to the body\n";
                j := 13;
                put "Forcing garbage collection:\n";
                do i < 20000 ==>
                    Short$New[]; i += 1;
                od;
                put "Finished allocating ";
                put i;
                put " objects\n";
                cc[j];
                0
            ni
        }
      ]
  ];
  put "\n";
  put "The last line should have contained a 13\n";
  put j;
  put "\nThe last line should have contained a 17\n";
ni
