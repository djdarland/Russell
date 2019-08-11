let
    z == 0.0;
    Complex == record { re,im : Float }
               with C {
                        * == func[x,y : val C]
                                 { C$Mk[re[x]*re[y] - im[x]*im[y],
                                        re[x]*im[y] + im[x]*re[y]] } };
    x == Complex$New[];
    i == Short$New[];
in
    re[x] := 0.707; im[x] := 0.707;  (* roughly 8th root of unity *)
    i := 0;
    do  i < 4 ==>   put[re[x]]; put[" + "]; put[im[x]]; put["i\n"];
                    x := x * x;
                    i := i + 1;
    od
ni
