let
    GaussianInteger ==
                record { Real,Imaginary: Short }
                with GI{
                    i ==  func[] val GI {
                              GI$Mk[0,1]
                          };
                    toGI == func[x: val Short] val GI {
                                GI$Mk[x,0]
                            };
                    + == func[x,y: val GI] {
                            GI$Mk[
                                Real[x] + Real[y],
                                Imaginary[x] + Imaginary[y]
                            ]
                         };
                    - == func[x,y: val GI] {
                            GI$Mk[
                                Real[x] - Real[y],
                                Imaginary[x] - Imaginary[y]
                            ]
                         };
                    * == func[x,y: val GI] {
                            GI$Mk[
                              Real[x] * Real[y] - Imaginary[x] * Imaginary[y],
                              Real[x] * Imaginary[y] + Imaginary[x] * Real[y]
                            ]
                        };
                    put == func[x: val GI] val Void {
                                   put[Real[x]];
                                   put["+"];
                                   put[Imaginary[x]];
                                   put["i"];
                               }
                }
                hide {Mk}
in use GaussianInteger in
    put[ toGI[5] + toGI[2] * i]; put ["\n"]
ni ni

