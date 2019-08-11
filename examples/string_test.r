let
    c == ChStr$New[]
    ; s == ChStr$New[]
    ; newlinecode == 10
    ; newline == func[]
		    { put[ ChStr$In[newlinecode] ] }
in
    put["enter a line:"]
    ; newline[]
    ; s := (c := getchar[FS])
    ; do
	Out[c] <> newlinecode ==> c := getchar[FS]
				  ; s := s ^* c
      od
    ; put["thanks, your string was"]
    ; newline[]
    ; put[ s ]
ni
