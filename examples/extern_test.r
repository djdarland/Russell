let
    fork == func[var Void] val Short { extern "_fork" };
    sleep == func[val Short; var Void] val Void { extern "_sleep" };
    pid == fork[];
in
    if
	pid = 0 ==> sleep[5]; put["Child here\n"];
    #   pid <> 0 ==> sleep[10]; put["Parent here\n"];
    fi
ni
