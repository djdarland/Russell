# Highly language dependent "optimizer"
# Assumptions -
#  - #COMP and #--- are used only as advertised in initenv.doc
#  - #ADD and #SUB are used as in initenv.r
#  - locations beyond the top of the stack are dead
#    except for between multiple instructions on #COMP line
#  - only #COMP lines may contain more than one instruction
#  - only instructions whose mnemonic starts with a 'j' rely on condition
#    codes
BEGIN           { push_suppressed = 0;
		  cc_not_set = 0;
		  suppressing = 0;
		  clobber_next = 0;
		}

suppressing > 2 { # in the middle of suppressing comparison
		  suppressing--;
		  suppressed[supp_size - suppressing] = $0;
		  next;
		}

/.*---/         { # runtime check - removed by optimizer
		    next;
		}

/.*/            { if (clobber_next) {
		      clobber_next = 0;
		      next;
		  }
                  if ($1 ~ /[0-9a-zA-Z_]*\:/) i = 2
		  else i = 1
		}

suppressing == 2  { if ($i == "movl" && $(i+1) == "(sp)+,r0") {
			suppressing--;
			suppressed[supp_size - 1] = $0;
			next;
		    } else {
			suppressing = 0;
			for (j = 0; j < supp_size - 1; j++) {
			    print suppressed[j];
			}
			print $0;
			next;
		    }
		  }
		    

suppressing == 1  { suppressing = 0;
                    if ($i == "jeql") {
			printf "\t%s\t%s\n", comp_op, $(i+1);
			next;
		    } else {
			for (j = 0; j < supp_size; j ++) {
			  print suppressed[j];
			}
			print $0;
			next;
		    }
		  }

/#COMP/           { # start of comparison
	            # suppress instructions following this one
		    if (push_suppressed) {
			printf "\tpushl\t%s\n", push_arg;
			push_suppressed = 0;
		    }
		    j = 1;
		    while ($j != "#COMP") { j++ };
		    suppressing = $(j+2) + 1;
		    comp_op = $(j+1);
		    supp_size = suppressing - 1; # of instructions stored
		  }

/#ADD/		  { if (push_suppressed) {
			printf "\taddw2\t%s,(sp)\n", push_arg;
			push_suppressed = 0;
			clobber_next = 1;
			next;
		    }
		  }

/#SUB/		  { if (push_suppressed) {
			printf "\tsubw3\t(sp),%s,(sp)\n", push_arg;
			push_suppressed = 0;
			clobber_next = 1;
			next;
		    }
		  }

$i == "pushl"   { # generate push only if next line is not a "pop"
		      if (push_suppressed) {
			  printf "\tpushl\t%s\n", push_arg;
		      }
		      push_arg = $(i+1);
		      push_suppressed = 1;
		      if (i == 2) { # emit label
			printf "%s\n", $1
		      }
		      next;
		}

($1 == "movl") && ($2 ~ /\(sp\)\+,.*/) {
		  # This is a "pop"
		    if (push_suppressed) {
			operands = $(i+1);
			dest = substr(operands,7,50);
			if (push_arg != dest) {
			    printf "\tmovl\t%s,%s\n", push_arg, dest;
			} else {
			    cc_not_set = 1;
			}
			push_suppressed = 0;
			next;
		    }
		}

($i ~ /j.*/)    { # Looks like a branch instruction
		  if (cc_not_set) {
		      printf "\ttstl\t%s\n", dest;
		  }
		}

		{ if (push_suppressed) {
		      push_suppressed = 0;
		      printf "\tpushl\t%s\n", push_arg;
		  }
		  print;
		  cc_not_set = 0;
		}
