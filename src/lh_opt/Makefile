RC= rc
Rf= -CNO

../RICopt: prepass.r read.i summaryio.o classify.i varstuff.i firstpass.i lastpass.i finalpass.i graph.o auxfns.o flocs.o labels.o rsort.o hash.o vars.o summary.o out.o refnum.o rc.o loclist.o locs.o defs.h ../root_path.h
	$(RC) $(Rf) -pL -o ../RICopt prepass.r auxfns.o

summaryio.o: summaryio.r
	$(RC) $(Rf) -pL -c summaryio.r

graph.o: graph.r hash.o resultsummary.o argsummary.o summary.o defs.h
	$(RC) $(Rf) -pL -c graph.r

resultsummary.o: resultsummary.r defs.h
	$(RC) $(Rf) -pL -c resultsummary.r

argsummary.o: argsummary.r
	$(RC) $(Rf) -pL -c argsummary.r

summary.o: summary.r hash.r defs.h
	$(RC) $(Rf) -pL -c summary.r

auxfns.o: auxfns.c
	cc -c auxfns.c

flocs.o: flocs.r loclist.o hash.o defs.h
	$(RC) $(Rf) -pL -c flocs.r

rc.o: rc.r refnum.o locs.o loclist.o defs.h
	$(RC) $(Rf) -pL -c rc.r

locs.o: locs.r loclist.o hash.o defs.h
	$(RC) $(Rf) -pL -c locs.r

args.o: args.r
	$(RC) $(Rf) -pL -c args.r

defs.h: op_codes.h

loclist.o: loclist.r defs.h
	$(RC) $(Rf) -pL -c loclist.r

refnum.o: refnum.r
	$(RC) $(Rf) -pL -c refnum.r

out.o: out.r defs.h
	$(RC) $(Rf) -pL -c out.r

hash.o: hash.r
	$(RC) $(Rf) -pL -c hash.r

labels.o: labels.r hash.o defs.h
	$(RC) $(Rf) -pL -c labels.r

rsort.o: rsort.r
	$(RC) $(Rf) -pL -c rsort.r

vars.o : vars.r slist.o defs.h
	$(RC) $(Rf) -pL -c vars.r

slist.o: slist.r dlist.o
	$(RC) $(Rf) -pL -c slist.r

dlist.o: dlist.r
	$(RC) $(Rf) -pL -c dlist.r
