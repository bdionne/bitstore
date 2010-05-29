.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W +debug_info -DTEST $<

MODS = dag\
       bitstore

ERL = erl -boot start_clean 

compile: ${MODS:%=%.beam} 
	@echo "make clean - clean up beam files"

all: compile 

clean:	
	rm -rf *.beam erl_crash.dump 

