
all: deps compile

compile:
	./rebar compile

deps:
	./rebar get-deps

clean: 
	./rebar clean


distclean: clean
	./rebar delete-deps

trigramsS.tab: src/354984si.ngl.gz ebin/indexer_trigrams.beam
	@erl -pa ebin -noshell -s indexer_trigrams make_tables\
                                        -s init stop



