ERLC=erlc
ERLC_FLAGS= +native
ERL_FILES=conc.erl conc_cserver.erl conc_tserver.erl \
  conc_eval.erl conc_load.erl conc_lib.erl bin_lib.erl
HRL_FILES=conc_lib.hrl
BEAM_FILES=$(patsubst %.erl,%.beam,$(ERL_FILES))

default: $(BEAM_FILES)

bin_lib.beam: bin_lib.erl
	$(ERLC) $<

%.beam: %.erl $(HRL_FILES)
	$(ERLC) $(ERLC_FLAGS) $<

clean:
	rm $(BEAM_FILES)
