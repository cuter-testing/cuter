ERLC=erlc
ERLC_FLAGS= +native
ERL_FILES=conc.erl conc_cserver.erl conc_tserver.erl \
  conc_eval.erl conc_load.erl conc_lib.erl bin_lib.erl
HRL_FILES=conc_lib.hrl
BEAM_FILES=$(patsubst %.erl,%.beam,$(ERL_FILES))

default: $(BEAM_FILES)

$(BEAM_FILES): $(ERL_FILES) $(HRL_FILES)
	$(ERLC) $(ERLC_FLAGS) $(ERL_FILES)
  
clean:
	rm $(BEAM_FILES)
