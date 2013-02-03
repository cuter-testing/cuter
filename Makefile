.PHONY: clean distclean all demo count

ERLC = erlc
WARNS = +warn_exported_vars +warn_unused_import +warn_missing_spec #+warn_untyped_record
ERLC_FLAGS = +debug_info $(WARNS)
ERL_FILES = conc.erl conc_cserver.erl conc_tserver.erl conc_symb.erl \
  conc_eval.erl conc_load.erl conc_lib.erl conc_encdec.erl coord.erl \
  bin_lib.erl
DEMO_BIN = demos/ebin
DEMO_SRC = demos/src
ERL_DEMO = genstress.erl bang.erl big.erl ehb.erl ets_test.erl mbrot.erl \
  parallel.erl pcmark.erl ran.erl serialmsg.erl timer_wheel.erl demo.erl
HRL_FILES = conc_lib.hrl
BEAM_FILES = $(patsubst %.erl,%.beam,$(ERL_FILES))
FULL_ERL_DEMO = $(patsubst %.erl,$(DEMO_SRC)/%.erl,$(ERL_DEMO))
BEAM_DEMO = $(patsubst $(DEMO_SRC)/%.erl,$(DEMO_BIN)/%.beam,$(FULL_ERL_DEMO))

default: $(BEAM_FILES) dialyzer

fast: $(BEAM_FILES)

all: $(BEAM_FILES) dialyzer demo

demo: $(BEAM_DEMO)

bin_lib.beam: bin_lib.erl
	$(ERLC) $(ERLC_FLAGS) $<

%.beam: %.erl $(HRL_FILES)
	$(ERLC) +native $(ERLC_FLAGS) $<

dialyzer: $(BEAM_FILES)
	dialyzer -Wunmatched_returns $(BEAM_FILES)

$(DEMO_BIN)/%.beam: $(DEMO_SRC)/%.erl
	$(ERLC) -o $(DEMO_BIN) $<

$(BEAM_DEMO): | $(DEMO_BIN)

$(DEMO_BIN):
	mkdir -p $(DEMO_BIN)

clean:
	$(RM) $(BEAM_FILES)

distclean: clean
	$(RM) -rf $(DEMO_BIN)

count:
	wc -l $(ERL_FILES)

