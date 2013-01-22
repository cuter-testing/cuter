.PHONY: clean distclean all bench count

ERLC = erlc
ERLC_FLAGS = +native
ERL_FILES = conc.erl conc_cserver.erl conc_tserver.erl conc_symb.erl \
  conc_eval.erl conc_load.erl conc_lib.erl bin_lib.erl
BENCH_BIN = bench/ebin
BENCH_SRC = bench/src
ERL_BENCH = genstress.erl bang.erl big.erl ehb.erl ets_test.erl mbrot.erl \
  parallel.erl pcmark.erl ran.erl serialmsg.erl timer_wheel.erl
HRL_FILES = conc_lib.hrl
BEAM_FILES = $(patsubst %.erl,%.beam,$(ERL_FILES))
FULL_ERL_BENCH = $(patsubst %.erl,$(BENCH_SRC)/%.erl,$(ERL_BENCH))
BEAM_BENCH = $(patsubst $(BENCH_SRC)/%.erl,$(BENCH_BIN)/%.beam,$(FULL_ERL_BENCH))

default: $(BEAM_FILES)

all: $(BEAM_FILES) bench

bench: $(BEAM_BENCH)

bin_lib.beam: bin_lib.erl
	$(ERLC) $<

%.beam: %.erl $(HRL_FILES)
	$(ERLC) $(ERLC_FLAGS) $<

$(BENCH_BIN)/%.beam: $(BENCH_SRC)/%.erl
	$(ERLC) $(ERLC_FLAGS) -o $(BENCH_BIN) $<

$(BEAM_BENCH): | $(BENCH_BIN)

$(BENCH_BIN):
	mkdir -p $(BENCH_BIN)

clean:
	$(RM) $(BEAM_FILES)

distclean: clean
	$(RM) -rf $(BENCH_BIN)

count:
	wc -l $(ERL_FILES)

