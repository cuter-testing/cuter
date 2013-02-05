.PHONY: clean distclean all demo

###----------------------------------------------------------------------
### Orientation information
###----------------------------------------------------------------------

TOP  = $(PWD)
SRC  = src
EBIN = ebin

ERLC = erlc

###----------------------------------------------------------------------
### Flags
###----------------------------------------------------------------------

WARNS = +warn_exported_vars +warn_unused_import +warn_missing_spec #+warn_untyped_record
ERLC_FLAGS = +debug_info $(WARNS)

SRC_MODULES = \
	bin_lib \
	concolic \
	concolic_cserver \
	concolic_encdec \
	concolic_eval \
	concolic_lib \
	concolic_load \
	concolic_symbolic \
	concolic_tserver \
	coordinator

## THIS CHUNK TO BE REVISED
DEMO_BIN = demos/ebin
DEMO_SRC = demos/src
ERL_DEMO = genstress.erl bang.erl big.erl ehb.erl ets_test.erl mbrot.erl \
  parallel.erl pcmark.erl ran.erl serialmsg.erl timer_wheel.erl demo.erl
FULL_ERL_DEMO = $(patsubst %.erl,$(DEMO_SRC)/%.erl,$(ERL_DEMO))
BEAM_DEMO = $(patsubst $(DEMO_SRC)/%.erl,$(DEMO_BIN)/%.beam,$(FULL_ERL_DEMO))

###----------------------------------------------------------------------
### Targets
###----------------------------------------------------------------------

TARGETS = concolic_src

ERL_DIRS = src

vpath %.erl $(ERL_DIRS)

default: $(TARGETS) dialyzer

fast: $(TARGETS)

all: default demo

concolic_src:	$(SRC_MODULES:%=$(EBIN)/%.beam)

demo: $(BEAM_DEMO)

$(EBIN)/bin_lib.beam: $(SRC)/bin_lib.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN) $<

$(EBIN)/%.beam: %.erl
	$(ERLC) +native $(ERLC_FLAGS) -o $(EBIN) $<

dialyzer: $(TARGETS)
	dialyzer -n -Wunmatched_returns $(EBIN)/*.beam

$(DEMO_BIN)/%.beam: $(DEMO_SRC)/%.erl
	$(ERLC) -o $(DEMO_BIN) $<

$(BEAM_DEMO): | $(DEMO_BIN)

$(DEMO_BIN):
	mkdir -p $(DEMO_BIN)

clean:
	$(RM) $(EBIN)/*.beam

distclean: clean
	$(RM) -rf $(DEMO_BIN)
