# -*- coding: utf-8 -*-

import cuter_common as cc
import cuter_logger as clg

class AbstractErlangSolver:
    """
    An abstract class that documents the API needed for solving.
    """

    def __init__(self):
        pass

    # =========================================================================
    # Public API.
    # =========================================================================

    def fix_parameter(self, parameter, value):
        """
        Fixes a symbolic variable to a specific value.
        """
        raise NotImplementedError("Method 'fix_parameter' is not implemented!")

    def add_axioms(self):
        """
        Adds the axioms from memory to the solver.
        """
        raise NotImplementedError("Method 'add_axioms' is not implemented!")

    def solve(self):
        """
        Solves a constraint set and returns the result.
        """
        raise NotImplementedError("Method 'solve' is not implemented!")

    def reset_solver(self):
        """
        Resets the solver.
        """
        raise NotImplementedError("Method 'reset_solver' is not implemented!")

    def encode_model(self):
        """
        Encodes the resulting model to JSON.
        """
        raise NotImplementedError("Method 'encode_model' is not implemented!")

    def command_toSolver(self, tp, json_data, rev):
        """
        Loads the recorded trace to memory.
        """
        opts_normal = {
            # Internal commands.
            cc.OP_PARAMS: self.mfa_params,
            cc.OP_SPEC: self.mfa_spec,
            cc.OP_UNFOLD_TUPLE: self.unfold_tuple,
            cc.OP_UNFOLD_LIST: self.unfold_list,
            cc.OP_MAKE_BITSTR: self.make_bitstr,
            cc.OP_CONCAT_SEGS: self.concat_segs,
            cc.OP_FRESH_LAMBDA_WITH_ARITY: self.fresh_closure,
            cc.OP_EVALUATED_CLOSURE: self.evaluated_closure,
            # Constraints.
            cc.OP_GUARD_TRUE: self.guard_true,
            cc.OP_GUARD_FALSE: self.guard_false,
            cc.OP_MATCH_EQUAL_TRUE: self.match_equal,
            cc.OP_MATCH_EQUAL_FALSE: self.match_not_equal,
            cc.OP_TUPLE_SZ: self.tuple_sz,
            cc.OP_TUPLE_NOT_SZ: self.tuple_not_sz,
            cc.OP_TUPLE_NOT_TPL: self.tuple_not_tpl,
            cc.OP_LIST_NON_EMPTY: self.list_nonempty,
            cc.OP_LIST_EMPTY: self.list_empty,
            cc.OP_LIST_NOT_LST: self.list_not_lst,
            cc.OP_EMPTY_BITSTR: self.empty_bitstr,
            cc.OP_NONEMPTY_BITSTR: self.nonempty_bitstr,
            cc.OP_BITMATCH_CONST_TRUE: self.bitmatch_const_true,
            cc.OP_BITMATCH_CONST_FALSE: self.bitmatch_const_false,
            cc.OP_BITMATCH_VAR_TRUE: self.bitmatch_var_true,
            cc.OP_BITMATCH_VAR_FALSE: self.bitmatch_var_false,
            cc.OP_LAMBDA: self.erl_lambda,
            # Erlang BIFs or MFAs treated as BIFs.
            cc.OP_HD: self.head,
            cc.OP_TL: self.tail,
            cc.OP_IS_INTEGER: self.is_integer,
            cc.OP_IS_ATOM: self.is_atom,
            cc.OP_IS_FLOAT: self.is_float,
            cc.OP_IS_LIST: self.is_list,
            cc.OP_IS_TUPLE: self.is_tuple,
            cc.OP_IS_BOOLEAN: self.is_boolean,
            cc.OP_IS_NUMBER: self.is_number,
            cc.OP_PLUS: self.plus,
            cc.OP_MINUS: self.minus,
            cc.OP_TIMES: self.times,
            cc.OP_RDIV: self.rdiv,
            cc.OP_IDIV_NAT: self.idiv_nat,
            cc.OP_REM_NAT: self.rem_nat,
            cc.OP_UNARY: self.unary,
            cc.OP_EQUAL: self.equal,
            cc.OP_UNEQUAL: self.unequal,
            cc.OP_FLOAT: self.to_float,
            cc.OP_BOGUS: self.bogus,
            cc.OP_ATOM_NIL: self.atom_nil,
            cc.OP_ATOM_HEAD: self.atom_head,
            cc.OP_ATOM_TAIL: self.atom_tail,
            cc.OP_LIST_TO_TUPLE: self.list_to_tuple,
            cc.OP_TUPLE_TO_LIST: self.tuple_to_list,
            cc.OP_LT_INT: self.lt_integers,
            cc.OP_LT_FLOAT: self.lt_floats,
            cc.OP_CONS: self.cons,
            cc.OP_TCONS: self.tcons,
            cc.OP_POW: self.pow,
            cc.OP_IS_BITSTRING: self.is_bitstring,
            cc.OP_IS_FUN: self.is_fun,
            cc.OP_IS_FUN_WITH_ARITY: self.is_fun_with_arity,
            cc.OP_TRUNC: self.trunc,
            cc.OP_BAND: self.band,
            cc.OP_BXOR: self.bxor,
            cc.OP_BOR: self.bor
        }

        opts_rev = {
            # Constraints.
            cc.OP_GUARD_TRUE: self.guard_true_reversed,
            cc.OP_GUARD_FALSE: self.guard_false_reversed,
            cc.OP_MATCH_EQUAL_TRUE: self.match_equal_reversed,
            cc.OP_MATCH_EQUAL_FALSE: self.match_not_equal_reversed,
            cc.OP_TUPLE_SZ: self.tuple_sz_reversed,
            cc.OP_TUPLE_NOT_SZ: self.tuple_not_sz_reversed,
            cc.OP_TUPLE_NOT_TPL: self.tuple_not_tpl_reversed,
            cc.OP_LIST_NON_EMPTY: self.list_nonempty_reversed,
            cc.OP_LIST_EMPTY: self.list_empty_reversed,
            cc.OP_LIST_NOT_LST: self.list_not_lst_reversed,
            cc.OP_EMPTY_BITSTR: self.empty_bitstr_reversed,
            cc.OP_NONEMPTY_BITSTR: self.nonempty_bitstr_reversed,
            cc.OP_BITMATCH_CONST_TRUE: self.bitmatch_const_true_reversed,
            cc.OP_BITMATCH_CONST_FALSE: self.bitmatch_const_false_reversed,
            cc.OP_BITMATCH_VAR_TRUE: self.bitmatch_var_true_reversed,
            cc.OP_BITMATCH_VAR_FALSE: self.bitmatch_var_false_reversed,
            cc.OP_NOT_LAMBDA_WITH_ARITY: self.not_lambda_with_arity_reversed,
            cc.OP_LAMBDA: self.erl_lambda_reversed,
            # Erlang BIFs or MFAs treated as BIFs.
            cc.OP_HD: self.head_reversed,
            cc.OP_TL: self.tail_reversed
        }

        # Call the appropriate function with the given payload.
        opts = opts_rev if rev else opts_normal
        if tp not in opts:
            fmt = ("Unknown JSON Command with\n"
                   "  ID: {}\n"
                   "  Data: {}\n"
                   "  Rev: {}")
            clg.debug_info(fmt.format(tp, json_data, rev))
        opts[tp](*json_data["a"])

    # =========================================================================
    # Private Methods.
    # =========================================================================

    # -------------------------------------------------------------------------
    # Parse internal commands.
    # -------------------------------------------------------------------------

    def mfa_params(self, *args):
        """
        Stores the entry point MFA's symbolic parameters.
        """
        raise NotImplementedError("Method 'mfa_params' is not implemented!")

    def mfa_spec(self, *spec):
        """
        Stores the spec of the entry point MFA.
        """
        raise NotImplementedError("Method 'mfa_spec' is not implemented!")

    def unfold_tuple(self, *terms):
        """
        Unfolds a symbolic tuple.
        """
        raise NotImplementedError("Method 'unfold_tuple' is not implemented!")

    def unfold_list(self, *terms):
        """
        Unfolds a symbolic list.
        """
        raise NotImplementedError("Method 'unfold_list' is not implemented!")

    def make_bitstr(self, symb, encodedValue, size):
        """
        Makes a bitstring by encoding an appropriate term.
        """
        raise NotImplementedError("Method 'make_bitstr' is not implemented!")

    def concat_segs(self, *terms):
        """
        Concatenates many bitstrings into a large binary.
        """
        raise NotImplementedError("Method 'concat_segs' is not implemented!")

    def fresh_closure(self, tFun, tArity):
        """
        Asserts that tFun is a closure with arity tArity.
        """
        raise NotImplementedError("Method 'fresh_closure' is not implemented!")

    def evaluated_closure(self, *args):
        """
        Asserts that the evaluation of a closure returns some specific terms.
        """
        raise NotImplementedError("Method 'evaluated_closure' is not implemented!")

    # -------------------------------------------------------------------------
    # Constraints.
    # -------------------------------------------------------------------------

    def guard_true(self, term):
        """
        Asserts the predicate: term1 == true
        """
        raise NotImplementedError("Method 'guard_true' is not implemented!")

    def guard_false(self, term):
        """
        Asserts the predicate: term1 == false
        """
        raise NotImplementedError("Method 'guard_false' is not implemented!")

    def match_equal(self, term1, term2):
        """
        Asserts the predicate: term1 == term2
        """
        raise NotImplementedError("Method 'match_equal' is not implemented!")

    def match_not_equal(self, term1, term2):
        """
        Asserts the predicate: term1 != term2
        """
        raise NotImplementedError("Method 'match_not_equal' is not implemented!")

    def tuple_sz(self, term, num):
        """
        Asserts that: term is a tuple of size num.
        """
        raise NotImplementedError("Method 'tuple_sz' is not implemented!")

    def tuple_not_sz(self, term, num):
        """
        Asserts that: term is not a tuple of size num.
        """
        raise NotImplementedError("Method 'tuple_not_sz' is not implemented!")

    def tuple_not_tpl(self, term, num):
        """
        Asserts that: term is not a tuple.
        """
        raise NotImplementedError("Method 'tuple_not_tpl' is not implemented!")

    def list_nonempty(self, term):
        """
        Asserts that: term is a nonempty list.
        """
        raise NotImplementedError("Method 'list_nonempty' is not implemented!")

    def list_empty(self, term):
        """
        Asserts that: term is an empty list.
        """
        raise NotImplementedError("Method 'list_empty' is not implemented!")

    def list_not_lst(self, term):
        """
        Asserts that: term is not list.
        """
        raise NotImplementedError("Method 'list_not_lst' is not implemented!")

    def empty_bitstr(self, term):
        """
        Asserts that: term is an empty bitstring.
        """
        raise NotImplementedError("Method 'empty_bitstr' is not implemented!")

    def nonempty_bitstr(self, term1, term2, term):
        """
        Asserts that: term is a nonempty bitstring.
        """
        raise NotImplementedError("Method 'nonempty_bitstr' is not implemented!")

    def bitmatch_const_true(self, termRest, cnstValue, size, termBitstr):
        """
        Asserts that: termBitstr == <<cnstValue/size, termRest>>.
        """
        raise NotImplementedError("Method 'bitmatch_const_true' is not implemented!")

    def bitmatch_const_false(self, cnstValue, size, termBitstr):
        """
        Asserts that: termBitstr =/= <<cnstValue/size, termRest>>.
        """
        raise NotImplementedError("Method 'bitmatch_const_false' is not implemented!")

    def bitmatch_var_true(self, term1, term2, size, termBitstr):
        """
        Asserts that: termBitstr == <<term1/size, term2>>.
        """
        raise NotImplementedError("Method 'bitmatch_var_true' is not implemented!")

    def bitmatch_var_false(self, size, termBitstr):
        """
        Asserts that: termBitstr =/= <<term1/size, term2>>.
        """
        raise NotImplementedError("Method 'bitmatch_var_false' is not implemented!")

    def erl_lambda(self, *args):
        """
        Asserts that a lambda application has succeeded.
        """
        raise NotImplementedError("Method 'erl_lambda' is not implemented!")

    # -------------------------------------------------------------------------
    # Reversed constraints.
    # -------------------------------------------------------------------------

    def guard_true_reversed(self, term):
        """
        Asserts the predicate: Not (term1 == true)
        """
        raise NotImplementedError("Method 'guard_true_reversed' is not implemented!")

    def guard_false_reversed(self, term):
        """
        Asserts the predicate: Not (term1 == false)
        """
        raise NotImplementedError("Method 'guard_false_reversed' is not implemented!")

    def match_equal_reversed(self, term1, term2):
        """
        Asserts the predicate: Not (term1 == term2)
        """
        raise NotImplementedError("Method 'match_equal_reversed' is not implemented!")

    def match_not_equal_reversed(self, term1, term2):
        """
        Asserts the predicate: Not (term1 != term2)
        """
        raise NotImplementedError("Method 'match_not_equal_reversed' is not implemented!")

    def tuple_sz_reversed(self, term, num):
        """
        Asserts that: term is not a tuple of size num.
        """
        raise NotImplementedError("Method 'tuple_sz_reversed' is not implemented!")

    def tuple_not_sz_reversed(self, term, num):
        """
        Asserts that: Not (term is not a tuple of size num).
        """
        raise NotImplementedError("Method 'tuple_not_sz_reversed' is not implemented!")

    def tuple_not_tpl_reversed(self, term, num):
        """
        Asserts that: Not (term is not a tuple).
        """
        raise NotImplementedError("Method 'tuple_not_tpl_reversed' is not implemented!")

    def list_nonempty_reversed(self, term):
        """
        Asserts that: Not (term is a nonempty list).
        """
        raise NotImplementedError("Method 'list_nonempty_reversed' is not implemented!")

    def list_empty_reversed(self, term):
        """
        Asserts that: Not (term is an empty list).
        """
        raise NotImplementedError("Method 'list_empty_reversed' is not implemented!")

    def list_not_lst_reversed(self, term):
        """
        Asserts that: Not (term is not list).
        """
        raise NotImplementedError("Method 'list_not_lst_reversed' is not implemented!")

    def empty_bitstr_reversed(self, term):
        """
        Asserts that: Not (term is an empty bitstring).
        """
        raise NotImplementedError("Method 'empty_bitstr_reversed' is not implemented!")

    def nonempty_bitstr_reversed(self, term1, term2, term):
        """
        Asserts that: Not (term is a nonempty bitstring).
        """
        raise NotImplementedError("Method 'nonempty_bitstr_reversed' is not implemented!")

    def bitmatch_const_true_reversed(self, termRest, cnstValue, size, termBitstr):
        """
        Asserts that: Not (termBitstr == <<cnstValue/size, termRest>>).
        """
        raise NotImplementedError("Method 'bitmatch_const_true_reversed' is not implemented!")

    def bitmatch_const_false_reversed(self, cnstValue, size, termBitstr):
        """
        Asserts that: Not (termBitstr =/= <<cnstValue/size, termRest>>).
        """
        raise NotImplementedError("Method 'bitmatch_const_false_reversed' is not implemented!")

    def bitmatch_var_true_reversed(self, term1, term2, size, termBitstr):
        """
        Asserts that: Not (termBitstr == <<term1/size, term2>>).
        """
        raise NotImplementedError("Method 'bitmatch_var_true_reversed' is not implemented!")

    def bitmatch_var_false_reversed(self, size, termBitstr):
        """
        Asserts that: Not (termBitstr =/= <<term1/size, term2>>).
        """
        raise NotImplementedError("Method 'bitmatch_var_false_reversed' is not implemented!")

    def erl_lambda_reversed(self, *args):
        """
        Asserts that a lambda application has failed.
        """
        raise NotImplementedError("Method 'erl_lambda_reversed' is not implemented!")

    def not_lambda_with_arity_reversed(self, tFun, tArity):
        """
        Asserts that: Not (tFun is not a function with arity tArity).
        """
        raise NotImplementedError("Method 'not_lambda_with_arity_reversed' is not implemented!")

    # -------------------------------------------------------------------------
    # Erlang BIFs or MFAs treated as BIFs.
    # -------------------------------------------------------------------------

    ### Operations on lists.

    def head(self, term1, term2):
        """
        Asserts that: term1 == hd(term2).
        """
        raise NotImplementedError("Method 'head' is not implemented!")

    def head_reversed(self, term1, term2):
        """
        Asserts that: Not (term1 == hd(term2)).
        """
        raise NotImplementedError("Method 'head_reversed' is not implemented!")

    def tail(self, term1, term2):
        """
        Asserts that: term1 == tl(term2).
        """
        raise NotImplementedError("Method 'tail' is not implemented!")

    def tail_reversed(self, term1, term2):
        """
        Asserts that: Not (term1 == tl(term2)).
        """
        raise NotImplementedError("Method 'tail_reversed' is not implemented!")

    def cons(self, term, term1, term2):
        """
        Asserts that: term = [term1 | term2].
        """
        raise NotImplementedError("Method 'cons' is not implemented!")

    ### Operations on atoms.

    def atom_nil(self, term, term1):
        """
        Asserts that: term = (term1 == '').
        """
        raise NotImplementedError("Method 'atom_nil' is not implemented!")

    def atom_head(self, term, term1):
        """
        Asserts that: term is the first character of term1.
        """
        raise NotImplementedError("Method 'atom_head' is not implemented!")

    def atom_tail(self, term, term1):
        """
        Asserts that: term is term1 without its first character.
        """
        raise NotImplementedError("Method 'atom_tail' is not implemented!")

    ### Operations on tuples.

    def tcons(self, *terms):
        """
        Asserts that: a term is tuple of many terms.
        """
        raise NotImplementedError("Method 'tcons' is not implemented!")

    ### Query types.

    def is_integer(self, term1, term2):
        """
        Asserts that: term1 == is_integer(term2).
        """
        raise NotImplementedError("Method 'is_integer' is not implemented!")

    def is_atom(self, term1, term2):
        """
        Asserts that: term1 == is_atom(term2).
        """
        raise NotImplementedError("Method 'is_atom' is not implemented!")

    def is_float(self, term1, term2):
        """
        Asserts that: term1 == is_float(term2).
        """
        raise NotImplementedError("Method 'is_float' is not implemented!")

    def is_list(self, term1, term2):
        """
        Asserts that: term1 == is_list(term2).
        """
        raise NotImplementedError("Method 'is_list' is not implemented!")

    def is_tuple(self, term1, term2):
        """
        Asserts that: term1 == is_tuple(term2).
        """
        raise NotImplementedError("Method 'is_tuple' is not implemented!")

    def is_boolean(self, term1, term2):
        """
        Asserts that: term1 == is_boolean(term2).
        """
        raise NotImplementedError("Method 'is_boolean' is not implemented!")

    def is_number(self, term1, term2):
        """
        Asserts that: term1 == is_number(term2).
        """
        raise NotImplementedError("Method 'is_number' is not implemented!")

    def is_bitstring(self, term1, term2):
        """
        Asserts that: term1 == is_bitstring(term2).
        """
        raise NotImplementedError("Method 'is_bitstring' is not implemented!")

    def is_fun(self, tResult, tFun):
        """
        Asserts that: tResult == is_function(tFun).
        """
        raise NotImplementedError("Method 'is_fun' is not implemented!")

    def is_fun_with_arity(self, tResult, tFun, tArity):
        """
        Asserts that: tResult == is_function(tFun, tArity).
        """
        raise NotImplementedError("Method 'is_fun_with_arity' is not implemented!")

    ### Arithmetic Operations.

    def plus(self, term, term1, term2):
        """
        Asserts that: term = term1 + term2.
        """
        raise NotImplementedError("Method 'plus' is not implemented!")

    def minus(self, term, term1, term2):
        """
        Asserts that: term = term1 - term2.
        """
        raise NotImplementedError("Method 'minus' is not implemented!")

    def times(self, term, term1, term2):
        """
        Asserts that: term = term1 * term2.
        """
        raise NotImplementedError("Method 'times' is not implemented!")

    def rdiv(self, term, term1, term2):
        """
        Asserts that: term = term1 / term2.
        """
        raise NotImplementedError("Method 'rdiv' is not implemented!")

    def idiv_nat(self, term, term1, term2):
        """
        Asserts that: term = term1 // term2.
        """
        raise NotImplementedError("Method 'idiv_nat' is not implemented!")

    def rem_nat(self, term, term1, term2):
        """
        Asserts that: term = term1 % term2.
        """
        raise NotImplementedError("Method 'rem_nat' is not implemented!")

    def unary(self, term, term1):
        """
        Asserts that: term = - term1.
        """
        raise NotImplementedError("Method 'unary' is not implemented!")

    def pow(self, term, term1, term2):
        """
        Asserts that: term = term1 ** term2.
        """
        raise NotImplementedError("Method 'pow' is not implemented!")

    def trunc(self, term, term1):
        """
        Asserts that: term is term1 truncated.
        """
        raise NotImplementedError("Method 'trunc' is not implemented!")

    ### Comparisons.

    def equal(self, term, term1, term2):
        """
        Asserts that: term = (term1 == term2).
        """
        raise NotImplementedError("Method 'equal' is not implemented!")

    def unequal(self, term, term1, term2):
        """
        Asserts that: term = (term1 =/= term2).
        """
        raise NotImplementedError("Method 'unequal' is not implemented!")

    def lt_integers(self, term, term1, term2):
        """
        Asserts that: term = (term1 < term2).
        """
        raise NotImplementedError("Method 'lt_integers' is not implemented!")

    def lt_floats(self, term, term1, term2):
        """
        Asserts that: term = (term1 < term2).
        """
        raise NotImplementedError("Method 'lt_floats' is not implemented!")

    ### Type conversions.

    def to_float(self, term, term1):
        """
        Asserts that: term = float(term1).
        """
        raise NotImplementedError("Method 'to_float' is not implemented!")

    def list_to_tuple(self, term, term1):
        """
        Asserts that: term = list_to_tuple(term1).
        """
        raise NotImplementedError("Method 'list_to_tuple' is not implemented!")

    def tuple_to_list(self, term, term1):
        """
        Asserts that: term = tuple_to_list(term1).
        """
        raise NotImplementedError("Method 'tuple_to_list' is not implemented!")

    ### Bogus operations (used for their side-effects in Erlang).

    def bogus(self, term, term1):
        """
        Asserts that: term == term1 (Identity function).
        """
        raise NotImplementedError("Method 'bogus' is not implemented!")

    ### Bitwise Operations.

    def band(self, term, term1, term2):
        """
        Asserts that: term = term1 & term2.
        """
        raise NotImplementedError("Method 'band' is not implemented!")

    def bxor(self, term, term1, term2):
        """
        Asserts that: term = term1 ^ term2.
        """
        raise NotImplementedError("Method 'bxor' is not implemented!")

    def bor(self, term, term1, term2):
        """
        Asserts that: term = term1 | term2.
        """
        raise NotImplementedError("Method 'bor' is not implemented!")
