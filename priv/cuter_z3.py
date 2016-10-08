# -*- coding: utf-8 -*-

import json
from z3 import *
import cuter_global as cglb
import cuter_logger as clg
import cuter_common as cc
import cuter_env as cenv
import cuter_types as ctp
import cuter_representation as crp
import cuter_generic_solver as cgs

# Set Z3Py params.
set_param(max_lines=1, max_width=1000000, max_depth=10000000, max_visited=1000000)
set_param('smt.bv.enable_int2bv', True)

class ErlangZ3(cgs.AbstractErlangSolver):
    def __init__(self):
        self.erl = crp.Erlang()
        self.env = cenv.Env()
        self.axs = []
        self.slv = Solver()
        self.slv.set(timeout=10000)
        if cglb.__LISTS_INTERP__ == cglb.LISTS_FORALL_PATS:
            self.slv.set(mbqi=False)
        else:
            self.slv.set(mbqi=True)
        self.slv.set(auto_config=False)

        self.check = None
        self.model = None

        # For the bitwise operations: band.
        # We've made the concession to be sound for integers up to 128 bits long.
        # The implementation is incomplete for larger integers.
        self.int2bvSize = 128
        self.max_pos = BitVecVal(2**(self.int2bvSize - 1) - 1, self.int2bvSize)
        self.max_uint = 2**self.int2bvSize - 1

    # =========================================================================
    # Public API.
    # =========================================================================

    def reset_solver(self):
        """
        Resets the solver.
        """
        self.slv = Solver()

    def add_axioms(self):
        """
        Adds the axioms from memory to the solver.
        """
        spec_axs = self.generateSpecAxioms()
        self.slv.add(simplify(spec_axs))
        self.slv.add(simplify(And(*self.axs)))

    def solve(self):
        """
        Solves a constraint set and returns the result.
        """
        self.check = self.slv.check()
        if self.check == sat:
            self.model = self.slv.model()
            return cc.SOLVER_STATUS_SAT
        elif self.check == unsat:
            return cc.SOLVER_STATUS_UNSAT
        elif self.check == unknown:
            clg.model_unknown(And(*self.axs))
            return cc.SOLVER_STATUS_UNKNOWN

    def fix_parameter(self, p, v):
        """
        Fixes a symbolic variable to a specific value.
        """
        x = self.decode_term(p)
        t = self.decode_term(v)
        self.slv.add(x == t)

    def encode_model(self):
        """
        Encodes the resulting model to JSON.
        """
        return [self.encode_parameter(p) for p in self.env.params]

    # =========================================================================
    # Private Methods.
    # =========================================================================

    # -------------------------------------------------------------------------
    # Encode / Decode Z3 Terms to JSON and vice versa.
    # -------------------------------------------------------------------------

    def encode_parameter(self, p):
        """
        Encodes a parameter and its value to JSON.
        """
        erl, model = self.erl, self.model
        x = self.env.lookup(p)
        v = model[x]
        encodedValue = {"t": cc.JSON_TYPE_ANY} if v is None else erl.encode(v, model)
        return erl.encodeSymbolic(p), encodedValue

    def decode_term(self, jdata):
        """
        Decodes a JSON term to its Z3 representation.
        """
        td = crp.TermDecoder(self.erl, self.env)
        dct = jdata["d"] if ("d" in jdata) else {}
        return td.decode(jdata, dct)

    # -------------------------------------------------------------------------
    # Parse internal commands.
    # -------------------------------------------------------------------------

    def mfa_params(self, *args):
        """
        Stores the entry point MFA's symbolic parameters.
        """
        e = self.env
        pms = []
        for x in args:
            s = x["s"]
            p = e.freshVar(s, self.erl.Term)
            pms.append(p)
            e.addParam(s)
        return pms

    def mfa_spec(self, *spec):
        """
        Stores the spec of the entry point MFA.
        """
        # FIXME Assume that there is only one clause.
        self.parseSpecClause(spec[0])

    def unfold_tuple(self, *terms):
        """
        Unfolds a symbolic tuple.
        """
        t, ts = self.decode_term(terms[0]), terms[1:]
        self.axs.append(self.erl.Term.is_tpl(t))
        t = self.erl.Term.tval(t)
        for x in ts:
            s = x["s"]
            self.axs.append(self.erl.List.is_cons(t))
            self.env.bind(s, self.erl.List.hd(t))
            t = self.erl.List.tl(t)
        self.axs.append(t == self.erl.List.nil)
        # Elaborate the type
        typ = self.env.lookupType(terms[0]["s"])
        typ.matchNTuple(len(terms[1:]))
        children = typ.getChildren()
        if children != None and len(children) == len(terms[1:]):
            for i in range(1, len(terms)):
                self.env.bindType(terms[i]["s"], children[i-1])

    def unfold_list(self, *terms):
        """
        Unfolds a symbolic list.
        """
        t, ts = self.decode_term(terms[0]), terms[1:]
        self.axs.append(self.erl.Term.is_lst(t))
        t = self.erl.Term.lval(t)
        for x in ts:
            s = x["s"]
            self.axs.append(self.erl.List.is_cons(t))
            self.env.bind(s, self.erl.List.hd(t))
            t = self.erl.List.tl(t)
        self.axs.append(t == self.erl.List.nil)
        # Elaborate the type
        typ = self.env.lookupType(terms[0]["s"])
        children = typ.matchNList(len(terms[1:]))
        if children != None and len(children) == len(terms[1:]):
            for i in range(1, len(terms)):
                self.env.bindType(terms[i]["s"], children[i-1])

    def make_bitstr(self, symb, encodedValue, size):
        """
        Makes a bitstring by encoding an appropriate term.
        """
        # TODO For now, expects size to be a concrete integer and encodedValue to be an integer.
        T, B = self.erl.Term, self.erl.BitStr
        s = symb["s"]
        enc = self.decode_term(encodedValue)
        szTerm = self.decode_term(size)
        sz = int(str(simplify(T.ival(szTerm)))) # Expect size to represent an Integer
        if sz == 0:
            self.env.bind(s, T.bin(0, B.bnil))
        else:
            # Add axioms.
            bits = [self.env.generate_bitvec(1) for _ in range(sz)]
            concBits = Concat(*bits) if len(bits) > 1 else bits[0]
            concHelper = self.env.generate_bitvec(sz)
            self.axs.extend([T.is_int(enc), T.ival(enc) == BV2Int(concHelper), concHelper == concBits])
            # Bind the symbolic result.
            b = B.bnil
            for bit in reversed(bits):
                b = B.bcons(bit, b)
            self.env.bind(s, T.bin(sz, b))

    def concat_segs(self, *terms):
        """
        Concatenates many bitstrings into a large binary.
        """
        T, B = self.erl.Term, self.erl.BitStr
        term1, term2, ts = terms[0], terms[1], terms[2:]
        s = term1["s"]
        t = self.decode_term(term2)
        sz = T.bsz(t)
        self.axs.append(T.is_bin(t))
        t = T.bval(t)
        for x in reversed(ts):
            if "s" in x:
                t = B.bcons(self.decode_term(x), t)
            else:
                nTerm = self.decode_term(x)
                n = int(str(simplify(T.ival(nTerm))))
                t = B.bcons(BitVecVal(n, 1), t)
        self.env.bind(s, T.bin(sz + len(ts), t))

    def fresh_closure(self, tFun, tArity):
        """
        Asserts that tFun is a closure with arity tArity.
        """
        erl = self.erl
        T, arity, = erl.Term, erl.arity
        sFun = self.env.freshVar(tFun["s"], T)
        sArity = self.decode_term(tArity)
        self.axs.extend([
            T.is_fun(sFun),
            T.is_int(sArity),
            arity(T.fval(sFun)) == T.ival(sArity)
        ])

    def evaluated_closure(self, *args):
        """
        Asserts that the evaluation of a closure returns some specific terms.
        """
        erl, env = self.erl, self.env
        T, L, arity, fmap = erl.Term, erl.List, erl.arity, erl.fmap
        tResult, tFun, tArgs = args[0], args[1], args[2:]
        sResult = self.decode_term(tResult)
        sFun = self.decode_term(tFun)
        sArgs = L.nil
        for t in reversed(tArgs):
            sArgs = L.cons(self.decode_term(t), sArgs)
        self.axs.extend([
            T.is_fun(sFun),
            arity(T.fval(sFun)) == len(tArgs),
            fmap(T.fval(sFun))[sArgs] == sResult
        ])
        # Elaborate the types.
        # FIXME What happens if they are concrete values?
        tpFun = env.lookupType(tFun["s"])
        tpFun.lambdaUsed()
        tpArgs, tpResult = tpFun.applyLambda(len(tArgs))
        for i, t in enumerate(tArgs):
            if "s" in t:
                currTp = env.lookupType(t["s"])
                if not currTp.unify(tpArgs[i]):
                    self.axs.append(True == False)
        if "s" in tResult:
            currTp = env.lookupType(tResult["s"])
            if not currTp.unify(tpResult):
                self.axs.append(True == False)

    # -------------------------------------------------------------------------
    # Constraints.
    # -------------------------------------------------------------------------

    def erl_lambda(self, *args):
        """
        Asserts that a lambda application has succeeded.
        """
        erl, env = self.erl, self.env
        T, L, arity, fmap = erl.Term, erl.List, erl.arity, erl.fmap
        tResult, tFun, tArgs = args[0], args[1], args[2:]
        sResult = self.env.freshVar(tResult["s"], T)
        sFun = self.decode_term(tFun)
        sArgs = L.nil
        for t in reversed(tArgs):
            sArgs = L.cons(self.decode_term(t), sArgs)
        self.axs.extend([
            T.is_fun(sFun),
            arity(T.fval(sFun)) == len(tArgs),
            fmap(T.fval(sFun))[sArgs] == sResult
        ])
        # Elaborate the types.
        # FIXME What happens if they are concrete values?
        tpFun = env.lookupType(tFun["s"])
        env.addRoot(tResult["s"])
        tpArgs, tpResult = tpFun.applyLambda(len(tArgs))
        for i, t in enumerate(tArgs):
            if "s" in t:
                currTp = env.lookupType(t["s"])
                if not currTp.unify(tpArgs[i]):
                    self.axs.append(True == False)
        env.bindType(tResult["s"], tpResult)

    def guard_true(self, term):
        """
        Asserts the predicate: term1 == true
        """
        t = self.decode_term(term)
        self.axs.append(t == self.erl.atmTrue)

    def guard_false(self, term):
        """
        Asserts the predicate: term1 == false
        """
        t = self.decode_term(term)
        self.axs.append(t == self.erl.atmFalse)

    def list_nonempty(self, term):
        """
        Asserts that: term is a nonempty list.
        """
        t = self.decode_term(term)
        self.axs.extend([
            self.erl.Term.is_lst(t),
            self.erl.List.is_cons(self.erl.Term.lval(t))
        ])
        # Elaborate the type
        typ = self.env.lookupType(term["s"])
        typ.matchCons()
  
    def list_empty(self, term):
        """
        Asserts that: term is an empty list.
        """
        t = self.decode_term(term)
        self.axs.extend([
            self.erl.Term.is_lst(t),
            self.erl.List.is_nil(self.erl.Term.lval(t))
        ])
        # Elaborate the type
        typ = self.env.lookupType(term["s"])
        typ.matchNil()

    def list_not_lst(self, term):
        """
        Asserts that: term is not list.
        """
        t = self.decode_term(term)
        self.axs.append(self.erl.Term.is_lst(t) == False)
        # Elaborate the type
        typ = self.env.lookupType(term["s"])
        typ.matchNotList()

    def match_equal(self, term1, term2):
        """
        Asserts the predicate: term1 == term2
        """
        t1 = self.decode_term(term1)
        t2 = self.decode_term(term2)
        self.axs.append(t1 == t2)
        # Elaborate the type
        if "s" not in term1 and term1 == {"t": cc.JSON_TYPE_LIST, "v": []}:
            tp = self.env.lookupType(term2["s"])
            tp.matchNil()
        elif "s" not in term2 and term2 == {"t": cc.JSON_TYPE_LIST, "v": []}:
            tp = self.env.lookupType(term1["s"])
            tp.matchNil()

    def match_not_equal(self, term1, term2):
        """
        Asserts the predicate: term1 != term2
        """
        t1 = self.decode_term(term1)
        t2 = self.decode_term(term2)
        self.axs.append(t1 != t2)
        # Elaborate the type
        if "s" not in term1 and term1 == {"t": cc.JSON_TYPE_LIST, "v": []}:
            tp = self.env.lookupType(term2["s"])
            tp.matchNotNil()
        elif "s" not in term2 and term2 == {"t": cc.JSON_TYPE_LIST, "v": []}:
            tp = self.env.lookupType(term1["s"])
            tp.matchNotNil()

    def tuple_sz(self, term, num):
        """
        Asserts that: term is a tuple of size num.
        """
        t = self.decode_term(term)
        nTerm = self.decode_term(num)
        n = int(str(simplify(self.erl.Term.ival(nTerm)))) # Expect num to represent an Integer
        self.axs.append(self.erl.Term.is_tpl(t))
        t = self.erl.Term.tval(t)
        for i in range(0, n):
            self.axs.append(self.erl.List.is_cons(t))
            t = self.erl.List.tl(t)
        self.axs.append(t == self.erl.List.nil)
        # Elaborate the type
        typ = self.env.lookupType(term["s"])
        typ.matchNTuple(n)

    def tuple_not_sz(self, term, num):
        """
        Asserts that: term is not a tuple of size num.
        """
        t = self.decode_term(term)
        nTerm = self.decode_term(num)
        n = int(str(simplify(self.erl.Term.ival(nTerm)))) # Expect num to represent an Integer
        self.axs.append(self.erl.Term.is_tpl(t))
        xs = []
        t = self.erl.Term.tval(t)
        for i in range(0, n):
            xs.append(self.erl.List.is_cons(t))
            t = self.erl.List.tl(t)
        xs.append(t == self.erl.List.nil)
        self.axs.append(Not(And(*xs)))
        # Elaborate the type
        typ = self.env.lookupType(term["s"])
        typ.notMatchNTuple(n)

    def tuple_not_tpl(self, term, num):
        """
        Asserts that: term is not a tuple.
        """
        t = self.decode_term(term)
        self.axs.append(self.erl.Term.is_tpl(t) == False)
        # Elaborate the type
        typ = self.env.lookupType(term["s"])
        typ.notMatchTuple()

    def empty_bitstr(self, term):
        """
        Asserts that: term is an empty bitstring.
        """
        T, B = self.erl.Term, self.erl.BitStr
        t = self.decode_term(term)
        self.axs.extend([
            T.is_bin(t),
            B.is_bnil(T.bval(t)),
            T.bsz(t) == 0
        ])

    def nonempty_bitstr(self, term1, term2, term):
        """
        Asserts that: term is a nonempty bitstring.
        """
        T, B = self.erl.Term, self.erl.BitStr
        s1 = term1["s"]
        s2 = term2["s"]
        t = self.decode_term(term)
        self.axs.extend([
            T.is_bin(t),
            B.is_bcons(T.bval(t)),
            T.bsz(t) > 0
        ])
        self.env.bind(s1, B.bhd(T.bval(t)))
        self.env.bind(s2, T.bin(T.bsz(t) - 1, B.btl(T.bval(t))))

    def bitmatch_const_true(self, termRest, cnstValue, size, termBitstr):
        """
        Asserts that: termBitstr == <<cnstValue/size, termRest>>.
        """
        # TODO For now, expects size to be a concrete integer and encodedValue to be an integer.
        s = termRest["s"]
        t = self.bitmatch_const_false_reversed(cnstValue, size, termBitstr)
        self.env.bind(s, t)

    def bitmatch_const_false(self, cnstValue, size, termBitstr):
        """
        Asserts that: termBitstr =/= <<cnstValue/size, termRest>>.
        """
        # TODO For now, expects size to be a concrete integer and encodedValue to be an integer.
        T, B = self.erl.Term, self.erl.BitStr
        axs = []
        cnst = self.decode_term(cnstValue)
        szTerm = self.decode_term(size)
        sz = int(str(simplify(T.ival(szTerm)))) # Expect size to represent an Integer
        t = self.decode_term(termBitstr)
        axs.extend([T.is_bin(t), T.bsz(t) >= sz])
        t = T.bval(t)
        bits = []
        for _ in range(sz):
            #axs.append(B.is_bcons(t))
            bits.append(B.bhd(t))
            t = B.btl(t)
        concBits = Concat(*bits) if len(bits) > 1 else bits[0]
        concHelper = self.env.generate_bitvec(sz)
        axs.extend([T.is_int(cnst), T.ival(cnst) == BV2Int(concHelper), concHelper == concBits])
        self.axs.append(Not(And(*axs)))

    def bitmatch_var_true(self, term1, term2, size, termBitstr):
        """
        Asserts that: termBitstr == <<term1/size, term2>>.
        """
        # TODO For now, expects size to be a concrete integer and term1 to represent an integer.
        T, B = self.erl.Term, self.erl.BitStr
        s1 = term1["s"]
        s2 = term2["s"]
        szTerm = self.decode_term(size)
        # Expect size to represent an Integer.
        sz = int(str(simplify(T.ival(szTerm))))
        t = self.decode_term(termBitstr)
        origSz = T.bsz(t)
        if sz == 0:
            self.env.bind(s1, T.int(0)) # FIXME
            self.env.bind(s2, t)
        else:
            self.axs.extend([T.is_bin(t), origSz >= sz])
            t = T.bval(t)
            bits = []
            for _ in range(sz):
                self.axs.append(B.is_bcons(t))
                bits.append(B.bhd(t))
                t = B.btl(t)
            concBits = Concat(*bits) if len(bits) > 1 else bits[0]
            concHelper = self.env.generate_bitvec(sz)
            self.axs.extend([concHelper == concBits])
            self.env.bind(s1, T.int(BV2Int(concHelper)))
            self.env.bind(s2, T.bin(origSz - sz, t))

    def bitmatch_var_false(self, size, termBitstr):
        """
        Asserts that: termBitstr =/= <<term1/size, term2>>.
        """
        # TODO For now, expects size to be a concrete integer and term1 to represent an integer.
        T, B = self.erl.Term, self.erl.BitStr
        axs = []
        szTerm = self.decode_term(size)
        # Expect size to represent an Integer.
        sz = int(str(simplify(T.ival(szTerm))))
        t = self.decode_term(termBitstr)
        axs.extend([T.is_bin(t), T.bsz(t) >= sz])
        self.axs.append(Not(And(*axs)))

    # -------------------------------------------------------------------------
    # Reversed constraints.
    # -------------------------------------------------------------------------

    def erl_lambda_reversed(self, *args):
        """
        Asserts that a lambda application has failed.
        """
        erl = self.erl
        T, arity = erl.Term, erl.arity
        tFun, tArgs = args[1], args[2:]
        sFun = self.decode_term(tFun)
        # TODO Add that the fun may be of the wrong arity as well.
        # And(T.is_fun(sFun), arity(T.fval(sFun)) != len(tArgs))
        self.axs.append(T.is_fun(sFun) == False)

    def guard_true_reversed(self, term):
        """
        Asserts the predicate: Not (term1 == true)
        """
        return self.guard_false(term)

    def guard_false_reversed(self, term):
        """
        Asserts the predicate: Not (term1 == false)
        """
        return self.guard_true(term)

    def match_equal_reversed(self, term1, term2):
        """
        Asserts the predicate: Not (term1 == term2)
        """
        self.match_not_equal(term1, term2)

    def match_not_equal_reversed(self, term1, term2):
        """
        Asserts the predicate: Not (term1 != term2)
        """
        self.match_equal(term1, term2)

    def list_nonempty_reversed(self, term):
        """
        Asserts that: Not (term is a nonempty list).
        """
        t = self.decode_term(term)
        xs = [
            self.erl.Term.is_lst(t),
            self.erl.List.is_cons(self.erl.Term.lval(t)),
        ]
        self.axs.append(Not(And(*xs)))
        # Elaborate the type
        typ = self.env.lookupType(term["s"])
        typ.revMatchCons()

    def list_empty_reversed(self, term):
        """
        Asserts that: Not (term is an empty list).
        """
        t = self.decode_term(term)
        xs = [
            self.erl.Term.is_lst(t),
            self.erl.List.is_cons(self.erl.Term.lval(t))
        ]
        self.axs.append(And(*xs))
        # Elaborate the type
        typ = self.env.lookupType(term["s"])
        typ.matchCons()

    def list_not_lst_reversed(self, term):
        """
        Asserts that: Not (term is not list).
        """
        t = self.decode_term(term)
        xs = [
            self.erl.Term.is_lst(t),
            self.erl.List.is_cons(self.erl.Term.lval(t))
        ]
        self.axs.append(And(*xs))
        # Elaborate the type
        typ = self.env.lookupType(term["s"])
        typ.matchCons()

    def tuple_sz_reversed(self, term, num):
        """
        Asserts that: term is not a tuple of size num.
        """
        t = self.decode_term(term)
        nTerm = self.decode_term(num)
        # Expect num to represent an Integer
        n = int(str(simplify(self.erl.Term.ival(nTerm))))
        xs = [self.erl.Term.is_tpl(t)]
        t = self.erl.Term.tval(t)
        for i in range(0, n):
          xs.append(self.erl.List.is_cons(t))
          t = self.erl.List.tl(t)
        xs.append(t == self.erl.List.nil)
        self.axs.append(Not(And(*xs)))

    def tuple_not_sz_reversed(self, term, num):
        """
        Asserts that: Not (term is not a tuple of size num).
        """
        t = self.decode_term(term)
        nTerm = self.decode_term(num)
        # Expect num to represent an Integer
        n = int(str(simplify(self.erl.Term.ival(nTerm))))
        self.axs.append(self.erl.Term.is_tpl(t))
        t = self.erl.Term.tval(t)
        for i in range(0, n):
            self.axs.append(self.erl.List.is_cons(t))
            t = self.erl.List.tl(t)
        self.axs.append(t == self.erl.List.nil)

    def tuple_not_tpl_reversed(self, term, num):
        """
        Asserts that: Not (term is not a tuple).
        """
        t = self.decode_term(term)
        nTerm = self.decode_term(num)
        # Expect num to represent an Integer
        n = int(str(simplify(self.erl.Term.ival(nTerm))))
        self.axs.append(self.erl.Term.is_tpl(t))
        t = self.erl.Term.tval(t)
        for i in range(0, n):
            self.axs.append(self.erl.List.is_cons(t))
            t = self.erl.List.tl(t)
        self.axs.append(t == self.erl.List.nil)

    def not_lambda_with_arity_reversed(self, tFun, tArity):
        """
        Asserts that: Not (tFun is not a function with arity tArity).
        """
        erl = self.erl
        T, arity, = erl.Term, erl.arity
        sFun = self.decode_term(tFun)
        sArity = self.decode_term(tArity)
        self.axs.extend([
            T.is_fun(sFun),
            T.is_int(sArity),
            arity(T.fval(sFun)) == T.ival(sArity)
        ])

    def empty_bitstr_reversed(self, term):
        """
        Asserts that: Not (term is an empty bitstring).
        """
        T, B = self.erl.Term, self.erl.BitStr
        t = self.decode_term(term)
        self.axs.extend([
            T.is_bin(t),
            T.bsz(t) > 0,
            B.is_bcons(T.bval(t))
        ])

    def nonempty_bitstr_reversed(self, term1, term2, term):
        """
        Asserts that: Not (term is a nonempty bitstring).
        """
        T, B = self.erl.Term, self.erl.BitStr
        t = self.decode_term(term)
        self.axs.extend([
            T.is_bin(t),
            T.bsz(t) == 0,
            B.is_bnil(T.bval(t))
        ])

    def bitmatch_const_true_reversed(self, termRest, cnstValue, size, termBitstr):
        """
        Asserts that: Not (termBitstr == <<cnstValue/size, termRest>>).
        """
        # TODO For now, expects size to be a concrete integer and encodedValue to be an integer.
        T, B = self.erl.Term, self.erl.BitStr
        axs = []
        cnst = self.decode_term(cnstValue)
        szTerm = self.decode_term(size)
        sz = int(str(simplify(T.ival(szTerm)))) # Expect size to represent an Integer
        t = self.decode_term(termBitstr)
        axs.extend([T.is_bin(t), T.bsz(t) >= sz])
        t = T.bval(t)
        bits = []
        for _ in range(sz):
            #axs.append(B.is_bcons(t))
            bits.append(B.bhd(t))
            t = B.btl(t)
        concBits = Concat(*bits) if len(bits) > 1 else bits[0]
        concHelper = self.env.generate_bitvec(sz)
        axs.extend([T.is_int(cnst), T.ival(cnst) == BV2Int(concHelper), concHelper == concBits])
        self.axs.append(Not(And(*axs)))
        #axs.extend([T.is_int(cnst), concHelper == concBits])
        #self.axs.append(And(*axs))
        #self.axs.append(T.ival(cnst) != BV2Int(concHelper))

    def bitmatch_const_false_reversed(self, cnstValue, size, termBitstr):
        """
        Asserts that: Not (termBitstr =/= <<cnstValue/size, termRest>>).
        """
        # TODO For now, expects size to be a concrete integer and encodedValue to be an integer.
        T, B = self.erl.Term, self.erl.BitStr
        cnst = self.decode_term(cnstValue)
        szTerm = self.decode_term(size)
        sz = int(str(simplify(T.ival(szTerm)))) # Expect size to represent an Integer
        if sz == 0:
            return T.bin(0, B.bnil)
        else:
            t = self.decode_term(termBitstr)
            origSz = simplify(T.bsz(t))
            self.axs.extend([simplify(T.is_bin(t)), origSz >= sz])
            t = T.bval(t)
            bits = []
            for _ in range(sz):
                self.axs.append(B.is_bcons(t))
                bits.append(simplify(B.bhd(t)))
                t = B.btl(t)
            concBits = Concat(*bits) if len(bits) > 1 else bits[0]
            concHelper = self.env.generate_bitvec(sz)
            self.axs.extend([simplify(T.is_int(cnst)), T.ival(cnst) == BV2Int(concHelper), concHelper == concBits])
            return T.bin(origSz - sz, t)

    def bitmatch_var_true_reversed(self, term1, term2, size, termBitStr):
        """
        Asserts that: Not (termBitstr == <<term1/size, term2>>).
        """
        # TODO For now, expects size to be a concrete integer and term1 to represent an integer.
        self.bitmatch_var_false(size, termBitStr)

    def bitmatch_var_false_reversed(self, size, termBitstr):
        """
        Asserts that: Not (termBitstr =/= <<term1/size, term2>>).
        """
        # TODO For now, expects size to be a concrete integer and term1 to represent an integer.
        T, B = self.erl.Term, self.erl.BitStr
        axs = []
        szTerm = self.decode_term(size)
        # Expect size to represent an Integer.
        sz = int(str(simplify(T.ival(szTerm))))
        t = self.decode_term(termBitstr)
        axs.extend([T.is_bin(t), T.bsz(t) >= sz])
        self.axs.append(And(*axs))

    # -------------------------------------------------------------------------
    # Erlang BIFs or MFAs treated as BIFs.
    # -------------------------------------------------------------------------

    ### Operations on lists.

    def head(self, term1, term2):
        """
        Asserts that: term1 == hd(term2).
        """
        s = term1["s"]
        t2 = self.decode_term(term2)
        self.axs.extend([
            self.erl.Term.is_lst(t2),
            self.erl.List.is_cons(self.erl.Term.lval(t2))
        ])
        self.env.bind(s, self.erl.List.hd(self.erl.Term.lval(t2)))
        # Elaborate the type
        tp = self.env.lookupType(term2["s"])
        tp.matchCons()
        children = tp.getChildren()
        if children != None and len(children) == 2:
            self.env.bindType(term1["s"], children[0])

    def head_reversed(self, term1, term2):
        """
        Asserts that: Not (term1 == hd(term2)).
        """
        t2 = self.decode_term(term2)
        self.axs.append(Or(
            self.erl.Term.is_lst(t2) == False,
            And(self.erl.Term.is_lst(t2), self.erl.List.is_nil(self.erl.Term.lval(t2)))
        ))
        # TODO Maybe elaborate type?

    def tail(self, term1, term2):
        """
        Asserts that: term1 == tl(term2).
        """
        s = term1["s"]
        t2 = self.decode_term(term2)
        self.axs.extend([
            self.erl.Term.is_lst(t2),
            self.erl.List.is_cons(self.erl.Term.lval(t2))
        ])
        self.env.bind(s, self.erl.Term.lst(self.erl.List.tl(self.erl.Term.lval(t2))))
        # Elaborate the type
        tp = self.env.lookupType(term2["s"])
        tp.matchCons()
        children = tp.getChildren()
        if children != None and len(children) == 2:
            self.env.bindType(term1["s"], children[1])

    def tail_reversed(self, term1, term2):
        """
        Asserts that: Not (term1 == tl(term2)).
        """
        t2 = self.decode_term(term2)
        self.axs.append(Or(
            self.erl.Term.is_lst(t2) == False,
            And(self.erl.Term.is_lst(t2), self.erl.List.is_nil(self.erl.Term.lval(t2)))
        ))
        # TODO Maybe elaborate type?

    def cons(self, term, term1, term2):
        """
        Asserts that: term = [term1 | term2].
        """
        T = self.erl.Term
        L = self.erl.List
        s = term["s"]
        t1 = self.decode_term(term1)
        t2 = self.decode_term(term2)
        self.axs.append(T.is_lst(t2))
        self.env.bind(s, T.lst(L.cons(t1, T.lval(t2))))
        # Elaborate the type
        tpH = self.env.lookupType(term1["s"]) if "s" in term1 else ctp.Type.generateAny()
        tpT = self.env.lookupType(term2["s"]) if "s" in term2 else ctp.Type.generateAny()
        tp = ctp.Type.makeCons(tpH, tpT)
        self.env.bindType(term["s"], tp)

    ### Operations on atoms.

    def atom_nil(self, term, term1):
        """
        Asserts that: term = (term1 == '').
        """
        s = term["s"]
        t1 = self.decode_term(term1)
        self.env.bind(s, If(
            t1 == self.erl.Term.atm(self.erl.Atom.anil),
            self.erl.atmTrue,
            self.erl.atmFalse
        ))

    def atom_head(self, term, term1):
        """
        Asserts that: term is the first character of term1.
        """
        T = self.erl.Term
        A = self.erl.Atom
        s = term["s"]
        t1 = self.decode_term(term1)
        self.axs.extend([
            T.is_atm(t1),
            A.is_acons(T.aval(t1))
        ])
        self.env.bind(s,
            T.int(A.ahd(T.aval(t1)))
        )

    def atom_tail(self, term, term1):
        """
        Asserts that: term is term1 without its first character.
        """
        T = self.erl.Term
        A = self.erl.Atom
        s = term["s"]
        t1 = self.decode_term(term1)
        self.axs.extend([
            T.is_atm(t1),
            A.is_acons(T.aval(t1))
        ])
        self.env.bind(s,
            T.atm(A.atl(T.aval(t1)))
        )

    ### Operations on tuples.

    def tcons(self, *terms):
        """
        Asserts that: a term is tuple of many terms.
        """
        T, L = self.erl.Term, self.erl.List
        s, ts = terms[0]["s"], map(self.decode_term, terms[1:])
        t = L.nil
        for x in reversed(ts):
            t = L.cons(x, t)
        self.env.bind(s, T.tpl(t))
        # Elaborate the type
        tps = [self.env.lookupType(x["s"]) if "s" in x else ctp.Type.generateAny() for x in terms[1:]]
        tp = ctp.Type.makeNTuple(len(terms[1:]), tps)
        self.env.bindType(terms[0]["s"], tp)

    ### Query types.

    def is_integer(self, term1, term2):
        """
        Asserts that: term1 == is_integer(term2).
        """
        s = term1["s"]
        t2 = self.decode_term(term2)
        self.env.bind(s, If(
            self.erl.Term.is_int(t2),
            self.erl.atmTrue,
            self.erl.atmFalse
        ))

    def is_atom(self, term1, term2):
        """
        Asserts that: term1 == is_atom(term2).
        """
        s = term1["s"]
        t2 = self.decode_term(term2)
        self.env.bind(s, If(
            self.erl.Term.is_atm(t2),
            self.erl.atmTrue,
            self.erl.atmFalse
        ))

    def is_float(self, term1, term2):
        """
        Asserts that: term1 == is_float(term2).
        """
        s = term1["s"]
        t2 = self.decode_term(term2)
        self.env.bind(s, If(
            self.erl.Term.is_real(t2),
            self.erl.atmTrue,
            self.erl.atmFalse
        ))

    def is_list(self, term1, term2):
        """
        Asserts that: term1 == is_list(term2).
        """
        s = term1["s"]
        t2 = self.decode_term(term2)
        self.env.bind(s, If(
            self.erl.Term.is_lst(t2),
            self.erl.atmTrue,
            self.erl.atmFalse
        ))

    def is_tuple(self, term1, term2):
        """
        Asserts that: term1 == is_tuple(term2).
        """
        s = term1["s"]
        t2 = self.decode_term(term2)
        self.env.bind(s, If(
            self.erl.Term.is_tpl(t2),
            self.erl.atmTrue,
            self.erl.atmFalse
        ))

    def is_boolean(self, term1, term2):
        """
        Asserts that: term1 == is_boolean(term2).
        """
        s = term1["s"]
        t2 = self.decode_term(term2)
        self.env.bind(s, If(
            Or(t2 == self.erl.atmTrue, t2 == self.erl.atmFalse),
            self.erl.atmTrue,
            self.erl.atmFalse
        ))

    def is_number(self, term1, term2):
        """
        Asserts that: term1 == is_number(term2).
        """
        s = term1["s"]
        t2 = self.decode_term(term2)
        self.env.bind(s, If(
            Or(self.erl.Term.is_real(t2), self.erl.Term.is_int(t2)),
            self.erl.atmTrue,
            self.erl.atmFalse
        ))

    def is_bitstring(self, term1, term2):
        """
        Asserts that: term1 == is_bitstring(term2).
        """
        s = term1["s"]
        t2 = self.decode_term(term2)
        self.env.bind(s, If(
            self.erl.Term.is_bin(t2),
            self.erl.atmTrue,
            self.erl.atmFalse
        ))

    def is_fun(self, tResult, tFun):
        """
        Asserts that: tResult == is_function(tFun).
        """
        T, atmTrue, atmFalse = self.erl.Term, self.erl.atmTrue, self.erl.atmFalse
        sResult = tResult["s"]
        sFun = self.decode_term(tFun)
        self.env.bind(sResult, If(
            T.is_fun(sFun),
            atmTrue,
            atmFalse
        ))

    def is_fun_with_arity(self, tResult, tFun, tArity):
        """
        Asserts that: tResult == is_function(tFun, tArity).
        """
        T, atmTrue, atmFalse, arity = self.erl.Term, self.erl.atmTrue, self.erl.atmFalse, self.erl.arity
        sResult = tResult["s"]
        sFun = self.decode_term(tFun)
        sArity = self.decode_term(tArity)
        self.env.bind(sResult, If(
            And([
                T.is_fun(sFun),
                T.is_int(sArity),
                arity(T.fval(sFun)) == T.ival(sArity)
            ]),
            atmTrue,
            atmFalse
        ))

    ### Arithmetic Operations.

    def plus(self, term, term1, term2):
        """
        Asserts that: term = term1 + term2.
        """
        T = self.erl.Term
        s = term["s"]
        t1 = self.decode_term(term1)
        t2 = self.decode_term(term2)
        self.axs.extend([
            Or(T.is_int(t1), T.is_real(t1)),
            Or(T.is_int(t2), T.is_real(t2))
        ])
        self.env.bind(s, If(
            And(T.is_int(t1), T.is_int(t2)),        # t1, t2: Int
            T.int(T.ival(t1) + T.ival(t2)),         # t = int(t1+t2)
            If(
                And(T.is_real(t1), T.is_real(t2)),    # t1, t2: Real
                T.real(T.rval(t1) + T.rval(t2)),      # t = real(t1+t2)
                If(
                    And(T.is_int(t1), T.is_real(t2)),   # t1: Int, t2: Real
                    T.real(T.ival(t1) + T.rval(t2)),    # t = real(t1+t2)
                    T.real(T.rval(t1) + T.ival(t2))     # t1: Real, t2: Int, t = real(t1+t2)
                )
            )
        ))

    def minus(self, term, term1, term2):
        """
        Asserts that: term = term1 - term2.
        """
        T = self.erl.Term
        s = term["s"]
        t1 = self.decode_term(term1)
        t2 = self.decode_term(term2)
        self.axs.extend([
            Or(T.is_int(t1), T.is_real(t1)),
            Or(T.is_int(t2), T.is_real(t2))
        ])
        self.env.bind(s, If(
            And(T.is_int(t1), T.is_int(t2)),        # t1, t2: Int
            T.int(T.ival(t1) - T.ival(t2)),         # t = int(t1-t2)
            If(
                And(T.is_real(t1), T.is_real(t2)),    # t1, t2: Real
                T.real(T.rval(t1) - T.rval(t2)),      # t = real(t1-t2)
                If(
                    And(T.is_int(t1), T.is_real(t2)),   # t1: Int, t2: Real
                    T.real(T.ival(t1) - T.rval(t2)),    # t = real(t1-t2)
                    T.real(T.rval(t1) - T.ival(t2))     # t1: Real, t2: Int, t = real(t1-t2)
                )
            )
        ))

    def times(self, term, term1, term2):
        """
        Asserts that: term = term1 * term2.
        """
        T = self.erl.Term
        s = term["s"]
        t1 = self.decode_term(term1)
        t2 = self.decode_term(term2)
        self.axs.extend([
            Or(T.is_int(t1), T.is_real(t1)),
            Or(T.is_int(t2), T.is_real(t2))
        ])
        self.env.bind(s, If(
            And(T.is_int(t1), T.is_int(t2)),        # t1, t2: Int
            T.int(T.ival(t1) * T.ival(t2)),         # t = int(t1*t2)
            If(
                And(T.is_real(t1), T.is_real(t2)),    # t1, t2: Real
                T.real(T.rval(t1) * T.rval(t2)),      # t = real(t1*t2)
                If(
                    And(T.is_int(t1), T.is_real(t2)),   # t1: Int, t2: Real
                    T.real(T.ival(t1) * T.rval(t2)),    # t = real(t1*t2)
                    T.real(T.rval(t1) * T.ival(t2))     # t1: Real, t2: Int, t = real(t1*t2)
                )
            )
        ))

    def rdiv(self, term, term1, term2):
        """
        Asserts that: term = term1 / term2.
        """
        T = self.erl.Term
        s = term["s"]
        t1 = self.decode_term(term1)
        t2 = self.decode_term(term2)
        self.axs.extend([
            Or(T.is_int(t1), T.is_real(t1)),
            Or(
                And(T.is_int(t2), T.ival(t2) != 0),
                And(T.is_real(t2), T.rval(t2) != 0.0)
            ),
        ])
        self.env.bind(s, If(
            And(T.is_int(t1), T.is_int(t2)),          # t1, t2: Int
            T.real(T.ival(t1) / ToReal(T.ival(t2))),  # t = int(t1/t2)
            If(
                And(T.is_real(t1), T.is_real(t2)),      # t1, t2: Real
                T.real(T.rval(t1) / T.rval(t2)),        # t = real(t1/t2)
                If(
                    And(T.is_int(t1), T.is_real(t2)),     # t1: Int, t2: Real
                    T.real(T.ival(t1) / T.rval(t2)),      # t = real(t1/t2)
                    T.real(T.rval(t1) / T.ival(t2))       # t1: Real, t2: Int, t = real(t1/t2)
                )
            )
        ))

    def idiv_nat(self, term, term1, term2):
        """
        Asserts that: term = term1 // term2.
        """
        T = self.erl.Term
        s = term["s"]
        t1 = self.decode_term(term1)
        t2 = self.decode_term(term2)
        self.axs.extend([
            T.is_int(t1),
            T.is_int(t2),
            T.ival(t1) >= 0,
            T.ival(t2) > 0
        ])
        self.env.bind(s, T.int(T.ival(t1) / T.ival(t2)))

    def rem_nat(self, term, term1, term2):
        """
        Asserts that: term = term1 % term2.
        """
        T = self.erl.Term
        s = term["s"]
        t1 = self.decode_term(term1)
        t2 = self.decode_term(term2)
        self.axs.extend([
            T.is_int(t1),
            T.is_int(t2),
            T.ival(t1) >= 0,
            T.ival(t2) > 0
        ])
        self.env.bind(s, T.int(T.ival(t1) % T.ival(t2)))

    def unary(self, term, term1):
        """
        Asserts that: term = - term1.
        """
        T = self.erl.Term
        s = term["s"]
        t1 = self.decode_term(term1)
        self.axs.append(Or(
            T.is_int(t1), T.is_real(t1)
        ))
        self.env.bind(s, If(
            T.is_int(t1),
            T.int( - T.ival(t1) ),
            T.real( - T.rval(t1) )
        ))

    def pow(self, term, term1, term2):
        """
        Asserts that: term = term1 ** term2.
        """
        ## FIXME Z3 only support nonlinear polynomial equations. Should look at http://www.cl.cam.ac.uk/~lp15/papers/Arith/.
        T = self.erl.Term
        s = term["s"]
        t1 = self.decode_term(term1)
        t2 = self.decode_term(term2)
        self.axs.extend([
            Or(T.is_int(t1), T.is_real(t1)),
            Or(T.is_int(t2), T.is_real(t2))
        ])
        self.env.bind(s, If(
            And(T.is_int(t1), T.is_int(t2)),        # t1, t2: Int
            T.real(T.ival(t1) ** T.ival(t2)),       # t = real(t1^t2)
            If(
                And(T.is_real(t1), T.is_real(t2)),    # t1, t2: Real
                T.real(T.rval(t1) ** T.rval(t2)),     # t = real(t1^t2)
                If(
                    And(T.is_int(t1), T.is_real(t2)),   # t1: Int, t2: Real
                    T.real(T.ival(t1) ** T.rval(t2)),   # t = real(t1^t2)
                    T.real(T.rval(t1) ** T.ival(t2))    # t1: Real, t2: Int, t = real(t1^t2)
                )
            )
        ))

    def trunc(self, term, term1):
        """
        Asserts that: term is term1 truncated.
        """
        T = self.erl.Term
        s = term["s"]
        t1 = self.decode_term(term1)
        self.axs.append(
            Or(T.is_int(t1), T.is_real(t1))
        )
        # ToInt(-1.75) == 2, ToInt(1.75) == 1
        self.env.bind(s, If(
            T.is_real(t1),
            If(
                T.rval(t1) > 0.0,
                T.int(ToInt(T.rval(t1))),
                T.int(- ToInt(- T.rval(t1)))
            ),
            t1
        ))

    ### Comparisons.

    def equal(self, term, term1, term2):
        """
        Asserts that: term = (term1 == term2).
        """
        s = term["s"]
        t1 = self.decode_term(term1)
        t2 = self.decode_term(term2)
        self.env.bind(s, If(
            t1 == t2,
            self.erl.atmTrue,
            self.erl.atmFalse
        ))

    def unequal(self, term, term1, term2):
        """
        Asserts that: term = (term1 =/= term2).
        """
        s = term["s"]
        t1 = self.decode_term(term1)
        t2 = self.decode_term(term2)
        self.env.bind(s, If(
            t1 == t2,
            self.erl.atmFalse,
            self.erl.atmTrue
        ))

    def lt_integers(self, term, term1, term2):
        """
        Asserts that: term = (term1 < term2).
        """
        T = self.erl.Term
        s = term["s"]
        t1 = self.decode_term(term1)
        t2 = self.decode_term(term2)
        self.axs.extend([
            T.is_int(t1), T.is_int(t2)
        ])
        self.env.bind(s, If(
            T.ival(t1) < T.ival(t2),
            self.erl.atmTrue,
            self.erl.atmFalse
        ))

    def lt_floats(self, term, term1, term2):
        """
        Asserts that: term = (term1 < term2).
        """
        T = self.erl.Term
        s = term["s"]
        t1 = self.decode_term(term1)
        t2 = self.decode_term(term2)
        self.axs.extend([
            T.is_real(t1), T.is_real(t2)
        ])
        self.env.bind(s, If(
            T.rval(t1) < T.rval(t2),
            self.erl.atmTrue,
            self.erl.atmFalse
        ))

    ### Type conversions.

    def to_float(self, term, term1):
        """
        Asserts that: term = float(term1).
        """
        T = self.erl.Term
        s = term["s"]
        t1 = self.decode_term(term1)
        self.axs.append(Or(
            T.is_int(t1),
            T.is_real(t1)
        ))
        self.env.bind(s, If(
            T.is_real(t1),
            t1,
            T.real( ToReal(T.ival(t1)) )
        ))

    def list_to_tuple(self, term, term1):
        """
        Asserts that: term = list_to_tuple(term1).
        """
        T = self.erl.Term
        s = term["s"]
        t1 = self.decode_term(term1)
        self.axs.append(T.is_lst(t1))
        self.env.bind(s, T.tpl(T.lval(t1)))
        typ = self.env.lookupType(term1["s"])
        self.env.bindType(term["s"], ctp.Type.listToTuple(typ))

    def tuple_to_list(self, term, term1):
        """
        Asserts that: term = tuple_to_list(term1).
        """
        T = self.erl.Term
        s = term["s"]
        t1 = self.decode_term(term1)
        self.axs.append(T.is_tpl(t1))
        self.env.bind(s, T.lst(T.tval(t1)))
        typ = self.env.lookupType(term1["s"])
        self.env.bindType(term["s"], ctp.Type.tupleToList(typ))

    ### Bogus operations (used for their side-effects in Erlang).

    def bogus(self, term, term1):
        """
        Asserts that: term == term1 (Identity function).
        """
        s = term["s"]
        t1 = self.decode_term(term1)
        self.env.bind(s, t1)

    ### Bitwise Operations.

    def band(self, term, term1, term2):
        """
        Asserts that: term = term1 & term2.
        """
        T = self.erl.Term
        s = term["s"]
        t1 = self.decode_term(term1)
        t2 = self.decode_term(term2)
        tmp = self.env.generate_bitvec(self.int2bvSize)
        b1 = self.env.generate_bitvec(self.int2bvSize)
        b2 = self.env.generate_bitvec(self.int2bvSize)
        self.axs.extend([
            T.is_int(t1),
            T.is_int(t2),
            If(
                T.ival(t1) >= 0,
                And(
                    T.ival(t1) == BV2Int(b1),
                    ULE(b1, self.max_pos)
                ),
                And(
                    T.ival(t1) == BV2Int(b1) - self.max_uint - 1,
                    ULT(self.max_pos, b1)
                )
            ),
            If(
                T.ival(t2) >= 0,
                And(
                    T.ival(t2) == BV2Int(b2),
                    ULE(b2, self.max_pos)
                ),
                And(
                    T.ival(t2) == BV2Int(b2) - self.max_uint - 1,
                    ULT(self.max_pos, b2)
                )
            ),
            b1 & b2 == tmp
        ])
        self.env.bind(s, If(
            ULE(tmp, self.max_pos),
            T.int(BV2Int(tmp)),
            T.int(BV2Int(tmp) - self.max_uint - 1)
        ))

    def bxor(self, term, term1, term2):
        """
        Asserts that: term = term1 ^ term2.
        """
        T = self.erl.Term
        s = term["s"]
        t1 = self.decode_term(term1)
        t2 = self.decode_term(term2)
        tmp = self.env.generate_bitvec(self.int2bvSize)
        b1 = self.env.generate_bitvec(self.int2bvSize)
        b2 = self.env.generate_bitvec(self.int2bvSize)
        self.axs.extend([
            T.is_int(t1),
            T.is_int(t2),
            If(
                T.ival(t1) >= 0,
                And(
                    T.ival(t1) == BV2Int(b1),
                    ULE(b1, self.max_pos)
                ),
                And(
                    T.ival(t1) == BV2Int(b1) - self.max_uint - 1,
                    ULT(self.max_pos, b1)
                )
            ),
            If(
                T.ival(t2) >= 0,
                And(
                    T.ival(t2) == BV2Int(b2),
                    ULE(b2, self.max_pos)
                ),
                And(
                    T.ival(t2) == BV2Int(b2) - self.max_uint - 1,
                    ULT(self.max_pos, b2)
                )
            ),
            b1 ^ b2 == tmp
        ])
        self.env.bind(s, If(
            ULE(tmp, self.max_pos),
            T.int(BV2Int(tmp)),
            T.int(BV2Int(tmp) - self.max_uint - 1)
        ))

    def bor(self, term, term1, term2):
        """
        Asserts that: term = term1 | term2.
        """
        T = self.erl.Term
        s = term["s"]
        t1 = self.decode_term(term1)
        t2 = self.decode_term(term2)
        tmp = self.env.generate_bitvec(self.int2bvSize)
        b1 = self.env.generate_bitvec(self.int2bvSize)
        b2 = self.env.generate_bitvec(self.int2bvSize)
        self.axs.extend([
            T.is_int(t1),
            T.is_int(t2),
            If(
                T.ival(t1) >= 0,
                And(
                    T.ival(t1) == BV2Int(b1),
                    ULE(b1, self.max_pos)
                ),
                And(
                    T.ival(t1) == BV2Int(b1) - self.max_uint - 1,
                    ULT(self.max_pos, b1)
                )
            ),
            If(
                T.ival(t2) >= 0,
                And(
                    T.ival(t2) == BV2Int(b2),
                    ULE(b2, self.max_pos)
                ),
                And(
                    T.ival(t2) == BV2Int(b2) - self.max_uint - 1,
                    ULT(self.max_pos, b2)
                )
            ),
            b1 | b2 == tmp
        ])
        self.env.bind(s, If(
            ULE(tmp, self.max_pos),
            T.int(BV2Int(tmp)),
            T.int(BV2Int(tmp) - self.max_uint - 1)
        ))

    # ----------------------------------------------------------------------
    # Define Type Specs
    # ----------------------------------------------------------------------

    def parseSpecClause(self, cl):
        pms = self.env.params
        for pm, tp in zip(pms, cl["p"]):
            s = self.env.lookup(pm)
            self.env.bindType(pm, ctp.Type(tp))

    # ----------------------------------------------------------------------
    # Generate Type Constraints
    # ----------------------------------------------------------------------

    def generateSpecAxioms(self):
        env = self.env
        axs = []
        allRoots = env.params + env.typeRoots
        for param in allRoots:
            tp = env.lookupType(param)
            sv = env.lookup(param)
            paramAxs = self.typeToAxioms(sv, tp)
            #clg.debug_info(param)
            #clg.debug_info(paramAxs)
            if paramAxs != None:
                axs.append(paramAxs)
        return And(*axs)

    def typeToAxioms(self, s, tp):
        T, L = self.erl.Term, self.erl.List
        actType = tp.getType()
        arg = actType["a"] if "a" in actType else None
        if tp.isAny():
            return self.anyToAxioms(s, arg)
        elif tp.isAtom():
            return self.atomToAxioms(s, arg)
        elif tp.isAtomLit():
            return self.atomLitToAxioms(s, arg)
        elif tp.isFloat():
            return self.floatToAxioms(s, arg)
        elif tp.isInteger():
            return self.integerToAxioms(s, arg)
        elif tp.isIntegerLit():
            return self.integerLitToAxioms(s, arg)
        elif tp.isList():
            return self.nilToAxioms(s, arg)  # Use the base case.
        elif tp.isNil():
            return self.nilToAxioms(s, arg)
        elif tp.isTuple():
            return self.tupleToAxioms(s, arg)
        elif tp.isTupleDet():
            return self.tupleDetToAxioms(s, arg)
        elif tp.isUnion():
            return self.unionToAxioms(s, arg)
        elif tp.isRange():
            return self.rangeToAxioms(s, arg)
        elif tp.isNonemptyList():
            return self.nonEmptyListToAxioms(s, arg)
        elif tp.isBitstring():
            return self.bitstringToAxioms(s, arg)
        elif tp.isGenericFun():
            return self.genericFunToAxioms(s, arg, tp.unconstrainedFun())
        elif tp.isFun():
            return self.funToAxioms(s, arg, tp.unconstrainedFun())
        elif tp.isCons():
            axs = [T.is_lst(s), L.is_cons(T.lval(s))]
            children = tp.getChildren()
            axsH = self.typeToAxioms(L.hd(T.lval(s)), children[0])
            if axsH != None:
                axs.append(axsH)
            axsT = self.typeToAxioms(T.lst(L.tl(T.lval(s))), children[1])
            if axsT != None:
                axs.append(axsT)
            return simplify(And(*axs))
        elif tp.isNTuple():
            children = tp.getChildren()
            axs = [T.is_tpl(s)]
            t = T.tval(s)
            for childTp in children:
                axs.append(L.is_cons(t))
                h, t = L.hd(t), L.tl(t)
                ax = self.typeToAxioms(h, childTp)
                if ax != None:
                    axs.append(ax)
            axs.append(L.is_nil(t))
            return And(*axs)

    def anyToAxioms(self, s, tp):
        return None

    def atomToAxioms(self, s, arg):
        return self.erl.Term.is_atm(s)

    def atomLitToAxioms(self, s, arg):
        atm = self.decode_term(arg)
        return (s == atm)

    def floatToAxioms(self, s, arg):
        return self.erl.Term.is_real(s)

    def integerToAxioms(self, s, arg):
        return self.erl.Term.is_int(s)

    def integerLitToAxioms(self, s, arg):
        i = self.decode_term(arg)
        return (s == i)

    def nilToAxioms(self, s, arg):
        T, L = self.erl.Term, self.erl.List
        return And(
            T.is_lst(s),
            L.is_nil(T.lval(s))
        )

    def tupleToAxioms(self, s, arg):
        return self.erl.Term.is_tpl(s)

    def tupleDetToAxioms(self, s, types):
        T, L = self.erl.Term, self.erl.List
        axms = [T.is_tpl(s)]
        t = T.tval(s)
        for tp in types:
            axms.append(L.is_cons(t))
            h, t = L.hd(t), L.tl(t)
            ax = self.typeDeclToAxioms(h, tp)
            if ax != None:
                axms.append(ax)
        axms.append(L.is_nil(t))
        return And(*axms)

    def unionToAxioms(self, s, types):
        axms = []
        for tp in types:
            ax = self.typeDeclToAxioms(s, tp)
            if ax != None:
                axms.append(ax)
        return Or(*axms)

    def rangeToAxioms(self, s, limits):
        T = self.erl.Term
        axs = [T.is_int(s)]
        if not ("tp" in limits[0] and limits[0]["tp"] == cc.JSON_ERLTYPE_INTEGER):
            l1 = self.decode_term(limits[0])
            axs.append(T.ival(s) >= T.ival(l1))
        if not ("tp" in limits[1] and limits[1]["tp"] == cc.JSON_ERLTYPE_INTEGER):
            l2 = self.decode_term(limits[1])
            axs.append(T.ival(s) <= T.ival(l2))
        return And(*axs)

    def nonEmptyListToAxioms(self, s, typ):
        T, L = self.erl.Term, self.erl.List
        ax = self.typeDeclToAxioms(s, typ)
        return And(ax, L.is_cons(T.lval(s)), L.is_nil(L.tl(T.lval(s))))  # Use base case.

    def bitstringToAxioms(self, s, args):
        T = self.erl.Term
        m = args[0]['v']  # Assume it's an integer
        n = args[1]['v']  # Assume it's an integer
        if m == 0 and n == 1:
            return T.is_bin(s)
        elif m == 0:
            return And(T.is_bin(s), simplify(T.bsz(s)) % n == 0)
        elif n == 0:
            return And(T.is_bin(s), simplify(T.bsz(s)) == m)
        else:
            sz = simplify(T.bsz(s))
            return And(T.is_bin(s), sz >= m, (sz - m) % n == 0)

    def genericFunToAxioms(self, s, args, unconstrained = True):
        axs = [self.erl.Term.is_fun(s)]
        if unconstrained:
            axs.extend(self.unconstraintedFunToAxioms(s, args))
        return And(*axs) if len(axs) > 1 else axs[0]

    def funToAxioms(self, s, args, unconstrained = True):
        T, arity, = self.erl.Term, self.erl.arity
        axs = [
            T.is_fun(s),
            arity(T.fval(s)) == len(args) - 1
        ]
        if unconstrained:
            axs.extend(self.unconstraintedFunToAxioms(s, args))
        return And(*axs)

    def typeDeclToAxioms(self, s, tp):
        opts = {
            cc.JSON_ERLTYPE_ANY: self.anyToAxioms,
            cc.JSON_ERLTYPE_ATOM: self.atomToAxioms,
            cc.JSON_ERLTYPE_ATOMLIT: self.atomLitToAxioms,
            cc.JSON_ERLTYPE_FLOAT: self.floatToAxioms,
            cc.JSON_ERLTYPE_INTEGER: self.integerToAxioms,
            cc.JSON_ERLTYPE_INTEGERLIT: self.integerLitToAxioms,
            cc.JSON_ERLTYPE_LIST: self.nilToAxioms,
            cc.JSON_ERLTYPE_NIL: self.nilToAxioms,
            cc.JSON_ERLTYPE_TUPLE: self.tupleToAxioms,
            cc.JSON_ERLTYPE_TUPLEDET: self.tupleDetToAxioms,
            cc.JSON_ERLTYPE_UNION: self.unionToAxioms,
            cc.JSON_ERLTYPE_RANGE: self.rangeToAxioms,
            cc.JSON_ERLTYPE_NONEMPTY_LIST: self.nonEmptyListToAxioms,
            cc.JSON_ERLTYPE_BITSTRING: self.bitstringToAxioms,
            cc.JSON_ERLTYPE_GENERIC_FUN: self.genericFunToAxioms,
            cc.JSON_ERLTYPE_FUN: self.funToAxioms
        }
        tpcode = tp["tp"]
        arg = tp["a"] if "a" in tp else None
        return opts[tpcode](s, arg)

    def unconstraintedFunToAxioms(self, sFun, args):
        T, L, env, fmap = self.erl.Term, self.erl.List, self.env, self.erl.fmap
        tRet = args[-1]
        sRet = env.justFreshVar(T)
        axs = []
        axRet = self.typeDeclToAxioms(sRet, tRet)
        if axRet != None:
            axs.append(axRet)
        # Funs
        if len(args) > 1:
            tArgs = args[:-1]
            sArgs = L.nil
            for tArg in reversed(tArgs):
                sArg = env.justFreshVar(T)
                axArg = self.typeDeclToAxioms(sArg, tArg)
                if axArg != None:
                    axs.append(axArg)
                sArgs = L.cons(sArg, sArgs)
            axs.append(fmap(T.fval(sFun))[sArgs] == sRet)
        # Generic Funs
        else:
            sArgs = env.justFreshVar(L)
            axs.append(Exists(sArgs, fmap(T.fval(sFun))[sArgs] == sRet))
        return axs

# #############################################################################
# Unit Tests
# #############################################################################

def test_model():
    erlz3 = ErlangZ3()
    T, L, A, B = erlz3.erl.Term, erlz3.erl.List, erlz3.erl.Atom, erlz3.erl.BitStr
    s1, s2, s3, s4 = "0.0.0.39316", "0.0.0.39317", "0.0.0.39318", "0.0.0.39319"
    expected = {
        s1: {"t":cc.JSON_TYPE_INT,"v":42},
        s2: {"t":cc.JSON_TYPE_ATOM,"v":[111,107]},
        s3: {"t":cc.JSON_TYPE_LIST,"v":[{"t":cc.JSON_TYPE_BITSTRING,"v":[]},{"t":cc.JSON_TYPE_INT,"v":2}]},
        s4: {"t":cc.JSON_TYPE_ANY}
    }
    [p1, p2, p3, p4] = erlz3.mfa_params({"s":s1}, {"s":s2}, {"s":s3}, {"s":s4})
    erlz3.axs.extend([
        p1 == T.int(42),
        p2 == T.atm(A.acons(111,A.acons(107,A.anil))),
        p3 == T.lst(L.cons(T.bin(0, B.bnil),L.cons(T.int(2),L.nil)))
    ])
    erlz3.add_axioms()
    assert erlz3.solve() == cc.SOLVER_STATUS_SAT, "Model in unsatisfiable"
    model = erlz3.encode_model()
    assert model != [], "The model is empty"
    for smb, v in model:
        s = smb["s"]
        assert expected[s] == v, "{} is {} instead of {}".format(s, expected[s], v)

def test_commands():
    ss = [{"s":"0.0.0.{0:05d}".format(i)} for i in range(50)]
    anyTerm = {"t":cc.JSON_TYPE_ANY}
    trueTerm = {"t":cc.JSON_TYPE_ATOM,"v":[116,114,117,101]}
    falseTerm = {"t":cc.JSON_TYPE_ATOM,"v":[102,97,108,115,101]}
    cmds = [
        ## Defining ss[0]
        ({"c":cc.OP_IS_INTEGER, "a":[ss[2], ss[0]]}, False),
        ({"c":cc.OP_MATCH_EQUAL_TRUE,"a":[deepcopy(trueTerm), ss[2]]}, False),
        ({"c":cc.OP_PLUS, "a":[ss[3], ss[0], {"t":cc.JSON_TYPE_INT,"v":1}]}, False),
        ({"c":cc.OP_EQUAL, "a":[ss[4], ss[3], {"t":cc.JSON_TYPE_INT,"v":3}]}, False),
        ({"c":cc.OP_GUARD_TRUE, "a":[ss[4]]}, False),
        ({"c":cc.OP_GUARD_TRUE, "a":[deepcopy(falseTerm)]}, True),
        ## Defining ss[1]
        ({"c":cc.OP_TUPLE_NOT_TPL, "a":[ss[1], {"t":cc.JSON_TYPE_INT,"v":1}]}, False),
        ({"c":cc.OP_TUPLE_SZ, "a":[ss[1], {"t":cc.JSON_TYPE_INT,"v":1}]}, True),
        ({"c":cc.OP_IS_FLOAT, "a":[ss[5], ss[1]]}, False),
        ({"c":cc.OP_GUARD_FALSE, "a":[ss[5]]}, True),
        ({"c":cc.OP_RDIV, "a":[ss[6], ss[1], {"t":cc.JSON_TYPE_INT,"v":2}]}, False),
        ({"c":cc.OP_UNARY, "a":[ss[7], ss[6]]}, False),
        ({"c":cc.OP_MATCH_EQUAL_TRUE, "a":[ss[7], {"t":cc.JSON_TYPE_FLOAT,"v":-10.25}]}, False),
        ## Defining ss[8]
        ({"c":cc.OP_IS_ATOM, "a":[ss[9], ss[8]]}, False),
        ({"c":cc.OP_ATOM_NIL, "a":[ss[10], ss[8]]}, False),
        ({"c":cc.OP_GUARD_FALSE, "a":[ss[10]]}, False),
        ({"c":cc.OP_ATOM_HEAD, "a":[ss[11], ss[8]]}, False),
        ({"c":cc.OP_ATOM_TAIL, "a":[ss[12], ss[8]]}, False),
        ({"c":cc.OP_MINUS, "a":[ss[13], ss[11], {"t":cc.JSON_TYPE_INT,"v":10}]}, False),
        ({"c":cc.OP_UNEQUAL, "a":[ss[14], ss[13], {"t":cc.JSON_TYPE_INT,"v":101}]}, False),
        ({"c":cc.OP_MATCH_EQUAL_TRUE, "a":[ss[14], deepcopy(trueTerm)]}, True),
        ({"c":cc.OP_MATCH_EQUAL_FALSE, "a":[ss[12], {"t":cc.JSON_TYPE_ATOM,"v":[107]}]}, True),
        ## Defining ss[15]
        ({"c":cc.OP_IS_LIST, "a":[ss[16], ss[15]]}, False),
        ({"c":cc.OP_GUARD_TRUE, "a":[ss[16]]}, False),
        ({"c":cc.OP_LIST_NON_EMPTY, "a":[ss[15]]}, False),
        ({"c":cc.OP_LIST_EMPTY, "a":[ss[15]]}, True),
        ({"c":cc.OP_LIST_NOT_LST, "a":[ss[15]]}, True),
        ({"c":cc.OP_HD, "a":[ss[17], ss[15]]}, False),
        ({"c":cc.OP_TL, "a":[ss[18], ss[15]]}, False),
        ({"c":cc.OP_LIST_EMPTY, "a":[ss[18]]}, False),
        ({"c":cc.OP_LIST_NON_EMPTY, "a":[ss[18]]}, True),
        ({"c":cc.OP_IS_BOOLEAN, "a":[ss[19], ss[17]]}, False),
        ({"c":cc.OP_MATCH_EQUAL_FALSE, "a":[ss[19], deepcopy(falseTerm)]}, False),
        ({"c":cc.OP_GUARD_TRUE, "a":[ss[17]]}, False),
        ({"c":cc.OP_LIST_NOT_LST, "a":[ss[17]]}, False),
        ({"c":cc.OP_TUPLE_SZ, "a":[ss[17], {"t":cc.JSON_TYPE_INT,"v":2}]}, True),
        ## Defining ss[20]
        ({"c":cc.OP_IS_TUPLE, "a":[ss[21], ss[20]]}, False),
        ({"c":cc.OP_GUARD_TRUE, "a":[ss[21]]}, False),
        ({"c":cc.OP_TUPLE_SZ, "a":[ss[20], {"t":cc.JSON_TYPE_INT,"v":2}]}, False),
        ({"c":cc.OP_TUPLE_NOT_SZ, "a":[ss[20], {"t":cc.JSON_TYPE_INT,"v":2}]}, True),
        ({"c":cc.OP_UNFOLD_TUPLE, "a":[ss[20], ss[22], ss[23]]}, False),
        ({"c":cc.OP_MATCH_EQUAL_TRUE, "a":[ss[22], {"t":cc.JSON_TYPE_INT,"v":42}]}, False),
        ({"c":cc.OP_TIMES, "a":[ss[24], ss[23], {"t":cc.JSON_TYPE_INT,"v":2}]}, False),
        ({"c":cc.OP_MATCH_EQUAL_TRUE, "a":[ss[24], {"t":cc.JSON_TYPE_INT,"v":10}]}, False),
        ({"c":cc.OP_IS_NUMBER, "a":[ss[25], ss[24]]}, False),
        ({"c":cc.OP_GUARD_TRUE, "a":[ss[25]]}, False),
        ({"c":cc.OP_TUPLE_NOT_TPL, "a":[ss[20], {"t":cc.JSON_TYPE_INT,"v":2}]}, True),
        ({"c":cc.OP_FLOAT, "a":[ss[26], ss[22]]}, False),
        ({"c":cc.OP_MATCH_EQUAL_TRUE, "a":[ss[26], {"t":cc.JSON_TYPE_FLOAT,"v":42.0}]}, False),
        ({"c":cc.OP_BOGUS, "a":[ss[26], ss[26]]}, False),
        ## Defining ss[27]
        ({"c":cc.OP_IDIV_NAT, "a":[ss[28], {"t":cc.JSON_TYPE_INT,"v":5}, {"t":cc.JSON_TYPE_INT,"v":2}]}, False),
        ({"c":cc.OP_REM_NAT, "a":[ss[29], {"t":cc.JSON_TYPE_INT,"v":5}, {"t":cc.JSON_TYPE_INT,"v":2}]}, False),
        ({"c":cc.OP_PLUS, "a":[ss[30], ss[28], ss[29]]}, False),
        ({"c":cc.OP_MATCH_EQUAL_TRUE, "a":[ss[30], {"t":cc.JSON_TYPE_INT,"v":3}]}, False),
        ({"c":cc.OP_MATCH_EQUAL_TRUE, "a":[ss[27], ss[30]]}, False),
        ({"c":cc.OP_LT_INT, "a":[ss[31], {"t":cc.JSON_TYPE_INT,"v":2}, ss[27]]}, False),
        ({"c":cc.OP_GUARD_TRUE, "a":[ss[31]]}, False),
        ({"c":cc.OP_LT_FLOAT, "a":[ss[32], {"t":cc.JSON_TYPE_FLOAT,"v":3.14}, {"t":cc.JSON_TYPE_FLOAT,"v":1.42}]}, False),
        ({"c":cc.OP_GUARD_FALSE, "a":[ss[32]]}, False),
        ## Defining ss[33]
        ({"c":cc.OP_UNFOLD_LIST, "a":[ss[33], ss[34], ss[35]]}, False),
        ({"c":cc.OP_TCONS, "a":[ss[36], ss[34], ss[35]]}, False),
        ({"c":cc.OP_LIST_TO_TUPLE, "a":[ss[36], ss[33]]}, False),
        ({"c":cc.OP_CONS, "a":[ss[37], ss[34], ss[35]]}, False),
        ({"c":cc.OP_MATCH_EQUAL_TRUE, "a":[ss[35], {"t":cc.JSON_TYPE_LIST,"v":[]}]}, False),
        ({"c":cc.OP_MATCH_EQUAL_TRUE, "a":[ss[34], {"t":cc.JSON_TYPE_INT,"v":2}]}, False),
        ## Defining ss[38]
        ({"c":cc.OP_MAKE_BITSTR, "a":[ss[39], ss[38], {"t":cc.JSON_TYPE_INT,"v":3}]}, False),
        ({"c":cc.OP_MATCH_EQUAL_TRUE, "a":[ss[39], {"t":cc.JSON_TYPE_BITSTRING,"v":[1,0,1]}]}, False),
    ]
    expected = {
        ss[0]["s"]: {"t":cc.JSON_TYPE_INT,"v":2},
        ss[1]["s"]: {"t":cc.JSON_TYPE_FLOAT,"v":20.5},
        ss[8]["s"]: {"t":cc.JSON_TYPE_ATOM,"v":[111,107]},
        ss[15]["s"]: {"t":cc.JSON_TYPE_LIST,"v":[deepcopy(trueTerm)]},
        ss[20]["s"]: {"t":cc.JSON_TYPE_TUPLE,"v":[{"t":cc.JSON_TYPE_INT,"v":42}, {"t":cc.JSON_TYPE_INT,"v":5}]},
        ss[27]["s"]: {"t":cc.JSON_TYPE_INT,"v":3},
        ss[33]["s"]: {"t":cc.JSON_TYPE_LIST,"v":[{"t":cc.JSON_TYPE_INT,"v":2}, {"t":cc.JSON_TYPE_LIST,"v":[]}]},
        ss[38]["s"]: {"t":cc.JSON_TYPE_INT,"v":5},
    }
    erlz3 = ErlangZ3()
    erlz3.mfa_params(ss[0], ss[1], ss[8], ss[15], ss[20], ss[27], ss[33], ss[38])
    for cmd, rvs in cmds:
        erlz3.command_toSolver(cmd["c"], cmd, rvs)
    erlz3.add_axioms()
    assert erlz3.solve() == cc.SOLVER_STATUS_SAT, "Model in unsatisfiable"
    model = erlz3.encode_model()
    assert model != [], "The model is empty"
    for smb, v in model:
        assert v == expected[smb["s"]], "{} is {} instead of {}".format(smb["s"], v, expected[smb["s"]])

if __name__ == '__main__':
    import json
    from copy import deepcopy
    cglb.init()
    test_model()
    test_commands()
