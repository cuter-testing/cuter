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
            return cc.mk_sat()
        elif self.check == unsat:
            return cc.mk_unsat()
        elif self.check == unknown:
            clg.model_unknown(And(*self.axs))
            return cc.mk_unknown()

    def fix_parameter(self, p, v):
        """
        Fixes a symbolic variable to a specific value.
        """
        x = self.decode_term(p)
        t = self.decode_term(v)
        self.slv.add(x == t)

    def encode_model(self):
        """
        Encodes the resulting model to a SolverResponse message.
        """
        m = cc.mk_model([self.encode_parameter(p) for p in self.env.params])
        return cc.mk_model_data(m)

    # =========================================================================
    # Private Methods.
    # =========================================================================

    # -------------------------------------------------------------------------
    # Encode / Decode Z3 Terms to Messages and vice versa.
    # -------------------------------------------------------------------------

    def encode_parameter(self, p):
        """
        Encodes a parameter and its value to a SolverResponse.ModelEntry message.
        """
        erl, model = self.erl, self.model
        x = self.env.lookup(p)
        v = model[x]
        encodedValue = cc.mk_any() if v is None else erl.encode(v, model)
        return cc.mk_model_entry(erl.encodeSymbolic(p), encodedValue)

    def decode_term(self, jdata):
        """
        Decodes an ErlangTerm message to its Z3 representation.
        """
        td = crp.TermDecoder(self.erl, self.env)
        return td.decodeTerm(jdata)

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
            s = x.value
            p = e.freshVar(s, self.erl.Term)
            pms.append(p)
            e.addParam(s)
        return pms

    def mfa_spec(self, spec):
        """
        Stores the spec of the entry point MFA.
        """
        # FIXME Assume that there is only one clause.
        clauses = cc.get_spec_clauses(spec)
        self.parseSpecClause(clauses[0])

    def unfold_tuple(self, *terms):
        """
        Unfolds a symbolic tuple.
        """
        t, ts = self.decode_term(terms[0]), terms[1:]
        self.axs.append(self.erl.Term.is_tpl(t))
        t = self.erl.Term.tval(t)
        for x in ts:
            s = cc.get_symb(x)
            self.axs.append(self.erl.List.is_cons(t))
            self.env.bind(s, self.erl.List.hd(t))
            t = self.erl.List.tl(t)
        self.axs.append(t == self.erl.List.nil)
        # Elaborate the type
        typ = self.env.lookupType(cc.get_symb(terms[0]))
        typ.matchNTuple(len(terms[1:]))
        children = typ.getChildren()
        if children != None and len(children) == len(terms[1:]):
            for i in range(1, len(terms)):
                self.env.bindType(cc.get_symb(terms[i]), children[i-1])

    def unfold_list(self, *terms):
        """
        Unfolds a symbolic list.
        """
        t, ts = self.decode_term(terms[0]), terms[1:]
        self.axs.append(self.erl.Term.is_lst(t))
        t = self.erl.Term.lval(t)
        for x in ts:
            s = cc.get_symb(x)
            self.axs.append(self.erl.List.is_cons(t))
            self.env.bind(s, self.erl.List.hd(t))
            t = self.erl.List.tl(t)
        self.axs.append(t == self.erl.List.nil)
        # Elaborate the type
        typ = self.env.lookupType(cc.get_symb(terms[0]))
        children = typ.matchNList(len(terms[1:]))
        if children != None and len(children) == len(terms[1:]):
            for i in range(1, len(terms)):
                self.env.bindType(cc.get_symb(terms[i]), children[i-1])

    def make_bitstr(self, symb, encodedValue, size):
        """
        Makes a bitstring by encoding an appropriate term.
        """
        # TODO For now, expects size to be a concrete integer and encodedValue to be an integer.
        T, B = self.erl.Term, self.erl.BitStr
        s = cc.get_symb(symb)
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
        s = cc.get_symb(term1)
        t = self.decode_term(term2)
        sz = T.bsz(t)
        self.axs.append(T.is_bin(t))
        t = T.bval(t)
        for x in reversed(ts):
            if cc.is_symb(x):
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
        sFun = self.env.freshVar(cc.get_symb(tFun), T)
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
        tpFun = env.lookupType(cc.get_symb(tFun))
        tpFun.lambdaUsed()
        tpArgs, tpResult = tpFun.applyLambda(len(tArgs))
        for i, t in enumerate(tArgs):
            if cc.is_symb(t):
                currTp = env.lookupType(cc.get_symb(t))
                if not currTp.unify(tpArgs[i]):
                    self.axs.append(True == False)
        if cc.is_symb(tResult):
            currTp = env.lookupType(cc.get_symb(tResult))
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
        sResult = self.env.freshVar(cc.get_symb(tResult), T)
        sFun = self.decode_term(tFun)
        sArgs = L.nil
        ss = []
        for t in reversed(tArgs):
            s = self.decode_term(t)
            ss.insert(0, s)
            sArgs = L.cons(s, sArgs)
        self.axs.extend([
            T.is_fun(sFun),
            arity(T.fval(sFun)) == len(tArgs),
            fmap(T.fval(sFun))[sArgs] == sResult
        ])
        # Elaborate the types.
        # FIXME What happens if they are concrete values?
        tpFun = env.lookupType(cc.get_symb(tFun))
        env.addRoot(cc.get_symb(tResult))
        tpArgs, tpResult = tpFun.applyLambda(len(tArgs))
        for i, t in enumerate(tArgs):
            if cc.is_symb(t):
                currTp = env.lookupType(cc.get_symb(t))
                if not currTp.unify(tpArgs[i]):
                    self.axs.append(True == False)
                if cc.is_type_bitstring(tpArgs[i].typ):
                    segSizes = cc.get_segment_size_from_bitstring(tpArgs[i].typ)
                    self.axs.append(self.bitstringToAxioms(ss[i], segSizes))
        env.bindType(cc.get_symb(tResult), tpResult)

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
        typ = self.env.lookupType(cc.get_symb(term))
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
        typ = self.env.lookupType(cc.get_symb(term))
        typ.matchNil()

    def list_not_lst(self, term):
        """
        Asserts that: term is not list.
        """
        t = self.decode_term(term)
        self.axs.append(self.erl.Term.is_lst(t) == False)
        # Elaborate the type
        typ = self.env.lookupType(cc.get_symb(term))
        typ.matchNotList()

    def match_equal(self, term1, term2):
        """
        Asserts the predicate: term1 == term2
        """
        t1 = self.decode_term(term1)
        t2 = self.decode_term(term2)
        self.axs.append(t1 == t2)
        # Elaborate the type
        if not cc.is_symb(term1) and term1 == cc.mk_list([]):
            tp = self.env.lookupType(cc.get_symb(term2))
            tp.matchNil()
        elif not cc.is_symb(term2) and term2 == cc.mk_list([]):
            tp = self.env.lookupType(cc.get_symb(term1))
            tp.matchNil()

    def match_not_equal(self, term1, term2):
        """
        Asserts the predicate: term1 != term2
        """
        t1 = self.decode_term(term1)
        t2 = self.decode_term(term2)
        self.axs.append(t1 != t2)
        # Elaborate the type
        if not cc.is_symb(term1) and term1 == cc.mk_list([]):
            tp = self.env.lookupType(cc.get_symb(term2))
            tp.matchNotNil()
        elif not cc.is_symb(term2) and term2 == cc.mk_list([]):
            tp = self.env.lookupType(cc.get_symb(term1))
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
        typ = self.env.lookupType(cc.get_symb(term))
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
        typ = self.env.lookupType(cc.get_symb(term))
        typ.notMatchNTuple(n)

    def tuple_not_tpl(self, term, num):
        """
        Asserts that: term is not a tuple.
        """
        t = self.decode_term(term)
        self.axs.append(self.erl.Term.is_tpl(t) == False)
        # Elaborate the type
        typ = self.env.lookupType(cc.get_symb(term))
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
        s1 = cc.get_symb(term1)
        s2 = cc.get_symb(term2)
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
        s = cc.get_symb(termRest)
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
        s1 = cc.get_symb(term1)
        s2 = cc.get_symb(term2)
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
        typ = self.env.lookupType(cc.get_symb(term))
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
        typ = self.env.lookupType(cc.get_symb(term))
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
        typ = self.env.lookupType(cc.get_symb(term))
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
        s = cc.get_symb(term1)
        t2 = self.decode_term(term2)
        self.axs.extend([
            self.erl.Term.is_lst(t2),
            self.erl.List.is_cons(self.erl.Term.lval(t2))
        ])
        self.env.bind(s, self.erl.List.hd(self.erl.Term.lval(t2)))
        # Elaborate the type
        tp = self.env.lookupType(cc.get_symb(term2))
        tp.matchCons()
        children = tp.getChildren()
        if children != None and len(children) == 2:
            self.env.bindType(cc.get_symb(term1), children[0])

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
        s = cc.get_symb(term1)
        t2 = self.decode_term(term2)
        self.axs.extend([
            self.erl.Term.is_lst(t2),
            self.erl.List.is_cons(self.erl.Term.lval(t2))
        ])
        self.env.bind(s, self.erl.Term.lst(self.erl.List.tl(self.erl.Term.lval(t2))))
        # Elaborate the type
        tp = self.env.lookupType(cc.get_symb(term2))
        tp.matchCons()
        children = tp.getChildren()
        if children != None and len(children) == 2:
            self.env.bindType(cc.get_symb(term1), children[1])

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
        s = cc.get_symb(term)
        t1 = self.decode_term(term1)
        t2 = self.decode_term(term2)
        self.axs.append(T.is_lst(t2))
        self.env.bind(s, T.lst(L.cons(t1, T.lval(t2))))
        # Elaborate the type
        tpH = self.env.lookupType(cc.get_symb(term1)) if cc.is_symb(term1) else ctp.Type.generateAny()
        tpT = self.env.lookupType(cc.get_symb(term2)) if cc.is_symb(term2) else ctp.Type.generateAny()
        tp = ctp.Type.makeCons(tpH, tpT)
        self.env.bindType(cc.get_symb(term), tp)

    ### Operations on atoms.

    def atom_nil(self, term, term1):
        """
        Asserts that: term = (term1 == '').
        """
        s = cc.get_symb(term)
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
        s = cc.get_symb(term)
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
        s = cc.get_symb(term)
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
        s, ts = cc.get_symb(terms[0]), map(self.decode_term, terms[1:])
        t = L.nil
        for x in reversed(ts):
            t = L.cons(x, t)
        self.env.bind(s, T.tpl(t))
        # Elaborate the type
        tps = [self.env.lookupType(cc.get_symb(x)) if cc.is_symb(x) else ctp.Type.generateAny() for x in terms[1:]]
        tp = ctp.Type.makeNTuple(len(terms[1:]), tps)
        self.env.bindType(cc.get_symb(terms[0]), tp)

    ### Query types.

    def is_integer(self, term1, term2):
        """
        Asserts that: term1 == is_integer(term2).
        """
        s = cc.get_symb(term1)
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
        s = cc.get_symb(term1)
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
        s = cc.get_symb(term1)
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
        s = cc.get_symb(term1)
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
        s = cc.get_symb(term1)
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
        s = cc.get_symb(term1)
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
        s = cc.get_symb(term1)
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
        s = cc.get_symb(term1)
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
        sResult = cc.get_symb(tResult)
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
        sResult = cc.get_symb(tResult)
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
        s = cc.get_symb(term)
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
        s = cc.get_symb(term)
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
        s = cc.get_symb(term)
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
        s = cc.get_symb(term)
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
        s = cc.get_symb(term)
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
        s = cc.get_symb(term)
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
        s = cc.get_symb(term)
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
        s = cc.get_symb(term)
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
        s = cc.get_symb(term)
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
        s = cc.get_symb(term)
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
        s = cc.get_symb(term)
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
        s = cc.get_symb(term)
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
        s = cc.get_symb(term)
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
        s = cc.get_symb(term)
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
        s = cc.get_symb(term)
        t1 = self.decode_term(term1)
        self.axs.append(T.is_lst(t1))
        self.env.bind(s, T.tpl(T.lval(t1)))
        typ = self.env.lookupType(cc.get_symb(term1))
        self.env.bindType(cc.get_symb(term), ctp.Type.listToTuple(typ))

    def tuple_to_list(self, term, term1):
        """
        Asserts that: term = tuple_to_list(term1).
        """
        T = self.erl.Term
        s = cc.get_symb(term)
        t1 = self.decode_term(term1)
        self.axs.append(T.is_tpl(t1))
        self.env.bind(s, T.lst(T.tval(t1)))
        typ = self.env.lookupType(cc.get_symb(term1))
        self.env.bindType(cc.get_symb(term), ctp.Type.tupleToList(typ))

    ### Bogus operations (used for their side-effects in Erlang).

    def bogus(self, term, term1):
        """
        Asserts that: term == term1 (Identity function).
        """
        s = cc.get_symb(term)
        t1 = self.decode_term(term1)
        self.env.bind(s, t1)

    ### Bitwise Operations.

    def band(self, term, term1, term2):
        """
        Asserts that: term = term1 & term2.
        """
        T = self.erl.Term
        s = cc.get_symb(term)
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
        s = cc.get_symb(term)
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
        s = cc.get_symb(term)
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

    def parseSpecClause(self, clause):
        pms = self.env.params
        paramTypes = cc.get_param_types_of_clause(clause)
        for pm, tp in zip(pms, paramTypes):
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
        if tp.isAny():
            return self.anyToAxioms(s)
        elif tp.isAtom():
            return self.atomToAxioms(s)
        elif tp.isAtomLit():
            return self.atomLitToAxioms(s, cc.get_literal_from_atomlit(actType))
        elif tp.isFloat():
            return self.floatToAxioms(s)
        elif tp.isInteger():
            return self.integerToAxioms(s)
        elif tp.isIntegerLit():
            return self.integerLitToAxioms(s, actType.literal)
        elif tp.isList():
            return self.nilToAxioms(s)  # Use the base case.
        elif tp.isNil():
            return self.nilToAxioms(s)
        elif tp.isTuple():
            return self.tupleToAxioms(s)
        elif tp.isTupleDet():
            return self.tupleDetToAxioms(s, cc.get_inner_types_from_tupledet(actType))
        elif tp.isUnion():
            return self.unionToAxioms(s, cc.get_inner_types_from_union(actType))
        elif tp.isRange():
            return self.rangeToAxioms(s, cc.get_range_bounds_from_range(actType))
        elif tp.isNonemptyList():
            return self.nonEmptyListToAxioms(s, cc.get_inner_type_from_nonempty_list(actType))
        elif tp.isBitstring():
            return self.bitstringToAxioms(s, cc.get_segment_size_from_bitstring(actType))
        elif tp.isGenericFun():
            return self.genericFunToAxioms(s, cc.get_funsig_from_fun(actType), tp.unconstrainedFun())
        elif tp.isFun():
            return self.funToAxioms(s, cc.get_funsig_from_fun(actType), tp.unconstrainedFun())
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

    def anyToAxioms(self, s):
        return None

    def atomToAxioms(self, s):
        return self.erl.Term.is_atm(s)

    def atomLitToAxioms(self, s, arg):
        atm = self.decode_term(arg)
        return (s == atm)

    def floatToAxioms(self, s):
        return self.erl.Term.is_real(s)

    def integerToAxioms(self, s):
        return self.erl.Term.is_int(s)

    def integerLitToAxioms(self, s, arg):
        i = self.decode_term(arg)
        return (s == i)

    def nilToAxioms(self, s):
        T, L = self.erl.Term, self.erl.List
        return And(
            T.is_lst(s),
            L.is_nil(T.lval(s))
        )

    def tupleToAxioms(self, s):
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
        if cc.has_lower_bound(limits):
            axs.append(T.ival(s) >= cc.get_lower_bound(limits))
        if cc.has_upper_bound(limits):
            axs.append(T.ival(s) <= cc.get_upper_bound(limits))
        return And(*axs)

    def nonEmptyListToAxioms(self, s, typ):
        T, L = self.erl.Term, self.erl.List
        ax = self.typeDeclToAxioms(s, typ)
        return And(ax, L.is_cons(T.lval(s)), L.is_nil(L.tl(T.lval(s))))  # Use base case.

    def bitstringToAxioms(self, s, segsize):
        T = self.erl.Term
        m = int(segsize.m)
        n = int(segsize.n)
        if m == 0 and n == 1:
            return T.is_bin(s)
        elif m == 0:
            return And(T.is_bin(s), simplify(T.bsz(s)) % n == 0)
        elif n == 0:
            return And(T.is_bin(s), simplify(T.bsz(s)) == m)
        else:
            sz = simplify(T.bsz(s))
            return And(T.is_bin(s), sz >= m, (sz - m) % n == 0)

    def genericFunToAxioms(self, s, funsig, unconstrained = True):
        axs = [self.erl.Term.is_fun(s)]
        if unconstrained:
            axs.extend(self.unconstraintedFunToAxioms(s, funsig))
        return And(*axs) if len(axs) > 1 else axs[0]

    def funToAxioms(self, s, funsig, unconstrained = True):
        T, arity, = self.erl.Term, self.erl.arity
        axs = [
            T.is_fun(s),
            arity(T.fval(s)) == cc.get_complete_funsig_arity(funsig)
        ]
        if unconstrained:
            axs.extend(self.unconstraintedFunToAxioms(s, funsig))
        return And(*axs)

    def typeDeclToAxioms(self, s, tp):
        if cc.is_type_any(tp):
            return self.anyToAxioms(s)
        elif cc.is_type_atom(tp):
            return self.atomToAxioms(s)
        elif cc.is_type_atomlit(tp):
            return self.atomLitToAxioms(s, cc.get_literal_from_atomlit(tp))
        elif cc.is_type_float(tp):
            return self.floatToAxioms(s)
        elif cc.is_type_integer(tp):
            return self.integerToAxioms(s)
        elif cc.is_type_integerlit(tp):
            return self.integerLitToAxioms(s, cc.get_literal_from_integerlit(tp))
        elif cc.is_type_list(tp):
            return self.nilToAxioms(s)  # Use the base case.
        elif cc.is_type_nil(tp):
            return self.nilToAxioms(s)
        elif cc.is_type_tuple(tp):
            return self.tupleToAxioms(s)
        elif cc.is_type_tupledet(tp):
            return self.tupleDetToAxioms(s, cc.get_inner_types_from_tupledet(tp))
        elif cc.is_type_union(tp):
            return self.unionToAxioms(s, cc.get_inner_types_from_union(tp))
        elif cc.is_type_range(tp):
            return self.rangeToAxioms(s, cc.get_range_bounds_from_range(tp))
        elif cc.is_type_nonempty_list(tp):
            return self.nonEmptyListToAxioms(s, cc.get_inner_type_from_nonempty_list(tp))
        elif cc.is_type_bitstring(tp):
            return self.bitstringToAxioms(s, cc.get_segment_size_from_bitstring(tp))
        elif cc.is_type_generic_fun(tp):
            return self.genericFunToAxioms(s, cc.get_funsig_from_fun(tp))
        elif cc.is_type_complete_fun(tp):
            return self.funToAxioms(s, cc.get_funsig_from_fun(tp))

    def unconstraintedFunToAxioms(self, sFun, funsig):
        T, L, env, fmap = self.erl.Term, self.erl.List, self.env, self.erl.fmap
        tRet = cc.get_rettype_from_funsig(funsig)
        sRet = env.justFreshVar(T)
        axs = []
        axRet = self.typeDeclToAxioms(sRet, tRet)
        if axRet != None:
            axs.append(axRet)
        # Funs
        if cc.is_complete_funsig(funsig):
            tArgs = cc.get_parameters_from_complete_funsig(funsig)
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
        s1: cc.mk_int(42),
        s2: cc.mk_atom([111,107]),
        s3: cc.mk_list([cc.mk_bitstring([]), cc.mk_int(2)]),
        s4: cc.mk_any()
    }
    [p1, p2, p3, p4] = erlz3.mfa_params(cc.mk_symb(s1), cc.mk_symb(s2), cc.mk_symb(s3), cc.mk_symb(s4))
    erlz3.axs.extend([
        p1 == T.int(42),
        p2 == T.atm(A.acons(111,A.acons(107,A.anil))),
        p3 == T.lst(L.cons(T.bin(0, B.bnil),L.cons(T.int(2),L.nil)))
    ])
    erlz3.add_axioms()
    assert cc.is_sat(erlz3.solve()), "Model in unsatisfiable"
    model = erlz3.encode_model()
    for entry in cc.get_model_entries(model):
        symb = entry.var
        assert cc.is_symb(symb)
        s = cc.get_symb(symb)
        v = entry.value
        assert expected[s] == v, "{} is {} instead of {}".format(s, expected[s], v)

def test_commands():
    from cuter_proto_log_entry_pb2 import LogEntry
    ss = [cc.mk_symb("0.0.0.{0:05d}".format(i)) for i in range(50)]
    anyTerm = cc.mk_any()
    trueTerm = cc.mk_atom([116,114,117,101])
    falseTerm = cc.mk_atom([102,97,108,115,101])
    cmds = [
        ## Defining ss[0]
        (cc.mk_log_entry(LogEntry.OP_IS_INTEGER, [ss[2], ss[0]]), False),
        (cc.mk_log_entry(LogEntry.OP_MATCH_EQUAL_TRUE, [deepcopy(trueTerm), ss[2]]), False),
        (cc.mk_log_entry(LogEntry.OP_PLUS, [ss[3], ss[0], cc.mk_int(1)]), False),
        (cc.mk_log_entry(LogEntry.OP_EQUAL, [ss[4], ss[3], cc.mk_int(3)]), False),
        (cc.mk_log_entry(LogEntry.OP_GUARD_TRUE, [ss[4]]), False),
        (cc.mk_log_entry(LogEntry.OP_GUARD_TRUE, [deepcopy(falseTerm)]), True),
        ## Defining ss[1]
        (cc.mk_log_entry(LogEntry.OP_TUPLE_NOT_TPL, [ss[1], cc.mk_int(1)]), False),
        (cc.mk_log_entry(LogEntry.OP_TUPLE_SZ, [ss[1], cc.mk_int(1)]), True),
        (cc.mk_log_entry(LogEntry.OP_IS_FLOAT, [ss[5], ss[1]]), False),
        (cc.mk_log_entry(LogEntry.OP_GUARD_FALSE, [ss[5]]), True),
        (cc.mk_log_entry(LogEntry.OP_RDIV, [ss[6], ss[1], cc.mk_int(2)]), False),
        (cc.mk_log_entry(LogEntry.OP_UNARY, [ss[7], ss[6]]), False),
        (cc.mk_log_entry(LogEntry.OP_MATCH_EQUAL_TRUE, [ss[7], cc.mk_float(-10.25)]), False),
        ## Defining ss[8]
        (cc.mk_log_entry(LogEntry.OP_IS_ATOM, [ss[9], ss[8]]), False),
        (cc.mk_log_entry(LogEntry.OP_ATOM_NIL, [ss[10], ss[8]]), False),
        (cc.mk_log_entry(LogEntry.OP_GUARD_FALSE, [ss[10]]), False),
        (cc.mk_log_entry(LogEntry.OP_ATOM_HEAD, [ss[11], ss[8]]), False),
        (cc.mk_log_entry(LogEntry.OP_ATOM_TAIL, [ss[12], ss[8]]), False),
        (cc.mk_log_entry(LogEntry.OP_MINUS, [ss[13], ss[11], cc.mk_int(10)]), False),
        (cc.mk_log_entry(LogEntry.OP_UNEQUAL, [ss[14], ss[13], cc.mk_int(101)]), False),
        (cc.mk_log_entry(LogEntry.OP_MATCH_EQUAL_TRUE, [ss[14], deepcopy(trueTerm)]), True),
        (cc.mk_log_entry(LogEntry.OP_MATCH_EQUAL_FALSE, [ss[12], cc.mk_atom([107])]), True),
        ## Defining ss[15]
        (cc.mk_log_entry(LogEntry.OP_IS_LIST, [ss[16], ss[15]]), False),
        (cc.mk_log_entry(LogEntry.OP_GUARD_TRUE, [ss[16]]), False),
        (cc.mk_log_entry(LogEntry.OP_LIST_NON_EMPTY, [ss[15]]), False),
        (cc.mk_log_entry(LogEntry.OP_LIST_EMPTY, [ss[15]]), True),
        (cc.mk_log_entry(LogEntry.OP_LIST_NOT_LST, [ss[15]]), True),
        (cc.mk_log_entry(LogEntry.OP_HD, [ss[17], ss[15]]), False),
        (cc.mk_log_entry(LogEntry.OP_TL, [ss[18], ss[15]]), False),
        (cc.mk_log_entry(LogEntry.OP_LIST_EMPTY, [ss[18]]), False),
        (cc.mk_log_entry(LogEntry.OP_LIST_NON_EMPTY, [ss[18]]), True),
        (cc.mk_log_entry(LogEntry.OP_IS_BOOLEAN, [ss[19], ss[17]]), False),
        (cc.mk_log_entry(LogEntry.OP_MATCH_EQUAL_FALSE, [ss[19], deepcopy(falseTerm)]), False),
        (cc.mk_log_entry(LogEntry.OP_GUARD_TRUE, [ss[17]]), False),
        (cc.mk_log_entry(LogEntry.OP_LIST_NOT_LST, [ss[17]]), False),
        (cc.mk_log_entry(LogEntry.OP_TUPLE_SZ, [ss[17], cc.mk_int(2)]), True),
        ## Defining ss[20]
        (cc.mk_log_entry(LogEntry.OP_IS_TUPLE, [ss[21], ss[20]]), False),
        (cc.mk_log_entry(LogEntry.OP_GUARD_TRUE, [ss[21]]), False),
        (cc.mk_log_entry(LogEntry.OP_TUPLE_SZ, [ss[20], cc.mk_int(2)]), False),
        (cc.mk_log_entry(LogEntry.OP_TUPLE_NOT_SZ, [ss[20], cc.mk_int(2)]), True),
        (cc.mk_log_entry(LogEntry.OP_UNFOLD_TUPLE, [ss[20], ss[22], ss[23]]), False),
        (cc.mk_log_entry(LogEntry.OP_MATCH_EQUAL_TRUE, [ss[22], cc.mk_int(42)]), False),
        (cc.mk_log_entry(LogEntry.OP_TIMES, [ss[24], ss[23], cc.mk_int(2)]), False),
        (cc.mk_log_entry(LogEntry.OP_MATCH_EQUAL_TRUE, [ss[24], cc.mk_int(10)]), False),
        (cc.mk_log_entry(LogEntry.OP_IS_NUMBER, [ss[25], ss[24]]), False),
        (cc.mk_log_entry(LogEntry.OP_GUARD_TRUE, [ss[25]]), False),
        (cc.mk_log_entry(LogEntry.OP_TUPLE_NOT_TPL, [ss[20], cc.mk_int(2)]), True),
        (cc.mk_log_entry(LogEntry.OP_FLOAT, [ss[26], ss[22]]), False),
        (cc.mk_log_entry(LogEntry.OP_MATCH_EQUAL_TRUE, [ss[26], cc.mk_float(42.0)]), False),
        (cc.mk_log_entry(LogEntry.OP_BOGUS, [ss[26], ss[26]]), False),
        ## Defining ss[27]
        (cc.mk_log_entry(LogEntry.OP_IDIV_NAT, [ss[28], cc.mk_int(5), cc.mk_int(2)]), False),
        (cc.mk_log_entry(LogEntry.OP_REM_NAT, [ss[29], cc.mk_int(5), cc.mk_int(2)]), False),
        (cc.mk_log_entry(LogEntry.OP_PLUS, [ss[30], ss[28], ss[29]]), False),
        (cc.mk_log_entry(LogEntry.OP_MATCH_EQUAL_TRUE, [ss[30], cc.mk_int(3)]), False),
        (cc.mk_log_entry(LogEntry.OP_MATCH_EQUAL_TRUE, [ss[27], ss[30]]), False),
        (cc.mk_log_entry(LogEntry.OP_LT_INT, [ss[31], cc.mk_int(2), ss[27]]), False),
        (cc.mk_log_entry(LogEntry.OP_GUARD_TRUE, [ss[31]]), False),
        (cc.mk_log_entry(LogEntry.OP_LT_FLOAT, [ss[32], cc.mk_float(3.14), cc.mk_float(1.42)]), False),
        (cc.mk_log_entry(LogEntry.OP_GUARD_FALSE, [ss[32]]), False),
        ## Defining ss[33]
        (cc.mk_log_entry(LogEntry.OP_UNFOLD_LIST, [ss[33], ss[34], ss[35]]), False),
        (cc.mk_log_entry(LogEntry.OP_TCONS, [ss[36], ss[34], ss[35]]), False),
        (cc.mk_log_entry(LogEntry.OP_LIST_TO_TUPLE, [ss[36], ss[33]]), False),
        (cc.mk_log_entry(LogEntry.OP_CONS, [ss[37], ss[34], ss[35]]), False),
        (cc.mk_log_entry(LogEntry.OP_MATCH_EQUAL_TRUE, [ss[35], cc.mk_list([])]), False),
        (cc.mk_log_entry(LogEntry.OP_MATCH_EQUAL_TRUE, [ss[34], cc.mk_int(2)]), False),
        ## Defining ss[38]
        (cc.mk_log_entry(LogEntry.OP_MAKE_BITSTR, [ss[39], ss[38], cc.mk_int(3)]), False),
        (cc.mk_log_entry(LogEntry.OP_MATCH_EQUAL_TRUE, [ss[39], cc.mk_bitstring([True,False,True])]), False)
    ]
    expected = {
        cc.get_symb(ss[0]): cc.mk_int(2),
        cc.get_symb(ss[1]): cc.mk_float(20.5),
        cc.get_symb(ss[8]): cc.mk_atom([111,107]),
        cc.get_symb(ss[15]): cc.mk_list([deepcopy(trueTerm)]),
        cc.get_symb(ss[20]): cc.mk_tuple([cc.mk_int(42), cc.mk_int(5)]),
        cc.get_symb(ss[27]): cc.mk_int(3),
        cc.get_symb(ss[33]): cc.mk_list([cc.mk_int(2), cc.mk_list([])]),
        cc.get_symb(ss[38]): cc.mk_int(5)
    }
    erlz3 = ErlangZ3()
    erlz3.mfa_params(ss[0], ss[1], ss[8], ss[15], ss[20], ss[27], ss[33], ss[38])
    for cmd, rvs in cmds:
        erlz3.command_toSolver(cmd, rvs)
    erlz3.add_axioms()
    assert cc.is_sat(erlz3.solve()), "Model in unsatisfiable"
    model = erlz3.encode_model()
    for entry in cc.get_model_entries(model):
        smb = entry.var
        assert cc.is_symb(smb), "Expected symbolic var but found value"
        s = cc.get_symb(smb)
        v = entry.value
        assert v == expected[s], "{} is {} instead of {}".format(s, v, expected[s])

if __name__ == '__main__':
    import json
    from copy import deepcopy
    cglb.init()
    test_model()
    test_commands()
