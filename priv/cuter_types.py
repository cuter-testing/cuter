#!/usr/bin/env python
# -*- coding: utf-8 -*-

from copy import deepcopy
import cuter_global as cglb
import cuter_logger as clg
import cuter_common as cc

class ErlType:
    """
    Representation of Types in Spec.Type form (as retrieved in the logs).
    """

    @classmethod
    def generateAnyType(cls):
        return cc.mk_type_any()

    @classmethod
    def generateTupleType(cls):
        return cc.mk_type_tuple()

    @classmethod
    def generateListAnyType(cls):
        return cc.mk_type_list(cls.generateAnyType())

    @classmethod
    def generateNTupleType(cls, sz):
        return cc.mk_type_ntuple(sz)

    @classmethod
    def generateConsType(cls):
        return cc.mk_type_cons()

    @classmethod
    def isAny(cls, typ):
        return cc.is_type_any(typ)

    @classmethod
    def isAtom(cls, typ):
        return cc.is_type_atom(typ)

    @classmethod
    def isAtomLit(cls, typ):
        return cc.is_type_atomlit(typ)

    @classmethod
    def isFloat(cls, typ):
        return cc.is_type_float(typ)

    @classmethod
    def isInteger(cls, typ):
        return cc.is_type_integer(typ)

    @classmethod
    def isIntegerLit(cls, typ):
        return cc.is_type_integerlit(typ)

    @classmethod
    def isList(cls, typ):
        return cc.is_type_list(typ)

    @classmethod
    def isNil(cls, typ):
        return cc.is_type_nil(typ)

    @classmethod
    def isTuple(cls, typ):
        return cc.is_type_tuple(typ)

    @classmethod
    def isTupleDet(cls, typ):
        return cc.is_type_tupledet(typ)

    @classmethod
    def isUnion(cls, typ):
        return cc.is_type_union(typ)

    @classmethod
    def isRange(cls, typ):
        return cc.is_type_range(typ)

    @classmethod
    def isNonemptyList(cls, typ):
        return cc.is_type_nonempty_list(typ)

    @classmethod
    def isBitstring(cls, typ):
      return cc.is_type_bitstring(typ)

    @classmethod
    def isGenericFun(cls, typ):
        return cc.is_type_generic_fun(typ)

    @classmethod
    def isFun(cls, typ):
        return cc.is_type_complete_fun(typ)

    @classmethod
    def isCons(cls, typ):
        return cc.is_type_cons(typ)

    @classmethod
    def isNTuple(cls, typ):
        return cc.is_type_ntuple(typ)

    @classmethod
    def setNonEmptyListType(cls, typ):
        cc.set_type_nonempty_list(typ)

    @classmethod
    def setConsType(cls, typ):
        cc.set_type_cons(typ)

    @classmethod
    def setNilType(cls, typ):
        cc.set_type_nil(typ)

    @classmethod
    def setNTupleType(cls, typ, n):
        cc.set_type_ntuple(typ, n)

    @classmethod
    def getListTypeFromNonemptyList(cls, typ):
        tp = deepcopy(typ)
        cc.set_type_list(tp)
        return tp

class Type:
    """
    Representation of Erlang datatypes used for generating type constraints incrementally.
    """
    def __init__(self, typ):
        self.typ = deepcopy(typ)
        self.isFinal = self.isFinalType(typ)
        self.hasFunBeenUsed = False  # Only applies to function types.
        self.children = None
        if ErlType.isNonemptyList(typ):
            inner_type = cc.get_inner_type_from_nonempty_list(self.typ)
            h = Type(deepcopy(inner_type))
            t = Type(ErlType.getListTypeFromNonemptyList(self.typ))
            ErlType.setConsType(self.typ)
            self.isFinal = True
            self.children = [h, t]

    def takenOverByType(self, tp):
        self.typ = tp.typ
        self.isFinal = tp.typ
        self.hasFunBeenUsed = tp.hasFunBeenUsed
        self.children = tp.children

    def getChildren(self):
        return self.children

    def getType(self):
        return self.typ

    def unconstrainedFun(self):
        return not self.hasFunBeenUsed

    @classmethod
    def generateAny(cls):
        return Type(ErlType.generateAnyType())

    @classmethod
    def listToTuple(cls, tp):
        # FIXME Retain the type of the list.
        return Type(ErlType.generateTupleType())

    @classmethod
    def tupleToList(cls, tp):
        if tp.isFinal:
            return Type(ErlType.generateListAnyType())
        else:
            # preprocess unions
            if ErlType.isUnion(tp.typ):
                inner_types = cc.get_inner_types_from_union(tp.typ)
                isCnd = lambda x: ErlType.isTuple(x) or ErlType.isTupleDet(x)
                candidates = [t for t in inner_types if isCnd(t)]
                if len(candidates) > 0:
                    tp.typ = candidates[0]
                    if ErlType.isTupleDet(tp.typ):
                        ts = cc.get_inner_types_from_tupledet(tp.typ)
                        sz = len(ts)
                        tp.matchNTuple(sz)
                else:
                    # TODO Log inconsistency
                    pass
            # actual type elaborations
            if ErlType.isTuple(tp.typ):
                return Type(ErlType.generateListAnyType())
            elif ErlType.isTupleDet(tp.typ):
                children = tp.getChildren()
                t = Type(ErlType.generateListAnyType())
                for child in reversed(children):
                    t = cls.makeCons(child, t)
                return t
            else:
                # TODO Log inconsistency (if is not any())
                return Type(ErlType.generateListAnyType())

    def applyLambda(self, arity):
        if ErlType.isGenericFun(self.typ):
            self.hasFunBeenUsed = True
            retType = cc.get_rettype_from_fun(self.typ)
            tpArgs = [Type(ErlType.generateAnyType()) for _ in range(arity)]
            return tpArgs, Type(deepcopy(retType))
        elif ErlType.isFun(self.typ):
            self.hasFunBeenUsed = True
            argsTypes = cc.get_parameters_from_complete_fun(self.typ)
            retType = cc.get_rettype_from_fun(self.typ)
            tpArgs = [Type(deepcopy(tp)) for tp in argsTypes]
            return tpArgs, Type(deepcopy(retType))
        else:
            # TODO Log inconsistency (if is not any())
            tpArgs = [Type(ErlType.generateAnyType()) for _ in range(arity)]
            return tpArgs, Type(ErlType.generateAnyType())

    def lambdaUsed(self):
        self.hasFunBeenUsed = True

    def matchCons(self):
        if not self.isFinal:
            # preprocess unions
            if ErlType.isUnion(self.typ):
                isL = lambda x: ErlType.isList(x) or ErlType.isNonemptyList(x)
                inner_types = cc.get_inner_types_from_union(self.typ)
                candidates = [tp for tp in inner_types if isL(tp)]
                if len(candidates) > 0:
                    self.typ = candidates[0]
                else:
                    # TODO Log inconsistency
                    pass
            # actual type elaborations
            if ErlType.isList(self.typ):
                it = cc.get_inner_type_from_list(self.typ)
                h = Type(deepcopy(it))
                t = Type(deepcopy(self.typ))
                ErlType.setConsType(self.typ)
                self.isFinal = True
                self.children = [h, t]
            elif ErlType.isNonemptyList(self.typ):
                it = cc.get_inner_type_from_nonempty_list(self.typ)
                h = Type(deepcopy(it))
                t = Type(ErlType.getListTypeFromNonemptyList(self.typ))
                ErlType.setConsType(self.typ)
                self.isFinal = True
                self.children = [h, t]
            elif ErlType.isAny(self.typ):
                pass
            else:
                # TODO Log inconsistency
                pass

    def revMatchCons(self):
        if not self.isFinal:
            # preprocess unions
            if ErlType.isUnion(self.typ):
                inner_types = cc.get_inner_types_from_union(self.typ)
                candidates = [tp for tp in inner_types if not ErlType.isNonemptyList(tp)]
                if len(candidates) > 0:
                    cc.set_inner_types_to_union(self.typ, candidates)
                    new_inner_types = cc.get_inner_types_from_union(self.typ)
                    for tp in new_inner_types:
                        if ErlType.isList(tp):
                            ErlType.setNilType(tp)
            # actual type elaborations
            if ErlType.isList(self.typ):
                ErlType.setNilType(self.typ)
                self.isFinal = True

    def matchNil(self):
        if not self.isFinal:
            # preprocess unions
            if ErlType.isUnion(self.typ):
                inner_types = cc.get_inner_types_from_union(self.typ)
                isCnd = lambda x: ErlType.isList(x) or ErlType.isNil(x)
                candidates = [tp for tp in inner_types if isCnd(tp)]
                if len(candidates) > 0:
                    self.typ = candidates[0]
                else:
                    # TODO Log inconsistency
                    pass
            # actual type elaborations
            if ErlType.isList(self.typ):
                ErlType.setNilType(self.typ)
                self.isFinal = True
            elif ErlType.isNil(self.typ):
                self.isFinal = True
            elif ErlType.isAny(self.typ):
                pass
            else:
                # TODO Log inconsistency
                pass

    def matchNotNil(self):
        if not self.isFinal:
            # preprocess unions
            if ErlType.isUnion(self.typ):
                inner_types = cc.get_inner_types_from_union(self.typ)
                candidates = [tp for tp in inner_types if not ErlType.isNil(tp)]
                for cnd in candidates:
                    if ErlType.isList(cnd):
                        ErlType.setNonEmptyListType(cnd)
                if len(candidates) > 0:
                    cc.set_inner_types_to_union(self.typ, candidates)
                else:
                    # TODO Log inconsistency
                    pass
            elif ErlType.isList(self.typ):
                self.matchCons()

    def matchNotList(self):
        if not self.isFinal:
            # preprocess unions
            if ErlType.isUnion(self.typ):
                inner_types = cc.get_inner_types_from_union(self.typ)
                isCnd = lambda x: not ErlType.isList(x) and not ErlType.isNil(x) and not ErlType.isNonemptyList(x)
                candidates = [tp for tp in inner_types if isCnd(tp)]
                if len(candidates) > 0:
                    cc.set_inner_types_to_union(self.typ, candidates)
                else:
                    # TODO Log inconsistency
                    pass
            if ErlType.isList(self.typ) or ErlType.isNil(self.typ) or ErlType.isNonemptyList(self.typ):
                # TODO Log inconsistency
                pass

    def matchNTuple(self, sz):
        if not self.isFinal:
            # preprocess unions
            if ErlType.isUnion(self.typ):
                inner_types = cc.get_inner_types_from_union(self.typ)
                isCnd = lambda x: ErlType.isTuple(x) or (ErlType.isTupleDet(x) and len(cc.get_inner_types_from_tupledet(x)) == sz)
                candidates = [tp for tp in inner_types if isCnd(tp)]
                if len(candidates) > 0:
                    self.typ = candidates[0]
                else:
                    # TODO Log inconsistency
                    pass
            # actual type elaborations
            if ErlType.isTuple(self.typ):
                pass
            elif ErlType.isTupleDet(self.typ) and len(cc.get_inner_types_from_tupledet(self.typ)) == sz:
                self.isFinal = True
                self.children = [Type(deepcopy(tp)) for tp in cc.get_inner_types_from_tupledet(self.typ)]
                ErlType.setNTupleType(self.typ, sz)
            elif ErlType.isAny(self.typ):
                pass
            else:
                # TODO Log inconsistency
                pass

    def notMatchNTuple(self, sz):
        if not self.isFinal:
            # preprocess unions
            if ErlType.isUnion(self.typ):
                inner_types = cc.get_inner_types_from_union(self.typ)
                isCnd = lambda x: not (ErlType.isTupleDet(x) and len(cc.get_inner_types_from_tupledet(x)) == sz)
                candidates = [tp for tp in inner_types if isCnd(tp)]
                if len(candidates) > 0:
                    cc.set_inner_types_to_union(self.typ, candidates)
                else:
                    # TODO Log inconsistency
                    pass
            # actual type elaborations
            if ErlType.isTupleDet(self.typ) and len(cc.get_inner_types_from_tupledet(self.typ)) == sz:
                # TODO Log inconsistency
                pass

    def notMatchTuple(self):
        if not self.isFinal:
            # preprocess unions
            if ErlType.isUnion(self.typ):
                inner_types = cc.get_inner_types_from_union(self.typ)
                isCnd = lambda x: not ErlType.isTupleDet(x) and not ErlType.isTuple(x)
                candidates = [tp for tp in inner_types if isCnd(tp)]
                if len(candidates) > 0:
                    cc.set_inner_types_to_union(self.typ, candidates)
                else:
                    # TODO Log inconsistency
                    pass
            # actual type elaborations
            if ErlType.isTupleDet(self.typ) or ErlType.isTuple(self.typ):
                # TODO Log inconsistency
                pass

    def matchNList(self, sz):
        if sz == 0:
            self.matchNil()
            return []
        else:
          if not self.isFinal:
              self.matchCons()
              if self.children == None:
                  return None
          if ErlType.isCons(self.typ):
              h, t = self.children[0], self.children[1]
              rec = t.matchNlist(sz - 1)
              return None if rec == None else [h] + rec
          else:
              return None

    @classmethod
    def makeNTuple(cls, sz, children):
        tp = Type(ErlType.generateNTupleType(sz))
        tp.children = children
        return tp

    @classmethod
    def makeCons(cls, h, t):
        tp = Type(ErlType.generateConsType())
        tp.children = [h, t]
        return tp

    def isFinalType(self, typ):
        if ErlType.isAny(typ):
            return True
        elif ErlType.isAtom(typ):
            return True
        elif ErlType.isAtomLit(typ):
            return True
        elif ErlType.isFloat(typ):
            return True
        elif ErlType.isInteger(typ):
            return True
        elif ErlType.isIntegerLit(typ):
            return True
        elif ErlType.isList(typ):
            return False
        elif ErlType.isNil(typ):
            return True
        elif ErlType.isTuple(typ):
            return True
        elif ErlType.isTupleDet(typ):
            return all(self.isFinalType(t) for t in cc.get_inner_types_from_tupledet(typ))
        elif ErlType.isUnion(typ):
            return all(self.isFinalType(t) for t in cc.get_inner_types_from_union(typ))
        elif ErlType.isRange(typ):
            return True
        elif ErlType.isNonemptyList(typ):
            return False
        elif ErlType.isBitstring(typ):
            return True
        elif ErlType.isCons(typ):
            return True
        elif ErlType.isNTuple(typ):
            return True
        elif ErlType.isGenericFun(typ):
            return True
        elif ErlType.isFun(typ):
            return True

    def unify(self, tp):
        # Any
        if self.isAny():
            self.takenOverByType(tp)
            return True
        if tp.isAny():
            return True
        # Atom
        if self.isAtom():
            return self.unifyWithAtom(tp)
        if self.isAtomLit():
            return self.unifyWithAtomLit(tp)
        # Integer
        if self.isInteger():
            return self.unifyWithInteger(tp)
        if self.isIntegerLit():
            return self.unifyWithIntegerLit(tp)
        # Float
        if self.isFloat():
            return self.unifyWithFloat(tp)
        # Bitstring
        if self.isBitstring():
            return self.unifyWithBitstring(tp)
        # Tuple
        if self.isTuple():
            return self.unifyWithTuple(tp)
        if self.isTupleDet():
            return self.unifyWithTupleDet(tp)
        if self.isNTuple():
            return self.unifywithNTuple(tp)
        # FIXME Add cases for unions, lists & funs.
        return True

    def unifyWithAtom(self, tp):
        if tp.isAtom():
            return True
        if tp.isAtomLit():
            self.takenOverByType(tp)
            return True
        if tp.isUnion():
            isCnd = lambda x: ErlType.isAtom(x)
            inner_types = cc.get_inner_types_from_union(tp.typ)
            candidates = [t for t in inner_types if isCnd(t)]
            if len(candidates) > 0:
                return True
            isCnd = lambda x: ErlType.isAtomLit(x)
            candidates = [t for t in inner_types if isCnd(t)]
            if len(candidates) > 0:
                cc.set_inner_types_to_union(tp, candidates)
                self.takenOverByType(tp)
                return True
        return False

    def unifyWithAtomLit(self, tp):
        if tp.isAtom():
            return True
        if tp.isAtomLit():
            return cc.get_literal_from_atomlit(self.typ) == cc.get_literal_from_atomlit(tp.typ)
        if tp.isUnion():
            isCnd = lambda x: ErlType.isAtom(x)
            inner_types = cc.get_inner_types_from_union(tp.typ)
            candidates = [t for t in inner_types if isCnd(t)]
            if len(candidates) > 0:
                return True
            isCnd = lambda x: ErlType.isAtomLit(x) and cc.get_literal_from_atomlit(self.typ) == cc.get_literal_from_atomlit(x)
            candidates = [t for t in inner_types if isCnd(t)]
            if len(candidates) > 0:
                return True
        return False

    def unifyWithInteger(self, tp):
        if tp.isInteger():
            return True
        if tp.isIntegerLit():
            self.takenOverByType(tp)
            return True
        if tp.isRange():
            self.takenOverByType(tp)
        if tp.isUnion():
            isCnd = lambda x: ErlType.isInteger(x)
            inner_types = cc.get_inner_types_from_union(tp.typ)
            candidates = [t for t in inner_types if isCnd(t)]
            if len(candidates) > 0:
                return True
            isCnd = lambda x: ErlType.isIntegerLit(x) or ErlType.isRange(x)
            candidates = [t for t in inner_types if isCnd(t)]
            if len(candidates) > 0:
                cc.set_inner_types_to_union(tp, candidates)
                self.takenOverByType(tp)
                return True
        return False

    def unifyWithIntegerLit(self, tp):
        if tp.isInteger():
            return True
        if tp.isIntegerLit():
            return cc.get_literal_from_integerlit(self.typ) == cc.get_literal_from_integerlit(tp.typ)
        if tp.isUnion():
            isCnd = lambda x: ErlType.isInteger(x)
            inner_types = cc.get_inner_types_from_union(tp.typ)
            candidates = [t for t in inner_types if isCnd(t)]
            if len(candidates) > 0:
                return True
            isCnd = lambda x: ErlType.isIntegerLit(x) and cc.get_literal_from_integerlit(self.typ) == cc.get_literal_from_integerlit(x)
            candidates = [t for t in inner_types if isCnd(t)]
            if len(candidates) > 0:
                return True
            # FIXME Check for range properly
            isCnd = lambda x: ErlType.isRange(x)
            candidates = [t for t in inner_types if isCnd(t)]
            if len(candidates) > 0:
                return True
        return False

    def unifyWithFloat(self, tp):
        if tp.isFloat():
            return True
        if tp.isUnion():
            isCnd = lambda x: ErlType.isFloat(x)
            inner_types = cc.get_inner_types_from_union(tp.typ)
            candidates = [t for t in inner_types if isCnd(t)]
            if len(candidates) > 0:
                return True
        return False

    def unifyWithBitstring(self, tp):
        if tp.isBitstring():
            # FIXME Have to check if m * n is compatible
            return True
        if tp.isUnion():
            isCnd = lambda x: ErlType.isBitstring(x)
            inner_types = cc.get_inner_types_from_union(tp.typ)
            candidates = [t for t in inner_types if isCnd(t)]
            if len(candidates) > 0:
                return True
        return False

    def unifyWithTuple(self, tp):
        if tp.isTuple():
            return True
        if tp.isTupleDet():
            self.takenOverByType(tp)
            return True
        if tp.isUnion():
            isCnd = lambda x: ErlType.isTuple(x)
            inner_types = cc.get_inner_types_from_union(tp.typ)
            candidates = [t for t in inner_types if isCnd(t)]
            if len(candidates) > 0:
                return True
            isCnd = lambda x: ErlType.isTupleDet(x)
            candidates = [t for t in inner_types if isCnd(t)]
            if len(candidates) > 0:
                # FIXME Have to check specify the type with tupledet
                return True
            isCnd = lambda x: ErlType.isNTuple(x)
            candidates = [t for t in inner_types if isCnd(t)]
            if len(candidates) > 0:
                # FIXME Have to check specify the type with ntuple
                return True
        return False

    def unifyWithTupleDet(self, tp):
        if tp.isTuple():
            return True
        if tp.isTupleDet():
            args = cc.get_inner_types_from_tupledet(self.typ)
            tpArgs = cc.get_inner_types_from_tupledet(tp.typ)
            if len(args) != len(tpArgs):
                return False
            else:
                # FIXME unify the types of the elements
                return True
        if tp.isNTuple():
            # FIXME unify with n-tuple
            return True
        if tp.isUnion():
            # unify with union
            return True
        return False

    def unifyWithNTuple(self, tp):
        if tp.isTuple():
            return True
        if tp.isTupleDet():
            n = cc.get_inner_types_from_tupledet(self.typ)
            tpArgs = cc.get_inner_types_from_tupledet(tp.typ)
            if n != len(tpArgs):
                return False
            else:
                # FIXME unify the types of the elements
                return True
        if tp.isNTuple():
            # FIXME unify with n-tuple
            return True
        if tp.isUnion():
            # unify with union
            return True
        return False

    def isAny(self):
        return ErlType.isAny(self.typ)

    def isAtom(self):
        return ErlType.isAtom(self.typ)

    def isAtomLit(self):
        return ErlType.isAtomLit(self.typ)

    def isFloat(self):
        return ErlType.isFloat(self.typ)

    def isInteger(self):
        return ErlType.isInteger(self.typ)

    def isIntegerLit(self):
        return ErlType.isIntegerLit(self.typ)

    def isList(self):
        return ErlType.isList(self.typ)

    def isNil(self):
        return ErlType.isNil(self.typ)

    def isTuple(self):
        return ErlType.isTuple(self.typ)

    def isTupleDet(self):
        return ErlType.isTupleDet(self.typ)

    def isUnion(self):
        return ErlType.isUnion(self.typ)

    def isRange(self):
        return ErlType.isRange(self.typ)

    def isNonemptyList(self):
        return ErlType.isNonemptyList(self.typ)

    def isBitstring(self):
        return ErlType.isBitstring(self.typ)

    def isGenericFun(self):
        return ErlType.isGenericFun(self.typ)

    def isFun(self):
        return ErlType.isFun(self.typ)

    def isCons(self):
        return ErlType.isCons(self.typ)

    def isNTuple(self):
        return ErlType.isNTuple(self.typ)
