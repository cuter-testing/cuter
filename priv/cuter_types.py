#!/usr/bin/env python
# -*- coding: utf-8 -*-

from copy import deepcopy
import cuter_global as cglb
import cuter_logger as clg
import cuter_common as cc

class ErlType:
    """
    Representation of Types in JSON form (as retrieved in the logs).
    """

    @classmethod
    def getType(cls, typ):
        return typ["tp"]

    @classmethod
    def setType(cls, typ, tp):
        typ["tp"] = tp

    @classmethod
    def getArgs(cls, typ):
        return typ["a"]

    @classmethod
    def setArgs(cls, typ, args):
        typ["a"] = args

    @classmethod
    def generateAnyType(cls):
        tp = {}
        cls.setType(tp, cc.JSON_ERLTYPE_ANY)
        return tp

    @classmethod
    def generateTupleType(cls):
        tp = {}
        cls.setType(tp, cc.JSON_ERLTYPE_TUPLE)
        return tp

    @classmethod
    def generateListAnyType(cls):
        tp = {}
        cls.setType(tp, cc.JSON_ERLTYPE_LIST)
        cls.setArgs(tp, cls.generateAnyType())
        return tp

    @classmethod
    def generateNTupleType(cls, sz):
        tp = {}
        cls.setNTupleType(tp, sz)
        return tp

    @classmethod
    def generateConsType(cls):
        tp = {}
        cls.setConsType(tp)
        return tp

    @classmethod
    def isAny(cls, typ):
        return cls.getType(typ) == cc.JSON_ERLTYPE_ANY

    @classmethod
    def isAtom(cls, typ):
        return cls.getType(typ) == cc.JSON_ERLTYPE_ATOM

    @classmethod
    def isAtomLit(cls, typ):
        return cls.getType(typ) == cc.JSON_ERLTYPE_ATOMLIT

    @classmethod
    def isFloat(cls, typ):
        return cls.getType(typ) == cc.JSON_ERLTYPE_FLOAT

    @classmethod
    def isInteger(cls, typ):
        return cls.getType(typ) == cc.JSON_ERLTYPE_INTEGER

    @classmethod
    def isIntegerLit(cls, typ):
        return cls.getType(typ) == cc.JSON_ERLTYPE_INTEGERLIT

    @classmethod
    def isList(cls, typ):
        return cls.getType(typ) == cc.JSON_ERLTYPE_LIST

    @classmethod
    def isNil(cls, typ):
        return cls.getType(typ) == cc.JSON_ERLTYPE_NIL

    @classmethod
    def isTuple(cls, typ):
        return cls.getType(typ) == cc.JSON_ERLTYPE_TUPLE

    @classmethod
    def isTupleDet(cls, typ):
        return cls.getType(typ) == cc.JSON_ERLTYPE_TUPLEDET

    @classmethod
    def isUnion(cls, typ):
        return cls.getType(typ) == cc.JSON_ERLTYPE_UNION

    @classmethod
    def isRange(cls, typ):
        return cls.getType(typ) == cc.JSON_ERLTYPE_RANGE

    @classmethod
    def isNonemptyList(cls, typ):
        return cls.getType(typ) == cc.JSON_ERLTYPE_NONEMPTY_LIST

    @classmethod
    def isBitstring(cls, typ):
      return cls.getType(typ) == cc.JSON_ERLTYPE_BITSTRING

    @classmethod
    def isGenericFun(cls, typ):
        return cls.getType(typ) == cc.JSON_ERLTYPE_GENERIC_FUN

    @classmethod
    def isFun(cls, typ):
        return cls.getType(typ) == cc.JSON_ERLTYPE_FUN

    @classmethod
    def isCons(cls, typ):
        return cls.getType(typ) == cc.JSON_ERLTYPE_CONS

    @classmethod
    def isNTuple(cls, typ):
        return cls.getType(typ) == cc.JSON_ERLTYPE_NTUPLE

    @classmethod
    def setNonEmptyListType(cls, typ):
        cls.setType(typ, cc.JSON_ERLTYPE_NONEMPTY_LIST)

    @classmethod
    def setConsType(cls, typ):
        cls.setType(typ, cc.JSON_ERLTYPE_CONS)
        cls.setArgs(typ, None)

    @classmethod
    def setNilType(cls, typ):
        cls.setType(typ, cc.JSON_ERLTYPE_NIL)
        cls.setArgs(typ, None)

    @classmethod
    def setNTupleType(cls, typ, n):
        cls.setType(typ, cc.JSON_ERLTYPE_NTUPLE)
        cls.setArgs(typ, n)

    @classmethod
    def getListTypeFromNonemptyList(cls, typ):
        tp = deepcopy(typ)
        cls.setType(tp, cc.JSON_ERLTYPE_LIST)
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
            h = Type(deepcopy(ErlType.getArgs(self.typ)))
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
                isCnd = lambda x: ErlType.isTuple(x) or ErlType.isTupleDet(x)
                candidates = [t for t in ErlType.getArgs(tp.typ) if isCnd(t)]
                if len(candidates) > 0:
                    tp.typ = candidates[0]
                    if ErlType.isTupleDet(tp.typ):
                        sz = len(ErlType.getArgs(tp.typ))
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
            [retType] = ErlType.getArgs(self.typ)
            tpArgs = [Type(ErlType.generateAnyType()) for _ in range(arity)]
            return tpArgs, Type(deepcopy(retType))
        elif ErlType.isFun(self.typ):
            self.hasFunBeenUsed = True
            types = ErlType.getArgs(self.typ)
            argsTypes, retType = types[:-1], types[-1]
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
                candidates = [tp for tp in ErlType.getArgs(self.typ) if isL(tp)]
                if len(candidates) > 0:
                    self.typ = candidates[0]
                else:
                    # TODO Log inconsistency
                    pass
            # actual type elaborations
            if ErlType.isList(self.typ):
                h = Type(deepcopy(ErlType.getArgs(self.typ)))
                t = Type(deepcopy(self.typ))
                ErlType.setConsType(self.typ)
                self.isFinal = True
                self.children = [h, t]
            elif ErlType.isNonemptyList(self.typ):
                h = Type(deepcopy(ErlType.getArgs(self.typ)))
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
                candidates = [tp for tp in ErlType.getArgs(self.typ) if not ErlType.isNonemptyList(tp)]
                if len(candidates) > 0:
                    ErlType.setArgs(self.typ, candidates)
                    for tp in ErlType.getArgs(self.typ):
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
                isCnd = lambda x: ErlType.isList(x) or ErlType.isNil(x)
                candidates = [tp for tp in ErlType.getArgs(self.typ) if isCnd(tp)]
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
                candidates = [tp for tp in ErlType.getArgs(self.typ) if not ErlType.isNil(tp)]
                for cnd in candidates:
                    if ErlType.isList(cnd):
                        ErlType.setNonEmptyListType(cnd)
                if len(candidates) > 0:
                    ErlType.setArgs(self.typ, candidates)
                else:
                    # TODO Log inconsistency
                    pass
            elif ErlType.isList(self.typ):
                self.matchCons()

    def matchNotList(self):
        if not self.isFinal:
            # preprocess unions
            if ErlType.isUnion(self.typ):
                isCnd = lambda x: not ErlType.isList(x) and not ErlType.isNil(x) and not ErlType.isNonemptyList(x)
                candidates = [tp for tp in ErlType.getArgs(self.typ) if isCnd(tp)]
                if len(candidates) > 0:
                    ErlType.setArgs(self.typ, candidates)
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
                isCnd = lambda x: ErlType.isTuple(x) or (ErlType.isTupleDet(x) and len(ErlType.getArgs(x)) == sz)
                candidates = [tp for tp in ErlType.getArgs(self.typ) if isCnd(tp)]
                if len(candidates) > 0:
                    self.typ = candidates[0]
                else:
                    # TODO Log inconsistency
                    pass
            # actual type elaborations
            if ErlType.isTuple(self.typ):
                pass
            elif ErlType.isTupleDet(self.typ) and len(ErlType.getArgs(self.typ)) == sz:
                self.isFinal = True
                self.children = [Type(deepcopy(tp)) for tp in ErlType.getArgs(self.typ)]
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
                isCnd = lambda x: not (ErlType.isTupleDet(x) and len(ErlType.getArgs(x)) == sz)
                candidates = [tp for tp in ErlType.getArgs(self.typ) if isCnd(tp)]
                if len(candidates) > 0:
                    ErlType.setArgs(self.typ, candidates)
                else:
                    # TODO Log inconsistency
                    pass
            # actual type elaborations
            if ErlType.isTupleDet(self.typ) and len(ErlType.getArgs(self.typ)) == sz:
                # TODO Log inconsistency
                pass

    def notMatchTuple(self):
        if not self.isFinal:
            # preprocess unions
            if ErlType.isUnion(self.typ):
                isCnd = lambda x: not ErlType.isTupleDet(x) and not ErlType.isTuple(x)
                candidates = [tp for tp in ErlType.getArgs(self.typ) if isCnd(tp)]
                if len(candidates) > 0:
                    ErlType.setArgs(self.typ, candidates)
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
            return all(self.isFinalType(t) for t in ErlType.getArgs(typ))
        elif ErlType.isUnion(typ):
            return all(self.isFinalType(t) for t in ErlType.getArgs(typ))
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
            candidates = [t for t in ErlType.getArgs(tp.typ) if isCnd(t)]
            if len(candidates) > 0:
                return True
            isCnd = lambda x: ErlType.isAtomLit(x)
            candidates = [t for t in ErlType.getArgs(tp.typ) if isCnd(t)]
            if len(candidates) > 0:
                ErlType.setArgs(candidates)
                self.takenOverByType(tp)
                return True
        return False

    def unifyWithAtomLit(self, tp):
        if tp.isAtom():
            return True
        if tp.isAtomLit():
            return ErlType.getArgs(self.typ) == ErlType.getArgs(tp.typ)
        if tp.isUnion():
            isCnd = lambda x: ErlType.isAtom(x)
            candidates = [t for t in ErlType.getArgs(tp.typ) if isCnd(t)]
            if len(candidates) > 0:
                return True
            isCnd = lambda x: ErlType.isAtomLit(x) and ErlType.getArgs(self.typ) == ErlType.getArgs(x)
            candidates = [t for t in ErlType.getArgs(tp.typ) if isCnd(t)]
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
            candidates = [t for t in ErlType.getArgs(tp.typ) if isCnd(t)]
            if len(candidates) > 0:
                return True
            isCnd = lambda x: ErlType.isIntegerLit(x) or ErlType.isRange(x)
            candidates = [t for t in ErlType.getArgs(tp.typ) if isCnd(t)]
            if len(candidates) > 0:
                ErlType.setArgs(candidates)
                self.takenOverByType(tp)
                return True
        return False

    def unifyWithIntegerLit(self, tp):
        if tp.isInteger():
            return True
        if tp.isIntegerLit():
            return ErlType.getArgs(self.typ) == ErlType.getArgs(tp.typ)
        if tp.isUnion():
            isCnd = lambda x: ErlType.isInteger(x)
            candidates = [t for t in ErlType.getArgs(tp.typ) if isCnd(t)]
            if len(candidates) > 0:
                return True
            isCnd = lambda x: ErlType.isIntegerLit(x) and ErlType.getArgs(self.typ) == ErlType.getArgs(x)
            candidates = [t for t in ErlType.getArgs(tp.typ) if isCnd(t)]
            if len(candidates) > 0:
                return True
            # FIXME Check for range properly
            isCnd = lambda x: ErlType.isRange(x)
            candidates = [t for t in ErlType.getArgs(tp.typ) if isCnd(t)]
            if len(candidates) > 0:
                return True
        return False

    def unifyWithFloat(self, tp):
        if tp.isFloat():
            return True
        if tp.isUnion():
            isCnd = lambda x: ErlType.isFloat(x)
            candidates = [t for t in ErlType.getArgs(tp.typ) if isCnd(t)]
            if len(candidates) > 0:
                return True
        return False

    def unifyWithBitstring(self, tp):
        if tp.isBitstring():
            # FIXME Have to check if m * n is compatible
            return True
        if tp.isUnion():
            isCnd = lambda x: ErlType.isBitstring(x)
            candidates = [t for t in ErlType.getArgs(tp.typ) if isCnd(t)]
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
            candidates = [t for t in ErlType.getArgs(tp.typ) if isCnd(t)]
            if len(candidates) > 0:
                return True
            isCnd = lambda x: ErlType.isTupleDet(x)
            candidates = [t for t in ErlType.getArgs(tp.typ) if isCnd(t)]
            if len(candidates) > 0:
                # FIXME Have to check specify the type with tupledet
                return True
            isCnd = lambda x: ErlType.isNTuple(x)
            candidates = [t for t in ErlType.getArgs(tp.typ) if isCnd(t)]
            if len(candidates) > 0:
                # FIXME Have to check specify the type with ntuple
                return True
        return False

    def unifyWithTupleDet(self, tp):
        if tp.isTuple():
            return True
        if tp.isTupleDet():
            args = ErlType.getArgs(self.typ)
            tpArgs =  ErlType.getArgs(tp.typ)
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
            n = ErlType.getArgs(self.typ)
            tpArgs =  ErlType.getArgs(tp.typ)
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
