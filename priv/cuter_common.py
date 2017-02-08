#!/usr/bin/env python
# -*- coding: utf-8 -*-

from cuter_global import *
from cuter_proto_erlang_term_pb2 import ErlangTerm
from cuter_proto_log_entry_pb2 import LogEntry
from cuter_proto_solver_response_pb2 import SolverResponse
from cuter_proto_spec_pb2 import Spec

def is_interpretable(entry):
  xs = set([LogEntry.OP_SPAWN, LogEntry.OP_SPAWNED,
            LogEntry.OP_MSG_SEND, LogEntry.OP_MSG_RECEIVE, LogEntry.OP_MSG_CONSUME])
  return entry.type not in xs

def is_reversible(entry):
  return entry.is_constraint

# =============================================================================
# Create LogEntry objects.
# =============================================================================

def mk_log_entry(typ, args, tag=None, is_constraint=None):
    """
    Parameters
        typ : LogEntry.Type
        args : list of ErlangTerm
        tag : integer
        is_constraint : boolean
    """
    e = LogEntry()
    e.type = typ
    e.arguments.extend(args)
    if not tag is None:
        e.tag = tag
    if not is_constraint is None:
        e.is_constraint = is_constraint
    return e

def get_tag(entry):
    """
    Parameters
        entry : LogEntry
    """
    return entry.tag

def get_spec(entry):
    """
    Parameters
        entry : an OP_SPEC LogEntry
    """
    assert entry.type == LogEntry.OP_SPEC
    return entry.spec

# =============================================================================
# Create / Manipulate ErlangTerm objects.
# =============================================================================

def mk_symb(s):
    """
    Parameters
        s : string
    """
    t = ErlangTerm()
    t.type = ErlangTerm.SYMBOLIC_VARIABLE
    t.value = s
    return t

def get_symb(t):
    """
    Parameters
        s : ErlangTerm
    """
    assert t.type == ErlangTerm.SYMBOLIC_VARIABLE
    return t.value

def is_symb(t):
    """
    Parameters
        s : ErlangTerm
    """
    return t.type == ErlangTerm.SYMBOLIC_VARIABLE

def mk_any():
    t = ErlangTerm()
    t.type = ErlangTerm.ANY
    return t

def mk_int(i):
    """
    Parameters
        i : int
    """
    t = ErlangTerm()
    t.type = ErlangTerm.INTEGER
    t.value = str(i)
    return t

def is_int(t):
    """
    Parameters
        t : ErlangTerm
    """
    return t.type == ErlangTerm.INTEGER

def get_int(t):
    """
    Parameters
        t : ErlangTerm
    """
    assert is_int(t)
    return int(t.value)

def mk_float(f):
    """
    Parameters
        f : float
    """
    t = ErlangTerm()
    t.type = ErlangTerm.FLOAT
    t.value = str(f)
    return t

def is_float(t):
    """
    Parameters
        t : ErlangTerm
    """
    return t.type == ErlangTerm.FLOAT

def get_float(t):
    """
    Parameters
        t : ErlangTerm
    """
    assert is_float(t)
    return float(t.value)

def mk_atom(chars):
    """
    Parameters
        chars : list of ints
    """
    t = ErlangTerm()
    t.type = ErlangTerm.ATOM
    t.atom_chars.extend(chars)
    return t

def is_atom(t):
    """
    Parameters
        t : ErlangTerm
    """
    return t.type == ErlangTerm.ATOM

def get_atom_chars(t):
    """
    Parameters
        t : ErlangTerm
    """
    assert is_atom(t)
    return t.atom_chars

def mk_tuple(subterms):
    """
    Parameters
        subterms : list of ErlangTerm
    """
    t = ErlangTerm()
    t.type = ErlangTerm.TUPLE
    t.subterms.extend(subterms)
    return t

def is_tuple(t):
    """
    Parameters
        t : ErlangTerm
    """
    return t.type == ErlangTerm.TUPLE

def get_tuple_subterms(t):
    """
    Parameters
        t : ErlangTerm
    """
    assert is_tuple(t)
    return t.subterms

def mk_list(subterms):
    """
    Parameters
        subterms : list of ErlangTerm
    """
    t = ErlangTerm()
    t.type = ErlangTerm.LIST
    t.subterms.extend(subterms)
    return t

def is_list(t):
    """
    Parameters
        t : ErlangTerm
    """
    return t.type == ErlangTerm.LIST

def get_list_subterms(t):
    """
    Parameters
        t : ErlangTerm
    """
    assert is_list(t)
    return t.subterms

def mk_bitstring(bits):
    """
    Parameters
        bits : list of bool
    """
    t = ErlangTerm()
    t.type = ErlangTerm.BITSTRING
    t.bits.extend(bits)
    return t

def is_bitstring(t):
    """
    Parameters
        t : ErlangTerm
    """
    return t.type == ErlangTerm.BITSTRING

def get_bits(t):
    """
    Parameters
        t : ErlangTerm
    """
    assert is_bitstring(t)
    return t.bits

def is_alias(t):
    """
    Parameters
        t : ErlangTerm
    """
    return t.type == ErlangTerm.SUBTERM

def get_alias(t):
    """
    Parameters
        t : ErlangTerm
    """
    assert is_alias(t)
    return t.value

def get_shared(t):
    """
    Parameters
        t : ErlangTerm
    """
    return t.shared

def mk_alias(s):
    """
    Parameters
        s : string
    """
    t = ErlangTerm()
    t.type = ErlangTerm.SUBTERM
    t.value = s
    return t

def mk_fun(arity, points, otherwise):
    """
    Parameters
        arity : integer
        points : list of FunEntry
        otherwise : ErlangTerm
    """
    t = ErlangTerm()
    t.type = ErlangTerm.FUN
    t.arity = arity
    t.otherwise.CopyFrom(otherwise)
    if len(points):
        t.points.extend(points)
    return t

def mk_const_fun(arity, otherwise):
    """
    Parameters
        arity : integer
        otherwise : ErlangTerm
    """
    return mk_fun(arity, [], otherwise)

def mk_fun_entry(args, value):
    """
    Parameters
        args : list of ErlangTerm
        value : ErlangTerm
    """
    e = ErlangTerm.FunEntry()
    e.arguments.extend(args)
    e.value.CopyFrom(value)
    return e

def get_value_from_fun_entry(entry):
    """
    Parameters
        entry : ErlangTerm.FunEntry
    """
    return entry.value

# =============================================================================
# Create / Manipulate SolverResponse objects.
# =============================================================================

def mk_sat():
    r = SolverResponse()
    r.type = SolverResponse.MODEL_STATUS
    r.status = SolverResponse.SAT
    return r

def is_sat(r):
    """
    Parameters
        r : SolverResponse
    """
    assert r.type == SolverResponse.MODEL_STATUS
    return r.status == SolverResponse.SAT

def mk_unsat():
    r = SolverResponse()
    r.type = SolverResponse.MODEL_STATUS
    r.status = SolverResponse.UNSAT
    return r

def mk_unknown():
    r = SolverResponse()
    r.type = SolverResponse.MODEL_STATUS
    r.status = SolverResponse.UNKNOWN
    return r

def mk_timeout():
    r = SolverResponse()
    r.type = SolverResponse.MODEL_STATUS
    r.status = SolverResponse.TIMEOUT
    return r

def mk_model_entry(var, value):
    """
    Parameters
        var : ErlangTerm
        value : ErlangTerm
    """
    e = SolverResponse.ModelEntry()
    e.var.CopyFrom(var)
    e.value.CopyFrom(value)
    return e

def mk_model(entries):
    """
    Parameters
        entries : list of SolverResponse.ModelEntry
    """
    m = SolverResponse.Model()
    m.entries.extend(entries)
    return m

def mk_model_data(model):
    """
    Parameters
        model : SolverResponse.Model
    """
    r = SolverResponse()
    r.type = SolverResponse.MODEL_DATA
    r.model.CopyFrom(model)
    return r

def get_model_entries(model_data):
    """
    Parameters
        model_data : SolverResponse that has a 'model' field
    """
    return model_data.model.entries

# =============================================================================
# Manipulate Spec objects.
# =============================================================================

def get_type_defs_of_spec(spec):
    """
    Parameters
        spec : Spec
    """
    return spec.typedefs

def get_spec_clauses(spec):
    """
    Parameters
        spec : Spec
    """
    return spec.clauses

def get_param_types_of_clause(funsig):
    """
    Parameters
        funsig : Spec.FunSig
    """
    return funsig.complete.parameters

def mk_typelist(types):
    """
    Parameters
        types : list of Spec.Type
    """
    t = Spec.TypeList()
    t.types.extend(types)
    return t

def is_type_message(msg):
    """
    Parameters
        msg : any protobuf message
    """
    return msg.DESCRIPTOR.full_name == "Spec.Type"

def mk_type_any():
    t = Spec.Type()
    t.type = Spec.ANY
    return t

def is_type_any(t):
    """
    Parameters
        t : Spec.Type
    """
    return t.type == Spec.ANY

def is_type_atom(t):
    """
    Parameters
        t : Spec.Type
    """
    return t.type == Spec.ATOM

def get_literal_from_atomlit(t):
    """
    Parameters
        t : Spec.Type
    """
    assert t.type == Spec.ATOM_LITERAL
    return t.literal

def is_type_atomlit(t):
    """
    Parameters
        t : Spec.Type
    """
    return t.type == Spec.ATOM_LITERAL

def is_type_float(t):
    """
    Parameters
        t : Spec.Type
    """
    return t.type == Spec.FLOAT

def is_type_integer(t):
    """
    Parameters
        t : Spec.Type
    """
    return t.type == Spec.INTEGER

def get_literal_from_integerlit(t):
    """
    Parameters
        t : Spec.Type
    """
    assert t.type == Spec.INTEGER_LITERAL
    return t.literal

def is_type_integerlit(t):
    """
    Parameters
        t : Spec.Type
    """
    return t.type == Spec.INTEGER_LITERAL

def mk_type_list(tp):
    """
    Parameters
        t : Spec.Type
    """
    t = Spec.Type()
    t.type = Spec.LIST
    t.inner_type.CopyFrom(tp)
    return t

def set_type_list(t):
    """
    Parameters
        t : Spec.Type
    """
    t.type = Spec.LIST

def get_inner_type_from_list(t):
    """
    Parameters
        t : Spec.Type
    """
    assert t.type == Spec.LIST
    return t.inner_type

def is_type_list(t):
    """
    Parameters
        t : Spec.Type
    """
    return t.type == Spec.LIST

def get_inner_type_from_nonempty_list(t):
    """
    Parameters
        t : Spec.Type
    """
    assert t.type == Spec.NONEMPTY_LIST
    return t.inner_type

def set_type_nonempty_list(t):
    """
    Parameters
        t : Spec.Type
    """
    t.type = Spec.NONEMPTY_LIST

def is_type_nonempty_list(t):
    """
    Parameters
        t : Spec.Type
    """
    return t.type == Spec.NONEMPTY_LIST

def set_type_nil(t):
    """
    Parameters
        t : Spec.Type
    """
    t.type = Spec.NIL
    t.ClearField("arg")

def is_type_nil(t):
    """
    Parameters
        t : Spec.Type
    """
    return t.type == Spec.NIL

def mk_type_tuple():
    t = Spec.Type()
    t.type = Spec.TUPLE
    return t

def is_type_tuple(t):
    """
    Parameters
        t : Spec.Type
    """
    return t.type == Spec.TUPLE

def get_inner_types_from_tupledet(t):
    """
    Parameters
        t : Spec.Type
    """
    assert t.type == Spec.TUPLEDET
    return t.inner_types.types

def is_type_tupledet(t):
    """
    Parameters
        t : Spec.Type
    """
    return t.type == Spec.TUPLEDET

def get_inner_types_from_union(t):
    """
    Parameters
        t : Spec.Type
    """
    assert t.type == Spec.UNION
    return t.inner_types.types

def set_inner_types_to_union(t, inner_types):
    """
    Parameters
        t : Spec.Type
        inner_types : list of Spec.Type
    """
    assert t.type == Spec.UNION
    t.ClearField("inner_types")
    ts = mk_typelist(inner_types)
    t.inner_types.CopyFrom(ts)

def is_type_union(t):
    """
    Parameters
        t : Spec.Type
    """
    return t.type == Spec.UNION

def has_lower_bound(bounds):
    """
    Parameters
        bounds : Spec.RangeBounds
    """
    return len(bounds.lower_bound) > 0

def get_lower_bound(bounds):
    """
    Parameters
        bounds : Spec.RangeBounds
    """
    return int(bounds.lower_bound) if has_lower_bound(bounds) else ""

def has_upper_bound(bounds):
    """
    Parameters
        bounds : Spec.RangeBounds
    """
    return len(bounds.upper_bound) > 0

def get_upper_bound(bounds):
    """
    Parameters
        bounds : Spec.RangeBounds
    """
    return int(bounds.upper_bound) if has_upper_bound(bounds) else ""

def get_range_bounds_from_range(t):
    """
    Parameters
        t : Spec.Type
    """
    assert t.type == Spec.RANGE
    return t.range_bounds

def is_type_range(t):
    """
    Parameters
        t : Spec.Type
    """
    return t.type == Spec.RANGE

def get_segment_size_from_bitstring(t):
    """
    Parameters
        t : Spec.Type
    """
    assert t.type == Spec.BITSTRING
    return t.segment_size

def is_type_bitstring(t):
    """
    Parameters
        t : Spec.Type
    """
    return t.type == Spec.BITSTRING

def is_complete_funsig(t):
    """
    Parameters
        t : Spec.FunSig
    """
    return t.HasField("complete")

def get_complete_funsig_arity(t):
    """
    Parameters
        t : Spec.FunSig
    """
    assert is_complete_funsig(t)
    return len(t.complete.parameters)

def get_parameters_from_complete_funsig(t):
    """
    Parameters
        t : Spec.FunSig
    """
    assert is_complete_funsig(t)
    return t.complete.parameters

def get_rettype_from_funsig(t):
    """
    Parameters
        t : Spec.FunSig
    """
    if is_complete_funsig(t):
        return t.complete.return_value
    else:
        return t.just_return

def get_rettype_from_fun(t):
    """
    Parameters
        t : Spec.Type
    """
    assert t.type == Spec.FUN
    return get_rettype_from_funsig(t.fun)

def get_parameters_from_complete_fun(t):
    """
    Parameters
        t : Spec.Type
    """
    assert t.type == Spec.FUN
    return get_parameters_from_complete_funsig(t.fun)

def get_funsig_from_fun(t):
    """
    Parameters
        t : Spec.Type
    """
    assert t.type == Spec.FUN
    return t.fun

def is_type_generic_fun(t):
    """
    Parameters
        t : Spec.Type
    """
    return t.type == Spec.FUN and t.fun.HasField("just_return")

def is_type_complete_fun(t):
    """
    Parameters
        t : Spec.Type
    """
    return t.type == Spec.FUN and t.fun.HasField("complete")

def mk_type_cons():
    t = Spec.Type()
    t.type = Spec.CONS
    return t

def set_type_cons(t):
    """
    Parameters
        t : Spec.Type
    """
    t.type = Spec.CONS
    t.ClearField("arg")

def is_type_cons(t):
    """
    Parameters
        t : Spec.Type
    """
    return t.type == Spec.CONS

def mk_type_ntuple(sz):
    """
    Parameters
        sz : integer
    """
    t = Spec.Type()
    t.type = Spec.NTUPLE
    t.ntuple_size = sz
    return t

def get_size_of_ntuple(t):
    """
    Parameters
        t : Spec.Type
    """
    assert t.type == Spec.NTUPLE
    return t.ntuple_size

def set_type_ntuple(t, sz):
    """
    Parameters
        t : Spec.Type
        sz : integer
    """
    t.type = Spec.NTUPLE
    t.ntuple_size = sz

def is_type_ntuple(t):
    """
    Parameters
        t : Spec.Type
    """
    return t.type == Spec.NTUPLE

def is_type_userdef(t):
    """
    Parameters
        t : Spec.Type
    """
    return t.type == Spec.USERDEF

def get_type_name_of_userdef(t):
    """
    Parameters
        t : Spec.Type
    """
    assert t.type == Spec.USERDEF
    return t.type_name

def get_typedef_name(tdef):
    """
    Parameters
        tdef : Spec.TypeDef
    """
    return tdef.name

def get_typedef_definition(tdef):
    """
    Parameters
        tdef : Spec.TypeDef
    """
    return tdef.definition
