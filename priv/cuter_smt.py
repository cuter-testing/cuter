# -*- coding: utf-8 -*-

import cuter_generic_solver as cgs
import cuter_common as cc
import cuter_logger as clg

class ErlangSMT(cgs.AbstractErlangSolver):
    def __init__(self):
        self.model = None
        self.params = []

    # =========================================================================
    # Public API.
    # =========================================================================

    def fix_parameter(self, p, v):
        """
        Fixes a symbolic variable to a specific value.
        """
        pass

    def add_axioms(self):
        """
        Adds the axioms from memory to the solver.
        """
        pass

    def solve(self):
        """
        Solves a constraint set and returns the result.
        """
        return cc.SOLVER_STATUS_SAT

    def reset_solver(self):
        """
        Resets the solver.
        """
        pass

    def encode_model(self):
        """
        Encodes the resulting model to JSON.
        """
        return [({"s": p}, {"t": cc.JSON_TYPE_INT, "v": 1}) for p in self.params]

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
        self.params = [x["s"] for x in args]

    def mfa_spec(self, *spec):
        """
        Stores the spec of the entry point MFA.
        """
        pass

    # -------------------------------------------------------------------------
    # Constraints.
    # -------------------------------------------------------------------------

    def match_equal(self, term1, term2):
        """
        Asserts the predicate: term1 == term2
        """
        pass

    def match_not_equal(self, term1, term2):
        """
        Asserts the predicate: term1 != term2
        """
        pass

    # -------------------------------------------------------------------------
    # Reversed constraints.
    # -------------------------------------------------------------------------

    def match_equal_reversed(self, term1, term2):
        """
        Asserts the predicate: Not (term1 == term2)
        """
        pass

    def match_not_equal_reversed(self, term1, term2):
        """
        Asserts the predicate: Not (term1 != term2)
        """
        pass
