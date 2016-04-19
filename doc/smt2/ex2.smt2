; Set parameters.
(set-logic ALL_SUPPORTED)
(set-option :produce-models true)

; Declare the datatypes and auxiliary functions.
(declare-datatypes () 
    ((Term (int (ival Int))
           (real (rval Real))
           (list (lval TList))
           (tuple (tval TList))
           (atom (aval IList))
           (bin (sz Int) (bval BVList))
           (fun (fval Int)))
    (TList (nil)
           (cons (hd Term) (tl TList)))
    (IList (inil)
           (icons (ihd Int) (itl IList)))
    (BVList (bnil) 
            (bcons (bhd (_ BitVec 1)) (btl BVList)))))

(declare-fun arity (Int) Int)
(declare-fun fmap (Int) (Array TList Term))

(declare-const atomTrue Term)
(assert (= atomTrue (atom (icons 116 (icons 114 (icons 117 (icons 101 inil)))))))
(declare-const atomFalse Term)
(assert (= atomFalse (atom (icons 102 (icons 97 (icons 108 (icons 115 (icons 101 inil))))))))

; Add the actual program.

; parameters
(declare-const x Term)
(declare-const y Term)

(assert (is-list x))
(assert (is-cons (lval x)))
(assert (is-tuple y))
(assert (is-cons (tval y)))
(assert (is-cons (tl (tval y))))

(check-sat)
(get-model)
;(get-value (x))
;(get-value (y))
