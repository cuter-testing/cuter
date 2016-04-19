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

; p0, p1, p2 are the parameters
(declare-const p0 Term)
(declare-const p1 Term)
(declare-const p2 Term)
; x1 - x13 are intermediate variables
(declare-const x1 Term)
(declare-const x2 Term)
(declare-const x3 Term)
(declare-const x4 Term)
(declare-const x5 Term)
(declare-const x6 Term)
(declare-const x7 Term)
(declare-const x8 Term)
(declare-const x9 Term)
(declare-const x10 Term)
(declare-const x11 Term)
(declare-const x12 Term)
(declare-const x13 Term)

;x1 = fun is_function/2(p2, 1)
(assert (ite
  (and (is-fun p2) (= (arity (fval p2)) 1))
  (= x1 atomTrue)
  (= x1 atomFalse)))

; x1 = true
(assert (= x1 atomTrue))

; x2 = p2(p1)
(assert (= (select (fmap (fval p2)) (cons p1 nil)) x2))


; istuple(x2, Just 2)
(assert (is-tuple x2))
(assert (is-cons (tval x2)))
(assert (is-cons (tl (tval x2))))

; x2 = {x3, x4}
(assert (= x2 (tuple (cons x3 (cons x4 nil)))))

; iscons(x4)
(assert (is-list x4))
(assert (is-cons (lval x4)))

; x4 = [x5 | x6]
(assert (= x4 (list (cons x5 (cons x6 nil)))))

; iscons(x6)
(assert (is-list x6))
(assert (is-cons (lval x6)))

; x6 = [x7 | x8]
(assert (= x6 (list (cons x7 (cons x8 nil)))))

; x9 = fun is_function/2(p2, 1)
(assert (ite
  (and (is-fun p2) (= (arity (fval p2)) 1))
  (= x9 atomTrue)
  (= x9 atomFalse)))

; x9 = true
(assert (= x9 atomTrue))

; x10 = p2(x5)
(assert (= (select (fmap (fval p2)) (cons x5 nil)) x10))

; x11 = fun is_integer/1(x10)
(assert (ite
  (is-int x10)
  (= x11 atomTrue)
  (= x11 atomFalse)))

; x11 = true
(assert (= x11 atomTrue))

; x12 = fun is_integer/1(p0)
(assert (ite
  (is-int p0)
  (= x12 atomTrue)
  (= x12 atomFalse)))

; x12 = true
(assert (= x12 atomTrue))

; x13 = fun +/2(x10, p0)
(assert (or (is-int x10) (is-real x10)))
(assert (or (is-int p0) (is-real p0)))
(assert (ite
  (and (is-int x10) (is-int p0))
  (= x13 (int (+ (ival x10) (ival p0))))
  (ite
    (and (is-real x10) (is-int p0))
    (= x13 (real (+ (rval x10) (ival p0))))
    (ite
      (and (is-int x10) (is-real p0))
      (= x13 (real (+ (ival x10) (rval p0))))
      (= x13 (real (+ (rval x10) (rval p0))))))))

; x13 <> 42
; (assert (not (= x13 (int 42))))

; x13 <> 42 (REVERSED)
(assert (not (not (= x13 (int 42)))))

(check-sat)
(get-model)
