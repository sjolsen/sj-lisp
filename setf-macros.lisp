;;;; Copyright © 2014 Stuart J. Olsen. See file "LICENSE."
;;;
;;;; Operations for juggling SETF operations, with a focus on avoiding the
;;;; multiple evaluation of places, as much for performance as for correctness.

(in-package :sj-lisp)


;;;; Macro EXCHANGEF

;;; Essentially the same as single-place SETF, but return the _old_ value rather
;;; than the new one. Especially useful for breaking apart lists.
(defmacro exchangef (place value &environment env)
  "Assign VALUE to PLACE, returning PLACE's previous values"
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place env)
    (let ((old-values (loop
                         repeat (length store-vars)
                         collecting (gensym))))
      `(let ,(mapcar #'list vars vals)
         (multiple-value-bind ,old-values ,reader-form
           (multiple-value-bind ,store-vars ,value
             ,writer-form
             (values ,@old-values)))))))


;;;; Macro EXCHANGEF/CHANGED

;;; The same as EXCHANGEF, but return as the second value whether the primary
;;; value was changed according to TEST. As a result of this, only the first
;;; primary value is returned (EXCHANGEF returns all primary values via VALUES).
(defmacro exchangef/changed (place value &key (test '#'eq) &environment env)
  "Assign VALUE to PLACE, returning PLACE's previous value and whether that
value changed according to TEST."
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place env)
    (let ((old-values (loop
                         repeat (length store-vars)
                         collecting (gensym))))
      `(let ,(mapcar #'list vars vals)
         (multiple-value-bind ,old-values ,reader-form
           (multiple-value-bind ,store-vars ,value
             ,writer-form
             (values ,(first store-vars)
                     (not (funcall ,test
                                   ,(first old-values)
                                   ,(first store-vars))))))))))


;;;; Macro SETF->CHANGED

;;; Like single-assignment SETF, but returns whether PLACE was changed according
;;; to TEST.
(defmacro setf->changed (place value &key (test '#'eq))
  `(nth-value 1 (exchangef/changed ,place ,value :test ,test)))


;;;; Macro SWAPF

;;; Exactly what it says on the tin. The purpose is served by ROTATEF with two
;;; arguments, but it can produce some hairy code. For example, with aggressive
;;; optimization, the following prints out 70 bytes of x86_64 on my machine with
;;; this SWAPF, 252 with the naïve ROTATEF-based implementation:
;;;
;;;    (DISASSEMBLE
;;;      (LAMBDA ()
;;;        (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0)))
;;;        (LET ((X 1) (Y 2)
;;;              (A 3) (B 4))
;;;          (SWAPF (VALUES X Y) (VALUES A B)))))
;;;
;;; The new value of the first place is returned. The SECOND-VALUE option exists
;;; because swapping the order of the arguments can affect the assigned
;;; values. Better that it be done here than that the whole macro be
;;; reimplemented with a one-character difference.
(defmacro swapf (place1 place2 &key second-value &environment env)
  "Swap the values of the two places, returning the first place's new value, or
the second's if so specified."
  (multiple-value-bind (vars1 vals1 store-vars1 writer-form1 reader-form1)
      (get-setf-expansion place1 env)
    (multiple-value-bind (vars2 vals2 store-vars2 writer-form2 reader-form2)
        (get-setf-expansion place2 env)
      `(let (,@(mapcar #'list vars1 vals1)
             ,@(mapcar #'list vars2 vals2))
         (multiple-value-bind ,store-vars2 ,reader-form1
           (multiple-value-bind ,store-vars1 ,reader-form2
             ,writer-form1
             ,writer-form2
             ,(if second-value
                  `(values ,@store-vars2)
                  `(values ,@store-vars1))))))))
