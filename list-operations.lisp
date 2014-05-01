;;;; Copyright © 2014 Stuart J. Olsen. See file "LICENSE."
;;;
;;;; Operations on lists. Where possible, both modifying and non-modifying
;;;; versions are given.

(in-package :sj-lisp)


;;;; Functions SPLIT-LIST, NSPLIT-LIST

;;; Given one list, return two freshly consed lists: a list containing the first
;;; N elements of LIST, and a list containing the rest of them. This is
;;; effectively the same as:
;;;
;;;    (VALUES (BUTLAST LIST (- (LENGTH LIST) N)
;;;            (LAST    LIST (- (LENGTH LIST) N)))
;;;
;;; but a bit more efficient, visiting each cell exactly once. If the FROM-END
;;; argument is supplied and is non-NIL, it is equivalent to:
;;;
;;;    (VALUES (BUTLAST LIST N
;;;            (LAST    LIST N))
;;;
;;; but again, probably faster. See also NSPLIT-LIST.
(defun split-list (list &optional (n 1) &key from-end)
  (declare (type list list)
           (type (integer 0) n))
  (if from-end
      (loop
         for head-cursor on list
         for tail-cursor on (nthcdr n list)
         collecting (car head-cursor) into first-list
         when (not (consp (cdr tail-cursor)))
         do (return (values first-list
                            (copy-list (cdr head-cursor)))))
      (loop
         for head-cursor on list
         for i from 1 to n
         collecting (car head-cursor) into first-list
         finally (return (values first-list
                                 (copy-list head-cursor))))))

;;; The mutating, non-consing version of SPLIT-LIST.
(defun nsplit-list (list &optional (n 1) &key from-end)
  (declare (type list list)
           (type (integer 0) n))
  (labels ((split-front (list n) ;; Break off and return the second list
             (if (zerop n)
                 list
                 (exchangef (cdr (nthcdr (1- n) list)) nil)))
           (split-back (list n) ;; Same
             (loop
                for head-cursor on list
                for tail-cursor on (nthcdr n list)
                when (not (consp (cdr tail-cursor)))
                do (return (exchangef (cdr head-cursor) nil)))))
    (let ((second-list
           (if from-end
               (split-back list n)
               (split-front list n))))
      (values (if (eq list second-list) nil list)
              second-list))))
