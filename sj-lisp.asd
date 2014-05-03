;;;; Copyright © 2014 Stuart J. Olsen. See file "LICENSE."

(defpackage :sj-lisp
  (:use :cl)
  (:export ;; helper-macros
           #:fbind
           #:with-hygienic-names
           ;; setf-macros
           #:exchangef
           #:swapf
           ;; list-operations
           #:split-list
           #:nsplit-list))

(asdf:defsystem :sj-lisp
  :description "General-purpose Common Lisp utilities"
  :author "Stuart Olsen <stuart@sj-olsen.com>"
  :components ((:file "helper-macros")
               (:file "setf-macros")
               (:file "list-operations"
                      :depends-on ("setf-macros"))))
