(asdf:operate 'asdf:load-op 'cffi)
(use-package 'cffi)

(define-foreign-library walk-ast
  (t (:default "walk-ast")))

(pushnew #P"./" *foreign-library-directories* :test #'equal)

(use-foreign-library walk-ast)

(defcfun "visit_ast" :int (str :pointer))

(defun main ()
  (with-foreign-string (str "assdfs")
    (format *standard-output* "~A~&" (visit-ast str))))
