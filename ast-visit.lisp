(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'sb-alien::define-alien-callback))

(load-shared-object "./walk-ast.so")

(define-alien-type CXChildVisitResult int)
(define-alien-type CXCursorKind int)
(define-alien-type CXCursor
    (struct CXCursor
            (kind int)                  ; enum, actually
            (xdata int)
            (data (array (* t) 3))))
(define-alien-type CXClientData (* t))
(define-alien-type CXChildVisitResult int)

(define-alien-variable "RECURSE" CXChildVisitResult)
(define-alien-variable "CONTINUE" CXChildVisitResult)
(define-alien-variable "BREAK" CXChildVisitResult)

(define-alien-routine visit_ast int
  (infile c-string)
  (clangArgs (* c-string))
  (numArgs int)
  (visit_ptr_arg
   (function CXChildVisitResult (* CXCursor) (* CXCursor))))

(define-alien-callback test-callback int
    ((c-a (* CXCursor))
     (c-b (* CXCursor)))
  (format *standard-output* "~A:~A~&" (slot c-a 'xdata) (slot c-b 'xdata))
  RECURSE)

(defun main ()
  (visit_ast (second *posix-argv*) nil 0 test-callback))
