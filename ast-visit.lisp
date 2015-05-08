(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi)
  (use-package 'cffi))

(define-foreign-library walk-ast
  (t (:default "walk-ast")))

(pushnew #P"./" *foreign-library-directories* :test #'equal)

(use-foreign-library walk-ast)

(defctype CXCursorKind :int)
(defcstruct CXCursor
  (kind CXCursorKind)
  (xdata :int)
  (data :pointer))
(defctype CXClientData :pointer)
(defctype CXChildVisitResult :int)

(defcfun "visit_ast" :int
  (infile :pointer) (clangArgs :pointer) (numArgs :int)
  (visit_ptr_arg :pointer))

(defcallback visit-call CXChildVisitResult
    ((cursor (:pointer (:struct CXCursor)))
     (parent (:pointer (:struct CXCursor))))
  (format t "~A~&"
          (list
           (foreign-slot-value cursor '(:struct CXCursor) 'xdata)
           (foreign-slot-value parent '(:struct CXCursor) 'xdata)))
  0)

;; (defcstruct test-struct
;;   (a :int))
;; (defcfun "test_struct_fun" :void (c :int) (fun :pointer))
;; (defcallback fun-test :void ((ts (:pointer (:struct test-struct))))
;;   (format t "~A~&"
;;           (foreign-slot-value ts '(:struct test-struct) 'a)))

(defun main ()
  (with-foreign-string (in-file "test/hello.cpp")
    (format *standard-output* "~A~&"
            (visit-ast in-file (null-pointer) 0 (callback visit-call)))))
