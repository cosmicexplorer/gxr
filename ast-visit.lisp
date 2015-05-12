(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'sb-alien::define-alien-callback))

(load-shared-object "./walk-ast.so")

(define-alien-type CXChildVisitResult int)
(define-alien-type CXCursorKind int)

(defconstant +CXCursor-data-len+ 3)
(define-alien-type CXCursor
    (struct CXCursor
            (kind int)                  ; enum, actually
            (xdata int)
            ;; NB: magic number used here because of macro annoyance
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

(defvar *cursor-stack* nil)

(defun c-array-to-lisp-vector (dims c-arr)
  (loop for x from 0 to (1- dims)
     with vt = (make-array 3)
     do (setf (aref vt x) (deref c-arr x))
     finally (return vt)))

(defvar *prev-cursor* nil)
(defvar *prev-parent* nil)

(defun make-cursor-plist (cx-cur)
  (list :kind (slot cx-cur 'kind)
        :xdata (slot cx-cur 'xdata)
        :data (c-array-to-lisp-vector +CXCursor-data-len+
                                      (slot cx-cur 'data))))

(define-alien-callback test-callback int
    ((current (* CXCursor))
     (parent (* CXCursor)))
  (let ((cur-obj (make-cursor-plist current))
        (parent-obj (if (null-alien parent) nil
                        (make-cursor-plist parent))))
    ;; this is causing memory errors lol
    ;; (setf *cursor-stack* (cons cur-obj *cursor-stack*))
    ;; (if parent-obj
    ;;     (setf *prev-parent* *prev-cursor*)
    ;;     (setf *prev-parent* parent-obj))
    ;; (setf *prev-cursor* cur-obj)
    (format *standard-output* "~A:~A~&"
            (getf cur-obj :kind)
            (if parent-obj (getf parent-obj :kind) "-")))
  RECURSE)

(defun main ()
  (visit_ast (second *posix-argv*) nil 0 test-callback)
  (format *error-output* "~A~&" (length *cursor-stack*)))
