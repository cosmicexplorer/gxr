;;; TODO: begin putting out html/js! all we need to do is find where each
;;; syntactic element maps to in the source, and only for the objects within the
;;; currently active file (which are small compared to every include ever)

(define-condition invalid-parse (error)
  ((text :initarg :text :reader text)))

(defvar default-block-size 1024)
(defvar num-states 2)

(defun make-empty-char-seq ()
  (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))

(defun push-token-onto-stack (root-stack cur-token c state-list)
  (vector-push-extend c cur-token)
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (setf (car root-stack) (cons (read-from-string cur-token) (car root-stack))
          state-list '(:pipe nil :quote nil))))

(defun parse-large-read-obj (str-to-parse state-list root-stack)
  ;; :pipe can be for either '|asdf| syms or :|asdf| atoms
  (unless state-list (setq state-list (list :pipe nil :quote nil)))
  (loop for ind from 0 upto (1- (length str-to-parse))
     with last-ind = 0 and cur-token = (make-empty-char-seq)
     with prev-is-backslash = nil
     do (let ((c (aref str-to-parse ind)))
          (if (char-equal c #\\)
              (setq prev-is-backslash (not prev-is-backslash))
              (cond
                ((and (or (char-equal c #\') (char-equal c #\:))
                      (not (or (getf state-list :pipe)
                               (getf state-list :quote))))
                 (vector-push-extend c cur-token))
                ((char-equal c #\|)
                 (if (getf state-list :pipe)
                     (progn
                       (push-token-onto-stack root-stack cur-token c state-list)
                       (setq cur-token (make-empty-char-seq)
                             last-ind ind))
                     (progn
                       (vector-push-extend c cur-token)
                       (unless (getf state-list :quote)
                         ;; always going to be directly after ' or :
                         (setf (getf state-list :pipe) t)))))
                ((char-equal c #\")
                 (if (getf state-list :quote)
                     (progn
                       (push-token-onto-stack root-stack cur-token c state-list)
                       (setq cur-token (make-empty-char-seq)
                             last-ind ind))
                     (progn
                       (vector-push-extend c cur-token)
                       (unless (getf state-list :pipe)
                         (setf (getf state-list :quote) t)))))
                ((and (char-equal c #\()
                      (not (or (getf state-list :pipe)
                               (getf state-list :quote))))
                 (unless (= (length cur-token) 0)
                   (error 'invalid-parse :text "cur-token should be 0 here!"))
                 (setq root-stack (cons nil root-stack)))
                ((and (char-equal c #\))
                      (not (or (getf state-list :pipe)
                               (getf state-list :quote))))
                 (unless (= (length cur-token) 0)
                   (error 'invalid-parse :text "cur-token should be 0 here!"))
                 (setq root-stack (cdr root-stack)
                       last-ind ind))
                ;; space or newline
                ((or (char-equal c #\ )
                     (char-equal c #\linefeed))
                 (when (or (getf state-list :pipe) (getf state-list :quote))
                   (vector-push-extend c cur-token)))
                (t (vector-push-extend c cur-token)))))
     finally (return (list root-stack
                           (subseq str-to-parse (1+ last-ind))
                           state-list))))

(defun read-large-object-without-error-checking (instream &optional block-size)
  "An iterative approach to lisp's (read) command which avoids recursion at the
cost of useful error checking. Reads from INSTREAM in blocks of size BLOCK-SIZE,
or 1024 if BLOCK-SIZE is not given."
  (let* ((array-len (if block-size block-size default-block-size))
         (seq (make-array array-len :element-type 'character :adjustable t
                          :fill-pointer array-len)))
    (setf (fill-pointer seq) (read-sequence seq instream))
    (loop with leftover-str = ""
       with state-list = nil and root-stack = nil
       while (not (zerop (fill-pointer seq)))
       do (destructuring-bind (obj rest-of-str ret-state-list)
              (parse-large-read-obj
               (concatenate 'string leftover-str seq)
               state-list root-stack)
            (setq leftover-str rest-of-str
                  state-list ret-state-list
                  root-stack obj))
       finally (return (destructuring-bind (obj rest-of-str ret-state-list)
                           (parse-large-read-obj
                            leftover-str state-list root-stack)
                         (if (or (> (length rest-of-str) 0)
                                 ;; FIX THE BELOW
                                 (< (count nil ret-state-list) num-states))
                             (error 'invalid-parse
                                    :text (list rest-of-str ret-state-list))
                             obj))))))

(defmacro check-types (checker-fun exception &rest objs)
  (cons
   'progn
   (mapcar
    (lambda (obj)
      `(unless (funcall ,checker-fun ,obj)
         (error ,exception :text ,obj)))
    objs)))

(define-condition invalid-file-name (error)
  ((text :initarg :text :reader text)))

;;; get instream into a string, then spit it out into an object
(defun read-output-into-obj-then-dump (instream outstream)
  (check-types #'streamp 'invalid-file-name instream outstream)
  (write-sequence
   (let ((obj
          (let ((*readtable* (copy-readtable nil)))
            (setf (readtable-case *readtable*) :preserve)
            (read instream))))
     (format *error-output* "~A ~A ~A~&"
             "the root of the AST has" (length (getf obj :|children|))
             "children")
     (write-to-string obj))
   outstream))

(define-condition invalid-standard-stream (error)
  ((text :initarg :text :reader text)))

(defun if-path-is-dash-then-standard-stream (filepath &rest args-to-open)
  (if (and (stringp filepath) (string= filepath "-"))
      (let ((direction (getf args-to-open :direction)))
        (cond ((or (eq direction :input)
                   (eq direction nil))
               *standard-input*)
              ((eq direction :output)
               *standard-output*)
              ;; can't use stdin/stdout/stderr as :io or :probe
              (t (error 'invalid-standard-stream :text
                        (concatenate 'string "the r/w specifier "
                                     (princ-to-string direction)
                                     " is not valid for a standard stream "
                                     "as specified by the filepath \"-\"")))))
      (apply #'open (cons filepath args-to-open))))

(defmacro with-open-files-as-type (filenames-types &rest body)
  `(let ,(cons '(retval nil)
               (mapcar
                (lambda (file-str-pair)
                  `(,(first file-str-pair)
                     (if-path-is-dash-then-standard-stream
                      ,@(cdr file-str-pair))))
                filenames-types))
     (unwind-protect (setq retval (progn ,@body))
       (mapcar (lambda (file-str-pair) (close (first file-str-pair)))
               (remove-if
                (lambda (pair) (and (stringp (second pair))
                                    (string= (second pair) "-")))
                ,(cons 'list (mapcar (lambda (item) (cons 'list item))
                                     filenames-types)))))
     retval))

(defun main ()
  (handler-case
      (if (or (< (length *posix-argv*) 2)
              (> (length *posix-argv*) 3))
          (error 'invalid-standard-stream :text
                 "Usage: parse-sexp PREPROC-INFILE AST-INFILE [OUTFILE]")
          (let ((ast-infile (second *posix-argv*))
                (outfile (if (> (length *posix-argv*) 2)
                             (third *posix-argv*) "-")))
            (with-open-files-as-type
                ((ast-instream ast-infile :direction :input)
                 (outstream outfile :direction :output
                            :if-exists :overwrite
                            :if-does-not-exist :create))
              (read-output-into-obj-then-dump ast-instream outstream))))
    (invalid-standard-stream (ex)
      (format *error-output* "~A~&" (text ex))
      (exit :abort t))))
