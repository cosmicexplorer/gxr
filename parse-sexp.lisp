(defmacro check-types (checker-fun exception &rest objs)
  (cons 'progn
        (mapcar
         (lambda (obj)
           `(unless (funcall ,checker-fun ,obj)
              (throw ,exception ,obj)))
         objs)))

;;; get instream into a string, then spit it out into an object
;;; TODO: read stdin and allow streaming! (file-length) doesn't work on stdin
(defun check-output (instream outstream)
  (check-types #'streamp 'invalid-file-name instream outstream)
  (write-sequence
   (write-to-string
    (let ((*readtable* (copy-readtable nil)))
      (setf (readtable-case *readtable*) :preserve)
      (read-from-string
       (let ((seq (make-string (file-length instream))))
         (read-sequence seq instream)
         seq))))
   outstream))

(defun main ()
  (if (or (< (length *posix-argv*) 2)
          (> (length *posix-argv*) 3))
      (format *error-output* "~A~&"
              "Usage: parse-sexp INFILE [OUTFILE]")
      (let ((infile (second *posix-argv*))
            (outfile (if (> (length *posix-argv*) 2)
                         (third *posix-argv*)
                         "-")))
        (let ((instream
               (if (string= infile "-")
                   *standard-input*
                   (open infile :direction :input)))
              (outstream
               (if (string= outfile "-")
                   *standard-output*
                   (open outfile :direction :output
                         :if-exists :overwrite
                         :if-does-not-exist :create))))
          (check-output instream outstream)
          (unless (eq instream *standard-input*)
            (close instream))
          (unless (eq outstream *standard-output*)
            (close outstream))))))
