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
;;; TODO: read stdin and allow streaming! (file-length) doesn't work on stdin
(defun read-output-into-obj-then-dump (instream outstream)
  (check-types #'streamp 'invalid-file-name instream outstream)
  (write-sequence
   (write-to-string
    (let ((*readtable* (copy-readtable nil)))
      (setf (readtable-case *readtable*) :preserve)
      (read-from-string
       (with-output-to-string (stringstream)
         (let ((block-size 1024))
           (loop with block = (make-array block-size
                                          :element-type 'character
                                          :adjustable t
                                          :fill-pointer block-size)
              while (not (zerop (fill-pointer block)))
              do (progn
                   (setf (fill-pointer block) (read-sequence block instream))
                   (write-sequence block stringstream))))))))
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
          (read-output-into-obj-then-dump instream outstream)
          (close instream)
          (close outstream)))))
