(defun remove-extension-from-file (file)
  (loop for ind from 0 to (1- (length file))
       with comma-index = (length file)
     do (when (char-equal #\. (aref file ind))
          (setq comma-index ind))
       finally (return (subseq file 0 comma-index))))

(defun local-compile ()
  (if (/= (length *posix-argv*) 3)
      (format
       *error-output* "~A~&~A~&~A~&"
       "Usage: sbcl-compile [-f|-c] INFILE"
       "'-f' compiles to fasl, '-c' to executable."
       "Run this as: 'sbcl --noinform --load sbcl-compile.lisp --quit ARGS'")
      (cond ((string= (second *posix-argv*) "-f")
             (compile-file (third *posix-argv*)))
            ((string= (second *posix-argv*) "-c")
             (load (third *posix-argv*))
             (save-lisp-and-die
              (remove-extension-from-file (third *posix-argv*))
              :toplevel 'main
              :executable t
              :purify t))
            (t (format *error-output* "~A~&"
                       "control argument should be -c or -f")))))
