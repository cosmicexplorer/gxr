;;; slurp in-stream real fast

(defmacro check-types (checker-fun exception objs)
  (cons 'progn
        (mapcar
         (lambda (obj)
           `(unless (funcall ,checker-fun ,obj)
              (throw ,exception ,obj)))
         objs)))

(defun check-output (in-file out-file out-obj-file)
  (check-types #'stringp 'invalid-file-name (in-file out-file out-obj-file))
  (let ((in-stream (open in-file :direction :input))
        (out-stream (open out-file
                          :direction :output
                          :if-exists :overwrite
                          :if-does-not-exist :create
                          ))
        (out-obj-stream (open out-obj-file
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)))
    (write-sequence
     (write-to-string
      (read-from-string
       (let ((seq (make-string (file-length in-stream))))
         (read-sequence seq in-stream)
         (write-sequence seq out-stream)
         seq)))
     out-obj-stream)
    (close in-stream)
    (close out-stream)
    (close out-obj-stream)))
