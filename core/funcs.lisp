(defun open_ded (path)
 (let ((in (open path :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
        while line do (format t "~a~%" line))
    (close in)))
)


(defun substringp (needle haystack &key (test 'char=))
  (search (string needle)
          (string haystack)
          :test test))

%"/home/oguz/Escritorio/ecsotm.ded"



(defun string-include (string1 string2)
  (let* ((string1 (string string1)) (length1 (length string1)))
    (if (zerop length1)
        nil 
        (labels ((sub (s)
                   (cond
                    ((> length1 (length s)) nil)
                    ((string= string1 s :end2 (length string1)) string1)
                    (t (sub (subseq s 1))))))
          (sub (string string2))))))