(defparameter *mylist* '(1))


(defun substringp (needle haystack &key (test 'char=))
  (search (string needle)
          (string haystack)
          :test test))



(defun findKeyID (str) ;finds the value of the unique key of the phon
	(parse-integer (setq key_id 
						(subseq str (+ (substringp "KEY" str) 4) ;substringp finds the index of "K", that's why we start from index of K plus 4
										 (substringp ")" str)))))


(defun open_ded (path)
 (let ((in (open path :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
        while line 
        	do (setq key_id (findkeyid line)
        		(if(key_id)
        				(setf (get 'key 'phon key_id) )
	        			(symbol-plist 'key))

        		)
        			  ;collect (map *mylist* #'digit-char-p line)

    )
    (close in)))
)



;"/home/oguz/Escritorio/ecsotm.ded"



(defun string-include (string1 string2)  ;(string-include 'ghf 'dghfd) => ghf
  (let* ((string1 (string string1)) (length1 (length string1)))
    (if (zerop length1)
        nil 
        (labels ((sub (s)
                   (cond
                    ((> length1 (length s)) nil)
                    ((string= string1 s :end2 (length string1)) string1)
                    (t (sub (subseq s 1))))))
          (sub (string string2))))))