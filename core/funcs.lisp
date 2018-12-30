(defparameter *mylist* '(1))


(defun substringp (needle haystack &key (test 'char=))
  (search (string needle)
          (string haystack)
          :test test))

(defun string-to-list (s)
  (assert (stringp s) (s) "~s :error, not a string")
  (coerce s 'list)
)



(defun findKeyID (str) ;finds the value of the unique key of the phon
  (defvar key_index NIL)
  
  (if (setq key_index (substringp "KEY" str))
          
          (setq key_index (+ key_index 4))
          (setq key_id    (parse-integer  (subseq str key_index (substringp ")" str)))))
)                  ;substringp finds the index of "K", that's why we start from index of K plus 4



(defun findPhon (str)
  (defvar afterphon (subseq str (+  5)))
  (defvar phon_index NIL)
  
  (if (setq phon_index (substringp "PHON" str))
          (setq afterphon (subseq str (+ phon_index 5)))
          (setq phon
            (subseq afterphon 0 (substringp ")" afterphon)))) ;substringp finds the index of "P", that's why we start from index of P plus 5
)




(defun open_ded (path)
 (defvar last_id 0)
 (let ((in (open path :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
        while line 
        	do 
        		(if (setq key_id (findkeyid line))
                (setq last_id key_id))

            (if(substringp "MORPH V" line)
        				(setf (get 'key 'id )last_id)
                (setf (get 'key 'phon ) (findPhon line))
	        			;(symbol-plist 'key)))

            )

  )
  (close in)))
)



;"/home/oguz/core/ecsotm.ded"



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