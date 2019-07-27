(defparameter *VERBS-IN-GRAMMAR* NIL)
(defparameter *lex-item-TEMPLATE* `((KEY nil) (PHON nil) (MORPH nil)
		      (SYN nil)
		      (SEM (LAM P (P X))) (PARAM 1.0)
		      (TAG nil)))
(defparameter *lex-rule-TEMPLATE* `((KEY nil) (INSYN NIL)(INSEM LF)
					(OUTSYN NIL)
					(OUTSEM (LAM LF (LAM P (P LF))))
					(INDEX NIL)
					(PARAM 1.0)))
(defparameter *SYNS* NIL)
(defparameter *LAST-KEY-ID* NIL)
(defparameter *ARGS* NIL)
;(defparameter *MORPHS* '(TVING>)) ;now is given as an arg to main
(defparameter *RAISED-LEX-RULES* NIL)
(defparameter *RAISED-LEX-ITEMS* NIL)
;--------get methods----------;

(defun get-morph (v)
  (second (assoc 'MORPH v)))

(defun get-phon (v)
  (second (assoc 'PHON v)))

(defun get-syn (l) ;only to print the SYN's of the entries of .ded file
	"get syn of the grammar"
	(dolist (keys l)
		(dolist (catz keys)
			(if (equal 'SYN (first catz)) (print catz)))))

(defun get-modal-of-dir (l)
	"if it is a complex cat, return the direction's modal if there is any"
	(assoc 'MODAL l))

(defun get-dir (l)
	"get the direction if it is a complex cat"
	(assoc 'DIR l))

(defun get-last-key-id (l)
	"latest key id in the structure"
	(dolist (keys (last l))
		(setf *LAST-KEY-ID* (second (assoc 'KEY keys)))))

(defun get-next-key-id ()
	"increment the last id in the structure and return it"
	(setf *LAST-KEY-ID* (+ 1 *LAST-KEY-ID*)))
	
;---------end of get methods-----------------------;

;-------------set methods-------------;
(defun set-syn (l X)
	"replaces the X in the structure (SYN X)"
	(rplacd (assoc 'syn l) (wrap X)))

(defun set-key (l X)
	"replaces the X in the structure (KEY X)"
	(rplacd (assoc 'key l) (wrap X)))

(defun set-phon (l X)
	"replaces the X in the structure (PHON X)"
	(rplacd (assoc 'phon l) (wrap X)))

(defun set-morph (l X)
	"replaces the X in the structure (MORPH X)"
	(rplacd (assoc 'morph l) (wrap X)))

(defun set-sem (l X)
	"replaces the X in the structure (SEM (LAMP P (P X)))"
	(rplacd (car (reverse (second (assoc 'sem l)))) (wrap X)))

(defun set-index (l X)
	"replaces the X in the structure (INDEX X)"
	(rplacd (assoc 'index l) (wrap X)))

(defun set-insyn (l X)
	"replaces the X in the structure (INSYN X)"
	(rplacd (assoc 'insyn l) (wrap X)))

(defun set-outsyn (l X)
	"replaces the X in the structure (OUTSYN X)"
	(rplacd (assoc 'outsyn l) (wrap X)))
;------------end of set methods-----------------;


(defun reduce-parenthesis (l)
	"get rid of the list's extra parentheses"
	(reduce #'union l))

(defun load-ded (path_to_ded)
	"load the ded file from a path"
	(load path_to_ded))

(defun find-morph-v (ccg-grammar morphs)
	"find verb morphemes"
	(dolist (entry ccg-grammar)
		(dolist (morph morphs)
			(if (equal morph (get-morph entry))
				(push entry *VERBS-IN-GRAMMAR*)))))


(defun wrap (x)
  "wrap code in parentheses"
  (list x))

(defun is-complex-cat (cat)
	"decide if it's a complex cat"
	(handler-case (assoc 'DIR cat)
		(error (c)
    	(format t "We caught a condition.~&")
    	(values NIL c))))
	

(defun type-raise (cat)
	"type raising operation on the given category"
		(if (not (is-complex-cat cat)) 
			(return-from type-raise))
		(let ((dir-of-cat (get-dir cat))
			 (modal-of-dir (get-modal-of-dir cat)))
			 (if (equal '(DIR BS) dir-of-cat)
				(push (append (wrap (car cat)) (wrap '(DIR FS)) (wrap modal-of-dir) (wrap cat)) *SYNS*) 
				(push (append (wrap (car cat)) (wrap '(DIR BS)) (wrap modal-of-dir) (wrap cat)) *SYNS*))
			 (push (car (reverse cat)) *ARGS*) 
			 (type-raise (car cat))))

(defun write-to-file (path file)
	"Writes the 'file' to a specified 'path'"
	(with-open-file (stream path
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
  (format stream (write-to-string file))
  (format t "File created at ~A~%" path)
  (format t "The rules also set to the global variable *RAISED-LEX-RULES*~%")))

(defun add-tr-to-grammar ()
	"add rules to the currently loaded grammar"
	(setf *ccg-grammar* (append *ccg-grammar* *RAISED-LEX-RULES*)))

(defun random-string (&optional (length 4) (alphabet "ABCDEFGHIJKLMNOPRSTUVYZWX1234567890"))
  "Returns a random alphabetic string.

The returned string will contain LENGTH characters chosen from
the vector ALPHABET.
"
  (loop with id = (make-string length)
        with alphabet-length = (length alphabet)
        for i below length
        do (setf (cl:aref id i)
                 (cl:aref alphabet (random alphabet-length)))
        finally (return id)))


;---------------------------------------------------------------
;------------to create lex-rule entries-------------------------
;---------------------------------------------------------------

(defun compile-tr (arg morphs) ;to simulate how the work flow looks like
	(progn
		(load-ded arg)
		(find-morph-v *ccg-grammar* morphs)
		(get-last-key-id *ccg-grammar*)
		(dolist (keys *VERBS-IN-GRAMMAR*)
			(dolist (cats-in-keys keys)
				(if  (equal 'SYN (first cats-in-keys)) 
					(type-raise (second cats-in-keys))))
			(loop while (not (equal 0 (length *SYNS*)))
				do(let ((temp (copy-alist *lex-rule-TEMPLATE*)))
				(set-insyn temp (pop *ARGS*))
				(set-outsyn temp (pop *SYNS*))
				(set-key temp (get-next-key-id))
				(set-index temp (random-string 4))
				(setf *RAISED-LEX-RULES* (append *RAISED-LEX-RULES* (wrap temp))))))
		(write-to-file "doc/raised-lex-rules.ded" *RAISED-LEX-RULES*)))



;---------------------------------------------------------------
;------------to create lex-item entries-------------------------
;---------------------------------------------------------------

(defun debug-tr (arg morphs) ;to simulate how the work flow looks like
	(progn
		(load-ded arg)
		(find-morph-v *ccg-grammar* morphs)
		(get-last-key-id *ccg-grammar*)
		(dolist (keys *VERBS-IN-GRAMMAR*)
			(dolist (cats-in-keys keys)
				(if  (equal 'SYN (first cats-in-keys)) 
					(type-raise (second cats-in-keys))))
			(loop while (not (equal 0 (length *SYNS*)))
				do(let ((temp (copy-alist *lex-item-TEMPLATE*)))
				(set-morph temp (get-morph keys))
				(set-phon temp (get-phon keys))
				(set-syn temp (pop *SYNS*)) ;pop *syns* until empty
				(set-sem temp (pop *ARGS*))
				(set-key temp (get-next-key-id))
				(setf *RAISED-LEX-ITEMS* (append *RAISED-LEX-ITEMS* (wrap temp))))))
		(write-to-file "doc/raised-lex-items.ded" *RAISED-LEX-ITEMS*)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;test start;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;below is a quick environment setting to test and develop
	;(load-ded "~/Desktop/g1.ded")
	;(find-morph-v *ccg-grammar*)
;test SYN

	(defvar test3 '((((BCAT S) (FEATS NIL)) (DIR BS) (MODAL STAR) ((BCAT NP) (FEATS NIL)))
	 (DIR FS) (MODAL STAR) ((BCAT NP1) (FEATS NIL)) (DIR FS) (MODAL STAR)
	 ((BCAT NP2) (FEATS NIL))))
	(defvar test2 '(((BCAT VP)(FEATS NIL)) (MODAL STAR) (DIR BS)
	(((BCAT S)(FEATS NIL)) (MODAL STAR) (DIR FS) ((BCAT NP)(FEATS NIL)))))
	(defvar test '((((((BCAT S) (FEATS ((TYPE INF)))) (DIR BS) (MODAL ALL)
          ((BCAT NP) (FEATS NIL)))
         (DIR BS) (MODAL ALL) ((BCAT AUX) (FEATS ((TYPE TEMP)))))
        (DIR FS) (MODAL ALL) ((BCAT (up)) (BCONST T) (FEATS NIL)))
       (DIR FS) (MODAL ALL) ((BCAT NP) (FEATS NIL))))
	(defvar test4 '(((((BCAT S) (FEATS ((TYPE Q)))) (DIR BS) (MODAL ALL)
      ((BCAT AUX) (FEATS ((TYPE BE)))))
     (DIR FS) (MODAL ALL) ((BCAT NP) (FEATS NIL)))
    (DIR BS) (MODAL ALL) ((BCAT NP) (FEATS NIL))))

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;test END;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
