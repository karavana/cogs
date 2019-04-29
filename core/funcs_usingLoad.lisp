(defparameter *VERBS-IN-GRAMMAR* NIL)
(defparameter *TEMPLATE* `((KEY nil) (PHON nil) (MORPH nil)
		      (SYN nil)
		      (SEM nil) (PARAM 1.0)
		      (INDEX nil)
		      (TAG nil)))
(defparameter *SYNS* NIL)
(defparameter *LAST-KEY-ID* NIL)
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
	"replaces the X in the structure (SEM X)"
	(rplacd (assoc 'sem l) (wrap X)))
;------------end of set methods-----------------;


(defun reduce-parenthesis (l)
	"get rid of the list's extra parentheses"
	(reduce #'union l))

(defun load-ded (path_to_ded)
	"load the ded file from a path"
	(load path_to_ded))

(defun find-morph-v (ccg-grammar)
	"find verb morphemes"
	(dolist (entry ccg-grammar)
		(if (equal 'V (get-morph entry)) ;todo: get V-like filters from a list
				(push entry *VERBS-IN-GRAMMAR*)
		)))


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
		(if (not (is-complex-cat cat)) 
			(return-from type-raise))
		(let ((dir-of-cat (get-dir cat))
				 (modal-of-dir (get-modal-of-dir cat)))
				 (if (equal '(DIR BS) dir-of-cat)
					(push (append (wrap (car cat)) (wrap '(DIR FS)) (wrap modal-of-dir) (wrap cat)) *SYNS*) 
					(push (append (wrap (car cat)) (wrap '(DIR BS)) (wrap modal-of-dir) (wrap cat)) *SYNS*)) 
				 (type-raise (car cat))))


(defun __main__ (arg) ;to simulate how the work flow looks like
	(progn
		(load-ded arg)
		(find-morph-v *ccg-grammar*)
		(get-last-key-id *ccg-grammar*)
		(dolist (keys *VERBS-IN-GRAMMAR*)
			(dolist (cats-in-keys keys)
				(if  (equal 'SYN (first cats-in-keys)) 
					(type-raise (second cats-in-keys))))
			(loop while (not (equal 0 (length *SYNS*)))
				do(let ((temp (copy-alist *TEMPLATE*)))
				(set-morph temp (get-morph keys))
				(set-phon temp (get-phon keys))
				(set-syn temp (pop *syns*)) ;pop *syns* until empty
				(set-key temp (get-next-key-id))
				(append *ccg-grammar* (wrap temp)))))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;test start;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;below is a quick environment setting to test and develop
	;(load-ded "~/Desktop/g1.ded")
	;(find-morph-v *ccg-grammar*)
	(defvar dummy `((KEY nil) (PHON nil) (MORPH nil)
		      (SYN nil)
		      (SEM nil) (PARAM 1.0)
		      (INDEX nil)
		      (TAG nil)))

;test SYN

	(defvar test3 '((((BCAT S) (FEATS NIL)) (DIR BS) (MODAL STAR) ((BCAT NP) (FEATS NIL)))
	 (DIR FS) (MODAL STAR) ((BCAT NP1) (FEATS NIL)) (DIR FS) (MODAL STAR)
	 ((BCAT NP2) (FEATS NIL))))
	(defvar test2 '(((BCAT VP)(FEATS NIL)) (MODAL STAR) (DIR BS)
	(((BCAT S)(FEATS NIL)) (MODAL STAR) (DIR FS) ((BCAT NP)(FEATS NIL)))))
	(defvar test '(((((BCAT S) (FEATS NIL)) (DIR BS) (MODAL STAR) ((BCAT NP) (FEATS NIL)))
	 (DIR FS) (MODAL STAR) ((BCAT NP1) (FEATS NIL))) (DIR FS) (MODAL STAR)
	 ((BCAT NP2) (FEATS NIL))))

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;test END;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
