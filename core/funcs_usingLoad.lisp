(defparameter *VERBS-IN-GRAMMAR* NIL)
(defparameter *TEMPLATE* `((KEY nil) (PHON nil) (MORPH nil)
		      (SYN nil)
		      (SEM nil) (PARAM 1.0)
		      (INDEX nil)
		      (TAG nil)))

;--------get methods----------;

(defun get-morph (v)
  (second (assoc 'MORPH v)))

(defun get-syn (l)
	"get syn of the grammar"
	(dolist (keys l)
		(dolist (catz keys)
			(if (equal 'SYN (first catz)) (print catz)))))

(defun get-next-arg (l)
	"get next argument to type-raise"
	(car (reverse l)))

(defun get-modal-of-dir (l)
	"if it is a complex cat, return the direction's modal if there is any"
	(assoc 'MODAL l))

(defun get-dir (l)
	"get the direction if it is a complex cat"
	(assoc 'DIR l))

;---------end of get methods-----------------------;

;-------------set methods-------------;
(defun set-syn (l X)
	"replaces the X in the structure (SYN X)"
	(rplacd (assoc 'syn l) (list X)))

(defun set-key (l X)
	"replaces the X in the structure (KEY X)"
	(rplacd (assoc 'key l) (list X)))

(defun set-phon (l X)
	"replaces the X in the structure (PHON X)"
	(rplacd (assoc 'phon l) (list X)))

(defun set-morph (l X)
	"replaces the X in the structure (MORPH X)"
	(rplacd (assoc 'morph l) (list X)))

(defun set-sem (l X)
	"replaces the X in the structure (SEM X)"
	(rplacd (assoc 'sem l) (list X)))
;------------end of set methods-----------------;


(defun reduce-parenthesis (l)
	"get rid of the list's extra parentheses"
	(reduce #'union l))

(defun load_ded (path_to_ded)
	"load the ded file from a path"
	(load path_to_ded))

(defun find_morph_v (ccg-grammar)
	"find verb morphemes"
	(dolist (entry ccg-grammar)
		(if (equal 'V (get-morph entry))
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
					(append (wrap (reverse (car cat))) (wrap '(DIR FS)) (wrap modal-of-dir) (wrap cat))  
					(append (wrap (reverse (car cat))) (wrap '(DIR BS)) (wrap modal-of-dir) (wrap cat))) ;TODO add this to the end of the .ded file
				 (type-raise (car cat))))









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;OLD CODE START;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;below is a quick environment setting to test and develop
	;(load_ded "~/Desktop/g1.ded")
	;(find_morph_v *ccg-grammar*)
	(defvar dummy `((KEY nil) (PHON nil) (MORPH nil)
		      (SYN nil)
		      (SEM nil) (PARAM 1.0)
		      (INDEX nil)
		      (TAG nil)))

	(defvar test3 '((((BCAT S) (FEATS NIL)) (DIR BS) (MODAL STAR) ((BCAT NP) (FEATS NIL)))
	 (DIR FS) (MODAL STAR) ((BCAT NP1) (FEATS NIL)) (DIR FS) (MODAL STAR)
	 ((BCAT NP2) (FEATS NIL))))
	(defvar test2 '(((BCAT VP)(FEATS NIL)) (MODAL STAR) (DIR BS)
	(((BCAT S)(FEATS NIL)) (MODAL STAR) (DIR FS) ((BCAT NP)(FEATS NIL)))))
	(defvar test '(((((BCAT S) (FEATS NIL)) (DIR BS) (MODAL STAR) ((BCAT NP) (FEATS NIL)))
	 (DIR FS) (MODAL STAR) ((BCAT NP1) (FEATS NIL))) (DIR FS) (MODAL STAR)
	 ((BCAT NP2) (FEATS NIL))))

	;type raise to the rightmost argument

	(defvar reversed_test nil)
	(defvar dir-of-cat nil)
	(defvar modal-of-dir nil)
	(setq reversed_test (reverse test))

	(loop is-complex-cat) ;argument left
		(do
			(pop reversed_test) ;this is the cat to be raised (car reversed_test)
			(setq modal-of-dir (pop reversed_test) ;modal of the direction (cadr reversed_test)
			(setq dir-of-cat (pop reversed_test)) ;direction of the cat to be type-raised (caddr reversed_test)


			(if (equal '(DIR BS) dir-of-cat)
				(append (wrap (reverse reversed_test)) (wrap '(DIR FS)) (wrap modal-of-dir) (wrap test))
				(append (wrap (reverse reversed_test)) (wrap '(DIR BS)) (wrap modal-of-dir) (wrap test)))

			(if (not (is-complex-cat reversed_test))
				(progn
					(setq test (reduce-parenthesis reversed_test))
					(setq reversed_test (reverse test)))
				
				(setq test (reverse reversed_test)))))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;OLD CODE END;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
