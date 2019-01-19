(defparameter VERBS-IN-GRAMMAR NIL)

(defun var-get-morph (v)
  (second (assoc 'morph v)))


(defun load_ded (path_to_ded)
	(load path_to_ded))

(defun find_morph_v (ccg-grammar)
	(dolist (entry ccg-grammar)
		(if (equal 'V (var-get-morph entry))
				(push entry VERBS-IN-GRAMMAR)
		)))


(defun wrap (x)
  "to wrap code in parentheses"
  (list x))


;below is for a quick environment setting to test and develop
(load_ded "~/Desktop/g1.ded")
(find_morph_v *ccg-grammar*)
(defvar test '((((BCAT S) (FEATS NIL)) (DIR BS) (MODAL STAR) ((BCAT NP) (FEATS NIL)))
 (DIR FS) (MODAL STAR) ((BCAT NP1) (FEATS NIL)) (DIR FS) (MODAL STAR)
 ((BCAT NP2) (FEATS NIL))))

;type raise to the rightmost argument
(setq reversed_test (reverse test))

;if there are still arguments --> length > 3
(pop reversed_test) ;this is the cat to be raised
(pop reversed_test) ;modal of the direction
(pop reversed_test) ;direction of the cat to be type-raised

if(;third pop's return value dir bs)
(append (wrap (reverse reversed_test)) '(DIR FS) (wrap test))
else
(append (wrap (reverse reversed_test)) '(DIR BS) (wrap test))



(defun raise_verbs )