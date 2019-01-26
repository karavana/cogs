(defparameter VERBS-IN-GRAMMAR NIL)

(defun var-get-morph (v)
  (second (assoc 'morph v)))


(defun load_ded (path_to_ded)
	"load the ded file from a path"
	(load path_to_ded))

(defun find_morph_v (ccg-grammar)
	"find verb morphemes"
	(dolist (entry ccg-grammar)
		(if (equal 'V (var-get-morph entry))
				(push entry VERBS-IN-GRAMMAR)
		)))


(defun wrap (x)
  "wrap code in parentheses"
  (list x))



;below is a quick environment setting to test and develop
	;(load_ded "~/Desktop/g1.ded")
	;(find_morph_v *ccg-grammar*)
	(defvar test3 '((((BCAT S) (FEATS NIL)) (DIR BS) (MODAL STAR) ((BCAT NP) (FEATS NIL)))
	 (DIR FS) (MODAL STAR) ((BCAT NP1) (FEATS NIL)) (DIR FS) (MODAL STAR)
	 ((BCAT NP2) (FEATS NIL))))
	(defvar test2 '(((BCAT VP)(FEATS NIL)) (MODAL STAR) (DIR BS)
	(((BCAT S)(FEATS NIL)) (MODAL STAR) (DIR FS) ((BCAT NP)(FEATS NIL)))))
	(defvar test '(((((BCAT S) (FEATS NIL)) (DIR BS) (MODAL STAR) ((BCAT NP) (FEATS NIL)))
	 (DIR FS) (MODAL STAR) ((BCAT NP1) (FEATS NIL))) (DIR FS) (MODAL STAR)
	 ((BCAT NP2) (FEATS NIL))))

	;type raise to the rightmost argument
	(setq reversed_test (reverse test))

	(if ;there are still arguments --> length > 3 (or another check, not sure how to decide)
		(pop reversed_test) ;this is the cat to be raised
		(pop reversed_test) ;modal of the direction
		(pop reversed_test) ;direction of the cat to be type-raised


		(if;third pop's return value dir bs)
			(append (wrap (reverse reversed_test)) '(DIR FS) (wrap test)))
		
		else
			(append (wrap (reverse reversed_test)) '(DIR BS) (wrap test))

		(if !(length reversed_test > 3)
			(setq test (reduce #'union reversed_test))
			(setq reversed_test (reverse test)))
		(else
		(setq test (reverse reversed_test)))
	)



