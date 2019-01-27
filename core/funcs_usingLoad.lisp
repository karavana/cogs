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
	(defvar reversed_test nil)
	(defvar dir-of-cat nil)
	(defvar modal-of-dir nil)
	(setq reversed_test (reverse test))

	(while (>= (length reversed_test) 3); need to use a real loop keyword, "while" doesnt seem to exist
		do(
			(pop reversed_test) ;this is the cat to be raised
			(setq modal-of-dir (pop reversed_test) ;modal of the direction
			(setq dir-of-cat (pop reversed_test)) ;direction of the cat to be type-raised


			(if (equal '(DIR BS) dir-of-cat)
				(append (wrap (reverse reversed_test)) (wrap '(DIR FS)) (wrap modal-of-dir) (wrap test))
				(append (wrap (reverse reversed_test)) (wrap '(DIR BS)) (wrap modal-of-dir) (wrap test)))

			(if (not (>= (length reversed_test) 3))
				(progn
					(setq test (reduce #'union reversed_test))
					(setq reversed_test (reverse test)))
				
				(setq test (reverse reversed_test))))
	)



