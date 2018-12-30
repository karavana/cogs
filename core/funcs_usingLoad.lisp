(defparameter VERBS-IN-GRAMMAR NIL)

(defun load_ded (path_to_ded)
	(load path_to_ded))

(defun find_morph_v (ccg-grammar)
	(dolist (entry ccg-grammar)
		(if (equal 'V (second (assoc 'morph entry)))
				(push entry VERBS-IN-GRAMMAR)
		)))

(defun raise_verbs )