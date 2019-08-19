;(defpackage :auto-tr
;  (:use :cl))         ; keep namespace separate from ccglab, which is in :cl-user -hhcb 

;(in-package :auto-tr)

(defccglab *VERBS-IN-GRAMMAR* NIL)
(defccglab *lex-item-TEMPLATE* `((KEY nil) (PHON nil) (MORPH nil)
		      (SYN nil)
		      (SEM (LAM P (P X))) (PARAM 1.0)
		      (TAG nil)))
(defccglab *lex-rule-TEMPLATE* `((KEY nil) (INSYN NIL)(INSEM LF)
					(OUTSYN NIL)
					(OUTSEM (LAM LF (LAM P (P LF))))  ; all auto-generated tr rules use these var names--no need for semantic unification
					(INDEX NIL)
					(PARAM 1.0)))  ;; may be different if an .ind file is compiled --hhcb
(defccglab *SYNS* NIL)
(defccglab *LAST-KEY-ID* NIL)
(defccglab *ARGS* NIL)
;(defccglab *MORPHS* '(TVING>)) ;now is given as an arg to main
(defccglab *RAISED-LEX-RULES* NIL)
(defccglab *RAISED-LEX-ITEMS* NIL)
;--------get methods----------;

(defun get-morph (v)
  (second (assoc 'MORPH v)))

(defun get-phon (v)
  (second (assoc 'PHON v)))

(defun get-modal-of-dir (l)
	"if it is a complex cat, return the direction's modal if there is any"
	(assoc 'MODAL l))

(defun get-dir (l)
	"get the direction if it is a complex cat"
	(assoc 'DIR l))

(defun get-last-key-id (l)
	"latest key id in the structure---no guarantee that .ded file is ordered by key; find the max"
	(setf *LAST-KEY-ID* -1) ; no negatives in translation from .ccg to .ded
	(dolist (e l)
		(if (< *LAST-KEY-ID* (second (assoc 'KEY e)))
		  (setf *LAST-KEY-ID* (second (assoc 'KEY e))))))

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

(defun load-gram (path_to_ded)
  "load the ded file from a path"
;  (in-package :cl-user) ; to load into *ccg-grammar* there
  (load path_to_ded)
;  (in-package :auto-tr))
)

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
	(setf cl-user::*ccg-grammar* (append cl-user::*ccg-grammar* (reverse *RAISED-LEX-RULES*)))
	(format t "Type-raising rules added at the end of *ccg-grammar*~%"))

(defun save-compile (fn)
  (add-tr-to-grammar)
  (save-grammar fn)
  (format t "~%saved."))

;---------------------------------------------------------------
;------------to create lex-rule entries-------------------------
;---------------------------------------------------------------

(defun compile-tr (arg morphs) 
  (setf *RAISED-LEX-RULES* NIL) ;set to default
  (setf *VERBS-IN-GRAMMAR* NIL)
  (load-gram arg)  ; ded and ind have same format--changed to load-gram
  (find-morph-v cl-user::*ccg-grammar* morphs)
  (get-last-key-id cl-user::*ccg-grammar*)
  (dolist (v-entry *VERBS-IN-GRAMMAR*)
    (type-raise (second (assoc 'SYN v-entry)))
    (loop while (not (equal 0 (length *SYNS*)))
	do (let ((temp (copy-alist *lex-rule-TEMPLATE*)))
	     (set-insyn temp (pop *ARGS*))   ; don't we need to initialize *ARGS* and *SYNS* to nil before the loop?
	     (set-outsyn temp (pop *SYNS*))
	     (set-key temp (get-next-key-id))
	     (set-index temp (gensym "auto-tr-"))
	     (push temp *RAISED-LEX-RULES*))))
  (reverse *RAISED-LEX-RULES*))

(defun compile-and-subsume-tr (arg morphs)
  "first finds all rules, then reduces the rule set to MGUs of pairs iteratively.
  We use hashtables to be compatible with MGU function cat-match---and for efficieny."
  (hash-tr (compile-tr (arg morphs)))
  (subsume-tr))

(defun debug-tr (arg morphs) ;to simulate how the work flow looks like
  (setq *RAISED-LEX-ITEMS* NIL) ;set to default
  (setq *VERBS-IN-GRAMMAR* NIL)
  (load-gram arg)
  (find-morph-v cl-user::*ccg-grammar* morphs)
  (get-last-key-id cl-user::*ccg-grammar*)
  (dolist (keys *VERBS-IN-GRAMMAR*)
    (dolist (cats-in-keys keys)
      (if  (equal 'SYN (first cats-in-keys)) 
	(type-raise (second cats-in-keys))))
    (loop while (not (equal 0 (length *SYNS*)))
	  do (let ((temp (copy-alist *lex-item-TEMPLATE*)))
	       (set-morph temp (get-morph keys))
	       (set-phon temp (get-phon keys))
	       (set-syn temp (pop *SYNS*)) ;pop *syns* until empty
	       (set-sem temp (pop *ARGS*))
	       (set-key temp (get-next-key-id))
	       (setf *RAISED-LEX-ITEMS* (append *RAISED-LEX-ITEMS* (wrap temp))))))
  (write-to-file "raised-lex-items.ded" *RAISED-LEX-ITEMS*))

;(in-package :cl-user) ; get out of package after load
