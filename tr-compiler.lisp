;;; ------------------------------------------
;;; A compiler for type-raising in CCG
;;; -implemented by Oguzhan Demir
;;;  with some contributions from Cem Bozsahin
;;; ------------------------------------------

(defccglab *ht-tr* nil) ; hash table for derived tr rules--for subsumption check after compile
                        ; key : lex rule key value: lex rule including key as hashtable
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
  (setf *ccg-grammar* (append *ccg-grammar* (reverse *RAISED-LEX-RULES*)))
  (format t "~%Type-raising rules added at the end of *ccg-grammar*"))

(defun mk-bcat (bcatht)
  (let ((feats nil)
	(bcat nil))
    (maphash #'(lambda (k v) (if (equal k 'BCAT)
			       (push (list k v) bcat)
			       (push (list k v) feats)))
	     bcatht)
    (append bcat (list (list 'FEATS feats)))))

(defun mk-nonht (ht)
  "return a list of non-hash-valued name-value pairs"
  (let ((feats nil))
    (maphash #'(lambda (k v) (if (not (hash-table-p v)) (push (list k v) feats)))
	     ht)
    (reverse feats)))

(defun mk-cat (synht &optional (res nil))
  (cond ((null synht) res)
	((basicp-hash synht) (append (mk-bcat synht) res)) ; only BCAT and other feats in same synht
	(t (append (list (mk-cat (machash 'RESULT synht)))
		   (mk-nonht synht)
		   (list (mk-cat (machash 'ARG synht)))
		   res))))

(defun mk-rule (key val)
  "turn  hashtable val back to list; for saving. INSYN and OUTSYN are hash values in val"
  (list (list 'INDEX (machash 'INDEX val))
	(list 'KEY key)
	(list 'PARAM (machash 'PARAM val))
	(list 'INSEM (machash 'INSEM val))
	(list 'OUTSEM (machash 'OUTSEM val))
	(list 'INSYN (mk-cat (machash 'INSYN val)))
	(list 'OUTSYN (mk-cat (machash 'OUTSYN val)))))

(defun save-compile (fn &optional (msg ""))
  (add-tr-to-grammar)
  (save-grammar fn)
  (format t "~%compiled~A and saved." msg))

(defun save-subsumption (fn)
  "the result of subsumption is in *ht-tr*."
  (let ((rules nil))
    (maphash 
      #'(lambda (k v)
	  (push (mk-rule k v) rules))
      *ht-tr*)
    (setf *RAISED-LEX-RULES* rules))
  (save-compile fn ", subsumed"))
	   
;;; ------------------------
;;; MGU for rule subsumption, adopted from ccglab's unification without -mgu suffix
;;; in rule subsumption, unlike in projection, we must pass on all unifiable features, rather than just check them.
;;; -hhcb
;;; ------------------------

(defun cat-match-mgu (sht1 sht2)
  (cond ((and (basicp-hash sht1) (basicp-hash sht2))
	 (let ((binds1 nil)
	       (binds2 nil))
	   (maphash #'(lambda (feat1 v1)  ; check sht1 feats and find binds
			(let ((v2 (machash feat1 sht2)))
			  (and v1 v2 (not (var? v1))(not (var? v2))(not (equal v1 v2))  ; changed eql to equal. (BCAT v)
			       (return-from cat-match-mgu (values nil nil nil)))        ; can have list value for v because of singletons
			  (push (list feat1 v1) binds1)))   ; just put them all in, hashtable semantics eliminates duplicates
		    sht1)
	   (maphash #'(lambda (feat2 v2)  ; if we are here, unification must have succeeded
			(push (list feat2 v2) binds2))
		    sht2)
	   (values t binds1 binds2)))
	((and (complexp-hash sht1) (complexp-hash sht2)
	      (eql (machash 'DIR sht1)(machash 'DIR sht2))
	      (mod-compatiblep (machash 'MODAL sht1) (machash 'MODAL sht2))
	      (multiple-value-bind (res1 b1 b2)
		(cat-match-mgu (machash 'ARG sht1) (machash 'ARG sht2))
		(and res1 (multiple-value-bind (res2 b12 b22)
			    (cat-match-mgu (machash 'RESULT sht1) (machash 'RESULT sht2))
			    (return-from cat-match-mgu (values res2 (append b12 b1) (append b22 b2))))))))
	(t (values nil nil nil))))

(defun realize-binds2-mgu (newht binds)
  (cond  ((basicp-hash newht)
	  (dolist (fv binds)
	    (setf (machash (first fv) newht) (second fv)))) ; just override if already exists--unification already succeeded
         (t (progn (realize-binds2-mgu (machash 'RESULT newht) binds)
                   (realize-binds2-mgu (machash 'ARG newht) binds))))
    newht)

(defun realize-binds-mgu (sht binds)
  (let ((newht (copy-hashtable sht)))
    (cond ((null binds) newht)
          (t (realize-binds2-mgu newht binds)))))

;---------------------------------------------------------------
;------------to create lex-rule entries-------------------------
;---------------------------------------------------------------

(defun compile-tr (arg morphs) 
  (setf *RAISED-LEX-RULES* NIL) ;set to default
  (setf *VERBS-IN-GRAMMAR* NIL)
  (load-gram arg)  ; ded and ind have same format--changed to load-gram
  (find-morph-v *ccg-grammar* morphs)
  (get-last-key-id *ccg-grammar*)
  (dolist (v-entry *VERBS-IN-GRAMMAR*)
    (type-raise (second (assoc 'SYN v-entry)))
    ;(format t "~%args:~A ~2%syns:~A" *ARGS* *SYNS*)
    (loop while *SYNS*
	do (let ((temp (copy-alist *lex-rule-TEMPLATE*)))
	     (set-insyn temp (pop *ARGS*))   ; CB note: a loop exhausts *SYNS* and *ARGS* later--no need to reinitialise
	     (set-outsyn temp (pop *SYNS*))
	     (set-key temp (get-next-key-id))
	     (set-index temp (gensym "_TRC"))
	     (push temp *RAISED-LEX-RULES*))))
  t)

(defun hash-tr ()
  "creates the hash table for inferred rules"
  (setf *ht-tr* (make-hash-table :test #'equal :size (+ (length *RAISED-LEX-RULES*) 10)))
  (dolist (lexr (reverse *RAISED-LEX-RULES*))
    (setf (machash (nv-list-val 'KEY lexr) *ht-tr*) (hash-lexrule lexr))))

(defun subsume-tr ()
  "v1 and v2 are hash values. INSYN and OUTSYN are hash-valued SYNs due to hash-tr; cat-match needs this."
  (let ((nochange nil))
    (loop  until nochange
	   do
	   (setf nochange t)
	   (maphash 
	     #'(lambda (k1 v1)
		 (maphash 
		   #'(lambda (k2 v2)
		       (if (not (equal k1 k2))
			 (multiple-value-bind (match1 inbinds1 inbinds2) 
			   (cat-match-mgu (machash 'INSYN v1)
					  (machash 'INSYN v2))
			   (and match1
				(multiple-value-bind (match2 outbinds1 outbinds2)
				  (cat-match-mgu (machash 'OUTSYN v1)
						 (machash 'OUTSYN v2))
				  (and match2     ; if both in and out do not match, they are different rules
				       (let 
					 ((newht (make-lrule-hashtable))
					  (key (get-next-key-id)))  ; keeping keys numeral to be consistent with ccglab
					 (setf (machash 'KEY newht) key)
					 (setf (machash 'INDEX newht) (gensym "_MGU")) 
					 (setf (machash 'PARAM newht) 1.0)  ; prior for inferred rules
					 (setf (machash 'INSEM newht) 'LF) 
					 (setf (machash 'OUTSEM newht) '(LAM LF (LAM P (P LF)))) ; this is universal
					 (setf (machash 'INSYN newht) 
					       (realize-binds-mgu (machash 'INSYN v1) (append inbinds1 inbinds2)))     ; MGU of input
					 (setf (machash 'OUTSYN newht) 
					       (realize-binds-mgu (machash 'OUTSYN v1) (append outbinds1 outbinds2))) ; MGU of output
					 (setf (machash key *ht-tr*) newht) ; added to table as it is looped
					 (setf nochange nil)
					 (remhash k1 *ht-tr*)
					 (remhash k2 *ht-tr*))))))))  ; cross your fingers for this destructive hash loop
		   *ht-tr*))
	     *ht-tr*))))

(defun compile-and-subsume-tr (gname vmorphs)
  "first finds all rules from grammar file with list of verbal POS in vmrophs, 
  then reduces the rule set to MGUs of pairs iteratively.
  We use hashtables to be compatible with MGU function cat-match---and for efficieny."
  (compile-tr gname vmorphs) ; result in *RAISED-LEX-RULES* in reverse order of find
  (hash-tr)
  (subsume-tr))

(defun debug-tr (arg morphs) ;to simulate how the work flow looks like
  (setq *RAISED-LEX-ITEMS* NIL) ;set to default
  (setq *VERBS-IN-GRAMMAR* NIL)
  (load-gram arg)
  (find-morph-v *ccg-grammar* morphs)
  (get-last-key-id *ccg-grammar*)
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

