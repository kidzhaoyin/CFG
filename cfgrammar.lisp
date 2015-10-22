(defparameter *grammar*
 '((sentence -> (noun-phrase verb-phrase) (sentence CConjunction sentence) (SConjunction sentence verb-phrase) (noun-phrase Aux verb-phrase-infinitive) (SConjunction sentence Aux verb-phrase-infinitive) (noun-phrase verb-phrase prepositional-phrase))
   (noun-phrase -> noun (Article noun) (Article adjectives noun) (Article adjectives noun prepositional-phrase) (adjectives noun) (noun noun) (adjectives noun noun prepositional-phrase) (noun-phrase and noun-phrase) (noun-phrase relative-clause) (Article noun noun))
   (relative-clause -> (RelativePronoun verb-phrase))
   (noun -> NounSin NounPlu)	 
   (adjectives -> (Adjective adjectives) Adjective)
   (prepositional-phrase -> Preposition (Preposition noun-phrase) (Article Preposition noun-phrase) (prepositional-phrase prepositional-phrase))
   (verb-phrase -> verbIntransitive (verbTransitive noun-phrase) (Adverb verb-phrase) (verb-phrase to verb-phrase-infinitive) (be adjectives) (verb-phrase Adverb prepositional-phrase) (verb-phrase prepositional-phrase))
   (Preposition -> of to with in as)
   (verbIntransitive -> VerbInfIntransitive VerbPastIntransitive)
   (verbTransitive -> VerbInfTransitive VerbPastTransitive)
   (verb-phrase-infinitive -> (VerbInfTransitive noun-phrase) (Adverb VerbInfTransitive noun-phrase) (VerbInfIntransitive Adverb prepositional-phrase) (VerbInfTransitive noun-phrase prepositional-phrase) (VerbInfIntransitive Adverb) (be adjectives prepositional-phrase) (Adverb verb-phrase-infinitive) (be adjectives to verb-phrase-infinitive))
   (Aux -> must will should)
   (Article -> the a this such that an no some)
   (NounSin -> man ball woman table education narrative polarization strategy asteroid eye there job business health care it death penalty case intent prosecution sky kid book face internet computer intelligence)
   (NounPlu -> there astronomers gains industries services telescopes weeks fans facts cats weekends programmers programs projects phones houses systems countries flowers foods burgers maps)
   (VerbInfTransitive -> counter be see pay achieve seek catch love eat hug welcome meet is) 
   (VerbInfIntransitive -> tend shout dance pay see play sit sleep)   
   (VerbPastTransitive -> were liked saw took hit kicked met paid played sang thanked watched studied learnt loved ate) 
   (VerbPastIntransitive -> tended walked jumped danced shouted swimmed laughed slept saw sitted fell cried rushed)
   (Adverb -> highly there well not hardly happily willingly out)
   (CConjunction -> and but or for)  
   (SConjunction -> whether why where when how)
   (RelativePronoun -> that which who)
   (Adjective -> higher prevailing flawed visible naked amateur able several solid decided few desired professional favored beautiful good red angry common little cute nice))
 "A grammar for a trivial subset of English.")

  (defun targeted-sentence (rules)
	 (apply-rules rules nil))

	;list of rules using DFS order
;Higher education must counter the prevailing narrative of polarization
(defparameter rules1 '((sentence 3) (noun-phrase 4) (Aux 0) (verb-phrase-infinitive 0) (adjectives 1) (noun 0) (VerbInfTransitive 0) (noun-phrase 3) (Adjective 0) (NounSin 4) (Article 0) (adjectives 1) (noun 0) (prepositional-phrase 1) (Adjective 1) (NounSin 5) (Preposition 0) (noun-phrase 0) (noun 0) (NounSin 6)))

;This flawed strategy will hardly achieve the desired intent
(defparameter rules2 '((sentence 3) (noun-phrase 2) (Aux 1) (verb-phrase-infinitive 1) (Article 2) (adjectives 1) (noun 0) (Adverb 4) (VerbInfTransitive 4) (noun-phrase 2) (Adjective 2) (NounSin 7) (Article 0) (adjectives 1) (noun 0) (Adjective 11) (NounSin 19)))

;There were solid job gains in several industries that tend to pay well such as business and professional services and health care
(defparameter rules3 '((sentence 5) (noun-phrase 0) (verb-phrase 1) (prepositional-phrase 2) (noun 1) (NounPlu 0) (verbTransitive 1) (noun-phrase 8) (VerbPastTransitive 0) (noun-phrase 6) (relative-clause 0) (adjectives 1) (noun 0) (noun 1) (prepositional-phrase 1) (Adjective 8) (NounSin 11) (NounPlu 2) (Preposition 3) (noun-phrase 4) (adjectives 1) (noun 1) (Adjective 7) (NounPlu 3) (RelativePronoun 0) (verb-phrase 3) (verb-phrase 0) (verb-phrase-infinitive 4) (verbIntransitive 0) (VerbInfIntransitive 0) (VerbInfIntransitive 3) (Adverb 2) (Article 3) (Preposition 4) (noun-phrase 7) (noun-phrase 0) (noun-phrase 7) (noun 0) (NounSin 12) (noun-phrase 4) (noun-phrase 5) (adjectives 1) (noun 1) (Adjective 12) (NounPlu 4) (noun 0) (noun 0) (NounSin 13) (NounSin 14)))

;The asteroid will not be visible to the naked eye but amateur astronomers should be able to see it with telescopes
(defparameter rules4 '((sentence 1) (sentence 3) (CConjunction 1) (sentence 3) (noun-phrase 1) (Aux 1) (verb-phrase-infinitive 6) (Article 0) (noun 0) (NounSin 8) (Adverb 3) (verb-phrase-infinitive 5) (adjectives 1) (prepositional-phrase 1) (Adjective 3) (Preposition 1) (noun-phrase 2) (Article 0) (adjectives 1) (noun 0) (Adjective 4) (NounSin 9) (noun-phrase 4) (Aux 2) (verb-phrase-infinitive 7) (adjectives 1) (noun 1) (Adjective 5) (NounPlu 1) (adjectives 1) (verb-phrase-infinitive 3) (Adjective 6) (VerbInfTransitive 2) (noun-phrase 0) (prepositional-phrase 1) (noun 0) (NounSin 15) (Preposition 2) (noun-phrase 0) (noun 1) (NounPlu 5)))

;Whether the prosecution will seek the death penalty in the case will be decided in a few weeks
(defparameter rules5 '((sentence 4) (SConjunction 0) (sentence 3) (noun-phrase 1) (Article 0) (noun 0) (NounSin 20) (Aux 1) (verb-phrase-infinitive 3) (VerbInfTransitive 5) (noun-phrase 9) (Article 0) (noun 0) (NounSin 16) (noun 0) (NounSin 17) (prepositional-phrase 1) (Preposition 3) (noun-phrase 1) (Article 0) (noun 0) (NounSin 18) (Aux 1) (verb-phrase-infinitive 5) (adjectives 1) (Adjective 9) (prepositional-phrase 1) (Preposition 3) (noun-phrase 2) (Article 1) (adjectives 1) (Adjective 10) (noun 1) (NounPlu 6)))

(defun apply-rules (rules sentence)
 (cond 
  ((null rules) sentence)
  ((null sentence) (apply-rules (rest rules) (elt (rule-rhs (assoc (car (car rules)) *grammar*)) (second (car rules)))))
  (t (let ((rule-to-rewrite (car (car rules))) (new-rule (elt (rule-rhs (assoc (car (car rules)) *grammar*)) (second (car rules)))))
      (apply-rules (rest rules) (rewrite-sentence nil sentence rule-to-rewrite new-rule)))))) 

	;simply rewrites a sentence replacing the first occurence of the variable "rule-to-rewrite" in "sentence-next" by the symbols in "new-rule" 
	;example: (rewrite-sentence nil '(THE MAN verb-phrase) 'verb-phrase '(Verb noun-phrase))
	;returns (THE MAN Verb noun-phrase)
(defun rewrite-sentence (sentence-pre sentence-next rule-to-rewrite new-rule)
  (cond ((null sentence-next) sentence-pre)
	 (t 
	  (if (equal (car sentence-next) rule-to-rewrite)
	    (append (append sentence-pre (if (listp new-rule) new-rule (list new-rule))) (rest sentence-next))
	    (rewrite-sentence (append sentence-pre (list (car sentence-next))) (rest sentence-next) rule-to-rewrite new-rule)))))


(defun random-elt (list)
 (elt list
  (random (length list))))

;helper to run recursive function random-sentence-generate
(defun random-sentence (phrase)
 (random-sentence-generate phrase 0))

;take the depth as another parameter and stops when depth exceeds 10. If not, continue rewrites and increments depth
(defun random-sentence-generate (phrase depth)   
 "Generate a random sentence or phrase"
 (cond ((listp phrase)
       (if (<= depth 10)
	(mappend (lambda (sent) (random-sentence-generate sent (+ 1 depth))) phrase)))
  ((rewrites phrase) 
  (if (<= depth 10)
  (random-sentence-generate (random-elt (rewrites phrase)) (+ depth 1))))
  (t (list phrase))))

(defun generate-tree (phrase)
 "Generate a random sentence or phrase,
 with a complete parse tree."
 (cond ((listp phrase)
	(mapcar #'generate-tree phrase))
  ((rewrites phrase)
   (cons phrase
    (generate-tree (random-elt (rewrites phrase)))))
  (t (list phrase))))

(defun mappend (fn list)
 "Append the results of calling fn on each element of list.
 Like mapcon, but uses append instead of nconc."
 (apply #'append (mapcar fn list)))

(defun rule-rhs (rule)
 "The right hand side of a rule."
 (rest (rest rule)))

(defun rewrites (category)
 "Return a list of the possible rewrites for this category."
(rule-rhs (assoc category *grammar*)))


;function to check valid based on length (less than 40) and repetition (except for closed class words)
;my rejection criteria:
;successive same closed class words are also not allowed, e.g., "but but", "and and"
;more than 3 'and' sentence will be rejected

(defun validp (sentence)
  (if (>= (length sentence) 40) NIL
   (progn
    (setq exist '())
    (check-repeat sentence exist 0))))

;recursive function to check for repetition
;parameters sen stands for sentence, exi stands for exist list
(defun check-repeat (sen exi andcount)
  (setq exeption '(and the for with but a of to such it were not that)) 
  (if (<= (length sen) 0)
   T
   (if (or (member (car sen) exeption) (null (member (car sen) exi)))
         (progn
           (if (string= (car sen) (car exi))  
           nil
           (progn
             (if (string= 'and (car sen))
		(progn
		  (if (<= andcount 3)
		    (progn
                     (setq exi (cons (car sen) exi))
                     (setq sen (cdr sen))
                     (check-repeat sen exi (+ andcount 1)))))
                (progn
		  (setq exi (cons (car sen) exi))
		  (setq sen (cdr sen))
		  (check-repeat sen exi andcount)))
))))))

(defun generateValid (sentence)
  (setq s (random-sentence 'sentence))
  (if (not (validp s))
   (generateValid 'sentence)
    s)
)

(defun write-to-file (sentence)
  (with-open-file (str "~/sentences_yz2426.txt"
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
    (format str sentence)))

(defun run2 ()
  (let ((sent (random-sentence 'sentence)))
    (write-to-file (if (validp sent) (format nil "+ ~S ~%" sent) (format nil "- ~S ~%" sent)))))

(defun loop-run (N)
  (loop for i from 1 to N do (run2)))
(random-sentence 'sentence)
