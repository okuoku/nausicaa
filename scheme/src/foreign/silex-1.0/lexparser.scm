; SILex - Scheme Implementation of Lex
; Copyright (C) 2001  Danny Dube'
; 
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;
; Fonctions auxilliaires du lexer
;

(define parse-spec-char
  (lambda (lexeme line column)
    (make-tok char-tok lexeme line column newline-ch)))

(define parse-digits-char
  (lambda (lexeme line column)
    (let* ((num (substring lexeme 1 (string-length lexeme)))
	   (n (string->number num)))
      (make-tok char-tok lexeme line column n))))

(define parse-quoted-char
  (lambda (lexeme line column)
    (let ((c (string-ref lexeme 1)))
      (make-tok char-tok lexeme line column (char->integer c)))))

(define parse-ordinary-char
  (lambda (lexeme line column)
    (let ((c (string-ref lexeme 0)))
      (make-tok char-tok lexeme line column (char->integer c)))))

(define string-downcase
  (lambda (s)
    (let* ((l (string->list s))
	   (ld (map char-downcase l)))
      (list->string ld))))

(define extract-id
  (lambda (s)
    (let ((len (string-length s)))
      (substring s 1 (- len 1)))))

(define parse-id
  (lambda (lexeme line column)
    (make-tok id-tok lexeme line column (string-downcase lexeme) lexeme)))

(define parse-id-ref
  (lambda (lexeme line column)
    (let* ((orig-name (extract-id lexeme))
	   (name (string-downcase orig-name)))
    (make-tok subst-tok lexeme line column name orig-name))))

(define parse-power-m
  (lambda (lexeme line column)
    (let* ((len (string-length lexeme))
	   (substr (substring lexeme 1 (- len 1)))
	   (m (string->number substr))
	   (range (cons m m)))
      (make-tok power-tok lexeme line column range))))

(define parse-power-m-inf
  (lambda (lexeme line column)
    (let* ((len (string-length lexeme))
	   (substr (substring lexeme 1 (- len 2)))
	   (m (string->number substr))
	   (range (cons m 'inf)))
      (make-tok power-tok lexeme line column range))))

(define parse-power-m-n
  (lambda (lexeme line column)
    (let ((len (string-length lexeme)))
      (let loop ((comma 2))
	(if (char=? (string-ref lexeme comma) #\,)
	    (let* ((sub1 (substring lexeme 1 comma))
		   (sub2 (substring lexeme (+ comma 1) (- len 1)))
		   (m (string->number sub1))
		   (n (string->number sub2))
		   (range (cons m n)))
	      (make-tok power-tok lexeme line column range))
	    (loop (+ comma 1)))))))




;
; Lexer generique
;

(define lexer-raw #f)
(define lexer-stack '())

(define lexer-alist #f)

(define lexer-buffer #f)
(define lexer-buffer-empty? #t)

(define lexer-history '())
(define lexer-history-interp #f)

(define init-lexer
  (lambda (port)
    (let* ((IS (lexer-make-IS 'port port 'all))
	   (action-lexer (lexer-make-lexer action-tables IS))
	   (class-lexer  (lexer-make-lexer class-tables  IS))
	   (macro-lexer  (lexer-make-lexer macro-tables  IS))
	   (regexp-lexer (lexer-make-lexer regexp-tables IS))
	   (string-lexer (lexer-make-lexer string-tables IS)))
      (set! lexer-raw #f)
      (set! lexer-stack '())
      (set! lexer-alist
	    (list (cons 'action action-lexer)
		  (cons 'class  class-lexer)
		  (cons 'macro  macro-lexer)
		  (cons 'regexp regexp-lexer)
		  (cons 'string string-lexer)))
      (set! lexer-buffer-empty? #t)
      (set! lexer-history '()))))

; Lexer brut
; S'assurer qu'il n'y a pas de risque de changer de
; lexer quand le buffer est rempli
(define push-lexer
  (lambda (name)
    (set! lexer-stack (cons lexer-raw lexer-stack))
    (set! lexer-raw (cdr (assq name lexer-alist)))))

(define pop-lexer
  (lambda ()
    (set! lexer-raw (car lexer-stack))
    (set! lexer-stack (cdr lexer-stack))))

; Traite le "unget" (capacite du unget: 1)
(define lexer2
  (lambda ()
    (if lexer-buffer-empty?
	(lexer-raw)
	(begin
	  (set! lexer-buffer-empty? #t)
	  lexer-buffer))))

(define lexer2-unget
  (lambda (tok)
    (set! lexer-buffer tok)
    (set! lexer-buffer-empty? #f)))

; Traite l'historique
(define lexer
  (lambda ()
    (let* ((tok (lexer2))
	   (tok-lexeme (get-tok-lexeme tok))
	   (hist-lexeme (if lexer-history-interp
			    (blank-translate tok-lexeme)
			    tok-lexeme)))
      (set! lexer-history (cons hist-lexeme lexer-history))
      tok)))

(define lexer-unget
  (lambda (tok)
    (set! lexer-history (cdr lexer-history))
    (lexer2-unget tok)))

(define lexer-set-blank-history
  (lambda (b)
    (set! lexer-history-interp b)))

(define blank-translate
  (lambda (s)
    (let ((ss (string-copy s)))
      (let loop ((i (- (string-length ss) 1)))
	(cond ((< i 0)
	       ss)
	      ((char=? (string-ref ss i) (integer->char tab-ch))
	       (loop (- i 1)))
	      ((char=? (string-ref ss i) #\newline)
	       (loop (- i 1)))
	      (else
	       (string-set! ss i #\space)
	       (loop (- i 1))))))))

(define lexer-get-history
  (lambda ()
    (let* ((rightlist (reverse lexer-history))
	   (str (apply string-append rightlist))
	   (strlen (string-length str))
	   (str2 (if (and (> strlen 0)
			  (char=? (string-ref str (- strlen 1)) #\newline))
		     str
		     (string-append str (string #\newline)))))
      (set! lexer-history '())
      str2)))




;
; Traitement des listes de tokens
;

(define de-anchor-tokens
  (let ((not-anchor-toks (make-dispatch-table number-of-tokens
					      (list (cons caret-tok     #f)
						    (cons dollar-tok    #f)
						    (cons <<EOF>>-tok   #f)
						    (cons <<ERROR>>-tok #f))
					      #t)))
    (lambda (tok-list)
      (if (null? tok-list)
	  '()
	  (let* ((tok (car tok-list))
		 (tok-type (get-tok-type tok))
		 (toks (cdr tok-list))
		 (new-toks (de-anchor-tokens toks)))
	    (cond ((vector-ref not-anchor-toks tok-type)
		   (cons tok new-toks))
		  ((or (= tok-type caret-tok) (= tok-type dollar-tok))
		   (let* ((line (get-tok-line tok))
			  (column (get-tok-column tok))
			  (attr (if (= tok-type caret-tok) caret-ch dollar-ch))
			  (new-tok (make-tok char-tok "" line column attr)))
		     (cons new-tok new-toks)))
		  ((= tok-type <<EOF>>-tok)
		   (lex-error (get-tok-line tok)
			      (get-tok-column tok)
			      "the <<EOF>> anchor must be used alone"
			      " and only after %%."))
		  ((= tok-type <<ERROR>>-tok)
		   (lex-error (get-tok-line tok)
			      (get-tok-column tok)
			      "the <<ERROR>> anchor must be used alone"
			      " and only after %%."))))))))

(define strip-end
  (lambda (l)
    (if (null? (cdr l))
	'()
	(cons (car l) (strip-end (cdr l))))))

(define extract-anchors
  (lambda (tok-list)
    (let* ((tok1 (car tok-list))
	   (line (get-tok-line tok1))
	   (tok1-type (get-tok-type tok1)))
      (cond ((and (= tok1-type <<EOF>>-tok) (null? (cdr tok-list)))
	     (make-rule line #t #f #f #f '() #f))
	    ((and (= tok1-type <<ERROR>>-tok) (null? (cdr tok-list)))
	     (make-rule line #f #t #f #f '() #f))
	    (else
	     (let* ((bol? (= tok1-type caret-tok))
		    (tok-list2 (if bol? (cdr tok-list) tok-list)))
	       (if (null? tok-list2)
		   (make-rule line #f #f bol? #f tok-list2 #f)
		   (let* ((len (length tok-list2))
			  (tok2 (list-ref tok-list2 (- len 1)))
			  (tok2-type (get-tok-type tok2))
			  (eol? (= tok2-type dollar-tok))
			  (tok-list3 (if eol?
					 (strip-end tok-list2)
					 tok-list2)))
		     (make-rule line #f #f bol? eol? tok-list3 #f)))))))))

(define char-list->conc
  (lambda (char-list)
    (if (null? char-list)
	(make-re epsilon-re)
	(let loop ((cl char-list))
	  (let* ((c (car cl))
		 (cl2 (cdr cl)))
	    (if (null? cl2)
		(make-re char-re c)
		(make-re conc-re (make-re char-re c) (loop cl2))))))))

(define parse-tokens-atom
  (let ((action-table
	 (make-dispatch-table
	  number-of-tokens
	  (list (cons lpar-tok
		      (lambda (tok tok-list macros)
			(parse-tokens-sub tok-list macros)))
		(cons dot-tok
		      (lambda (tok tok-list macros)
			(cons (make-re class-re dot-class) (cdr tok-list))))
		(cons subst-tok
		      (lambda (tok tok-list macros)
			(let* ((name (get-tok-attr tok))
			       (ass (assoc name macros)))
			  (if ass
			      (cons (cdr ass) (cdr tok-list))
			      (lex-error (get-tok-line tok)
					 (get-tok-column tok)
					 "unknown macro \""
					 (get-tok-2nd-attr tok)
					 "\".")))))
		(cons char-tok
		      (lambda (tok tok-list macros)
			(let ((c (get-tok-attr tok)))
			  (cons (make-re char-re c) (cdr tok-list)))))
		(cons class-tok
		      (lambda (tok tok-list macros)
			(let ((class (get-tok-attr tok)))
			  (cons (make-re class-re class) (cdr tok-list)))))
		(cons string-tok
		      (lambda (tok tok-list macros)
			(let* ((char-list (get-tok-attr tok))
			       (re (char-list->conc char-list)))
			  (cons re (cdr tok-list))))))
	  (lambda (tok tok-list macros)
	    (lex-error (get-tok-line tok)
		       (get-tok-column tok)
		       "syntax error in regular expression.")))))
    (lambda (tok-list macros)
      (let* ((tok (car tok-list))
	     (tok-type (get-tok-type tok))
	     (action (vector-ref action-table tok-type)))
	(action tok tok-list macros)))))

(define check-power-tok
  (lambda (tok)
    (let* ((range (get-tok-attr tok))
	   (start (car range))
	   (end (cdr range)))
      (if (or (eq? 'inf end) (<= start end))
	  range
	  (lex-error (get-tok-line tok)
		     (get-tok-column tok)
		     "incorrect power specification.")))))

(define power->star-plus
  (lambda (re range)
    (power->star-plus-rec re (car range) (cdr range))))

(define power->star-plus-rec
  (lambda (re start end)
    (cond ((eq? end 'inf)
	   (cond ((= start 0)
		  (make-re star-re re))
		 ((= start 1)
		  (make-re plus-re re))
		 (else
		  (make-re conc-re
			   re
			   (power->star-plus-rec re (- start 1) 'inf)))))
	  ((= start 0)
	   (cond ((= end 0)
		  (make-re epsilon-re))
		 ((= end 1)
		  (make-re question-re re))
		 (else
		  (make-re question-re
			   (power->star-plus-rec re 1 end)))))
	  ((= start 1)
	   (if (= end 1)
	       re
	       (make-re conc-re re (power->star-plus-rec re 0 (- end 1)))))
	  (else
	   (make-re conc-re
		    re
		    (power->star-plus-rec re (- start 1) (- end 1)))))))

(define parse-tokens-fact
  (let ((not-op-toks (make-dispatch-table number-of-tokens
					  (list (cons question-tok #f)
						(cons plus-tok     #f)
						(cons star-tok     #f)
						(cons power-tok    #f))
					  #t)))
    (lambda (tok-list macros)
      (let* ((result (parse-tokens-atom tok-list macros))
	     (re (car result))
	     (tok-list2 (cdr result)))
	(let loop ((re re) (tok-list3 tok-list2))
	  (let* ((tok (car tok-list3))
		 (tok-type (get-tok-type tok)))
	    (cond ((vector-ref not-op-toks tok-type)
		   (cons re tok-list3))
		  ((= tok-type question-tok)
		   (loop (make-re question-re re) (cdr tok-list3)))
		  ((= tok-type plus-tok)
		   (loop (make-re plus-re re) (cdr tok-list3)))
		  ((= tok-type star-tok)
		   (loop (make-re star-re re) (cdr tok-list3)))
		  ((= tok-type power-tok)
		   (loop (power->star-plus re (check-power-tok tok))
			 (cdr tok-list3))))))))))

(define parse-tokens-conc
  (lambda (tok-list macros)
    (let* ((result1 (parse-tokens-fact tok-list macros))
	   (re1 (car result1))
	   (tok-list2 (cdr result1))
	   (tok (car tok-list2))
	   (tok-type (get-tok-type tok)))
      (cond ((or (= tok-type pipe-tok)
		 (= tok-type rpar-tok))
	     result1)
	    (else ; Autres facteurs
	     (let* ((result2 (parse-tokens-conc tok-list2 macros))
		    (re2 (car result2))
		    (tok-list3 (cdr result2)))
	       (cons (make-re conc-re re1 re2) tok-list3)))))))

(define parse-tokens-or
  (lambda (tok-list macros)
    (let* ((result1 (parse-tokens-conc tok-list macros))
	   (re1 (car result1))
	   (tok-list2 (cdr result1))
	   (tok (car tok-list2))
	   (tok-type (get-tok-type tok)))
      (cond ((= tok-type pipe-tok)
	     (let* ((tok-list3 (cdr tok-list2))
		    (result2 (parse-tokens-or tok-list3 macros))
		    (re2 (car result2))
		    (tok-list4 (cdr result2)))
	       (cons (make-re or-re re1 re2) tok-list4)))
	    (else ; rpar-tok
	     result1)))))

(define parse-tokens-sub
  (lambda (tok-list macros)
    (let* ((tok-list2 (cdr tok-list)) ; Manger le lpar-tok
	   (result (parse-tokens-or tok-list2 macros))
	   (re (car result))
	   (tok-list3 (cdr result))
	   (tok-list4 (cdr tok-list3))) ; Manger le rpar-tok
      (cons re tok-list4))))

(define parse-tokens-match
  (lambda (tok-list line)
    (let loop ((tl tok-list) (count 0))
      (if (null? tl)
	  (if (> count 0)
	      (lex-error line
			 #f
			 "mismatched parentheses."))
	  (let* ((tok (car tl))
		 (tok-type (get-tok-type tok)))
	    (cond ((= tok-type lpar-tok)
		   (loop (cdr tl) (+ count 1)))
		  ((= tok-type rpar-tok)
		   (if (zero? count)
		       (lex-error line
				  #f
				  "mismatched parentheses."))
		   (loop (cdr tl) (- count 1)))
		  (else
		   (loop (cdr tl) count))))))))

; Ne traite pas les anchors
(define parse-tokens
  (lambda (tok-list macros)
    (if (null? tok-list)
	(make-re epsilon-re)
	(let ((line (get-tok-line (car tok-list))))
	  (parse-tokens-match tok-list line)
	  (let* ((begin-par (make-tok lpar-tok "" line 1))
		 (end-par (make-tok rpar-tok "" line 1)))
	    (let* ((tok-list2 (append (list begin-par)
				      tok-list
				      (list end-par)))
		   (result (parse-tokens-sub tok-list2 macros)))
	      (car result))))))) ; (cdr result) == () obligatoirement

(define tokens->regexp
  (lambda (tok-list macros)
    (let ((tok-list2 (de-anchor-tokens tok-list)))
      (parse-tokens tok-list2 macros))))

(define tokens->rule
  (lambda (tok-list macros)
    (let* ((rule (extract-anchors tok-list))
	   (tok-list2 (get-rule-regexp rule))
	   (tok-list3 (de-anchor-tokens tok-list2))
	   (re (parse-tokens tok-list3 macros)))
      (set-rule-regexp rule re)
      rule)))

; Retourne une paire: <<EOF>>-action et vecteur des regles ordinaires
(define adapt-rules
  (lambda (rules)
    (let loop ((r rules) (revr '()) (<<EOF>>-action #f) (<<ERROR>>-action #f))
      (if (null? r)
	  (cons (or <<EOF>>-action default-<<EOF>>-action)
		(cons (or <<ERROR>>-action default-<<ERROR>>-action)
		      (list->vector (reverse revr))))
	  (let ((r1 (car r)))
	    (cond ((get-rule-eof? r1)
		   (if <<EOF>>-action
		       (lex-error (get-rule-line r1)
				  #f
				  "the <<EOF>> anchor can be "
				  "used at most once.")
		       (loop (cdr r)
			     revr
			     (get-rule-action r1)
			     <<ERROR>>-action)))
		  ((get-rule-error? r1)
		   (if <<ERROR>>-action
		       (lex-error (get-rule-line r1)
				  #f
				  "the <<ERROR>> anchor can be "
				  "used at most once.")
		       (loop (cdr r)
			     revr
			     <<EOF>>-action
			     (get-rule-action r1))))
		  (else
		   (loop (cdr r)
			 (cons r1 revr)
			 <<EOF>>-action
			 <<ERROR>>-action))))))))




;
; Analyseur de fichier lex
;

(define parse-hv-blanks
  (lambda ()
    (let* ((tok (lexer))
	   (tok-type (get-tok-type tok)))
      (if (or (= tok-type hblank-tok)
	      (= tok-type vblank-tok))
	  (parse-hv-blanks)
	  (lexer-unget tok)))))

(define parse-class-range
  (lambda ()
    (let* ((tok (lexer))
	   (tok-type (get-tok-type tok)))
      (cond ((= tok-type char-tok)
	     (let* ((c (get-tok-attr tok))
		    (tok2 (lexer))
		    (tok2-type (get-tok-type tok2)))
	       (if (not (= tok2-type minus-tok))
		   (begin
		     (lexer-unget tok2)
		     (cons c c))
		   (let* ((tok3 (lexer))
			  (tok3-type (get-tok-type tok3)))
		     (cond ((= tok3-type char-tok)
			    (let ((c2 (get-tok-attr tok3)))
			      (if (> c c2)
				  (lex-error (get-tok-line tok3)
					     (get-tok-column tok3)
					     "bad range specification in "
					     "character class;"
					     #\newline
					     "the start character is "
					     "higher than the end one.")
				  (cons c c2))))
		           ((or (= tok3-type rbrack-tok)
				(= tok3-type minus-tok))
			    (lex-error (get-tok-line tok3)
				       (get-tok-column tok3)
				       "bad range specification in "
				       "character class; a specification"
				       #\newline
				       "like \"-x\", \"x--\" or \"x-]\" has "
				       "been used."))
			   ((= tok3-type eof-tok)
			    (lex-error (get-tok-line tok3)
				       #f
				       "eof of file found while parsing "
				       "a character class.")))))))
	    ((= tok-type minus-tok)
	     (lex-error (get-tok-line tok)
			(get-tok-column tok)
			"bad range specification in character class; a "
			"specification"
			#\newline
			"like \"-x\", \"x--\" or \"x-]\" has been used."))
            ((= tok-type rbrack-tok)
	     #f)
	    ((= tok-type eof-tok)
	     (lex-error (get-tok-line tok)
			#f
			"eof of file found while parsing "
			"a character class."))))))

(define parse-class
  (lambda (initial-class negative-class? line column)
    (push-lexer 'class)
    (let loop ((class initial-class))
      (let ((new-range (parse-class-range)))
	(if new-range
	    (loop (class-union (list new-range) class))
	    (let ((class (if negative-class?
			     (class-compl class)
			     class)))
	      (pop-lexer)
	      (make-tok class-tok "" line column class)))))))

(define parse-string
  (lambda (line column)
    (push-lexer 'string)
    (let ((char-list (let loop ()
		       (let* ((tok (lexer))
			      (tok-type (get-tok-type tok)))
			 (cond ((= tok-type char-tok)
				(cons (get-tok-attr tok) (loop)))
			       ((= tok-type doublequote-tok)
				(pop-lexer)
				'())
			       (else ; eof-tok
				(lex-error (get-tok-line tok)
					   #f
					   "end of file found while "
					   "parsing a string.")))))))
      (make-tok string-tok "" line column char-list))))

(define parse-regexp
  (let* ((end-action
	  (lambda (tok loop)
	    (lexer-unget tok)
	    (pop-lexer)
	    (lexer-set-blank-history #f)
	    `()))
	 (action-table
	  (make-dispatch-table
	   number-of-tokens
	   (list (cons eof-tok end-action)
		 (cons hblank-tok end-action)
		 (cons vblank-tok end-action)
		 (cons lbrack-tok
		       (lambda (tok loop)
			 (let ((tok1 (parse-class (list)
						  #f
						  (get-tok-line tok)
						  (get-tok-column tok))))
			   (cons tok1 (loop)))))
		 (cons lbrack-rbrack-tok
		       (lambda (tok loop)
			 (let ((tok1 (parse-class
				      (list (cons rbrack-ch rbrack-ch))
				      #f
				      (get-tok-line tok)
				      (get-tok-column tok))))
			   (cons tok1 (loop)))))
		 (cons lbrack-caret-tok
		       (lambda (tok loop)
			 (let ((tok1 (parse-class (list)
						  #t
						  (get-tok-line tok)
						  (get-tok-column tok))))
			   (cons tok1 (loop)))))
		 (cons lbrack-minus-tok
		       (lambda (tok loop)
			 (let ((tok1 (parse-class
				      (list (cons minus-ch minus-ch))
				      #f
				      (get-tok-line tok)
				      (get-tok-column tok))))
			   (cons tok1 (loop)))))
		 (cons doublequote-tok
		       (lambda (tok loop)
			 (let ((tok1 (parse-string (get-tok-line tok)
						   (get-tok-column tok))))
			   (cons tok1 (loop)))))
		 (cons illegal-tok
		       (lambda (tok loop)
			 (lex-error (get-tok-line tok)
				    (get-tok-column tok)
				    "syntax error in macro reference."))))
	   (lambda (tok loop)
	     (cons tok (loop))))))
    (lambda ()
      (push-lexer 'regexp)
      (lexer-set-blank-history #t)
      (parse-hv-blanks)
      (let loop ()
	(let* ((tok (lexer))
	       (tok-type (get-tok-type tok))
	       (action (vector-ref action-table tok-type)))
	  (action tok loop))))))

(define parse-ws1-regexp  ; Exige un blanc entre le nom et la RE d'une macro
  (lambda ()
    (let* ((tok (lexer))
	   (tok-type (get-tok-type tok)))
      (cond ((or (= tok-type hblank-tok) (= tok-type vblank-tok))
	     (parse-regexp))
	    (else  ; percent-percent-tok, id-tok ou illegal-tok
	     (lex-error (get-tok-line tok)
			(get-tok-column tok)
			"white space expected."))))))

(define parse-macro
  (lambda (macros)
    (push-lexer 'macro)
    (parse-hv-blanks)
    (let* ((tok (lexer))
	   (tok-type (get-tok-type tok)))
      (cond ((= tok-type id-tok)
	     (let* ((name (get-tok-attr tok))
		    (ass (assoc name macros)))
	       (if ass
		   (lex-error (get-tok-line tok)
			      (get-tok-column tok)
			      "the macro \""
			      (get-tok-2nd-attr tok)
			      "\" has already been defined.")
		   (let* ((tok-list (parse-ws1-regexp))
			  (regexp (tokens->regexp tok-list macros)))
		     (pop-lexer)
		     (cons name regexp)))))
            ((= tok-type percent-percent-tok)
	     (pop-lexer)
	     #f)
	    ((= tok-type illegal-tok)
	     (lex-error (get-tok-line tok)
			(get-tok-column tok)
			"macro name expected."))
	    ((= tok-type eof-tok)
	     (lex-error (get-tok-line tok)
			#f
			"end of file found before %%."))))))

(define parse-macros
  (lambda ()
    (let loop ((macros '()))
      (let ((macro (parse-macro macros)))
	(if macro
	    (loop (cons macro macros))
	    macros)))))

(define parse-action-end
  (lambda (<<EOF>>-action? <<ERROR>>-action? action?)
    (let ((act (lexer-get-history)))
      (cond (action?
	     act)
	    (<<EOF>>-action?
	     (string-append act default-<<EOF>>-action))
	    (<<ERROR>>-action?
	     (string-append act default-<<ERROR>>-action))
	    (else
	     (string-append act default-action))))))

(define parse-action
  (lambda (<<EOF>>-action? <<ERROR>>-action?)
    (push-lexer 'action)
    (let loop ((action? #f))
      (let* ((tok (lexer))
	     (tok-type (get-tok-type tok)))
	(cond ((= tok-type char-tok)
	       (loop #t))
	      ((= tok-type hblank-tok)
	       (loop action?))
	      ((= tok-type vblank-tok)
	       (push-lexer 'regexp)
	       (let* ((tok (lexer))
		      (tok-type (get-tok-type tok))
		      (bidon (lexer-unget tok)))
		 (pop-lexer)
		 (if (or (= tok-type hblank-tok)
			 (= tok-type vblank-tok))
		     (loop action?)
		     (begin
		       (pop-lexer)
		       (parse-action-end <<EOF>>-action?
					 <<ERROR>>-action?
					 action?)))))
	      (else ; eof-tok
	       (lexer-unget tok)
	       (pop-lexer)
	       (parse-action-end <<EOF>>-action?
				 <<ERROR>>-action?
				 action?)))))))

(define parse-rule
  (lambda (macros)
    (let ((tok-list (parse-regexp)))
      (if (null? tok-list)
	  #f
	  (let* ((rule (tokens->rule tok-list macros))
		 (action
		  (parse-action (get-rule-eof? rule) (get-rule-error? rule))))
	    (set-rule-action rule action)
	    rule)))))

(define parse-rules
  (lambda (macros)
    (parse-action #f #f)
    (let loop ()
      (let ((rule (parse-rule macros)))
	(if rule
	    (cons rule (loop))
	    '())))))

(define parser
  (lambda (filename)
    (let* ((port (open-input-file filename))
	   (port-open? #t))
      (lex-unwind-protect (lambda ()
			    (if port-open?
				(close-input-port port))))
      (init-lexer port)
      (let* ((macros (parse-macros))
	     (rules (parse-rules macros)))
	(close-input-port port)
	(set! port-open? #f)
	(adapt-rules rules)))))
