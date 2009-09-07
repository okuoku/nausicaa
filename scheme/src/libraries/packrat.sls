;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: packrat parser library
;;;Date: Sun Sep  6, 2009
;;;
;;;Abstract
;;;
;;;	The original code is distrubuted at:
;;;
;;;		<http://dev.lshift.net/tonyg/json-scheme/>
;;;
;;;	and it works with MzScheme.
;;;
;;;Copyright (c) 2004, 2005 Tony Garnock-Jones <tonyg@kcbbs.gen.nz>
;;;Copyright (c) 2005 LShift Ltd. <query@lshift.net>
;;;
;;;Port to R6RS and Nausicaa integration by Marco Maggi.
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT. IN  NO EVENT SHALL THE AUTHORS  OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.
;;;


#!r6rs
(library (packrat)
  (export

    ;; lexical token
    make-lexical-token		lexical-token?
    lexical-token-value
    lexical-token-category
    lexical-token-source

    ;; source location
    make-source-location source-location?
    source-location-input
    source-location-line
    source-location-column
    source-location-offset
    source-location-length

    source-location>?
    top-source-location update-source-location source-location->string

    ;; parsing error
    make-parse-error parse-error?
    parse-error-position parse-error-expected parse-error-messages parse-error->list
    make-parse-error/expected make-parse-error/message
    parse-error->parse-result parse-error-empty? merge-parse-errors

    ;; parse result
    make-parse-result parse-result?
    parse-result-successful? parse-result-semantic-value parse-result-next parse-result-error
    make-parse-result/success make-parse-result/expected make-parse-result/message
    make-parse-result/merge-errors

    ;; input token stream
    parse-results? parse-results-position
    parse-results-base parse-results-next
    parse-results-token-kind
    parse-results-token-value

    prepend-base
    prepend-semantic-value

    ;;
    base-generator->results
    results->result

    ;;
    packrat-check-base
    packrat-check
    packrat-or
    packrat-unless

    packrat-parser
    packrat-lambda
    packrat-lambda*
    packrat-parse
    try-packrat-parse-pattern

    packrat-port-results
    packrat-string-results
    packrat-list-results)
  (import (rnrs)
    (rnrs mutable-pairs)
    (lists))


;;; helpers

(define gensym
  (let ((counter 0))
    (lambda ()
      (set! counter (+ 1 counter))
      (string->symbol (string-append "gensym-" (number->string counter))))))

(define-syntax define-inline
  (syntax-rules ()
    ((_ (?name ?arg ...) ?form0 ?form ...)
     (define-syntax ?name
       (syntax-rules ()
	 ((_ ?arg ...)
	  (begin ?form0 ?form ...)))))))


(define-record-type parse-error
  (fields (immutable position)
	  (immutable expected)
	  (immutable messages)))

(define (make-parse-error/expected pos expected-terminal-category)
  (make-parse-error pos (list expected-terminal-category) '()))

(define (make-parse-error/message pos error-message)
  (make-parse-error pos '() (list error-message)))

(define (parse-error-empty? e)
  (and (null? (parse-error-expected e))
       (null? (parse-error-messages e))))

(define (merge-parse-errors e1 e2)
  ;;Merge two  PARSE-ERROR records and return  the resulting PARSE-ERROR
  ;;record, which can be a new record or one among E1 and E2.
  ;;
  (cond ((not e1) e2)
	((not e2) e1)
	(else
	 (let ((p1 (parse-error-position e1))
	       (p2 (parse-error-position e2)))
	   (cond ((or (source-location>? p1 p2)
		      (parse-error-empty? e2))
		  e1)
		 ((or (source-location>? p2 p1)
		      (parse-error-empty? e1))
		  e2)
		 (else
		  (make-parse-error p1
				    (lset-union equal?
						(parse-error-expected e1)
						(parse-error-expected e2))
				    (lset-union equal?
						(parse-error-messages e1)
						(parse-error-messages e2)))))))))

(define (parse-error->list e)
  (and e (list (source-location->string (parse-error-position e))
	       (parse-error-expected e)
	       (parse-error-messages e))))


(define-record-type parse-result
  (fields (immutable successful?)
	  (immutable semantic-value)
	  (immutable next)
	  (immutable error)))
		;#f if none, but usually a parse-error structure

(define (make-parse-result/success semantic-value next)
  (make-parse-result #t semantic-value next #f))

(define (make-parse-result/expected position expected-terminal-category)
  (parse-error->parse-result (make-parse-error/expected position expected-terminal-category)))

(define (make-parse-result/message position error-message-string)
  (parse-error->parse-result (make-parse-error/message position error-message-string)))

(define (parse-error->parse-result parse-error)
  (make-parse-result #f #f #f parse-error))

(define (make-parse-result/merge-errors result error)
  (make-parse-result (parse-result-successful?    result)
		     (parse-result-semantic-value result)
		     (parse-result-next           result)
		     (merge-parse-errors (parse-result-error result) error)))


(define-record-type parse-results
  (fields (immutable position)
		;a parse-position or #f if unknown
	  (immutable base)
		;a value, #f indicating 'none' or 'eof'
	  (mutable next parse-results-next* set-parse-results-next!)
		;a parse-results, or a nullary function delivering same,
		;or #f for nothing next (eof)
	  (mutable map parse-results-map set-parse-results-map!)))
		;an alist mapping a nonterminal to a parse-result

(define (make-parse-results/success position base next-generator)
  (make-parse-results position base next-generator '()))

(define (make-parse-results/end-of-input position)
  (make-parse-results position #f #f '()))

(define (prepend-base pos base next)
  (make-parse-results pos base next '()))

(define (prepend-semantic-value pos key result next)
  (make-parse-results pos #f #f
		      (list (cons key (make-parse-result/success result next)))))

(define (base-generator->results lexer)
  ;;Note: applies first next-generator, to get first result
  (define (results-generator)
    (let* ((token	(lexer))
	   (position	(lexical-token-source token)))
      (if (eoi-token? token)
	  (make-parse-results/end-of-input position)
	(make-parse-results/success position token results-generator))))
  (results-generator))

(define (results->result results key fn)
  (let ((results-map (parse-results-map results)))
    (cond
     ((assv key results-map) =>
      (lambda (entry)
	(if (not (cdr entry))
	    (error #f "recursive parse rule" key)
	  (cdr entry))))
     (else (let ((cell (cons key #f)))
	     (set-parse-results-map! results (cons cell results-map))
	     (let ((result (fn)))
	       (set-cdr! cell result)
	       result))))))

(define (parse-results-next results)
  (let ((next (parse-results-next* results)))
    (if (procedure? next)
	(let ((next-value (next)))
	  (set-parse-results-next! results next-value)
	  next-value)
      next)))

(define (parse-results-token-kind results)
  (lexical-token-category/or-false (parse-results-base results)))

(define (parse-results-token-value results)
  (let ((token (parse-results-base results)))
    (and (not-eoi-token? token)
	 (lexical-token-value token))))


(define-record-type (lexical-token make-lexical-token lexical-token?)
  (fields (immutable category lexical-token-category)
	  (immutable source   lexical-token-source)
	  (immutable value    lexical-token-value))
  (nongenerative nausicaa:lexical-token))

(define-record-type (source-location make-source-location source-location?)
  (fields (immutable input   source-location-input)
	  (immutable line    source-location-line)
	  (immutable column  source-location-column)
	  (immutable offset  source-location-offset)
	  (immutable length  source-location-length))
  (nongenerative nausicaa:source-location))

(define-inline (eoi-token? token)
  (eq? '*eoi* (lexical-token-category token)))

(define-inline (not-eoi-token? token)
  (not (eoi-token? token)))

(define (lexical-token-category/or-false token)
  ;;Return the token category, or #f  if TOKEN is #f or the end-of-input
  ;;marker.
  ;;
  (and token
       (let ((category (lexical-token-category token)))
	 (and (not (eq? '*eoi* category)) category))))

(define (source-location>? a b)
  (cond ((not a) #f)
	((not b) #t)
	(else
	 (let ((la (source-location-line a))
	       (lb (source-location-line b)))
	   (or (> la lb)
	       (and (= la lb)
		    (> (source-location-column a)
		       (source-location-column b))))))))

(define (source-location->string pos)
  (if (not pos)
      "<??>"
    (string-append (source-location-input pos)
		   ":"
		   (number->string (source-location-line   pos))
		   ":"
		   (number->string (source-location-column pos)))))

(define (top-source-location input-spec)
  (make-source-location input-spec 1 0 0 0))

(define (update-source-location pos ch)
  (if (not pos)
      #f
    (let ((input	(source-location-input  pos))
	  (line		(source-location-line   pos))
	  (column	(source-location-column pos)))
      (case ch
	((#\return)
	 (make-source-location input line 0))
	((#\newline)
	 (make-source-location input (+ line 1) 0))
	((#\tab)
	 (make-source-location input line (* (div (+ column 8) 8) 8)))
	(else
	 (make-source-location input line (+ column 1)))))))


(define (packrat-check-base token-kind k)
  (lambda (results)
    (let ((token (parse-results-base results)))
      (if (eq? (lexical-token-category/or-false token) token-kind)
	  ((k (lexical-token-value token)) (parse-results-next results))
	(make-parse-result/expected (parse-results-position results)
				    (if (not token-kind)
					'*eoi*
				      token-kind))))))

(define (packrat-check parser k)
  (lambda (results)
    (let ((result (parser results)))
      (if (parse-result-successful? result)
	  (make-parse-result/merge-errors ((k (parse-result-semantic-value result))
					   (parse-result-next result))
					  (parse-result-error result))
	result))))

(define (packrat-or p1 p2)
  (lambda (results)
    (let ((result (p1 results)))
      (if (parse-result-successful? result)
	  result
	(make-parse-result/merge-errors (p2 results)
					(parse-result-error result))))))

(define (packrat-unless explanation p1 p2)
  (lambda (results)
    (let ((result (p1 results)))
      (if (parse-result-successful? result)
	  (make-parse-result/message (parse-results-position results)
			       explanation)
	(p2 results)))))


(define-syntax packrat-parser
  (syntax-rules (<- quote ! :@ /)
    ((_ start (nonterminal (alternative body0 body ...) ...) ...)
     (let ()
       (define nonterminal
	 (lambda (results)
	   (results->result results 'nonterminal
			    (lambda ()
			      ((packrat-parser #f "alts" nonterminal
					       ((begin body0 body ...) alternative) ...)
			       results)))))
       ...
       start))

    ((_ #f "alts" nt (body alternative))
     (packrat-parser #f "alt" nt body alternative))

    ((_ #f "alts" nt (body alternative) rest0 rest ...)
     (packrat-or (packrat-parser #f "alt" nt body alternative)
		 (packrat-parser #f "alts" nt rest0 rest ...)))

    ((_ #f "alt" nt body ())
     (lambda (results) (make-parse-result/success body results)))

    ((_ #f "alt" nt body ((! fails ...) rest ...))
     (packrat-unless (string-append "Nonterminal " (symbol->string 'nt)
				    " expected to fail "
				    (object->external-representation '(fails ...)))
		     (packrat-parser #f "alt" nt #t (fails ...))
		     (packrat-parser #f "alt" nt body (rest ...))))

    ((_ #f "alt" nt body ((/ alternative ...) rest ...))
     (packrat-check (packrat-parser #f "alts" nt (#t alternative) ...)
		    (lambda (result) (packrat-parser #f "alt" nt body (rest ...)))))

    ((_ #f "alt" nt body (var <- 'val rest ...))
     (packrat-check-base 'val
			 (lambda (var)
			   (packrat-parser #f "alt" nt body (rest ...)))))

    ((_ #f "alt" nt body (var <- :@ rest ...))
     (lambda (results)
       (let ((var (parse-results-position results)))
	 ((packrat-parser #f "alt" nt body (rest ...)) results))))

    ((_ #f "alt" nt body (var <- val rest ...))
     (packrat-check val
		    (lambda (var)
		      (packrat-parser #f "alt" nt body (rest ...)))))

    ((_ #f "alt" nt body ('val rest ...))
     (packrat-check-base 'val
			 (lambda (dummy)
			   (packrat-parser #f "alt" nt body (rest ...)))))

    ((_ #f "alt" nt body (val rest ...))
     (packrat-check val
		    (lambda (dummy)
		      (packrat-parser #f "alt" nt body (rest ...)))))))


(define-record-type packrat-parse-pattern
  (fields (immutable binding-names)
	  (immutable parser-proc)))

(define (try-packrat-parse-pattern pat bindings results ks kf)
  ((packrat-parse-pattern-parser-proc pat) bindings results ks kf))

(define-syntax packrat-lambda
  (syntax-rules ()
    ((_ (?binding ...) ?body ...)
     (packrat-lambda* succeed fail (?binding ...)
		      (let ((value (begin ?body ...)))
			(succeed value))))))

(define-syntax packrat-lambda*
  (syntax-rules ()
    ((_ ?succeed ?fail (?binding ...) ?body ...)
     (make-packrat-parse-pattern
      '()
      (lambda (bindings results ks kf)
	(let ((?succeed	(lambda (value)
			  (ks bindings (make-parse-result/success value results))))
	      (?fail	(lambda (error-maker . args)
			  (kf (apply error-maker (parse-results-position results) args))))
	      (?binding	(cond ((assq 'binding bindings) => cdr)
			      (else (error #f "missing binding" 'binding))))
	      ...)
	  ?body ...))))))

(define (object->external-representation o)
  (let-values (((port getter) (open-string-output-port)))
    (write o port)
    (getter)))

(define (packrat-parse table)
  (define (make-nsv-result results)
    (make-parse-result/success 'no-semantic-value results))

  (define (merge-success-with-errors err ks)
    (lambda (bindings result)
      (ks bindings (make-parse-result/merge-errors result err))))

  (define (merge-failure-with-errors err kf)
    (lambda (err1)
      (kf (merge-parse-errors err1 err))))

  (define (all-binding-names parse-patterns)
    (append-map packrat-parse-pattern-binding-names parse-patterns))

  (define (parse-alternatives alts0)
    (cond
     ((null? alts0) (make-packrat-parse-pattern
		     '()
		     (lambda (bindings results ks kf) (kf #f))))
     ((null? (cdr alts0)) (parse-simple (car alts0)))
     (else
      (let ((alts (map parse-simple alts0)))
	(make-packrat-parse-pattern
	 (all-binding-names alts) ;; should be a union rather than a product, technically
	 (lambda (bindings results ks kf)
	   (let try ((err #f)
		     (alts alts))
	     (if (null? alts)
		 (kf err)
	       (try-packrat-parse-pattern
		(car alts) bindings results
		(merge-success-with-errors err ks)
		(lambda (err1) (try (merge-parse-errors err1 err)
				    (cdr alts))))))))))))

  (define (extract-sequence seq)
    (cond ((null? seq)
	   '())
	  ((null? (cdr seq))
	   (cons (parse-simple (car seq)) '()))
	  ((eq? (cadr seq) '+)
	   (cons (parse-repetition (car seq) 1 #f)
		 (extract-sequence (cddr seq))))
	  ((eq? (cadr seq) '*)
	   (cons (parse-repetition (car seq) 0 #f)
		 (extract-sequence (cddr seq))))
	  ((eq? (cadr seq) '?)
	   (cons (parse-repetition (car seq) 0 1)
		 (extract-sequence (cddr seq))))
	  ((eq? (cadr seq) '<-)
	   (if (null? (cddr seq))
	       (error #f "bad binding form" seq)
	     (cons (parse-binding (car seq)
				  (parse-simple (caddr seq)))
		   (extract-sequence (cdddr seq)))))
	  (else
	   (cons (parse-simple     (car seq))
		 (extract-sequence (cdr seq))))))

  (define (parse-sequence seq)
    (let ((parsers (extract-sequence seq)))
      (make-packrat-parse-pattern
       (all-binding-names parsers)
       (lambda (bindings results ks kf)
	 (let continue ((bindings bindings)
			(results results)
			(err #f)
			(parsers parsers))
	   (cond
	    ((null? parsers) (ks bindings (make-parse-result/merge-errors
					   (make-nsv-result results) err)))
	    ((null? (cdr parsers))
	     (try-packrat-parse-pattern
	      (car parsers) bindings results
	      (merge-success-with-errors err ks)
	      (merge-failure-with-errors err kf)))
	    (else
	     (try-packrat-parse-pattern
	      (car parsers) bindings results
	      (lambda (new-bindings result)
		(continue new-bindings
			  (parse-result-next result)
			  (merge-parse-errors err (parse-result-error result))
			  (cdr parsers)))
	      (merge-failure-with-errors err kf)))))))))

  (define (parse-literal-string str)
    (let ((len (string-length str)))
      (make-packrat-parse-pattern
       '()
       (lambda (bindings starting-results ks kf)
	 (let loop ((pos 0)
		    (results starting-results))
	   (if (= pos len)
	       (ks bindings (make-parse-result/success str results))
	     (let ((v (parse-results-token-value results)))
	       (if (and (char? v)
			(char=? v (string-ref str pos)))
		   (loop (+ pos 1) (parse-results-next results))
		 (kf (make-parse-error/expected (parse-results-position starting-results)
						str))))))))))

  (define (parse-char-set* predicate expected)
    (make-packrat-parse-pattern
     '()
     (lambda (bindings results ks kf)
       (let ((v (parse-results-token-value results)))
	 (if (and (char? v) (predicate v))
	     (ks bindings (make-parse-result/success v (parse-results-next results)))
	   (kf (make-parse-error/expected (parse-results-position results) expected)))))))

  (define (parse-char-set set-spec optional-arg)
    (cond
     ((string? set-spec)
      (let ((chars (string->list set-spec)))
	(parse-char-set* (lambda (ch) (memv ch chars)) (or optional-arg `(one-of ,set-spec)))))
     ((procedure? set-spec)
      (parse-char-set* set-spec (or optional-arg `(char-predicate ,set-spec))))
     (else
      (error #f "bad char set specification" set-spec))))

  (define (parse-simple simple)
    (cond
     ((string? simple) (parse-literal-string simple))
     ((eq? simple ':@) (make-packrat-parse-pattern
			'()
			(lambda (bindings results ks kf)
			  (ks bindings (make-parse-result/success
					(parse-results-position results) results)))))
     ((symbol? simple) (parse-goal simple))
     ((packrat-parse-pattern? simple) simple) ;; extension point
     ((pair? simple) (case (car simple)
		       ((/) (parse-alternatives (cdr simple)))
		       ((&) (parse-follow (cdr simple)))
		       ((!) (parse-no-follow (cdr simple)))
		       ((quote) (parse-base-token (cadr simple)))
		       ((/:) (parse-char-set (cadr simple) (and (pair? (cddr simple))
								(caddr simple))))
		       (else (parse-sequence simple))))
     ((or (char? simple)
	  (not simple))
      (parse-base-token simple))
     ((null? simple)
      (parse-sequence simple))
     (else
      (error #f "bad syntax pattern" simple))))

  (define (parse-follow seq)
    (let ((parser (parse-sequence seq)))
      (make-packrat-parse-pattern
       (packrat-parse-pattern-binding-names parser)
       (lambda (bindings results ks kf)
	 (try-packrat-parse-pattern
	  parser bindings results
	  (lambda (bindings result)
	    (ks bindings
		(make-parse-result/merge-errors
		 (make-parse-result/success (parse-result-semantic-value result)
					    results)
		 (parse-result-error result))))
	  kf)))))

  (define (explain-no-follow results seq)
    (make-parse-error/message (parse-results-position results)
			      (string-append "failed no-follow rule: "
					     (object->external-representation seq))))

  (define (parse-no-follow seq)
    (let ((parser (parse-sequence seq)))
      (make-packrat-parse-pattern
       '()
       (lambda (bindings results ks kf)
	 (try-packrat-parse-pattern
	  parser bindings results
	  (lambda (bindings result)
	    (kf (explain-no-follow results seq)))
	  (lambda (err)
	    (ks bindings (make-nsv-result results))))))))

  (define (parse-base-token token)
    (make-packrat-parse-pattern
     '()
     (lambda (bindings results ks kf)
       (let ((token (parse-results-base results)))
	 (if (eq? (lexical-token-category/or-false token) token)
	     (ks bindings (make-parse-result/success (lexical-token-value token)
						     (parse-results-next results)))
	   (kf (make-parse-error/expected (parse-results-position results)
					  (if (not token)
					      '*eoi*
					    token))))))))

  (define (rotate-bindings binding-names child-bindings)
    (let ((seed (fold (lambda (bindings seed)
			(map (lambda (name val)
			       (cond
				((assq name bindings) =>
				 (lambda (entry)
				   (cons (cdr entry) val)))
				(else val)))
			  binding-names
			  seed))
		      (map (lambda (name) '()) binding-names)
		      child-bindings)))
      (map cons binding-names seed)))

  (define (explain-too-many results counter maxrep simple)
    (lambda (bindings result)
      (make-parse-result/message (parse-results-position results)
				 (string-append "expected maximum "
						(number->string maxrep)
						" repetition(s) of rule "
						(object->external-representation simple)
						", but saw at least "
						(number->string counter)))))

  (define (prepare-bindings binding-names nested-bindings results err0)
    (lambda (err)
      (make-parse-result/merge-errors
       (make-parse-result/success (rotate-bindings binding-names nested-bindings)
				  results)
       (merge-parse-errors err err0))))

  (define (parse-repetition simple minrep maxrep)
    (let* ((parser (parse-simple simple))
	   (repeated-names (packrat-parse-pattern-binding-names parser))
	   (repetition-id (gensym)))

      (define (repeat counter err0 nested-bindings results)
	(define (consume-one failure-k)
	  (try-packrat-parse-pattern
	   parser '() results
	   (lambda (bindings result)
	     (repeat (+ counter 1)
		     (merge-parse-errors (parse-result-error result) err0)
		     (cons bindings nested-bindings)
		     (parse-result-next result)))
	   failure-k))
	(cond
	 ((< counter minrep)
	  (consume-one (lambda (err1) (parse-error->parse-result (merge-parse-errors err1 err0)))))
	 ((or (not maxrep) (< counter maxrep))
	  (consume-one (prepare-bindings repeated-names nested-bindings results err0)))
	 (else
	  (try-packrat-parse-pattern
	   parser '() results
	   (explain-too-many results counter maxrep simple)
	   (prepare-bindings repeated-names nested-bindings results err0)))))

      (make-packrat-parse-pattern
       repeated-names
       (lambda (bindings results ks kf)
	 (results->result/k bindings results repetition-id
			    (lambda ()
			      (repeat 0 #f '() results))
			    (lambda (bindings result)
			      (let ((rotated-nested-bindings (parse-result-semantic-value result)))
				(ks (append rotated-nested-bindings bindings)
				    result)))
			    kf)))))

  (define (parse-binding name parser)
    (make-packrat-parse-pattern
     (list name)
     (lambda (bindings results ks kf)
       (try-packrat-parse-pattern
	parser bindings results
	(lambda (bindings result)
	  (ks (cons (cons name (parse-result-semantic-value result)) bindings)
	      result))
	kf))))

  (define (results->result/k bindings results goal filler ks kf)
    (let ((result (results->result results goal filler)))
      (if (parse-result-successful? result)
	  (ks bindings result)
	(kf (parse-result-error result)))))

  (define compiled-table
    (map (lambda (entry)
	   (unless (= (length entry) 2)
	     (error #f "ill-formed rule entry" entry))
	   (cons (car entry) (parse-simple (cadr entry))))
      table))

  (define (parse-goal goal)
    (unless (assq goal table)
      (error #f "unknown rule name" goal))
    (make-packrat-parse-pattern
     '()
     (lambda (bindings results ks kf)
       (let ((rule (cond ((assq goal compiled-table)
			  => cdr)
			 (else
			  (error #f "unknown rule name" goal)))))
	 (results->result/k bindings results goal
			    (lambda ()
			      (try-packrat-parse-pattern
			       rule '() results
			       (lambda (bindings1 result) result)
			       parse-error->parse-result))
			    ks kf)))))

  parse-goal)	;end of PACKRAT-PARSE


(define (packrat-port-results filename p)
  (base-generator->results
   (let ((ateof #f)
         (pos   (top-source-location filename)))
     (lambda ()
       (if ateof
           (values pos #f)
	 (let ((x (read-char p)))
	   (if (eof-object? x)
	       (begin
		 (set! ateof #t)
		 (values pos #f))
	     (let ((old-pos pos))
	       (set! pos (update-source-location pos x))
	       (values old-pos (cons x x))))))))))

(define (packrat-string-results filename s)
  (base-generator->results
   (let ((idx 0)
         (len (string-length s))
         (pos (top-source-location filename)))
     (lambda ()
       (if (= idx len)
           (values pos #f)
	 (let ((x (string-ref s idx))
	       (old-pos pos))
	   (set! pos (update-source-location pos x))
	   (set! idx (+ idx 1))
	   (values old-pos (cons x x))))))))

(define (packrat-list-results tokens)
  (base-generator->results
   (let ((stream tokens))
     (lambda ()
       (if (null? stream)
	   (values #f #f)
	 (let ((base-token (car stream)))
	   (set! stream (cdr stream))
	   (values #f base-token)))))))


;;;; done

)

;;; end of file
