;;;SRFI-14 character-sets library --
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
;;;Copyright (c) 1988-1995  Massachusetts Institute of Technology.
;;;Ported from MIT Scheme runtime by Brian D.  Carlstrom.
;;;Massively rehacked & extended by Olin Shivers 6/98.
;;;Massively redesigned and rehacked 5/2000 during SRFI process.
;;;Modified by Derick Eddington for R6RS compatibility.
;;;
;;;This   material  was  developed   by  the   Scheme  project   at  the
;;;Massachusetts  Institute  of  Technology,  Department  of  Electrical
;;;Engineering and Computer Science.  Permission to copy and modify this
;;;software, to redistribute either  the original software or a modified
;;;version, and to use this software for any purpose is granted, subject
;;;to the following restrictions and understandings.
;;;
;;;1. Any copy made of  this software must include this copyright notice
;;;   in full.
;;;
;;;2. Users  of this software  agree to make  their best efforts  (a) to
;;;   return to  the MIT Scheme  project any improvements  or extensions
;;;   that they make, so that  these may be included in future releases;
;;;   and (b) to inform MIT of noteworthy uses of this software.
;;;
;;;3. All materials  developed  as  a  consequence  of the  use of  this
;;;   software shall  duly acknowledge such use, in  accordance with the
;;;   usual standards of acknowledging credit in academic research.
;;;
;;;4. MIT has made no  warrantee or representation that the operation of
;;;   this software will  be error-free, and MIT is  under no obligation
;;;   to  provide  any  services,  by  way of  maintenance,  update,  or
;;;   otherwise.
;;;
;;;5. In  conjunction  with  products  arising  from  the  use  of  this
;;;   material, there shall  be no use of the  name of the Massachusetts
;;;   Institute  of Technology  nor  of any  adaptation  thereof in  any
;;;   advertising,  promotional,  or   sales  literature  without  prior
;;;   written consent from MIT in each case.
;;;



#!r6rs
(library (char-sets)
  (export

    ;; creating
    char-set-copy char-set char-set/unique

    ;; predicates and assertions
    char-set?		assert-char-set
    assert-char-set/or-false
    assert-list-of-chars

    ;; inspection
    char-set-size

    ;; conversion
    list->char-set	list->char-set!		char-set->list
    string->char-set	string->char-set!	char-set->string
    ->char-set

    ;; searching
    char-set-count char-set-contains?
    char-set-every char-set-any char-set-find

    ;; filtering
    char-set-filter	char-set-filter!
    char-set-partition	char-set-partition!
    char-set-remove
    char-set-remove*	char-set-remove*!
    char-set-delete-duplicates
    char-set-delete-duplicates!

    ;; iteration
    char-set-cursor	char-set-cursor-ref
    end-of-char-set?	char-set-cursor-next
    char-set-fold	char-set-unfold		char-set-unfold!
    char-set-for-each	char-set-map

    ;; set operations
    char-set=?		char-set<=?
    char-set-adjoin
    char-set-union		char-set-union!
    char-set-intersection	char-set-intersection!
    char-set-difference		char-set-difference!
    char-set-xor		char-set-xor!
    char-set-diff+intersection	char-set-diff+intersection!

    ;; predefined
;;     char-set:lower-case  char-set:upper-case  char-set:title-case
;;     char-set:letter      char-set:digit       char-set:letter+digit
;;     char-set:graphic     char-set:printing    char-set:whitespace
;;     char-set:iso-control char-set:punctuation char-set:symbol
;;     char-set:hex-digit   char-set:blank       char-set:ascii
;;     char-set:empty       char-set:full
    )
  (import (scheme)
    (rnrs mutable-strings)
    (rnrs r5rs)
    (lists))



;;;; helpers

(define-syntax append-char-lists
  (syntax-rules ()
    ((_ ?ell0 ?ell ...)
     (append (list-copy ?ell0)
	     (list-copy ?ell)
	     ...))))

(define-syntax append-char-lists!
  (syntax-rules ()
    ((_ ?ell0 ?ell ...)
     (append! ?ell0 ?ell ...))))

(define-syntax make-char-set-eat-list
  (syntax-rules ()
    ((_ ?expr)
     (let ((l ?expr))
       (make-char-set l (length l))))))


;;;; record definition and constructors

(define-record-type (char-set-record make-char-set char-set?)
  (fields (immutable list	char-set-rep)
	  (immutable length	char-set-size)))

(define (char-set . chars)
  (make-char-set-eat-list chars))

(define (char-set/unique . chars)
  (make-char-set-eat-list (delete-duplicates chars char=?)))

(define (char-set-copy cs)
  (make-char-set (list-copy (char-set-rep cs))
		 (char-set-size cs)))


;;;; predicates and assertions

(define (assert-list-of-chars obj func-name)
  (define (error)
    (assertion-violation func-name
	    "expected list of characters" obj))
  (unless (list? obj) (error))
  (for-each
      (lambda (ch)
	(unless (char? ch) (error)))
    obj)
  obj)

(define (assert-char-set obj func-name)
  (unless (char-set? obj)
    (assertion-violation func-name
      "expected char set" obj))
  obj)

(define (assert-char-set/or-false obj func-name)
  (unless (or (not obj) (char-set? obj))
    (assertion-violation func-name
      "expected char set" obj))
  obj)



;;;; conversion

(define list->char-set
  (case-lambda
   ((chars)
    (make-char-set-eat-list (list-copy chars)))
   ((chars base-cs)
    (make-char-set-eat-list (append-char-lists chars (char-set-rep base-cs))))))

(define (list->char-set! chars base-cs)
  (make-char-set-eat-list (append-char-lists! chars (char-set-rep base-cs))))

(define (char-set->list cs)
  (list-copy (char-set-rep cs)))

;;; --------------------------------------------------------------------

(define string->char-set
  (case-lambda
   ((str)
    (make-char-set (string->list str) (string-length str)))
   ((str base-cs)
    (make-char-set-eat-list
     (append-char-lists (string->list str)
			(char-set-rep base-cs))))))

(define (string->char-set! str base-cs)
  (make-char-set-eat-list
   (append-char-lists! (string->list str)
		       (char-set-rep base-cs))))

(define (char-set->string cs)
  (list->string (char-set-rep cs)))

;;; --------------------------------------------------------------------

(define (->char-set obj)
  ((cond ((char-set?	obj)	char-set-copy)
	 ((string?	obj)	string->char-set)
	 ((list?	obj)	list->char-set)
	 ((char?	obj)	char-set)
	 (else
	  (assertion-violation '->char-set
	    "cannot convert to char set" obj)))
   obj))


;;;; searching

(define (char-set-contains? cs char)
  (and (member* char (char-set-rep cs) char=?)
       #t))

(define (char-set-count pred cs)
  (count pred (char-set-rep cs)))

(define (char-set-find pred cs)
  (find pred (char-set-rep cs)))

(define (char-set-every pred cs)
  (every pred (char-set-rep cs)))

(define (char-set-any pred cs)
  (any pred (char-set-rep cs)))



;;;; filtering

(define char-set-filter
  (case-lambda
   ((pred cs)
    (make-char-set-eat-list
     (filter pred (char-set-rep cs))))
   ((pred cs base-cs)
    (make-char-set-eat-list
     (filter pred
       (append-char-lists (char-set-rep cs)
			  (char-set-rep base-cs)))))))

(define (char-set-filter! pred cs base-cs)
  (make-char-set-eat-list
   (filter! pred (append-char-lists! (char-set-rep cs)
				     (char-set-rep base-cs)))))

;;; --------------------------------------------------------------------

(define (char-set-partition pred cs)
  (receive (in out)
      (partition pred (char-set-rep cs))
    (values (make-char-set-eat-list in)
	    (make-char-set-eat-list out))))

(define (char-set-partition! pred cs)
  (receive (in out)
      (partition! pred (char-set-rep cs))
    (values (make-char-set-eat-list in)
	    (make-char-set-eat-list out))))

;;; --------------------------------------------------------------------

(define (char-set-remove* pred cs)
  (make-char-set-eat-list
   (remove* pred (char-set-rep cs))))

(define (char-set-remove*! pred cs)
  (make-char-set-eat-list
   (remove*! pred (char-set-rep cs))))

;;; --------------------------------------------------------------------

(define (char-set-remove char cs)
  (make-char-set-eat-list
   (remove char (char-set-rep cs))))

(define (char-set-delete-duplicates cs)
  (make-char-set-eat-list
   (delete-duplicates (char-set-rep cs))))

(define (char-set-delete-duplicates! cs)
  (delete-duplicates! (char-set-rep cs))
  cs)


;;;; iteration

(define char-set-cursor		char-set-rep)
(define end-of-char-set?	null?)
(define char-set-cursor-ref	car)
(define char-set-cursor-next	cdr)

;;; --------------------------------------------------------------------

(define (char-set-for-each proc cs)
  (for-each proc (char-set-rep cs)))

(define (char-set-map proc cs)
  (make-char-set-eat-list
   (map proc (char-set-rep cs))))

;;; --------------------------------------------------------------------

(define (char-set-fold kons knil cs)
  (fold kons knil (char-set-rep cs)))

(define char-set-unfold
  (case-lambda
   ((stop? map-to-char seed-step seed)
    (make-char-set-eat-list
     (unfold stop? map-to-char seed-step seed)))
   ((stop? map-to-char seed-step seed base-cs)
    (make-char-set-eat-list
     (append-char-lists (unfold stop? map-to-char seed-step seed)
			(list-copy (char-set-rep base-cs)))))))

(define (char-set-unfold! stop? map-to-char seed-step seed base-cs)
  (make-char-set-eat-list
   (append-char-lists! (unfold stop? map-to-char seed-step seed)
		       (char-set-rep base-cs))))


;;;; set operations

(define char-set=?
  (case-lambda
   (()
    #t)
   ((cs)
    #t)
   ((cs0 . args)
    (apply lset=? char=?
	   (char-set-rep cs0)
	   (map char-set-rep args)))))

(define char-set<=?
  (case-lambda
   (()
    #t)
   ((cs)
    #t)
   ((cs0 . args)
    (apply lset<=? char=?
	   (char-set-rep cs0)
	   (map char-set-rep args)))))

;;; --------------------------------------------------------------------

(define char-set-adjoin
  (case-lambda
   (()
    char-set:empty)
   ((cs)
    (char-set-copy cs))
   ((cs0 . args)
    (make-char-set-eat-list
     (apply lset-adjoin char=?
	    (char-set-rep cs0)
	    (map char-set-rep args))))))

;;; --------------------------------------------------------------------

(define char-set-union
  (case-lambda
   (()
    char-set:empty)
   ((cs)
    (char-set-copy cs))
   ((cs0 . args)
    (make-char-set-eat-list
     (apply lset-union char=?
	    (char-set-rep cs0)
	    (map char-set-rep args))))))

(define char-set-union!
  (case-lambda
   (()
    char-set:empty)
   ((cs)
    cs)
   ((cs0 . args)
    (make-char-set-eat-list
     (apply lset-union! char=?
	    (char-set-rep cs0)
	    (map char-set-rep args))))))

;;; --------------------------------------------------------------------

(define char-set-intersection
  (case-lambda
   (()
    char-set:empty)
   ((cs)
    (char-set-copy cs))
   ((cs0 . args)
    (make-char-set-eat-list
     (apply lset-intersection char=?
	    (char-set-rep cs0)
	    (map char-set-rep args))))))

(define char-set-intersection!
  (case-lambda
   (()
    char-set:empty)
   ((cs)
    (char-set-copy cs))
   ((cs0 . args)
    (make-char-set-eat-list
     (apply lset-intersection! char=?
	    (char-set-rep cs0)
	    (map char-set-rep args))))))

;;; --------------------------------------------------------------------

(define char-set-difference
  (case-lambda
   (()
    char-set:empty)
   ((cs)
    (char-set-copy cs))
   ((cs0 . args)
    (make-char-set-eat-list
     (apply lset-difference char=?
	    (char-set-rep cs0)
	    (map char-set-rep args))))))

(define char-set-difference!
  (case-lambda
   (()
    char-set:empty)
   ((cs)
    (char-set-copy cs))
   ((cs0 . args)
    (make-char-set-eat-list
     (apply lset-difference! char=?
	    (char-set-rep cs0)
	    (map char-set-rep args))))))

;;; --------------------------------------------------------------------

(define char-set-xor
  (case-lambda
   (()
    char-set:empty)
   ((cs)
    (char-set-copy cs))
   ((cs0 . args)
    (make-char-set-eat-list
     (apply lset-xor char=?
	    (char-set-rep cs0)
	    (map char-set-rep args))))))

(define char-set-xor!
  (case-lambda
   (()
    char-set:empty)
   ((cs)
    (char-set-copy cs))
   ((cs0 . args)
    (make-char-set-eat-list
     (apply lset-xor! char=?
	    (char-set-rep cs0)
	    (map char-set-rep args))))))

;;; --------------------------------------------------------------------

(define char-set-diff+intersection
  (case-lambda
   (()
    char-set:empty)
   ((cs)
    (char-set-copy cs))
   ((cs0 . args)
    (receive (diff inter)
	(apply lset-diff+intersection char=?
	       (char-set-rep cs0)
	       (map char-set-rep args))
      (values (make-char-set-eat-list diff)
	      (make-char-set-eat-list inter))))))

(define char-set-diff+intersection!
  (case-lambda
   (()
    char-set:empty)
   ((cs)
    (char-set-copy cs))
   ((cs0 . args)
    (receive (diff inter)
	(apply lset-diff+intersection! char=?
	       (char-set-rep cs0)
	       (map char-set-rep args))
      (values (make-char-set-eat-list diff)
	      (make-char-set-eat-list inter))))))



;;;; predefined char sets

(define char-set:empty
  (char-set))

;; (define char-set:full
;;   (char-set-complement char-set:empty))

;; (define char-set:lower-case
;;   (let* ((a-z (ucs-range->char-set #x61 #x7B))
;; 	 (latin1 (ucs-range->char-set! #xdf #xf7  #t a-z))
;; 	 (latin2 (ucs-range->char-set! #xf8 #x100 #t latin1)))
;;     (char-set-append! latin2 (integer->char #xb5))))

;; (define char-set:upper-case
;;   (let ((A-Z (ucs-range->char-set #x41 #x5B)))
;;     ;; Add in the Latin-1 upper-case chars.
;;     (ucs-range->char-set! #xd8 #xdf #t
;; 			  (ucs-range->char-set! #xc0 #xd7 #t A-Z))))

(define char-set:title-case
  char-set:empty)

;; (define char-set:letter
;;   (let ((u/l (char-set-union char-set:upper-case char-set:lower-case)))
;;     (char-set-append! u/l
;; 		      (integer->char #xaa)	; FEMININE ORDINAL INDICATOR
;; 		      (integer->char #xba))))	; MASCULINE ORDINAL INDICATOR

(define char-set:digit
  (string->char-set "0123456789"))

(define char-set:hex-digit
  (string->char-set "0123456789abcdefABCDEF"))

;; (define char-set:letter+digit
;;   (char-set-union char-set:letter char-set:digit))

(define char-set:punctuation
  (make-char-set-eat-list
   (append-char-lists
    (string->list "!\"#%&'()*,-./:;?@[\\]_{}")
    (map integer->char '(#xA1 ; INVERTED EXCLAMATION MARK
			 #xAB ; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
			 #xAD ; SOFT HYPHEN
			 #xB7 ; MIDDLE DOT
			 #xBB ; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
			 #xBF))))) ; INVERTED QUESTION MARK

(define char-set:symbol
  (make-char-set-eat-list
   (append-char-lists (string->list "$+<=>^`|~")
		      (map integer->char '(#x00A2 ; CENT SIGN
					   #x00A3 ; POUND SIGN
					   #x00A4 ; CURRENCY SIGN
					   #x00A5 ; YEN SIGN
					   #x00A6 ; BROKEN BAR
					   #x00A7 ; SECTION SIGN
					   #x00A8 ; DIAERESIS
					   #x00A9 ; COPYRIGHT SIGN
					   #x00AC ; NOT SIGN
					   #x00AE ; REGISTERED SIGN
					   #x00AF ; MACRON
					   #x00B0 ; DEGREE SIGN
					   #x00B1 ; PLUS-MINUS SIGN
					   #x00B4 ; ACUTE ACCENT
					   #x00B6 ; PILCROW SIGN
					   #x00B8 ; CEDILLA
					   #x00D7 ; MULTIPLICATION SIGN
					   #x00F7))))) ; DIVISION SIGN

;; (define char-set:graphic
;;   (char-set-union char-set:letter+digit
;; 		  char-set:punctuation
;; 		  char-set:symbol))

(define char-set:whitespace
  (make-char-set-eat-list
   (map integer->char '(#x09		    ; HORIZONTAL TABULATION
			#x0A		    ; LINE FEED
			#x0B		    ; VERTICAL TABULATION
			#x0C		    ; FORM FEED
			#x0D		    ; CARRIAGE RETURN
			#x20		    ; SPACE
			#xA0))))

;; (define char-set:printing
;;   (char-set-union char-set:whitespace
;; 		  char-set:graphic)) ; NO-BREAK SPACE

(define char-set:blank
  (make-char-set-eat-list
   (map integer->char '(#x09			; HORIZONTAL TABULATION
			#x20			; SPACE
			#xA0))))		; NO-BREAK SPACE


;; (define char-set:iso-control
;;   (ucs-range->char-set! #x7F #xA0 #t (ucs-range->char-set 0 32)))

;; (define char-set:ascii
;;   (ucs-range->char-set 0 128))



;;;; done

)

;;; end of file
