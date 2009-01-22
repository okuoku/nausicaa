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
    char-set-copy char-set

    ;; predicates and assertions
    char-set? assert-list-of-chars

    ;; inspection
    char-set-size

    ;; conversion
    list->char-set	list->char-set!		char-set->list
    string->char-set	string->char-set!	char-set->string
    ucs-range->char-set	ucs-range->char-set!
    ->char-set

    ;; searching
    char-set-count char-set-contains?
    char-set-every char-set-any char-set-find

    ;; filtering
    char-set-filter	char-set-filter!
    char-set-partition	char-set-partition!
    char-set-remove
    char-set-remove*	char-set-remove*!

    ;; iteration
    char-set-cursor	char-set-cursor-ref
    end-of-char-set?	char-set-cursor-next
    char-set-fold	char-set-unfold		char-set-unfold!
    char-set-for-each	char-set-map

    ;; set operations
    char-set=?		char-set<=?

    ;; predefined
    char-set:lower-case  char-set:upper-case  char-set:title-case
    char-set:letter      char-set:digit       char-set:letter+digit
    char-set:graphic     char-set:printing    char-set:whitespace
    char-set:iso-control char-set:punctuation char-set:symbol
    char-set:hex-digit   char-set:blank       char-set:ascii
    char-set:empty       char-set:full)
  (import (scheme)
    (rnrs mutable-strings)
    (rnrs r5rs)
    (lists))



;;;; helpers

(define-syntax xor-char-lists
  (syntax-rules ()
    ((_ ?ell0 ?ell ...)
     (lset-xor char=? ?ell0 ?ell ...))))

(define-syntax xor-char-lists!
  (syntax-rules ()
    ((_ ?ell0 ?ell ...)
     (lset-xor! char=? ?ell0 ?ell ...))))

(define-syntax make-char-set-eat-list
  (syntax-rules ()
    ((_ ?expr)
     (let ((l ?expr))
       (make-char-set l (length l))))))

;;; --------------------------------------------------------------------

(define-syntax check-arg
  (syntax-rules ()
    ((_ ?pred ?val ?caller)
     (unless (?pred ?val)
       (error ?caller (quote ?val))))))

;;;Parse, type-check & default a final optional BASE-CS parameter from a
;;;rest argument. Return  a *fresh copy* of the  underlying string.  The
;;;default is the empty set.
(define (%default-base base-cs proc-name)
  (if base-cs
      (if (char-set? base-cs)
	  (string-copy (char-set-rep base-cs))
	(assertion-violation proc-name
	  "BASE-CS parameter not a char-set" base-cs))
    (make-string 256 (integer->char 0))))

;;;These  internal  functions  hide  a  lot of  the  dependency  on  the
;;;underlying  string  representation  of  char sets.   They  should  be
;;;inlined if possible.
(define (si=0? s i) (zero? (char->integer (string-ref s i))))
(define (si=1? s i) (not (si=0? s i)))
(define c0 (integer->char 0))
(define c1 (integer->char 1))
(define (si s i) (char->integer (string-ref s i)))
(define (%set0! s i) (string-set! s i c0))
(define (%set1! s i) (string-set! s i c1))

;;; These do various "s[i] := s[i] op val" operations -- see
;;; %CHAR-SET-ALGEBRA. They are used to implement the various
;;; set-algebra procedures.
(define (setv!   s i v) (string-set! s i (integer->char v))) ; SET to a Value.
(define (%not!   s i v) (setv! s i (- 1 v)))
(define (%and!   s i v) (if (zero? v) (%set0! s i)))
(define (%or!    s i v) (if (not (zero? v)) (%set1! s i)))
(define (%minus! s i v) (if (not (zero? v)) (%set0! s i)))
(define (%xor!   s i v) (if (not (zero? v)) (setv! s i (- 1 (si s i)))))


;;;; record definition and constructors

(define-record-type (char-set-record make-char-set char-set?)
  (fields (immutable list	char-set-rep)
	  (immutable length	char-set-size)))

(define (char-set . chars)
  (make-char-set-eat-list (delete-duplicates chars char=?)))

(define (char-set-copy cs)
  (make-char-set (list-copy (char-set-rep cs))
		 (char-set-size cs)))


;;;; predicates and assertions

(define (assert-list-of-chars chars func-name)
  (for-each
      (lambda (ch)
	(unless (char? ch)
	  (assertion-violation func-name)))
    chars))


;;;; conversion

(define list->char-set
  (case-lambda
   ((chars)
    (make-char-set-eat-list (list-copy chars)))
   ((chars base-cs)
    (make-char-set-eat-list (xor-char-lists chars (char-set-rep base-cs))))))

(define (list->char-set! chars base-cs)
  (make-char-set-eat-list (xor-char-lists! chars (char-set-rep base-cs))))

(define (char-set->list cs)
  (list-copy (char-set-rep cs)))

;;; --------------------------------------------------------------------

(define string->char-set
  (case-lambda
   ((str)
    (make-char-set (string->list str) (string-length str)))
   ((str base-cs)
    (make-char-set-eat-list
     (xor-char-lists (string->list str)
		     (char-set-rep base-cs))))))

(define (string->char-set! str base-cs)
  (make-char-set-eat-list
   (xor-char-lists! (string->list str)
		    (char-set-rep base-cs))))

(define (char-set->string cs)
  (list->string (char-set-rep cs)))

;;; --------------------------------------------------------------------

(define ucs-range->char-set
  (case-lambda
   ((lower upper)
    (raise-unimplemented-error 'ucs-range->char-set))
   ((lower upper base-cs)
    (raise-unimplemented-error 'ucs-range->char-set))))

(define (ucs-range->char-set! lower upper base-cs)
  (raise-unimplemented-error 'ucs-range->char-set!))

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
  (member* char (char-set-rep cs) char=?))

(define (char-set-count pred cset)
  (count pred (char-set-rep cs)))

(define (char-set-every pred cs)
  (every pred (char-set-rep cs)))

(define (char-set-any pred cs)
  (any pred (char-set-rep cs)))

(define (char-set-find pred cs)
  (find pred (char-set-rep cs)))



;;;; filtering

(define char-set-filter
  (case-lambda
   ((pred cs)
    (make-char-set-eat-list
     (filter pred (char-set-rep cs))))
   ((pred cs base-cs)
    (make-char-set-eat-list
     (filter pred
       (xor-char-lists (char-set-rep cs)
		       (char-set-rep base-cs)))))))

(define (char-set-filter! pred cs base-cs)
  (make-char-set-eat-list
   (filter! pred (xor-char-lists! (char-set-rep cs)
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
   ((stop? map-to-result seed-step seed)
    (make-char-set-eat-list
     (unfold stop? map-to-result seed-step seed)))
   ((stop? map-to-result seed-step seed base-cs)
    (make-char-set-eat-list
     (xor-char-lists (unfold stop? map-to-result seed-step seed)
		     (char-set-rep base-cs))))))

(define char-set-unfold!
  (case-lambda
   ((stop? map-to-result seed-step seed)
    (make-char-set-eat-list
     (unfold! stop? map-to-result seed-step seed)))
   ((stop? map-to-result seed-step seed base-cs)
    (make-char-set-eat-list
     (xor-char-lists! (unfold! stop? map-to-result seed-step seed)
		      (char-set-rep base-cs))))))


;;;; set operations

(define char-set=?
  (case-lambda
   (()
    #t)
   ((cs)
    #t)
   ((cs0 . args)
    (apply list=? (char-set-rep cs0) (map char-set-rep args)))))

(define (char-set<=? . rest)
  (case-lambda
   (()
    #t)
   ((cs)
    #t)
   ((cs0 . args)
    (apply list<=? (char-set-rep cs0) (map char-set-rep args)))))


;;;; predefined char sets

(define char-set:empty
  (char-set))

(define char-set:full
  (char-set-complement char-set:empty))

(define char-set:lower-case
  (let* ((a-z (ucs-range->char-set #x61 #x7B))
	 (latin1 (ucs-range->char-set! #xdf #xf7  #t a-z))
	 (latin2 (ucs-range->char-set! #xf8 #x100 #t latin1)))
    (char-set-adjoin! latin2 (integer->char #xb5))))

(define char-set:upper-case
  (let ((A-Z (ucs-range->char-set #x41 #x5B)))
    ;; Add in the Latin-1 upper-case chars.
    (ucs-range->char-set! #xd8 #xdf #t
			  (ucs-range->char-set! #xc0 #xd7 #t A-Z))))

(define char-set:title-case char-set:empty)

(define char-set:letter
  (let ((u/l (char-set-union char-set:upper-case char-set:lower-case)))
    (char-set-adjoin! u/l
		      (integer->char #xaa)	; FEMININE ORDINAL INDICATOR
		      (integer->char #xba))))	; MASCULINE ORDINAL INDICATOR

(define char-set:digit     (string->char-set "0123456789"))
(define char-set:hex-digit (string->char-set "0123456789abcdefABCDEF"))

(define char-set:letter+digit
  (char-set-union char-set:letter char-set:digit))

(define char-set:punctuation
  (make-char-set-eat-list
   (xor-char-lists (string->char-set "!\"#%&'()*,-./:;?@[\\]_{}")
		   (map integer->char '(#xA1 ; INVERTED EXCLAMATION MARK
					#xAB ; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
					#xAD ; SOFT HYPHEN
					#xB7 ; MIDDLE DOT
					#xBB ; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
					#xBF))))) ; INVERTED QUESTION MARK

(define char-set:symbol
  (make-char-set-eat-list
   (xor-char-lists (string->char-set "$+<=>^`|~")
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

(define char-set:graphic
  (char-set-union char-set:letter+digit
		  char-set:punctuation
		  char-set:symbol))

(define char-set:whitespace
  (make-char-set-eat-list
   (map integer->char '(#x09		    ; HORIZONTAL TABULATION
			#x0A		    ; LINE FEED
			#x0B		    ; VERTICAL TABULATION
			#x0C		    ; FORM FEED
			#x0D		    ; CARRIAGE RETURN
			#x20		    ; SPACE
			#xA0))))

(define char-set:printing
  (char-set-union char-set:whitespace
		  char-set:graphic)) ; NO-BREAK SPACE

(define char-set:blank
  (make-char-set-eat-list
   (map integer->char '(#x09			; HORIZONTAL TABULATION
			#x20			; SPACE
			#xA0))))		; NO-BREAK SPACE


(define char-set:iso-control
  (ucs-range->char-set! #x7F #xA0 #t (ucs-range->char-set 0 32)))

(define char-set:ascii
  (ucs-range->char-set 0 128))


;;;; done

)

;;; end of file
