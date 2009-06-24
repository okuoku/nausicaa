;;
;; Part of: Nausicaa
;; Contents: char-sets library
;; Date: Fri Jun 12, 2009
;;
;; Abstract
;;
;;
;;
;; Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
;;
;; This program is free software:  you can redistribute it and/or modify
;; it under the terms of the  GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is  distributed in the hope that it  will be useful, but
;; WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;; MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;; General Public License for more details.
;;
;; You should  have received  a copy of  the GNU General  Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;



#!r6rs
(library (char-sets)
  (export

    ;; bounds
    char-set-lower-bound char-set-upper-bound
    char-set-inner-upper-bound char-set-inner-lower-bound

    ;; constructors
    (rename (true-char-set char-set)) char-set-copy
    char-set-add char-set-add!

    ;; inspection
    char-set-size (rename (domain-ref char-set-domain-ref))
    char-set-write

    ;; predicates
    (rename (true-char-set? char-set?))
    char-set-empty? char-set-contains?
    char-set=? char-set<? char-set-subset? char-set-strict-subset?

    ;; set operations
    char-set-intersection char-set-union
    char-set-difference char-set-complement

    ;; string operations
    string->char-set

    ;; list operations
    char-set-for-each char-set-every
    char-set-any char-set-fold
    char-set->list

    ;; predefined
    char-set:empty       char-set:full

    char-set:ascii
    char-set:ascii/dec-digit	(rename (char-set:ascii/dec-digit char-set:ascii/digit))
    char-set:ascii/oct-digit	char-set:ascii/hex-digit
    char-set:ascii/lower-case	char-set:ascii/upper-case
    char-set:ascii/letter	char-set:ascii/letter+digit
    char-set:ascii/punctuation	char-set:ascii/symbol
    char-set:ascii/control	char-set:ascii/whitespace
    char-set:ascii/graphic	char-set:ascii/printable
    char-set:ascii/blank

;;     char-set:lower-case  char-set:upper-case  char-set:title-case
;;     char-set:letter      char-set:dec-digit   char-set:letter+digit
;;     char-set:graphic     char-set:printing    char-set:whitespace
;;     char-set:iso-control char-set:punctuation char-set:symbol
;;     char-set:hex-digit   char-set:blank
    )
  (import (rnrs)
    (one-dimension-cc))



(define-record-type char-set
  (fields (mutable domain domain-ref domain-set!)))

(define (true-char-set . args)
  (make-char-set (apply make-domain args)))

(define (true-char-set? cs)
  (and (char-set? cs)
       (domain? (domain-ref cs))))

(define (char-set-copy cs)
  (make-char-set (domain-copy (domain-ref cs))))

(define (char-set-add cs obj)
  (cond ((char? obj)
	 (make-char-set (domain-add-item (domain-ref cs) obj)))
	((range? obj)
	 (make-char-set (domain-add-range (domain-ref cs) obj)))
	(else
	 (assertion-violation 'char-set-add
	   "attempt to add an invalid object to a char-set" obj))))

(define (char-set-add! cs obj)
  (domain-set! cs (char-set-add cs obj)))

(define (char-set-size cs)
  (domain-size (domain-ref cs)))

(define (char-set-empty? cs)
  (domain-empty? (domain-ref cs)))

(define (char-set-contains? cs item)
  (domain-contains? (domain-ref cs) item))

(define (char-set=? cs-a cs-b)
  (domain=? (domain-ref cs-a) (domain-ref cs-b)))

(define (char-set<? cs-a cs-b)
  (domain<? (domain-ref cs-a) (domain-ref cs-b)))

(define (char-set-subset? cs . cs-args)
  (let loop ((domain      (domain-ref cs))
	     (domain-args (map domain-ref cs-args)))
    (or (null? domain-args)
	(let ((next-domain (car domain-args)))
	  (and (domain-subset? domain next-domain)
	       (loop next-domain (cdr domain-args)))))))

(define (char-set-strict-subset? cs . cs-args)
  (let loop ((domain      (domain-ref cs))
	     (domain-args (map domain-ref cs-args)))
    (or (null? domain-args)
	(let ((next-domain (car domain-args)))
	  (and (domain-strict-subset? domain next-domain)
	       (loop next-domain (cdr domain-args)))))))

(define (char-set-intersection cs . cs-args)
  (make-char-set (fold-left (lambda (domain domain-prev)
			      (domain-intersection domain domain-prev))
			    (domain-ref cs)
			    (map domain-ref cs-args))))

(define (char-set-union cs . cs-args)
  (make-char-set (fold-left (lambda (domain domain-prev)
			      (domain-union domain domain-prev))
			    (domain-ref cs)
			    (map domain-ref cs-args))))

(define (char-set-difference cs . cs-args)
  (make-char-set (fold-left (lambda (domain domain-prev)
			      (domain-difference domain domain-prev))
			    (domain-ref cs)
			    (map domain-ref cs-args))))

(define char-set-complement
  (case-lambda
   ((cs)
    (char-set-complement cs char-set:full))
   ((cs universe)
    (make-char-set (domain-complement (domain-ref cs) (domain-ref universe))))))

(define (char-set-for-each proc cs)
  (domain-for-each proc (domain-ref cs)))

(define (char-set-every proc cs)
  (domain-every proc (domain-ref cs)))

(define (char-set-any proc cs)
  (domain-any proc (domain-ref cs)))

(define (char-set-fold kons knil cs)
  (domain-fold kons knil (domain-ref cs)))

(define (char-set->list cs)
  (domain->list (domain-ref cs)))

(define (string->char-set str)
  (make-char-set (string->domain str)))

(define char-set-write
  (case-lambda
   ((cs)
    (char-set-write cs (current-output-port)))
   ((cs port)
    (display "(char-set " port)
    (for-each (lambda (range)
		(display (string-append
			  "'("
			  "#\\x" (number->string (char->integer (car range)) 16)
			  " . "
			  "#\\x" (number->string (char->integer (cdr range)) 16)
			  ") ") port))
      (domain-ref cs))
    (display #\) port))))



(define inclusive-lower-bound		0)
(define exclusive-inner-upper-bound	#xD800)
(define exclusive-inner-lower-bound	#xDFFF)
(define inclusive-upper-bound		#x10FFFF)

(define char-set-lower-bound		(integer->char inclusive-lower-bound))
(define char-set-inner-upper-bound	(integer->char (- exclusive-inner-upper-bound 1)))
(define char-set-inner-lower-bound	(integer->char (+ 1 exclusive-inner-lower-bound)))
(define char-set-upper-bound		(integer->char inclusive-upper-bound))

(define (number-in-range? x)
  (or (and (<= inclusive-lower-bound x)
	   (<  x exclusive-inner-upper-bound))
      (and (<  exclusive-inner-lower-bound x)
	   (<= x inclusive-upper-bound))))

(define char-type
  (%make-type-descriptor char? char=? char<? char<=?
			 (lambda (a b) (if (char<? a b) a b)) ; min
			 (lambda (a b) (if (char<? a b) b a)) ; max
			 (lambda (ch range) ; item-prev
			   (let* ((x  (- (char->integer ch) 1)))
			     (and (number-in-range? x)
				  (let ((ch (integer->char x)))
				    (if range
					(and (<= (char->integer (car range)) x)
					     ch)
				      ch)))))
			 (lambda (ch range) ; item-next
			   (let* ((x  (+ 1 (char->integer ch))))
			     (and (number-in-range? x)
				  (let ((ch (integer->char x)))
				    (if range
					(and (<= x (char->integer (cdr range)))
					     ch)
				      ch)))))
			 (lambda (a b) (+ 1 (- (char->integer a) (char->integer b)))) ; char-minus
			 (lambda (ch) ch))) ; char-copy


;;;; domain wrappers for ranges

(define (make-range a b)
  (%make-range char-type a b))

(define (range-copy a)
  (%range-copy char-type a))

(define (range? a)
  (%range? char-type a))

(define (range-contains? range obj)
  (%range-contains? char-type range obj))

(define (range-length range)
  (%range-length char-type range))

(define (range=? range-a range-b)
  (%range=? char-type range-a range-b))

(define (range<? range-a range-b)
  (%range<? char-type range-a range-b))

(define (range<=? range-a range-b)
  (%range<=? char-type range-a range-b))

(define (range-contiguous? range-a range-b)
  (%range-contiguous? char-type range-a range-b))

(define (range-subset? range-a range-b)
  (%range-subset? char-type range-a range-b))

(define (range-strict-subset? range-a range-b)
  (%range-strict-subset? char-type range-a range-b))

(define (range-start<? range-a range-b)
  (%range-start<? char-type range-a range-b))

(define (range-start<=? range-a range-b)
  (%range-start<=? char-type range-a range-b))

(define (range-overlapping? range-a range-b)
  (%range-overlapping? char-type range-a range-b))

(define (range-concatenate range-a range-b)
  (%range-concatenate char-type range-a range-b))

(define (range-intersection range-a range-b)
  (%range-intersection char-type range-a range-b))

(define (range-union range-a range-b)
  (%range-union char-type range-a range-b))

(define (range-difference range-a range-b)
  (%range-difference char-type range-a range-b))

(define (range-for-each proc range)
  (%range-for-each char-type proc range))

(define (range-every proc range)
  (%range-every char-type proc range))

(define (range-any proc range)
  (%range-any char-type proc range))

(define (range-fold kons knil range)
  (%range-fold char-type kons knil range))

(define (range->list range)
  (%range->list char-type range))


;;;; domain wrappers for characters

(define (make-domain . args)
  (apply %make-domain char-type args))

(define (domain-copy domain)
  (%domain-copy char-type domain))

(define (domain-add-item domain obj)
  (%domain-add-item char-type domain obj))

(define (domain-add-range domain new-range)
  (%domain-add-range char-type domain new-range))

(define (domain? domain)
  (%domain? char-type domain))

(define (domain-size domain)
  (%domain-size char-type domain))

(define domain-empty? %domain-empty?)

(define (domain-contains? domain obj)
  (%domain-contains? char-type domain obj))

(define (domain=? domain-a domain-b)
  (%domain=? char-type domain-a domain-b))

(define (domain<? domain-a domain-b)
  (%domain<? char-type domain-a domain-b))

(define (domain-subset? domain-a domain-b)
  (%domain-subset? char-type domain-a domain-b))

(define (domain-strict-subset? domain-a domain-b)
  (%domain-strict-subset? char-type domain-a domain-b))

(define (domain-intersection domain-a domain-b)
  (%domain-intersection char-type domain-a domain-b))

(define (domain-union domain-a domain-b)
  (%domain-union char-type domain-a domain-b))

(define (domain-difference domain-a domain-b)
  (%domain-difference char-type domain-a domain-b))

(define (domain-complement domain universe)
  (%domain-complement char-type domain universe))

(define (domain-for-each proc domain)
  (%domain-for-each char-type proc domain))

(define (domain-every proc domain)
  (%domain-every char-type proc domain))

(define (domain-any proc domain)
  (%domain-any char-type proc domain))

(define (domain-fold kons knil domain)
  (%domain-fold char-type kons knil domain))

(define (domain->list domain)
  (%domain->list char-type domain))

(define (string->domain str)
  (apply make-domain (string->list str)))


;;;; basic predefined char sets

(define char-set:empty (make-char-set '()))

(define char-set:full
  (true-char-set `(,char-set-lower-bound . ,char-set-inner-upper-bound)
		 `(,char-set-inner-lower-bound . ,char-set-upper-bound)))


;;;; ASCII predefined char sets

(define char-set:ascii
  ;;Notice  that ASCII  has numeric  codes in  the range  [0,  127]; the
  ;;numeric code 127 is included, and the number of codes is 128.
  (true-char-set '(#\x0 . #\x127)))

(define char-set:ascii/dec-digit
  (true-char-set '(#\0 . #\9)))

(define char-set:ascii/oct-digit
  (true-char-set '(#\0 . #\7)))

(define char-set:ascii/hex-digit
  (true-char-set '(#\0 . #\9) '(#\a . #\f) '(#\A . #\F)))

(define char-set:ascii/lower-case
  (true-char-set '(#\a . #\z)))

(define char-set:ascii/upper-case
  (true-char-set '(#\A . #\Z)))

(define char-set:ascii/letter
  (char-set-union char-set:ascii/lower-case char-set:ascii/upper-case))

(define char-set:ascii/letter+digit
  (char-set-union char-set:ascii/letter char-set:ascii/dec-digit))

(define char-set:ascii/punctuation
  ;;Yes I have verified that all of these have numeric code in the range
  ;;[0, 127] (Marco Maggi, Tue Jun 23, 2009).
  (true-char-set #\! #\" #\# #\% #\& #\' #\( #\) #\* #\, #\- #\.
		 #\/ #\: #\; #\? #\@ #\[ #\\ #\] #\_ #\{ #\}))

(define char-set:ascii/symbol
  ;;Yes I have verified that all of these have numeric code in the range
  ;;[0, 127] (Marco Maggi, Tue Jun 23, 2009).
  (true-char-set #\$ #\+ #\< #\= #\> #\^ #\` #\| #\~))

(define char-set:ascii/control
  ;;Notice that control characters are the ones whose numeric code is in
  ;;the range [0, 31] plus 127; the number of control characters is 33.
  (true-char-set '(#\x0 . #\x31) (integer->char 127)))

(define char-set:ascii/whitespace
  (true-char-set #\x0009		  ; HORIZONTAL TABULATION
		 #\x000A		  ; LINE FEED
		 #\x000B		  ; VERTICAL TABULATION
		 #\x000C		  ; FORM FEED
		 #\x000D		  ; CARRIAGE RETURN
		 #\x0020))		  ; SPACE

(define char-set:ascii/blank
  (true-char-set #\tab #\space))

(define char-set:ascii/graphic
  (char-set-union char-set:ascii/letter+digit
		  char-set:ascii/punctuation
		  char-set:ascii/symbol))

(define char-set:ascii/printable
  (char-set-union char-set:ascii/whitespace
		  char-set:ascii/graphic)) ; NO-BREAK SPACE



(define char-set:lower-case
  (true-char-set '(#\x0061 . #\x007A)
		 '(#\x00DF . #\x00F6)
		 '(#\x00F8 . #\x00FF)))
  ; #\x00B5 this is the
  ;
  ; 00B5;MICRO SIGN;Ll;0;L;<compat> 03BC;;;;N;;;039C;;039C

(define char-set:upper-case
  (true-char-set '(#\x0041 . #\x005B) ; A-Z
		 #\x00D8
		 #\x00DF
		 '(#\x00C0 . #\x00D7)))

(define char-set:title-case
  char-set:empty)

;; (define char-set:letter
;;   (let ((cs (char-set-union char-set:upper-case char-set:lower-case)))
;;     (char-set-add (char-set-add cs (N #\xaa)) ; FEMININE ORDINAL INDICATOR
;; 		  (N #\xba)))) ; MASCULINE ORDINAL INDICATOR

(define char-set:letter+digit #f)

(define char-set:punctuation
  (true-char-set
   #\! #\" #\# #\% #\& #\' #\( #\) #\* #\, #\- #\.
   #\/ #\: #\; #\? #\@ #\[ #\\ #\] #\_ #\{ #\}
   #\x00A1	     ; INVERTED EXCLAMATION MARK
   #\x00AB	     ; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
   #\x00AD	     ; SOFT HYPHEN
   #\x00B7	     ; MIDDLE DOT
   #\x00BB	     ; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
   #\x00BF))	     ; INVERTED QUESTION MARK

(define char-set:symbol
  (true-char-set
   #\$ #\+ #\< #\= #\> #\^ #\` #\| #\~
   #\x00A2	     ; CENT SIGN
   #\x00A3	     ; POUND SIGN
   #\x00A4	     ; CURRENCY SIGN
   #\x00A5	     ; YEN SIGN
   #\x00A6	     ; BROKEN BAR
   #\x00A7	     ; SECTION SIGN
   #\x00A8	     ; DIAERESIS
   #\x00A9	     ; COPYRIGHT SIGN
   #\x00AC	     ; NOT SIGN
   #\x00AE	     ; REGISTERED SIGN
   #\x00AF	     ; MACRON
   #\x00B0	     ; DEGREE SIGN
   #\x00B1	     ; PLUS-MINUS SIGN
   #\x00B4	     ; ACUTE ACCENT
   #\x00B6	     ; PILCROW SIGN
   #\x00B8	     ; CEDILLA
   #\x00D7	     ; MULTIPLICATION SIGN
   #\x00F7))	     ; DIVISION SIGN

(define char-set:whitespace
  (true-char-set #\x0009		; HORIZONTAL TABULATION
		 #\x000A		; LINE FEED
		 #\x000B		; VERTICAL TABULATION
		 #\x000C		; FORM FEED
		 #\x000D		; CARRIAGE RETURN
		 #\x0020		; SPACE
		 #\x00A0))

(define char-set:blank
  (true-char-set #\x0009   ; HORIZONTAL TABULATION
		 #\x0020   ; SPACE
		 #\x00A0)) ; NO-BREAK SPACE

(define char-set:iso-control
  (true-char-set '(#\x0 . #\x32) #\x007F #\x00A0))


;;;; done

)

;;; end of file
