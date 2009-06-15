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

    ;; constructors
    (rename (full-char-set char-set)) char-set-copy
    char-set-add char-set-add!

    ;; inspection
    char-set-size (rename (domain-ref char-set-domain-ref))

    ;; predicates
    (rename (full-char-set? char-set?))
    char-set-empty? char-set-contains?
    char-set=? char-set<? char-set-subset? char-set-strict-subset?

    ;; set operations
    char-set-intersection char-set-union
    char-set-difference char-set-complement

    ;; list operations
    char-set-for-each char-set-every
    char-set-any char-set-fold
    char-set->list string->char-set

    ;; predefined
;;     char-set:lower-case  char-set:upper-case  char-set:title-case
;;     char-set:letter      char-set:digit       char-set:letter+digit
;;     char-set:graphic     char-set:printing    char-set:whitespace
;;     char-set:iso-control char-set:punctuation char-set:symbol
;;     char-set:hex-digit   char-set:blank       char-set:ascii
;;     char-set:empty       char-set:full
    )
  (import (rnrs)
    (one-dimension))



(define-record-type char-set
  (fields (mutable domain domain-ref domain-set!)))

(define (full-char-set . args)
  (make-char-set (apply make-domain args)))

(define (full-char-set? cs)
  (and (char-set? cs)
       (domain? (domain-ref cs))))

(define (char-set-copy cs)
  (make-char-set (domain-copy (domain-ref cs))))

(define (char-set-add cs obj)
  (cond ((char? obj)
	 (domain-add-item (domain-ref cs) obj))
	((range? obj)
	 (domain-add-range (domain-ref cs) obj))
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

(define (char-set-subset? cs-a cs-b)
  (domain-subset? (domain-ref cs-a) (domain-ref cs-b)))

(define (char-set-strict-subset? cs-a cs-b)
  (domain-strict-subset? (domain-ref cs-a) (domain-ref cs-b)))

(define (char-set-intersection cs-a cs-b)
  (make-char-set (domain-intersection (domain-ref cs-a) (domain-ref cs-b))))

(define (char-set-union cs-a cs-b)
  (make-char-set (domain-union (domain-ref cs-a) (domain-ref cs-b))))

(define (char-set-difference cs-a cs-b)
  (make-char-set (domain-difference (domain-ref cs-a) (domain-ref cs-b))))

(define (char-set-complement cs)
  (make-char-set (domain-complement (domain-ref cs))))

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



(define inclusive-lower-bound (integer->char 0))

;;*FIXME* This  is a bug.  The  correct exclusive upper  bound should be
;;1+#x10FFFF.   With this  setting  the last  character  in the  Unicode
;;encoding (#x10FFFF) is excluded from the char sets.
(define exclusive-upper-bound (integer->char #x10FFFF))

(define (char-next item)
  (integer->char (+ 1 (char->integer item))))

(define (char- past start)
  (- (char->integer past)
     (char->integer start)))

(define (char-copy ch)
  ch)


;;;; domain wrappers for ranges

(define (make-range a b)
  (%make-range a b char? char<?))

(define (range-copy a)
  (%range-copy a char-copy))

(define (range? a)
  (%range? a char? char<?))

(define (range-contains? range obj)
  (%range-contains? range obj char<? char<=?))

(define (range-length range)
  (%range-length range char-))

(define (range=? range-a range-b)
  (%range=? range-a range-b char=?))

(define (range<? range-a range-b)
  (%range<? range-a range-b char<=?))

(define (range<=? range-a range-b)
  (%range<=? range-a range-b char<=?))

(define (range-contiguous? range-a range-b)
  (%range-contiguous? range-a range-b char=?))

(define (range-subset? range-a range-b)
  (%range-subset? range-a range-b char<=?))

(define (range-strict-subset? range-a range-b)
  (%range-strict-subset? range-a range-b char<=?))

(define (range-start<? range-a range-b)
  (%range-start<? range-a range-b char<?))

(define (range-start<=? range-a range-b)
  (%range-start<=? range-a range-b char<=?))

(define (range-overlapping? range-a range-b)
  (%range-overlapping? range-a range-b char<? char<=?))

(define (range-concatenate range-a range-b)
  (%range-concatenate range-a range-b char<?))

(define (range-intersection range-a range-b)
  (%range-intersection range-a range-b char<? char<=?))

(define (range-union range-a range-b)
  (%range-union range-a range-b char=? char<? char<=?))

(define (range-difference range-a range-b)
  (%range-difference range-a range-b char=? char<?))

(define (range-for-each proc range)
  (%range-for-each proc range char<=? char-next))

(define (range-every proc range)
  (%range-every proc range char<? char-next))

(define (range-any proc range)
  (%range-any proc range char<? char-next))

(define (range-fold kons knil range)
  (%range-fold kons knil range char<? char-next))

(define (range->list range)
  (%range->list range char<? char-next))


;;;; domain wrappers for characters

(define (make-domain . args)
  (apply %make-domain char? char=? char<? char<=? char-next args))

(define (domain-copy domain)
  (%domain-copy domain char-copy))

(define (domain-add-item domain obj)
  (%domain-add-item domain obj char=? char<? char<=? char-next))

(define (domain-add-range domain new-range)
  (%domain-add-range domain new-range char=? char<? char<=?))

(define (domain? domain)
  (%domain? domain char? char<? char<=? inclusive-lower-bound exclusive-upper-bound))

(define (domain-size domain)
  (%domain-size domain char-))

(define domain-empty? %domain-empty?)

(define (domain-contains? domain obj)
  (%domain-contains? domain obj char<? char<=?))

(define (domain=? domain-a domain-b)
  (%domain=? domain-a domain-b char=?))

(define (domain<? domain-a domain-b)
  (%domain<? domain-a domain-b char<=?))

(define (domain-subset? domain-a domain-b)
  (%domain-subset? domain-a domain-b char<=?))

(define (domain-strict-subset? domain-a domain-b)
  (%domain-strict-subset? domain-a domain-b char<=?))

(define (domain-intersection domain-a domain-b)
  (%domain-intersection domain-a domain-b char=? char<? char<=?))

(define (domain-union domain-a domain-b)
  (%domain-union domain-a domain-b char=? char<? char<=?))

(define (domain-difference domain-a domain-b)
  (%domain-difference domain-a domain-b char=? char<? char<=?))

(define (domain-complement domain)
  (%domain-complement domain char=? char<? char<=?
		      inclusive-lower-bound exclusive-upper-bound))

(define (domain-for-each proc domain)
  (%domain-for-each proc domain char<=? char-next))

(define (domain-every proc domain)
  (%domain-every proc domain char<? char-next))

(define (domain-any proc domain)
  (%domain-any proc domain char<? char-next))

(define (domain-fold kons knil domain)
  (%domain-fold kons knil domain char<? char-next))

(define (domain->list domain)
  (%domain->list domain char<? char-next))

(define (string->domain str)
  (apply make-domain (string->list str)))


;;;; predefined char sets

(define char-set:empty '())

;;(define char-set:full (list (cons )))

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

;; (define char-set:punctuation
;;   (make-char-set-eat-list
;;    (append-char-lists
;;     (string->list "!\"#%&'()*,-./:;?@[\\]_{}")
;;     (map integer->char '(#xA1 ; INVERTED EXCLAMATION MARK
;; 			 #xAB ; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
;; 			 #xAD ; SOFT HYPHEN
;; 			 #xB7 ; MIDDLE DOT
;; 			 #xBB ; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
;; 			 #xBF))))) ; INVERTED QUESTION MARK

;; (define char-set:symbol
;;   (make-char-set-eat-list
;;    (append-char-lists (string->list "$+<=>^`|~")
;; 		      (map integer->char '(#x00A2 ; CENT SIGN
;; 					   #x00A3 ; POUND SIGN
;; 					   #x00A4 ; CURRENCY SIGN
;; 					   #x00A5 ; YEN SIGN
;; 					   #x00A6 ; BROKEN BAR
;; 					   #x00A7 ; SECTION SIGN
;; 					   #x00A8 ; DIAERESIS
;; 					   #x00A9 ; COPYRIGHT SIGN
;; 					   #x00AC ; NOT SIGN
;; 					   #x00AE ; REGISTERED SIGN
;; 					   #x00AF ; MACRON
;; 					   #x00B0 ; DEGREE SIGN
;; 					   #x00B1 ; PLUS-MINUS SIGN
;; 					   #x00B4 ; ACUTE ACCENT
;; 					   #x00B6 ; PILCROW SIGN
;; 					   #x00B8 ; CEDILLA
;; 					   #x00D7 ; MULTIPLICATION SIGN
;; 					   #x00F7))))) ; DIVISION SIGN

;; (define char-set:graphic
;;   (char-set-union char-set:letter+digit
;; 		  char-set:punctuation
;; 		  char-set:symbol))

;; (define char-set:whitespace
;;   (make-char-set-eat-list
;;    (map integer->char '(#x09		    ; HORIZONTAL TABULATION
;; 			#x0A		    ; LINE FEED
;; 			#x0B		    ; VERTICAL TABULATION
;; 			#x0C		    ; FORM FEED
;; 			#x0D		    ; CARRIAGE RETURN
;; 			#x20		    ; SPACE
;; 			#xA0))))

;; (define char-set:printing
;;   (char-set-union char-set:whitespace
;; 		  char-set:graphic)) ; NO-BREAK SPACE

;; (define char-set:blank
;;   (make-char-set-eat-list
;;    (map integer->char '(#x09			; HORIZONTAL TABULATION
;; 			#x20			; SPACE
;; 			#xA0))))		; NO-BREAK SPACE


;; (define char-set:iso-control
;;   (ucs-range->char-set! #x7F #xA0 #t (ucs-range->char-set 0 32)))

;; (define char-set:ascii
;;   (ucs-range->char-set 0 128))



;;;; done

)

;;; end of file
