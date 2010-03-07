;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for base64 ASCII armor of bytevectors
;;;Date: Sun Mar  7, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


(import (nausicaa)
  (armor base64)
  (armor conditions)
  (checks)
  (parameters))

(check-set-mode! 'report-failed)
(display "*** testing ASCII armor base64\n")


;;;; helpers

(define (subbytevector src start past)
  (let ((dst (make-bytevector (- past start))))
    (do ((i 0 (+ 1 i))
	 (j start (+ 1 j)))
	((= j past)
	 dst)
      (bytevector-u8-set! dst i (bytevector-u8-ref src j)))))


;;;; parameters

(define encoding
  (make-parameter #t))

(define padding?
  (make-parameter #t))


;;;; base64 test vectors

(define test-vectors
  ;; binary			encoded padded		encoded non-padded
  '((""				""			"")
    ("this is a test\n"		"dGhpcyBpcyBhIHRlc3QK"	"dGhpcyBpcyBhIHRlc3QK")

    ("The short red fox ran quickly through the green field and jumped over the tall brown bear\n"
     "VGhlIHNob3J0IHJlZCBmb3ggcmFuIHF1aWNrbHkgdGhyb3VnaCB0aGUgZ3JlZW4gZmllbGQgYW5kIGp1bXBlZCBvdmVyIHRoZSB0YWxsIGJyb3duIGJlYXIK"
     "VGhlIHNob3J0IHJlZCBmb3ggcmFuIHF1aWNrbHkgdGhyb3VnaCB0aGUgZ3JlZW4gZmllbGQgYW5kIGp1bXBlZCBvdmVyIHRoZSB0YWxsIGJyb3duIGJlYXIK")

    ("The short red fox ran quickly through the green field and jumped over the tall brown bear\n"
     "VGhlIHNob3J0IHJlZCBmb3ggcmFuIHF1aWNrbHkgdGhyb3VnaCB0aGUgZ3JlZW4gZmllbGQgYW5kIGp1bXBlZCBvdmVyIHRoZSB0YWxsIGJyb3duIGJlYXIK"
     "VGhlIHNob3J0IHJlZCBmb3ggcmFuIHF1aWNrbHkgdGhyb3VnaCB0aGUgZ3JlZW4gZmllbGQgYW5kIGp1bXBlZCBvdmVyIHRoZSB0YWxsIGJyb3duIGJlYXIK")

    ("Le Poete est semblable au prince des nuees Qui hante la tempete e se rit de l'archer; Exile sul le sol au milieu des huees, Ses ailes de geant l'empechent de marcher."
     "TGUgUG9ldGUgZXN0IHNlbWJsYWJsZSBhdSBwcmluY2UgZGVzIG51ZWVzIFF1aSBoYW50ZSBsYSB0ZW1wZXRlIGUgc2Ugcml0IGRlIGwnYXJjaGVyOyBFeGlsZSBzdWwgbGUgc29sIGF1IG1pbGlldSBkZXMgaHVlZXMsIFNlcyBhaWxlcyBkZSBnZWFudCBsJ2VtcGVjaGVudCBkZSBtYXJjaGVyLg=="
     "TGUgUG9ldGUgZXN0IHNlbWJsYWJsZSBhdSBwcmluY2UgZGVzIG51ZWVzIFF1aSBoYW50ZSBsYSB0ZW1wZXRlIGUgc2Ugcml0IGRlIGwnYXJjaGVyOyBFeGlsZSBzdWwgbGUgc29sIGF1IG1pbGlldSBkZXMgaHVlZXMsIFNlcyBhaWxlcyBkZSBnZWFudCBsJ2VtcGVjaGVudCBkZSBtYXJjaGVyLg")))


;;;; base64 encoding and decoding routines

(define (encode binary)
  ;;Encode BINARY which  must be a Scheme string  or bytevector.  Return
  ;;two values:  (1) the resulting boolean from  the encoding functions,
  ;;(2) a string representing the encoded data.
  ;;
  ;;Make use of the ENCODING and PADDING? parameters.
  ;;
  (let* ((ctx		(make-<base64-encode-ctx> (encoding) (padding?)))
	 (src		(if (string? binary) (string->utf8 binary) binary))
	 (src-len	(bytevector-length src))
	 (dst		(make-bytevector (base64-encode-length src-len (padding?)))))
    (receive (dst-next src-next)
	(base64-encode-update! ctx dst 0 src 0 src-len)
      (receive (result dst-next src-next)
	  (base64-encode-final! ctx dst dst-next src src-next src-len)
	(list result (utf8->string (subbytevector dst 0 dst-next)))))))

(define (decode binary string-result?)
  ;;Decode BINARY which  must be a Scheme string  or bytevector.  Return
  ;;two  values: the  boolean result  from the  decoding  functions, the
  ;;decoded data.
  ;;
  ;;If STRING-RESULT?  is  true, the decoded data is  returned as Scheme
  ;;string; else it is returned as Scheme bytevector.
  ;;
  ;;Make use of the ENCODING and PADDING? parameters.
  ;;
  (define (%output dst dst-past)
    (let ((bv (subbytevector dst 0 dst-past)))
      (if string-result?
	  (utf8->string bv)
	bv)))
  (let* ((ctx		(make-<base64-decode-ctx> (encoding) (padding?)))
	 (src		(if (string? binary)
			    (string->utf8 binary)
			  binary))
	 (src-len	(bytevector-length src))
	 (dst		(make-bytevector (base64-decode-length src-len (padding?)))))
    (receive (result dst-next src-next)
	(base64-decode-update! ctx dst 0 src 0 src-len)
      (if result
	  (list result (%output dst dst-next))
	(receive (result dst-next src-next)
	    (base64-decode-final! ctx dst dst-next src src-next src-len)
	  (list result (%output dst dst-next)))))))


(parametrise ((check-test-name	'one)
	      (encoding		'base64)
	      (padding?		#t)
	      (debugging	#t))

  (check
      (encode "ABC")
    => '(#t "QUJD"))

  (check
      (decode "QUJD" #t)
    => '(#t "ABC"))

  (let ((plain "") (encoded ""))
    (check (encode plain)	=> `(#t ,encoded))
    (check (decode encoded #t)	=> `(#t ,plain)))

  (let ((plain "H") (encoded "SA=="))
    (check (encode plain)	=> `(#t ,encoded))
    (check (decode encoded #t)	=> `(#t ,plain)))

  (let ((plain "He") (encoded "SGU="))
    (check (encode plain)	=> `(#t ,encoded))
    (check (decode encoded #t)	=> `(#t ,plain)))

  (let ((plain "Hel") (encoded "SGVs"))
    (check (encode plain)	=> `(#t ,encoded))
    (check (decode encoded #t)	=> `(#t ,plain)))

  (let ((plain "Hell")  (encoded "SGVsbA=="))
    (check (encode plain)	=> `(#t ,encoded))
    (check (decode encoded #t)	=> `(#t ,plain)))

  (let ((plain "Hello") (encoded "SGVsbG8="))
    (check (encode plain)	=> `(#t ,encoded))
    (check (decode encoded #t)	=> `(#t ,plain)))

  (for-each (lambda (triplet)
  	      (let ((binary	(car   triplet))
  		    (padded	(cadr  triplet)))
  		(check
  		    `(,binary -> ,(encode binary))
  		  => `(,binary -> (#t ,padded)))
  		(check
  		    `(,padded -> ,(decode padded (string? binary)))
  		  => `(,padded -> (#t ,binary)))
  		))
    test-vectors)

;;; --------------------------------------------------------------------

;;   (check
;;       (guard (E ((armor-invalid-input-byte-condition? E)
;; ;;;		   (display (condition-irritants E))(newline)
;; 		 #t)
;; 		(else
;; 		 #f))
;; 	(decode "ABCDE0AA" #f))
;;     => #t)

;;   (check
;;       (guard (E ((armor-invalid-padding-condition? E)
;; ;;;		   (display (condition-irritants E))(newline)
;; 		 #t)
;; 		(else
;; 		 (debug-print-condition "invalid padding" E)
;; 		 #f))
;; 	(decode "A=======" #f))
;;     => #t)

;;   (check
;;       (guard (E ((armor-invalid-padding-condition? E)
;; ;;;		   (display (condition-irritants E))(newline)
;; 		 #t)
;; 		(else
;; 		 (debug-print-condition "invalid padding" E)
;; 		 #f))
;; 	(decode "ACA=====" #f))
;;     => #t)

;;   (check
;;       (guard (E ((armor-invalid-padding-condition? E)
;; ;;;		   (display (condition-irritants E))(newline)
;; 		 #t)
;; 		(else
;; 		 (debug-print-condition "invalid padding" E)
;; 		 #f))
;; 	(decode "A=CA====" #f))
;;     => #t)

;;   (check	;invalid input length
;;       (let ((ctx (make-<base64-decode-ctx> 'base64 #t 'upper)))
;; 	(guard (E ((armor-invalid-input-length-condition? E)
;; ;;;		   (display (condition-irritants E))(newline)
;; 		   #t)
;; 		  (else
;; 		   (debug-print-condition "invalid input length" E)
;; 		   #f))
;; 	  (receive (a b c)
;; 	      (base64-decode-final! ctx
;; 				    (make-bytevector 256) 0
;; 				    '#vu8(70 71 72 73 74 75 76) 0 7)
;; 	    (list a b c))))
;;     => #t)

;;; --------------------------------------------------------------------
;;; encoding show off for the documentation

;;   (let ()

;;     (define ctx
;;       (make-<base64-encode-ctx> 'base64 #t 'upper))

;;     (define bin-1.bv
;;       '#vu8(0 1 2 3 4))
;;     (define bin-1.len
;;       (bytevector-length bin-1.bv))

;;     (define asc-1.len
;;       (base64-encode-update-length bin-1.len))
;;     (define asc-1.bv
;;       (make-bytevector asc-1.len))

;;     (define-values (dst-next-1 src-next-1)
;;       (base64-encode-update! ctx
;; 			     asc-1.bv 0
;; 			     bin-1.bv 0 bin-1.len))

;;     ;; (display (list (utf8->string asc-1.bv)
;;     ;; 		   asc-1.len
;;     ;; 		   dst-next-1
;;     ;; 		   src-next-1))
;;     ;; (newline)

;;     (define bin-2.bv
;;       '#vu8(0 1 2 3 4 5 6))
;;     (define bin-2.len
;;       (bytevector-length bin-2.bv))

;;     (define asc-2.len
;;       (base64-encode-update-length bin-2.len))
;;     (define asc-2.bv
;;       (make-bytevector asc-2.len))

;;     (define-values (dst-next-2 src-next-2)
;;       (base64-encode-update! ctx
;;     			     asc-2.bv 0
;;     			     bin-2.bv 0 bin-2.len))

;;     ;; (display (list (utf8->string (subbytevector asc-2.bv 0 dst-next-2))
;;     ;; 		   asc-2.len
;;     ;; 		   dst-next-2
;;     ;; 		   src-next-2))
;;     ;; (newline)

;;     (define bin-3.bv
;;       '#vu8(0 1 2 3 4 5 6))
;;     (define bin-3.len
;;       (bytevector-length bin-3.bv))

;;     (define delta
;;       (- bin-2.len src-next-2))

;;     (define bin-23.len
;;       (+ bin-3.len delta))

;;     (define bin-23.bv
;;       (let ((bv (make-bytevector bin-23.len)))
;; 	(do ((i 0          (+ 1 i))
;; 	     (j src-next-2 (+ 1 j)))
;; 	    ((= i delta))
;; 	  (bytevector-u8-set! bv i (bytevector-u8-ref bin-2.bv j)))
;; 	(do ((i delta (+ 1 i))
;; 	     (j 0     (+ 1 j)))
;; 	    ((= j bin-3.len)
;; 	     bv)
;; 	  (bytevector-u8-set! bv i (bytevector-u8-ref bin-3.bv j)))))

;;     (define asc-3.len
;;       (base64-encode-length bin-23.len #t))

;;     (define asc-3.bv
;;       (make-bytevector asc-3.len))

;;     (define-values (result dst-next-3 src-next-3)
;;       (base64-encode-final! ctx
;; 			    asc-3.bv 0
;; 			    bin-23.bv 0 bin-23.len))

;;     ;; (display (list (utf8->string asc-3.bv)
;;     ;;		   result
;;     ;; 		   dst-next-3
;;     ;; 		   src-next-3))
;;     ;; (newline)

;;     (check
;; 	(parametrise ((encoding		'base64)
;; 		      (padding?		#t))
;; 	  (encode '#vu8(0 1 2 3 4 0 1 2 3 4 5 6 0 1 2 3 4 5 6)))
;;       => '(#t "AAAQEAYEAAAQEAYEAUDAAAICAMCAKBQ="))

;;     #f)

;;; --------------------------------------------------------------------
;;; decoding show off for the documentation

;;   (let ()

;;     (define ctx
;;       (make-<base64-decode-ctx> 'base64 #t 'upper))

;;     (define asc-1.bv
;;       (string->utf8 "AAAQEAYE"))
;;     (define asc-1.len
;;       (bytevector-length asc-1.bv))

;;     (define bin-1.len
;;       (base64-decode-update-length asc-1.len))
;;     (define bin-1.bv
;;       (make-bytevector bin-1.len))

;;     (define-values (finished-1? dst-next-1 src-next-1)
;;       (base64-decode-update! ctx
;; 			     bin-1.bv 0
;; 			     asc-1.bv 0 asc-1.len))

;;     ;; (display (list bin-1.bv
;;     ;; 		   finished-1?
;;     ;; 		   asc-1.len
;;     ;; 		   dst-next-1
;;     ;; 		   src-next-1))
;;     ;; (newline)

;;     (define asc-2.bv
;;       (string->utf8 "AAAQEAYEAU"))
;;     (define asc-2.len
;;       (bytevector-length asc-2.bv))

;;     (define bin-2.len
;;       (base64-decode-update-length asc-2.len))
;;     (define bin-2.bv
;;       (make-bytevector bin-2.len))

;;     (define-values (finished-2? dst-next-2 src-next-2)
;;       (base64-decode-update! ctx
;;     			     bin-2.bv 0
;;     			     asc-2.bv 0 asc-2.len))

;;     ;; (display (list bin-2.bv
;;     ;; 		   bin-2.len
;;     ;; 		   finished-2?
;;     ;; 		   dst-next-2
;;     ;; 		   src-next-2))
;;     ;; (newline)

;;     (define asc-3.bv
;;       (string->utf8 "DAAAICAMCAKBQ="))
;;     (define asc-3.len
;;       (bytevector-length asc-3.bv))

;;     (define delta
;;       (- asc-2.len src-next-2))

;;     (define asc-23.len
;;       (+ asc-3.len delta))

;;     (define asc-23.bv
;;       (let ((bv (make-bytevector asc-23.len)))
;;     	(do ((i 0          (+ 1 i))
;;     	     (j src-next-2 (+ 1 j)))
;;     	    ((= i delta))
;;     	  (bytevector-u8-set! bv i (bytevector-u8-ref asc-2.bv j)))
;;     	(do ((i delta (+ 1 i))
;;     	     (j 0     (+ 1 j)))
;;     	    ((= j asc-3.len)
;;     	     bv)
;;     	  (bytevector-u8-set! bv i (bytevector-u8-ref asc-3.bv j)))))

;;     (define bin-3.len
;;       (base64-decode-length asc-23.len #t))

;;     (define bin-3.bv
;;       (make-bytevector bin-3.len))

;;     (define-values (result dst-next-3 src-next-3)
;;       (base64-decode-final! ctx
;;     			    bin-3.bv 0
;;     			    asc-23.bv 0 asc-23.len))

;;     ;; (display (list (subbytevector bin-3.bv 0 dst-next-3)
;;     ;; 		   result
;;     ;; 		   'bin-3.len bin-3.len
;;     ;; 		   'dst-next-3 dst-next-3
;;     ;; 		   'src-next-3 src-next-3))
;;     ;; (newline)

;;     (check
;;     	(parametrise ((encoding		'base64)
;;     		      (padding?		#t))
;;     	  (decode "AAAQEAYEAAAQEAYEAUDAAAICAMCAKBQ=" #f))
;;       => '(#t #vu8(0 1 2 3 4 0 1 2 3 4 5 6 0 1 2 3 4 5 6)))

;;     #f)

  #t)


(parametrise ((check-test-name	'one)
	      (encoding		'base64)
	      (padding?		#f)
	      (debugging	#t))

  (check
      (encode "ABC")
    => '(#t "QUJD"))

  (check
      (decode "QUJD" #t)
    => '(#t "ABC"))

  (let ((plain "") (encoded ""))
    (check (encode plain)	=> `(#t ,encoded))
    (check (decode encoded #t)	=> `(#t ,plain)))

  (let ((plain "H") (encoded "SA"))
    (check (encode plain)	=> `(#t ,encoded))
    (check (decode encoded #t)	=> `(#t ,plain)))

  (let ((plain "He") (encoded "SGU"))
    (check (encode plain)	=> `(#t ,encoded))
    (check (decode encoded #t)	=> `(#t ,plain)))

  (let ((plain "Hel") (encoded "SGVs"))
    (check (encode plain)	=> `(#t ,encoded))
    (check (decode encoded #t)	=> `(#t ,plain)))

  (let ((plain "Hell")  (encoded "SGVsbA"))
    (check (encode plain)	=> `(#t ,encoded))
    (check (decode encoded #t)	=> `(#t ,plain)))

  (let ((plain "Hello") (encoded "SGVsbG8"))
    (check (encode plain)	=> `(#t ,encoded))
    (check (decode encoded #t)	=> `(#t ,plain)))

  #f)


;;;; done

(check-report)

;;; end of file
