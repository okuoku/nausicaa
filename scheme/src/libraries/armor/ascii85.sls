;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: ascii armor with ascii85 format
;;;Date: Tue Jan 26, 2010
;;;
;;;Abstract
;;;
;;;	This  code is  a  Scheme swirling  of  C language  code from  an
;;;	ASCII85 distribution by Paul Haahr which claims that the code is
;;;	in   the  public  domain,   although  he   did  not   write  it.
;;;	http://www.webcom.com/~haahr/
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


(library (armor ascii85)
  (export

    <ascii85-encode-ctx>		<ascii85-decode-ctx>
    make-<ascii85-encode-ctx>		make-<ascii85-decode-ctx>
    <ascii85-encode-ctx>?		<ascii85-decode-ctx>?

    ascii85-encode-init!
    ascii85-encode-update!		ascii85-decode-update!
    ascii85-encode-final!

    ascii85-encode-initial-length
    ascii85-encode-length		ascii85-decode-length
    ascii85-encode-partial-length
    ascii85-encode-final-length

    ascii85-encode-finished?		ascii85-decode-finished?)
  (import (rnrs)
    (armor helpers))


;;;; helpers

;; (define << bitwise-arithmetic-shift-left)
;; (define >> bitwise-arithmetic-shift-right)

;; (define-syntax incr!
;;   (syntax-rules ()
;;     ((_ ?id)
;;      (set! ?id (+ ?id 1)))
;;     ((_ ?id ?delta)
;;      (set! ?id (+ ?id ?delta)))))

;; (define-syntax decr!
;;   (syntax-rules ()
;;     ((_ ?id)
;;      (set! ?id (- ?id 1)))
;;     ((_ ?id ?delta)
;;      (set! ?id (- ?id ?delta)))))

;; (define-syntax defmacro
;;   (syntax-rules ()
;;     ((_ (?name ?arg ...) ?form0 ?form ...)
;;      (define-syntax ?name
;;        (syntax-rules ()
;; 	 ((_ ?arg ...)
;; 	  (begin ?form0 ?form ...)))))))

(define-syntax define-encode-accessor
  (lambda (stx)
    (define (%field->accessor field-stx)
      (string->symbol (string-append "<ascii85-encode-ctx>-"
				     (symbol->string (syntax->datum field-stx)))))
    (define (%field->mutator field-stx)
      (string->symbol (string-append "<ascii85-encode-ctx>-"
				     (symbol->string (syntax->datum field-stx))
				     "-set!")))
    (syntax-case stx ()
      ((_ ?record ?field)
       (with-syntax ((ACCESSOR	(datum->syntax #'field (%field->accessor #'?field)))
		     (MUTATOR	(datum->syntax #'field (%field->mutator  #'?field))))
       #'(define-syntax ?field
	   (identifier-syntax
	    (?id
	     (ACCESSOR ?record))
	    ((set! ?id ?e)
	     (MUTATOR ?record ?e)))))))))

(define-syntax define-decode-accessor
  (lambda (stx)
    (define (%field->accessor field-stx)
      (string->symbol (string-append "<ascii85-decode-ctx>-"
				     (symbol->string (syntax->datum field-stx)))))
    (define (%field->mutator field-stx)
      (string->symbol (string-append "<ascii85-decode-ctx>-"
				     (symbol->string (syntax->datum field-stx))
				     "-set!")))
    (syntax-case stx ()
      ((_ ?record ?field)
       (with-syntax ((ACCESSOR	(datum->syntax #'field (%field->accessor #'?field)))
		     (MUTATOR	(datum->syntax #'field (%field->mutator  #'?field))))
       #'(define-syntax ?field
	   (identifier-syntax
	    (?id
	     (ACCESSOR ?record))
	    ((set! ?id ?e)
	     (MUTATOR ?record ?e)))))))))


(define-record-type <ascii85-encode-ctx>
  (fields (mutable tuple)
		;tuple to be encoded, at most 4 bytes
	  (mutable count)
		;number of bytes in the tuple
	  (mutable status))
		;current encoder status:
		;
		;not-initialised, processing-data, finished
  (protocol (lambda (maker)
	      (lambda ()
		(maker 0 0 'not-initialised)))))

(define-record-type <ascii85-decode-ctx>
  (fields (mutable tuple)
		;tuple to be decode, at most 4 bytes
	  (mutable count)
		;number of bytes in the tuple
	  (mutable status)
		;current decoder state:
		;
		;expecting-less-than	expecting-opening-tilde
		;processing-data
		;found-closing-tilde	finished
	  (mutable allow-blanks?))
  (protocol (lambda (maker)
	      (lambda (allow-blanks?)
		(maker 0 0 'expecting-less-than allow-blanks?)))))



(define less-than-char		(char->integer #\<))
(define greater-than-char	(char->integer #\>))
(define tilde-char		(char->integer #\~))
(define bang-char		(char->integer #\!))
(define z-char			(char->integer #\z))
(define u-char			(char->integer #\u))

(define table
  (vector (* 85 85 85 85)
	  (* 85 85 85)
	  (* 85 85)
	  85
	  1))

(define-syntax pow85
  (syntax-rules ()
    ((_ ?idx)
     (vector-ref table ?idx))))

(define blanks
  (map char->integer '(#\newline #\return #\tab #\vtab #\space #\page)))


(define ascii85-encode-initial-length 2)

(define (ascii85-encode-final-length len)
  (+ 2 (ascii85-encode-partial-length len)))

(define (ascii85-encode-partial-length len)
  (if (< 4 len)
      (receive (ratio rest)
	  (div-and-mod len 4)
	(+ 1 rest (* 5 ratio)))
    (+ 1 len)))

(define (ascii85-encode-length len)
  (+ 2 (ascii85-encode-final-length len)))

(define (ascii85-decode-length len)
  (assert (<= 4 len))
  (let ((len (- len 4)))
    (if (<= 5 len)
	(receive (ratio rest)
	    (div-and-mod len 5)
	  (+ 1 rest (* 4 (div len 5))))
      4)))	;remember that a single char could be #\z !!!


(define (ascii85-encode-init! ctx dst-bv dst-start)
  (define-encode-accessor ctx status)
  (if (eq? status 'not-initialised)
      (begin
	(set! status 'processing-data)
	(bytevector-u8-set! dst-bv dst-start       less-than-char)
	(bytevector-u8-set! dst-bv (+ 1 dst-start) tilde-char)
	2)
    (error 'ascii85-encode-init!
      "attempt to initialise output sequence with invalid status in ASCII85 context" ctx)))

(define (ascii85-encode-final! ctx dst-bv dst-start)
  (define-encode-accessor ctx status)
  (define-encode-accessor ctx count)
  (define-encode-accessor ctx tuple)
  (if (eq? status 'processing-data)
      (let ((i 0))
	(set! status 'finished)
	(when (< 0 count)
	  (incr! i (%encode tuple count dst-bv dst-start)))
	(bytevector-u8-set! dst-bv (+   i dst-start) tilde-char)
	(bytevector-u8-set! dst-bv (+ 1 i dst-start) greater-than-char)
	(+ 2 i))
    (error 'ascii85-encode-final!
      "attempt to finalise output sequence with invalid status in ASCII85 context" ctx)))

(define (ascii85-encode-update! ctx dst-bv dst-start src-bv src-start src-len)
  (define-encode-accessor ctx status)
  (define-encode-accessor ctx count)
  (define-encode-accessor ctx tuple)
  (if (eq? status 'processing-data)
      (let loop ((i 0) (j 0))
	(if (= j src-len)
	    i
	  (let ((byte (bytevector-u8-ref src-bv (+ j src-start))))
	    (case count
	      ((0)
	       (set! tuple (bitwise-ior tuple (<< byte 24)))
	       (incr! count)
	       (loop i (+ 1 j)))
	      ((1)
	       (set! tuple (bitwise-ior tuple (<< byte 16)))
	       (incr! count)
	       (loop i (+ 1 j)))
	      ((2)
	       (set! tuple (bitwise-ior tuple (<< byte 8)))
	       (incr! count)
	       (loop i (+ 1 j)))
	      ((3)
	       (set! tuple (bitwise-ior tuple byte))
	       (incr! count)
	       (if (= 0 tuple)
		   (begin
		     (bytevector-u8-set! dst-bv (+ i dst-start) z-char)
		     (incr! i))
		 (incr! i (%encode tuple count dst-bv (+ i dst-start))))
	       (set! tuple 0)
	       (set! count 0)
	       (loop i (+ 1 j)))))))
    (error 'ascii85-encode-update!
      "attempt to process data with invalid status in ASCII85 context" ctx)))

(define (%encode tuple count dst-bv dst-start)
  (let loop ((i 0)
	     (rest-list (let loop ((k 0) (tuple tuple) (rest-list '()))
			  (if (<= 5 k)
			      rest-list
			    (receive (ratio rest)
				(div-and-mod tuple 85)
			      (loop (+ 1 k) ratio (cons rest rest-list)))))))
    (if (< count i)
	i
      (begin
	(bytevector-u8-set! dst-bv (+ i dst-start) (+ bang-char (car rest-list)))
	(loop (+ 1 i) (cdr rest-list))))))

(define (ascii85-encode-finished? ctx)
  (eq? 'finished (<ascii85-encode-ctx>-status ctx)))


(define (ascii85-decode-update! ctx dst-bv dst-start src-bv src-start src-len)
  (define-decode-accessor ctx count)
  (define-decode-accessor ctx tuple)
  (define-decode-accessor ctx status)
  (define-decode-accessor ctx allow-blanks?)
  (let loop ((i 0) (j 0))
    (if (= j src-len)
	i
      (let ((byte (bytevector-u8-ref src-bv (+ j src-start))))
	(cond ((= byte less-than-char)
	       (case status
		 ((expecting-less-than)
		  (set! status 'expecting-opening-tilde)
		  (loop i (+ 1 j)))
		 ((processing-data)
		  (incr! i (%decode-byte ctx dst-bv (+ i dst-start) byte))
		  (loop i (+ 1 j)))
		 (else
		  (error 'ascii85-decode-update!
		    "invalid < character inside ASCII85 stream"))))

	      ((= byte greater-than-char)
	       (case status
		 ((found-closing-tilde)
		  (set! status 'finished)
		  i)
		 ((processing-data)
		  (incr! i (%decode-byte ctx dst-bv (+ i dst-start) byte))
		  (loop i (+ 1 j)))
		 (else
		  (error 'ascii85-decode-update!
		    "invalid > character in ASCII85 data"))))

	      ((= byte tilde-char)
	       (case status
		 ((expecting-opening-tilde)
		  (set! status 'processing-data)
		  (loop i (+ 1 j)))
		 ((processing-data)
		  (set! status 'found-closing-tilde)
		  (unless (= 0 count)
		    (decr! count)
		    (incr! tuple (pow85 count))
		    (incr! i (wput tuple count dst-bv (+ i dst-start))))
		  (loop i (+ 1 j)))
		 (else
		  (error 'ascii85-decode-update!
		    "invalid ~ character in ASCII85 data"))))

	      ((= byte z-char)
	       (cond ((not (eq? status 'processing-data))
		      (error 'ascii85-decode-update!
			"invalid character in ASCII85 stream"))
		     ((not (= 0 count))
		      (error 'ascii85-decode-update!
			"invalid z character inside ASCII85 5-tuple"))
		     (else
		      (bytevector-u8-set! dst-bv (+ i dst-start) 0)
		      (incr! i)
		      (bytevector-u8-set! dst-bv (+ i dst-start) 0)
		      (incr! i)
		      (bytevector-u8-set! dst-bv (+ i dst-start) 0)
		      (incr! i)
		      (bytevector-u8-set! dst-bv (+ i dst-start) 0)
		      (loop (+ 1 i) (+ 1 j)))))

	      ((memv byte blanks)
	       (cond ((not (eq? status 'processing-data))
		      (error 'ascii85-decode-update!
			"invalid character in ASCII85 stream"))
		     (allow-blanks?
		      (loop i (+ 1 j)))
		     (else
		      (error 'ascii85-decode-update!
			"invalid blank character in ASCII85 data" byte))))

	      (else
	       (cond ((not (eq? status 'processing-data))
		      (error 'ascii85-decode-update!
			"invalid character in ASCII85 stream"))
		     ((or (< u-char byte) (< byte bang-char))
		      (error 'ascii65-decode-update!
			"invalid character in ASCII85 data" byte))
		     (else
		      (incr! i (%decode-byte ctx dst-bv (+ i dst-start) byte))
		      (loop i (+ 1 j))))))))))

(define (%decode-byte ctx dst-bv dst-start byte)
  (define-decode-accessor ctx count)
  (define-decode-accessor ctx tuple)
  (let ((i 0))
    (incr! tuple (* (- byte bang-char) (pow85 count)))
    (incr! count)
    (when (= 5 count)
      (incr! i (wput tuple 4 dst-bv (+ i dst-start)))
      (set! count 0)
      (set! tuple 0))
    i))

(define (wput tuple bytes dst-bv dst-start)
  (case bytes
    ((4)
     (bytevector-u8-set! dst-bv dst-start       (bitwise-and #xFF (>> tuple 24)))
     (bytevector-u8-set! dst-bv (+ 1 dst-start) (bitwise-and #xFF (>> tuple 16)))
     (bytevector-u8-set! dst-bv (+ 2 dst-start) (bitwise-and #xFF (>> tuple 8)))
     (bytevector-u8-set! dst-bv (+ 3 dst-start) (bitwise-and #xFF tuple))
     4)
    ((3)
     (bytevector-u8-set! dst-bv dst-start       (bitwise-and #xFF (>> tuple 24)))
     (bytevector-u8-set! dst-bv (+ 1 dst-start) (bitwise-and #xFF (>> tuple 16)))
     (bytevector-u8-set! dst-bv (+ 2 dst-start) (bitwise-and #xFF (>> tuple 8)))
     3)
    ((2)
     (bytevector-u8-set! dst-bv dst-start       (bitwise-and #xFF (>> tuple 24)))
     (bytevector-u8-set! dst-bv (+ 1 dst-start) (bitwise-and #xFF (>> tuple 16)))
     2)
    ((1)
     (bytevector-u8-set! dst-bv dst-start       (bitwise-and #xFF (>> tuple 24)))
     1)))

(define (ascii85-decode-finished? ctx)
  (eq? 'finished (<ascii85-decode-ctx>-status ctx)))


;;;; done

)

;;; end of file
