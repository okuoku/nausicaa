;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: comma-separated value (CSV) tokeniser
;;;Date: Mon Jul 20, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
;;;
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


#!r6rs
(library (csv)
  (export

    ;; reader specification
    make-csv-reader-spec	csv-reader-spec?
    :newline-type		:separator-chars
    :quote-char			:quote-doubling-escapes?
    :comment-chars		:whitespace-chars
    :strip-leading-whitespace?	:strip-trailing-whitespace?
    :newlines-in-quotes?


    )
  (import (rnrs)
    (keywords)
    (silex lexer)
    (csv strings-lexer))


;;;; reader specification

(define-record-type (:reader-spec :reader-spec-make csv-reader-spec?)
  (fields (mutable newline-type)
	  (mutable separator-chars)
	  (mutable quote-char)
	  (mutable quote-doubling-escapes?)
	  (mutable comment-chars)
	  (mutable whitespace-chars)
	  (mutable strip-leading-whitespace?)
	  (mutable strip-trailing-whitespace?)
	  (mutable newlines-in-quotes?)))

(define-keyword :newline-type)
(define-keyword :separator-chars)
(define-keyword :quote-char)
(define-keyword :quote-doubling-escapes?)
(define-keyword :comment-chars)
(define-keyword :whitespace-chars)
(define-keyword :strip-leading-whitespace?)
(define-keyword :strip-trailing-whitespace?)
(define-keyword :newlines-in-quotes?)

(define (make-csv-reader-spec . options)
  (let-keywords options #f ((newline-type		:newline-type               'lax)
			    (separator-chars		:separator-chars            '(#\,))
			    (quote-char			:quote-char                 #\")
			    (quote-doubling-escapes?	:quote-doubling-escapes?    #t)
			    (comment-chars		:comment-chars              '())
			    (whitespace-chars		:whitespace-chars           '(#\space))
			    (strip-leading-whitespace?	:strip-leading-whitespace?  #f)
			    (strip-trailing-whitespace?	:strip-trailing-whitespace? #f)
			    (newlines-in-quotes?	:newlines-in-quotes?        #t))
    (:reader-spec-make newline-type
		       separator-chars
		       quote-char
		       quote-doubling-escapes?
		       comment-chars
		       whitespace-chars
		       strip-leading-whitespace?
		       strip-trailing-whitespace?
		       newlines-in-quotes?)))


;;;; helpers



;;;; tokeniser




;;;; done

)

;;; end of file
