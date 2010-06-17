;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: URI handling
;;;Date: Wed Jun  2, 2010
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


(library (uri)
  (export
    <uri>
    )
  (import (nausicaa)
    (prefix (uri low) uri:))


;;;; helpers



(define-class <uri>
  (nongenerative nausicaa:uri:<uri>)

  (maker ()
	 (:decoded-scheme	#f)
	 (:decoded-authority	#f)
	 (:decoded-path		#f)
	 (:decoded-query	#f)
	 (:decoded-fragment	#f)

	 (:encoded-scheme	#f)
	 (:encoded-authority	#f)
	 (:encoded-path		#f)
	 (:encoded-query	#f)
	 (:encoded-fragment	#f)
	 )

  (protocol (lambda (make-top)
	      (lambda (	;
		       decoded-scheme decoded-authority decoded-path decoded-query decoded-fragment
		       encoded-scheme encoded-authority encoded-path encoded-query encoded-fragment)
		((make-top)
		 #f ;cached-string
		 #f ;cached-bytevector
		 decoded-scheme decoded-authority decoded-path decoded-query decoded-fragment
		 encoded-scheme encoded-authority encoded-path encoded-query encoded-fragment
		 ))))

  (fields (mutable cached-string)
	  (mutable cached-bytevector))

  (fields (mutable cached-decoded-scheme)
	  (mutable cached-decoded-authority)
	  (mutable cached-decoded-path)
	  (mutable cached-decoded-query)
	  (mutable cached-decoded-fragment))

  (fields (mutable cached-encoded-scheme)
	  (mutable cached-encoded-authority)
	  (mutable cached-encoded-path)
	  (mutable cached-encoded-query)
	  (mutable cached-encoded-fragment))

  (virtual-fields (immutable decoded-scheme)
		  (immutable decoded-authority)
		  (immutable decoded-path)
		  (immutable decoded-query)
		  (immutable decoded-fragment))

  (virtual-fields (immutable encoded-scheme)
		  (immutable encoded-authority)
		  (immutable encoded-path)
		  (immutable encoded-query)
		  (immutable encoded-fragment))

  (virtual-fields (immutable string)
		  (immutable bytevector))
  )


(let-syntax ((define-decoded-field (syntax-rules ()
				     ((_ ?var ?name ?encoded ?decoded)
				      (define (?name (?var <uri>))
					(or ?decoded
					    (and ?encoded
						 (begin0-let ((bv (uri:percent-decode ?encoded)))
						   (set! ?decoded bv)))))))))

  (define-decoded-field o <uri>-decoded-scheme    o.cached-encoded-scheme    o.cached-decoded-scheme)
  (define-decoded-field o <uri>-decoded-authority o.cached-encoded-authority o.cached-decoded-authority)
  (define-decoded-field o <uri>-decoded-query     o.cached-encoded-query     o.cached-decoded-query)
  (define-decoded-field o <uri>-decoded-fragment  o.cached-encoded-fragment  o.cached-decoded-fragment)

  (define (<uri>-decoded-path (o <uri>))
    (or o.cached-decoded-path
	(and o.cached-encoded-path
	     (begin0-let ((ell (map (lambda (bv)
				      (uri:percent-decode bv))
				 o.cached-encoded-path)))
	       (set! o.cached-decoded-path ell)))))
  )

;;; --------------------------------------------------------------------

(let-syntax ((define-encoded-field (syntax-rules ()
				     ((_ ?var ?name ?encoded ?decoded)
				      (define (?name (?var <uri>))
					(or ?encoded
					    (and ?decoded
						 (begin0-let ((bv (uri:percent-encode ?decoded)))
						   (set! ?encoded bv)))))))))

  (define-encoded-field o <uri>-encoded-scheme    o.cached-encoded-scheme    o.cached-decoded-scheme)
  (define-encoded-field o <uri>-encoded-authority o.cached-encoded-authority o.cached-decoded-authority)
  (define-encoded-field o <uri>-encoded-query     o.cached-encoded-query     o.cached-decoded-query)
  (define-encoded-field o <uri>-encoded-fragment  o.cached-encoded-fragment  o.cached-decoded-fragment)

  (define (<uri>-encoded-path (o <uri>))
    (or o.cached-encoded-path
	(and o.cached-decoded-path
	     (begin0-let ((ell (map (lambda (bv)
				      (uri:percent-encode bv))
				 o.cached-decoded-path)))
	       (set! o.cached-encoded-path ell)))))
  )


(define (<uri>-string (o <uri>))
  (or o.cached-string
      (begin0-let ((out (utf8->string o.bytevector)))
	(set! o.cached-string out))))

(define (<uri>-bytevector (o <uri>))
  (or o.cached-bytevector
      (receive (port getter)
	  (open-bytevector-output-port)
	(let-syntax ((%put-bv	(syntax-rules ()
				  ((_ ?thing)
				   (put-bytevector port ?thing))))
		     (%put-u8	(syntax-rules ()
				  ((_ ?thing)
				   (put-u8 port ?thing)))))

	  (when o.encoded-scheme
	    (%put-bv o.encoded-scheme)
	    (%put-u8 58)) ;58 = :

	  (when o.encoded-authority
	    (%put-u8 47) ;47 = /
	    (%put-u8 47)
	    (%put-bv o.encoded-authority))

	  (for-each (lambda (bv)
		      (%put-u8 47) ;47 = /
		      (%put-bv bv))
	    o.encoded-path)

	  (when o.encoded-query
	    (%put-u8 63) ;63 = ?
	    (%put-bv o.encoded-query))

	  (when o.encoded-fragment
	    (%put-u8 35) ;35 = #
	    (%put-bv o.encoded-fragment))

	  (begin0-let ((out (getter)))
	    (set! o.cached-bytevector out))))))


;;;; done

)

;;; end of file
