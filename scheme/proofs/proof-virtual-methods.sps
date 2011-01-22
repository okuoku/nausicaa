;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: attempts for virtual methods implementation
;;;Date: Fri May 28, 2010
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


#!r6rs
(import (nausicaa)
  (rename (only (nausicaa language classes) method)
	  (method virtual-method))
  (nausicaa checks))

(check-set-mode! 'report-failed)
(display "*** proof of virtual methods\n")


(parametrise ((check-test-name	'boh))

  (define (unimplemented-method . args)
    (syntax-violation #f "unimplemented method" #f #f))

  (define virtual-methods-table
    (make-parameter #f))

  (let ()

    (define-inline (make-root-superclass-protocol-with-virtual-methods given-protocol)
      (lambda (make-superclass)
	(given-protocol
	 (lambda superclass-args
	   (lambda args
	     (apply (apply make-superclass superclass-args)
		    (virtual-methods-table) args))))))

    (define-inline (make-root-public-protocol-with-virtual-methods methods-table given-protocol)
      (lambda (make-superclass)
	(given-protocol
	 (lambda superclass-args
	   (lambda args
	     (apply (apply make-superclass superclass-args)
		    methods-table args))))))

    (define-inline (make-protocol-with-virtual-methods virtual-methods given-protocol)
      (lambda (make-superclass)
	(parametrise ((virtual-methods-table virtual-methods))
	  (given-protocol make-superclass))))

    (define <alpha>-given-protocol
      (lambda (make-top)
	(lambda (a b)
	  ((make-top) a b))))

    (define-class <alpha>
      (fields (immutable __virtual_methods__)
	      a b)
      (virtual-method (red (o <alpha>))
		      ((vector-ref o.__virtual_methods__ 0) o))
      (virtual-method (blue (o <alpha>))
		      ((vector-ref o.__virtual_methods__ 1) o))
      (public-protocol
       (make-root-public-protocol-with-virtual-methods
	the-<alpha>-virtual-methods <alpha>-given-protocol))
      (superclass-protocol
       (make-root-superclass-protocol-with-virtual-methods <alpha>-given-protocol)))

    (define the-<alpha>-virtual-methods
      (make-vector 2 unimplemented-method))

    (define-class <beta>
      (inherit <alpha>)
      (fields c d)
      (method (red (o <beta>))
	o.a)
      (method (blue (o <beta>))
	o.b)
      (protocol
       (lambda (make-superclass)
	 (parametrise ((virtual-methods-table the-<beta>-virtual-methods))
	   (<beta>-given-protocol make-superclass)))
       #;(make-protocol-with-virtual-methods
		 the-<beta>-virtual-methods
		 <beta>-given-protocol))
      )

    (define <beta>-given-protocol
      (lambda (make-alpha)
	(lambda (a b c d)
	  ((make-alpha a b) c d))))

    (define the-<beta>-virtual-methods
      (vector <beta>-red <beta>-blue))

    (check
	(let (((o <alpha>) (make <beta> 1 2 3 4)))
	  (o.red))
	=> #f)

    #f)

  #t)


;;;; done

(check-report)

;;; end of file
