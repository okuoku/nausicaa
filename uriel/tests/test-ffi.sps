;;;
;;;Part of: Uriel libraries
;;;Contents: tests for ffi library
;;;Date: Tue Nov 18, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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


;;;; setup

(import (rnrs)
  (srfi parameters)
  (uriel printing)
  (uriel ffi)
  (uriel ffi sizeof)
  (uriel test))

(check-set-mode! 'report-failed)


;;;; memory allocation and accessors

(parameterize ((testname 'memory))

  (check
      (let* ((p (primitive-malloc (expt 10 5)))
	     (d (begin
		  (pointer-set-c-char! p 100 65)
		  (pointer-ref-c-signed-char p 100))))
	(primitive-free p)
	d)
    => 65)
  (check
      (let* ((p (primitive-malloc (expt 10 5)))
	     (d (begin
		  (pointer-set-c-char! p 100 128)
		  (pointer-ref-c-signed-char p 100))))
	(primitive-free p)
	d)
    => -128)

;;; --------------------------------------------------------------------

  (check
      (let* ((p (primitive-malloc (expt 10 5)))
	     (d (begin
		  (pointer-set-c-char! p 100 65)
		  (pointer-ref-c-unsigned-char p 100))))
	(primitive-free p)
	d)
    => 65)
  (check
      (let* ((p (primitive-malloc (expt 10 5)))
	     (d (begin
		  (pointer-set-c-char! p 100 128)
		  (pointer-ref-c-unsigned-char p 100))))
	(primitive-free p)
	d)
    => 128)

;;; --------------------------------------------------------------------

;;   (check
;;       (let* ((p (primitive-malloc (expt 10 5)))
;; 	     (d (begin
;; 		  (pointer-set-c-short! p 100 65)
;; 		  (pointer-ref-c-signed-short p 100))))
;; 	(primitive-free p)
;; 	d)
;;     => 65)
;;   (check
;;       (let* ((p (primitive-malloc (expt 10 5)))
;; 	     (d (begin
;; 		  (pointer-set-c-short! p 100 32768)
;; 		  (pointer-ref-c-signed-short p 100))))
;; 	(primitive-free p)
;; 	d)
;;     => -32768)

;;; --------------------------------------------------------------------

;;   (check
;;       (let* ((p (primitive-malloc (expt 10 5)))
;; 	     (d (begin
;; 		  (pointer-set-c-short! p 100 65)
;; 		  (pointer-ref-c-unsigned-short p 100))))
;; 	(primitive-free p)
;; 	d)
;;     => 65)
;;   (check
;;       (let* ((p (primitive-malloc (expt 10 5)))
;; 	     (d (begin
;; 		  (pointer-set-c-short! p 100 32768)
;; 		  (pointer-ref-c-unsigned-short p 100))))
;; 	(primitive-free p)
;; 	d)
;;     => 32768)

;;; --------------------------------------------------------------------

;;   (check
;;       (let* ((p (primitive-malloc (expt 10 5)))
;; 	     (d (begin
;; 		  (pointer-set-c-int! p 100 65)
;; 		  (pointer-ref-c-signed-int p 100))))
;; 	(primitive-free p)
;; 	d)
;;     => 65)
;;   (check
;;       (let* ((p (primitive-malloc (expt 10 5)))
;; 	     (d (begin
;; 		  (pointer-set-c-int! p 100 (expt 2 31))
;; 		  (pointer-ref-c-signed-int p 100))))
;; 	(primitive-free p)
;; 	d)
;;     => (- (expt 2 31)))

;;; --------------------------------------------------------------------

;;   (check
;;       (let* ((p (primitive-malloc (expt 10 5)))
;; 	     (d (begin
;; 		  (pointer-set-c-int! p 100 65)
;; 		  (pointer-ref-c-unsigned-int p 100))))
;; 	(primitive-free p)
;; 	d)
;;     => 65)
;;   (check
;;       (let* ((p (primitive-malloc (expt 10 5)))
;; 	     (d (begin
;; 		  (pointer-set-c-int! p 100 (expt 2 31))
;; 		  (pointer-ref-c-unsigned-int p 100))))
;; 	(primitive-free p)
;; 	d)
;;     => (expt 2 31))

;;; --------------------------------------------------------------------

;;   (check
;;       (let* ((p (primitive-malloc (expt 10 5)))
;; 	     (d (begin
;; 		  (pointer-set-c-long! p 64 65)
;; 		  (pointer-ref-c-signed-long p 64))))
;; 	(primitive-free p)
;; 	d)
;;     => 65)
;;   (check
;;       (let* ((p (primitive-malloc (expt 10 5)))
;; 	     (d (begin
;; 		  (pointer-set-c-long! p 64 (expt 2 (if on-64-bits-system 63 31)))
;; 		  (pointer-ref-c-signed-long p 64))))
;; 	(primitive-free p)
;; 	d)
;;     => (- (expt 2 (if on-64-bits-system 63 31))))

;;; --------------------------------------------------------------------

;;   (check
;;       (let* ((p (primitive-malloc (expt 10 5)))
;; 	     (d (begin
;; 		  (pointer-set-c-long! p 64 65)
;; 		  (pointer-ref-c-unsigned-long p 64))))
;; 	(primitive-free p)
;; 	d)
;;     => 65)
;;   (check
;;       (let* ((p (primitive-malloc (expt 10 5)))
;; 	     (d (begin
;; 		  (pointer-set-c-long! p 64 (expt 2 (if on-64-bits-system 63 31)))
;; 		  (pointer-ref-c-unsigned-long p 64))))
;; 	(primitive-free p)
;; 	d)
;;     => (expt 2 (if on-64-bits-system 63 31)))

;;; --------------------------------------------------------------------

;;   (check
;;       (let* ((p (primitive-malloc (expt 10 5)))
;; 	     (d (begin
;; 		  (pointer-set-c-float! p 64 4.5)
;; 		  (pointer-ref-c-float p 64))))
;; 	(primitive-free p)
;; 	d)
;;     => 4.5)

;;   (check
;;       (let* ((p (primitive-malloc (expt 10 5)))
;; 	     (d (begin
;; 		  (pointer-set-c-double! p 64 4.5)
;; 		  (pointer-ref-c-double p 64))))
;; 	(primitive-free p)
;; 	d)
;;     => 4.5)

;;; --------------------------------------------------------------------

;;   (check
;;       (let* ((p (primitive-malloc (expt 10 5)))
;; 	     (d (begin
;; 		  (pointer-set-c-pointer! p 100 (integer->pointer 90))
;; 		  (pointer-ref-c-pointer p 100))))
;; 	(primitive-free p)
;; 	(pointer->integer d))
;;     => 90)

  )


;;;; string functions

(parameterize ((testname 'string))

  (check
      (cstring->string (string->cstring "ciao"))
    => "ciao")

  (check
      (cstring->string (string->cstring ""))
    => "")

  (check
      (strlen (string->cstring "ciao"))
    => 4)

  (check
      (strlen (string->cstring ""))
    => 0))


;;;; done

(check-report)

;;; end of file
