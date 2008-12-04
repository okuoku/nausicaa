;;;
;;;Part of: Nausicaa/Glibc
;;;Contents: test for streams
;;;Date: Thu Dec  4, 2008
;;;Time-stamp: <2008-12-04 13:51:57 marco>
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
  (uriel lang)
  (uriel printing)
  (uriel test)
  (uriel getenv)
  (uriel ffi)
  (only (string-lib) string-join)
  (glibc streams)
  (glibc streams-extended)
  (glibc streams-unlocked))

(check-set-mode! 'report-failed)

(define TMPDIR (getenv "TMPDIR"))
(define s->c string-or-symbol->cstring/compensated)
(define the-pathname (string-join (list TMPDIR "name.ext")))
(define the-string "Le Poete est semblable au prince des nuees
Qui hante la tempete e se rit de l'archer;
Exile sul le sol au milieu des huees,
Ses ailes de geant l'empechent de marcher.")



;;;; code

(check
    (let ((pathname the-pathname))
      (with-compensations
	(let* ((S (fopen pathname "w+"))
	       (p (s->c the-string))
	       (len (strlen p)))
	  (fwrite p 1 len S)
	  (fread p len 1 S)
	  (fclose S)
	  (cstring->string p))))
  => the-string)

(check
    (let ((pathname the-pathname))
      (with-compensations
	(let* ((S	(fopen pathname "w+"))
	       (p	(s->c the-string))
	       (len	(strlen p)))
	  (fwrite p 1 len S)
	  (fseek S 0 valueof-seek-set)
	  (begin0
	      (list (getline S len)
		    (getline S len))
	    (fclose S)))))
  => '("Le Poete est semblable au prince des nuees\n"
       "Qui hante la tempete e se rit de l'archer;\n"))



;;;; done

(check-report)

;;; end of file
