;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for the cstring library
;;;Date: Wed Dec 17, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008-2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (nausicaa checks)
  (nausicaa ffi memory)
  (nausicaa ffi cstrings))

(check-set-mode! 'report-failed)
(display "*** testing cstrings\n")


(parameterize ((check-test-name 'inspection))

  (check
      (with-compensations
	(strlen (string->cstring "ciao" malloc/c)))
    => 4)

  (check
      (with-compensations
	(strlen (string->cstring "" malloc/c)))
    => 0)

  (check
      (with-compensations
	(strlen (string->cstring "c" malloc/c)))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(let ((a (string->cstring "ciao" malloc/c))
	      (b (string->cstring "ciao" malloc/c)))
	  (strcmp a b)))
    => 0)

  (check
      (with-compensations
	(let ((a (string->cstring "ciaoa" malloc/c))
	      (b (string->cstring "ciao" malloc/c)))
	  (< 0 (strcmp a b))))
    => #t)

  (check
      (with-compensations
	(let ((a (string->cstring "ciao" malloc/c))
	      (b (string->cstring "ciaoa" malloc/c)))
	  (> 0 (strcmp a b))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(let ((a (string->cstring "ciao" malloc/c))
	      (b (string->cstring "ciao" malloc/c)))
	  (strncmp a b 3)))
    => 0)

  (check
      (with-compensations
	(let ((a (string->cstring "ciaao" malloc/c))
	      (b (string->cstring "ciauo" malloc/c)))
	  (> 0 (strncmp a b 4))))
    => #t)

  (check
      (with-compensations
	(let ((a (string->cstring "ciauo" malloc/c))
	      (b (string->cstring "ciaao" malloc/c)))
	  (< 0 (strncmp a b 4))))
    => #t)

  (check
      (with-compensations
	(let ((a (string->cstring "ciauo" malloc/c))
	      (b (string->cstring "ciaao" malloc/c)))
	  (strncmp a b 3)))
    => 0)

  #t)


(parameterize ((check-test-name 'conversion-string))

  (check
      (with-compensations
	(cstring->string (string->cstring "ciao" malloc/c)))
    => "ciao")

  (check
      (with-compensations
	(cstring->string (string->cstring "" malloc/c)))
    => "")

  (check
      (with-compensations
	(strlen (string->cstring "ciao" malloc/c)))
    => 4)

  (check
      (with-compensations
	(strlen (string->cstring "" malloc/c)))
    => 0)

  (check
      (with-compensations
	(cstring->string (string->cstring "ciao, hello" malloc/c) 4))
    => "ciao")

  (check
      (with-compensations
	(cstring->string (string->cstring "ciao, hello" malloc/c) 1))
    => "c")

  (check
      (with-compensations
	(cstring->string (string->cstring "ciao, hello" malloc/c) 0))
    => "")

  (check
      (with-compensations
	(cstring->string (string->cstring "ciao, hello" malloc/c) 11))
    => "ciao, hello")

  (check
      (with-compensations
	(cstring->string (string->cstring/c "ciao, hello") 4))
    => "ciao")

  #t)


(parameterize ((check-test-name 'conversion-memblock))

  (check
      (with-compensations
	(memblock->string (string->memblock "ciao" malloc/c)))
    => "ciao")

  (check
      (with-compensations
	(memblock->string (string->memblock "" malloc/c)))
    => "")

  (check
      (with-compensations
	(<memblock>-size (string->memblock "ciao" malloc/c)))
    => 4)

  (check
      (with-compensations
	(<memblock>-size (string->memblock "" malloc/c)))
    => 0)

  (check
      (with-compensations
	(memblock->string (string->memblock "ciao, hello" malloc/c) 4))
    => "ciao")

  (check
      (with-compensations
	(memblock->string (string->memblock "ciao, hello" malloc/c) 1))
    => "c")

  (check
      (with-compensations
	(memblock->string (string->memblock "ciao, hello" malloc/c) 0))
    => "")

  (check
      (with-compensations
	(memblock->string (string->memblock "ciao, hello" malloc/c) 11))
    => "ciao, hello")

  (check
      (with-compensations
	(memblock->string (string->memblock/c "ciao, hello") 4))
    => "ciao")

  #t)


(parameterize ((check-test-name 'operations))

  (check
      (with-compensations
	(let* ((s (string->cstring "ciao" malloc/c))
	       (p (malloc/c (+ 1 (strlen s)))))
	  (strcpy p s)
	  (cstring->string p)))
    => "ciao")

  (check
      (with-compensations
	(let* ((s (string->cstring "ciao hello" malloc/c))
	       (p (malloc/c 5)))
	  (strncpy p s 4)
	  (cstring->string p)))
    => "ciao")

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(cstring->string (strdup (string->cstring "ciao" malloc/c) malloc/c)))
    => "ciao")

  (check
      (with-compensations
	(cstring->string (strndup (string->cstring "ciao hello" malloc/c) 4 malloc/c)))
    => "ciao")

  #t)


(parameterize ((check-test-name 'argv))

  (check
      (with-compensations
	(argv->strings (strings->argv '("alpha" "beta" gamma))))
    => '("alpha" "beta" "gamma"))

  (check
      (with-compensations
	(argv-length (strings->argv '("alpha" "beta" gamma))))
    => 3)

  #t)


;;;; done

(check-report)

;;; end of file
