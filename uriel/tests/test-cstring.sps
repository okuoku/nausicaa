;;;
;;;Part of: Nausicaa/Uriel
;;;Contents: tests for the cstring library
;;;Date: Wed Dec 17, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
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

(import (r6rs)
  (uriel lang)
  (uriel test)
  (uriel memory)
  (uriel cstring))

(check-set-mode! 'report-failed)



(parameterize ((testname 'inspection))

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
  )



(parameterize ((testname 'conversion))

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
	(cstring->string/len (string->cstring "ciao, hello" malloc/c) 4))
    => "ciao")

  (check
      (with-compensations
	(cstring->string/len (string->cstring "ciao, hello" malloc/c) 1))
    => "c")

  (check
      (with-compensations
	(cstring->string/len (string->cstring "ciao, hello" malloc/c) 0))
    => "")

  (check
      (with-compensations
	(cstring->string/len (string->cstring "ciao, hello" malloc/c) 11))
    => "ciao, hello")

  (check
      (with-compensations
	(cstring->string/len (string->cstring/c "ciao, hello") 4))
    => "ciao")

  )




(parameterize ((testname 'operations))

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

)



(parameterize ((testname 'argv))

  (check
      (with-compensations
	(argv->strings (strings->argv '("alpha" "beta" gamma))))
    => '("alpha" "beta" "gamma"))

  (check
      (with-compensations
	(argv-length (strings->argv '("alpha" "beta" gamma))))
    => 3)

  )



;;;; done

(check-report)

;;; end of file
