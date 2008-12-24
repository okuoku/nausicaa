;;;
;;;Part of: Nausicaa/Uriparser
;;;Contents: tests for the low level Uriparser interface
;;;Date: Wed Dec 24, 2008
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

(import (r6rs)
  (uriel lang)
  (uriel foreign)
  (uriel test)
  (uriparser stub)
  (uriparser sizeof))

(check-set-mode! 'report-failed)



(parameterize ((testname 'error-codes))

  (check
      (integer? (symbol->uriparser-error 'URI_SUCCESS))
    => #t)

  (check
      (integer? (symbol->uriparser-error 'URI_ERROR_ADDBASE_REL_BASE))
    => #t)

  (check
      (integer? (symbol->uriparser-error 'URI_ERROR_MALLOC))
    => #t)

  (check
      (integer? (symbol->uriparser-error 'URI_ERROR_NOT_IMPLEMENTED))
    => #t)

  (check
      (integer? (symbol->uriparser-error 'URI_ERROR_NULL))
    => #t)

  (check
      (integer? (symbol->uriparser-error 'URI_ERROR_OUTPUT_TOO_LARGE))
    => #t)

  (check
      (integer? (symbol->uriparser-error 'URI_ERROR_RANGE_INVALID))
    => #t)

  (check
      (integer? (symbol->uriparser-error 'URI_ERROR_REMOVEBASE_REL_BASE))
    => #t)

  (check
      (integer? (symbol->uriparser-error 'URI_ERROR_REMOVEBASE_REL_SOURCE))
    => #t)

  (check
      (integer? (symbol->uriparser-error 'URI_ERROR_SYNTAX))
    => #t)

  (check
      (integer? (symbol->uriparser-error 'URI_ERROR_TOSTRING_TOO_LONG))
    => #t)

  (check
      (symbol->uriparser-error 'woppa)
    => #f)

  )



(parameterize ((testname 'error-codes-or-error))

  (check
      (integer? (symbol->uriparser-error/or-error 'URI_SUCCESS))
    => #t)

  (check
      (integer? (symbol->uriparser-error/or-error 'URI_ERROR_ADDBASE_REL_BASE))
    => #t)

  (check
      (integer? (symbol->uriparser-error/or-error 'URI_ERROR_MALLOC))
    => #t)

  (check
      (integer? (symbol->uriparser-error/or-error 'URI_ERROR_NOT_IMPLEMENTED))
    => #t)

  (check
      (integer? (symbol->uriparser-error/or-error 'URI_ERROR_NULL))
    => #t)

  (check
      (integer? (symbol->uriparser-error/or-error 'URI_ERROR_OUTPUT_TOO_LARGE))
    => #t)

  (check
      (integer? (symbol->uriparser-error/or-error 'URI_ERROR_RANGE_INVALID))
    => #t)

  (check
      (integer? (symbol->uriparser-error/or-error 'URI_ERROR_REMOVEBASE_REL_BASE))
    => #t)

  (check
      (integer? (symbol->uriparser-error/or-error 'URI_ERROR_REMOVEBASE_REL_SOURCE))
    => #t)

  (check
      (integer? (symbol->uriparser-error/or-error 'URI_ERROR_SYNTAX))
    => #t)

  (check
      (integer? (symbol->uriparser-error/or-error 'URI_ERROR_TOSTRING_TOO_LONG))
    => #t)

  (check
      (guard (exc (else #t))
	(symbol->uriparser-error/or-error 'woppa))
    => #t)

  )



(parameterize ((testname 'error-symbols))

  (check
      (symbol? (uriparser-error->symbol URI_SUCCESS))
    => #t)

  (check
      (symbol? (uriparser-error->symbol URI_ERROR_ADDBASE_REL_BASE))
    => #t)

  (check
      (symbol? (uriparser-error->symbol URI_ERROR_MALLOC))
    => #t)

  (check
      (symbol? (uriparser-error->symbol URI_ERROR_NOT_IMPLEMENTED))
    => #t)

  (check
      (symbol? (uriparser-error->symbol URI_ERROR_NULL))
    => #t)

  (check
      (symbol? (uriparser-error->symbol URI_ERROR_OUTPUT_TOO_LARGE))
    => #t)

  (check
      (symbol? (uriparser-error->symbol URI_ERROR_RANGE_INVALID))
    => #t)

  (check
      (symbol? (uriparser-error->symbol URI_ERROR_REMOVEBASE_REL_BASE))
    => #t)

  (check
      (symbol? (uriparser-error->symbol URI_ERROR_REMOVEBASE_REL_SOURCE))
    => #t)

  (check
      (symbol? (uriparser-error->symbol URI_ERROR_SYNTAX))
    => #t)

  (check
      (symbol? (uriparser-error->symbol URI_ERROR_TOSTRING_TOO_LONG))
    => #t)

  (check
      (uriparser-error->symbol 100000)
    => #f)

  )



(parameterize ((testname 'error-symbols-or-error))

  (check
      (symbol? (uriparser-error->symbol/or-error URI_SUCCESS))
    => #t)

  (check
      (symbol? (uriparser-error->symbol/or-error URI_ERROR_ADDBASE_REL_BASE))
    => #t)

  (check
      (symbol? (uriparser-error->symbol/or-error URI_ERROR_MALLOC))
    => #t)

  (check
      (symbol? (uriparser-error->symbol/or-error URI_ERROR_NOT_IMPLEMENTED))
    => #t)

  (check
      (symbol? (uriparser-error->symbol/or-error URI_ERROR_NULL))
    => #t)

  (check
      (symbol? (uriparser-error->symbol/or-error URI_ERROR_OUTPUT_TOO_LARGE))
    => #t)

  (check
      (symbol? (uriparser-error->symbol/or-error URI_ERROR_RANGE_INVALID))
    => #t)

  (check
      (symbol? (uriparser-error->symbol/or-error URI_ERROR_REMOVEBASE_REL_BASE))
    => #t)

  (check
      (symbol? (uriparser-error->symbol/or-error URI_ERROR_REMOVEBASE_REL_SOURCE))
    => #t)

  (check
      (symbol? (uriparser-error->symbol/or-error URI_ERROR_SYNTAX))
    => #t)

  (check
      (symbol? (uriparser-error->symbol/or-error URI_ERROR_TOSTRING_TOO_LONG))
    => #t)

  (check
      (guard (exc (else #t))
	(uriparser-error->symbol/or-error 100000))
    => #t)

  )



(parameterize ((testname 'strerror))

  (check
      (string? (uriparser-strerror 'URI_SUCCESS))
    => #t)

  (check
      (string? (uriparser-strerror 'URI_ERROR_ADDBASE_REL_BASE))
    => #t)

  (check
      (string? (uriparser-strerror 'URI_ERROR_MALLOC))
    => #t)

  (check
      (string? (uriparser-strerror 'URI_ERROR_NOT_IMPLEMENTED))
    => #t)

  (check
      (string? (uriparser-strerror 'URI_ERROR_NULL))
    => #t)

  (check
      (string? (uriparser-strerror 'URI_ERROR_OUTPUT_TOO_LARGE))
    => #t)

  (check
      (string? (uriparser-strerror 'URI_ERROR_RANGE_INVALID))
    => #t)

  (check
      (string? (uriparser-strerror 'URI_ERROR_REMOVEBASE_REL_BASE))
    => #t)

  (check
      (string? (uriparser-strerror 'URI_ERROR_REMOVEBASE_REL_SOURCE))
    => #t)

  (check
      (string? (uriparser-strerror 'URI_ERROR_SYNTAX))
    => #t)

  (check
      (string? (uriparser-strerror 'URI_ERROR_TOSTRING_TOO_LONG))
    => #t)

  )



;;;; done

(check-report)

;;; end of file
