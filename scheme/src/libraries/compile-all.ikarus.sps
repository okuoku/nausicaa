;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: compile script for Ikarus
;;;Date: Mon Jan 19, 2009
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

(import
    (only (language-extensions))
  (only (unimplemented))
  (only (conditions))
  (only (parameters))
  (only (pretty-print))
  (only (nausicaa common))
  (only (nausicaa))
  (only (lists))
  (only (one-dimension-cc))
  (only (one-dimension-co))
  (only (char-sets))
  (only (char-sets blocks))
  (only (char-sets categories))
  (only (strings))
  (only (vectors))
  (only (debugging))
  (only (checks))
  (only (loops))
  (only (format))
  (only (random))
  (only (random vectors))
  (only (random strings))
  (only (random distributions))
  (only (random borosh))
  (only (comparisons))
  (only (arrays))

  (only (scmobj))

  (only (deferred-exceptions))
  (only (compensations))
  (only (object-properties))
  (only (cleanup-handlers))
  (only (queues))
  (only (combinators))
  (only (sentinel))
  (only (variables))
  (only (keywords))
  (only (irregex))
  (only (pregexp))

  (only (silex lexer))
  (only (silex))

  (only (lalr common))
  (only (lalr lr-driver))
  (only (lalr glr-driver))
  (only (lalr))

  (only (csv strings-lexer))
  (only (csv unquoted-data-lexer))
  (only (csv unquoted-data-comma-lexer))
  (only (csv))

  (only (infix string-lexer))
  (only (infix sexp-parser))
  (only (infix string-parser))
  (only (infix))
  (only (infix syntax))

  (only (email addresses quoted-text-lexer))
  (only (email addresses comments-lexer))
  (only (email addresses domain-literals-lexer))
  (only (email addresses lexer))
;;;  (only (email addresses parser))
  (only (email addresses))

  (only (foreign))
  (only (foreign guarded-malloc))
  (only (peekers-and-pokers))
  )

;;; end of file
