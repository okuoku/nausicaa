;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: compile script for Ikarus
;;;Date: Mon Jan 19, 2009
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

(import (rnrs)

  ;; Core libraries
  (only (cond-expand))
  (only (unimplemented))
  (only (conditions))
  (only (language-extensions))
  (only (parameters))
  (only (pretty-print))
  (only (shared-structures))
  (only (classes))
  (only (labels))
  (only (deferred-exceptions))
  (only (compensations))
  (only (nausicaa))

  ;; Basic libraries
  (only (generics))
  (only (profiling))
  (only (enumerations))
  (only (lists))
  (only (one-dimension-cc))
  (only (one-dimension-co))
  (only (char-sets))
  (only (char-sets blocks))
  (only (char-sets categories))
  (only (strings))
  (only (streams))
  (only (vectors))
  (only (debugging))
  (only (checks))
  (only (loops))
  (only (formations))
  (only (randomisations))
  (only (randomisations vectors))
  (only (randomisations strings))
  (only (randomisations distributions))
  (only (randomisations borosh))
  (only (comparisons))
  (only (arrays))
  (only (times-and-dates))

  (only (sentinel))
  (only (variables))
  (only (keywords))

  (only (object-properties))
  (only (cleanup-handlers))

  (only (queues))
  (only (stacks))

  ;;High level libraries.
  (only (scmobj))
  (only (scmobj utils))

  (only (combinators))
  (only (irregex))
  (only (pregexp))

  (only (sexps))
  (only (matches))

  (only (parser-tools source-location))
  (only (parser-tools lexical-token))

  (only (silex lexer))
  (only (silex))

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

  (only (packrat))

  (only (email addresses common))
  (only (email addresses quoted-text-lexer))
  (only (email addresses comments-lexer))
  (only (email addresses domain-literals-lexer))
  (only (email addresses lexer))
  (only (email addresses parser))
  (only (email addresses))

  (only (getopts))
  (only (libraries))

  (only (armor conditions))
  (only (armor base16))
  (only (armor base32))
  (only (armor base64))
  (only (armor base91))
  (only (armor ascii85))
  (only (armor newlines))
  (only (armor quoted-printable))

  (only (ffi))
  (only (ffi utilities))
  (only (ffi parametrised))

  (only (ffi memory))
  (only (ffi memory mempool))
  (only (ffi memory refcount))
  (only (ffi memory membuffers))
  (only (ffi memory guarded-malloc))

  (only (ffi cstrings))
  (only (ffi errno))

  )

;;; end of file
