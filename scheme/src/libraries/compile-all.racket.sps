;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: compile script for Racket
;;;Date: Mon Apr  4, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (rnrs)

  ;; Core libraries
  (only (nausicaa language cond-expand))
  (only (nausicaa language unimplemented))
  (only (nausicaa language conditions))
  (only (nausicaa language extensions))
  (only (nausicaa language parameters))
  (only (nausicaa language pretty-print))
  (only (nausicaa language shared-structures))
  (only (nausicaa language sentinel))
  (only (nausicaa language getenv))
  (only (nausicaa configuration))
  (only (nausicaa contracts))
  (only (nausicaa language assertions))
  (only (nausicaa language makers))
  (only (nausicaa language identifier-properties))
  (only (nausicaa language classes))
  (only (nausicaa language generics))
  (only (nausicaa language deferred-exceptions))
  (only (nausicaa language compensations))
  (only (nausicaa language matches))
  (only (nausicaa))
  (only (nausicaa mutable-pairs))
  (only (nausicaa mutable-strings))

  ;; Basic libraries
  (only (nausicaa generics object-to-string))
  (only (nausicaa profiling))
  (only (nausicaa enumerations))
  (only (nausicaa lists))
  (only (nausicaa one-dimension-cc))
  (only (nausicaa one-dimension-co))
  (only (nausicaa char-sets))
  (only (nausicaa char-sets blocks))
  (only (nausicaa char-sets categories))
  (only (nausicaa asciis))
  (only (nausicaa bytevectors u8))
  (only (nausicaa strings))
  (only (nausicaa strings xstrings))
  (only (nausicaa streams))
  (only (nausicaa vectors))
  (only (nausicaa vectors xvectors))
  (only (nausicaa bytevectors u8))
  (only (nausicaa debugging))
  (only (nausicaa checks))
  (only (nausicaa loops))
  (only (nausicaa formations))
  (only (nausicaa randomisations))
  (only (nausicaa randomisations vectors))
  (only (nausicaa randomisations strings))
  (only (nausicaa randomisations distributions))
  (only (nausicaa randomisations borosh))
  (only (nausicaa comparisons))
  (only (nausicaa arrays))
  (only (nausicaa msgcat))
  (only (nausicaa times-and-dates))

  (only (nausicaa variables))
  (only (nausicaa keywords))

  (only (nausicaa object-properties))
  (only (nausicaa cleanup-handlers))

  (only (nausicaa queues))
  (only (nausicaa stacks))

  ;;High level libraries.
  (only (nausicaa combinators))
  (only (nausicaa irregex))
  (only (nausicaa pregexp))

  (only (nausicaa parser-tools source-location))
  (only (nausicaa parser-tools lexical-token))
  (only (nausicaa parser-tools))

  (only (nausicaa silex lexer))
  (only (nausicaa silex))
  (only (nausicaa silex utilities))

  (only (nausicaa lalr lr-driver))
  (only (nausicaa lalr glr-driver))
  (only (nausicaa lalr))

  (only (nausicaa csv strings-lexer))
  (only (nausicaa csv unquoted-data-lexer))
  (only (nausicaa csv unquoted-data-comma-lexer))
  (only (nausicaa csv))

  (only (nausicaa infix string-lexer))
  (only (nausicaa infix sexp-parser))
  (only (nausicaa infix string-parser))
  (only (nausicaa infix))

  (only (nausicaa packrat))

  (only (nausicaa email addresses common))
  (only (nausicaa email addresses quoted-text-lexer))
  (only (nausicaa email addresses comments-lexer))
  (only (nausicaa email addresses domain-literals-lexer))
  (only (nausicaa email addresses lexer))
  (only (nausicaa email addresses parser))
  (only (nausicaa email addresses))

  (only (nausicaa r6rs lexer))
  (only (nausicaa r6rs parser))

  (only (nausicaa getopts))
  (only (nausicaa json))
  (only (nausicaa uri))
  (only (nausicaa net ipv6-addresses))
  (only (nausicaa submodules))
  (only (nausicaa interps))
  (only (nausicaa evaluations))

  (only (nausicaa armor conditions))
  (only (nausicaa armor base16))
  (only (nausicaa armor base32))
  (only (nausicaa armor base64))
  (only (nausicaa armor base91))
  (only (nausicaa armor ascii85))
  (only (nausicaa armor newlines))
  (only (nausicaa armor quoted-printable))

;;; (only (nausicaa xml markups lexer))
;;; (only (nausicaa xml markups parser))

;;   (only (nausicaa ffi))

;;   (only (nausicaa ffi memory))
;;   (only (nausicaa ffi memory mempool))
;;   (only (nausicaa ffi memory refcount))
;;   (only (nausicaa ffi memory membuffers))
;; ;;;  (only (nausicaa ffi memory guarded-malloc))

;;   (only (nausicaa ffi cstrings))
;;   (only (nausicaa ffi errno))

  )

;;; end of file
