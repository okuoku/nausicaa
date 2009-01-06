;;;
;;;Part of: Nausicaa/SRFI
;;;Contents: compile script
;;;Date: Thu Dec 25, 2008
;;;
;;;Abstract
;;;
;;;	Order  does matter!!!  Libraries  that are  imported into  other
;;;	libraries must be compiled first.
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

(import (rnrs)
  (larceny compiler))

;;; --------------------------------------------------------------------

(compile-library "srfi/private/let-opt.sls"
		 "srfi/private/let-opt.larceny.slfasl")

;;; --------------------------------------------------------------------

(compile-library "srfi/and-let-star.sls"
		 "srfi/and-let-star.larceny.slfasl")

(compile-library "srfi/parameters.larceny.sls"
		 "srfi/parameters.larceny.slfasl")

(compile-library "srfi/cond-expand/implementation-features.larceny.sls"
		 "srfi/cond-expand/implementation-features.larceny.slfasl")

(compile-library "srfi/cond-expand/registry.sls"
		 "srfi/cond-expand/registry.larceny.slfasl")

(compile-library "srfi/cond-expand.sls"
		 "srfi/cond-expand.larceny.slfasl")

(compile-library "srfi/receive.sls"
		 "srfi/receive.larceny.slfasl")

(compile-library "srfi/sharing.sls"
		 "srfi/sharing.larceny.slfasl")

(compile-library "srfi/lists/compat.larceny.sls"
		 "srfi/lists/compat.larceny.slfasl")

(compile-library "srfi/lists.sls"
 		 "srfi/lists.larceny.slfasl")

(compile-library "srfi/records.sls"
		 "srfi/records.larceny.slfasl")

(compile-library "srfi/char-set.sls"
		 "srfi/char-set.larceny.slfasl")

(compile-library "srfi/strings.sls"
		 "srfi/strings.larceny.slfasl")

(compile-library "srfi/format/compat.larceny.sls"
		 "srfi/format/compat.larceny.slfasl")

(compile-library "srfi/format.sls"
		 "srfi/format.larceny.slfasl")

(compile-library "srfi/time/compat.larceny.sls"
		 "srfi/time/compat.larceny.slfasl")

(compile-library "srfi/time.sls"
		 "srfi/time.larceny.slfasl")

(compile-library "srfi/cut.sls"
		 "srfi/cut.larceny.slfasl")

(compile-library "srfi/random.sls"
		 "srfi/random.larceny.slfasl")

(compile-library "srfi/rec.sls"
		 "srfi/rec.larceny.slfasl")

(compile-library "srfi/args-fold.sls"
		 "srfi/args-fold.larceny.slfasl")

(compile-library "srfi/streams.sls"
		 "srfi/streams.larceny.slfasl")

(compile-library "srfi/eager-comprehensions.sls"
		 "srfi/eager-comprehensions.larceny.slfasl")

(compile-library "srfi/vectors.sls"
		 "srfi/vectors.larceny.slfasl")

(compile-library "srfi/general-cond.sls"
		 "srfi/general-cond.larceny.slfasl")

(compile-library "srfi/compare.sls"
		 "srfi/compare.larceny.slfasl")

(compile-library "srfi/lightweight-testing/compat.larceny.sls"
		 "srfi/lightweight-testing/compat.larceny.slfasl")

(compile-library "srfi/lightweight-testing.sls"
		 "srfi/lightweight-testing.larceny.slfasl")

(compile-library "srfi/environment-variables.larceny.sls"
		 "srfi/environment-variables.larceny.slfasl")

;;; --------------------------------------------------------------------

(compile-library "check-lib.sls"	"check-lib.larceny.slfasl")
(compile-library "env-lib.sls"		"env-lib.larceny.slfasl")
(compile-library "features-lib.sls"	"features-lib.larceny.slfasl")
(compile-library "format-lib.sls"	"format-lib.larceny.slfasl")
(compile-library "lang-lib.sls"		"lang-lib.larceny.slfasl")
(compile-library "list-lib.sls"		"list-lib.larceny.slfasl")
(compile-library "loop-lib.sls"		"loop-lib.larceny.slfasl")
(compile-library "string-lib.sls"	"string-lib.larceny.slfasl")
(compile-library "vector-lib.sls"	"vector-lib.larceny.slfasl")

;;; end of file
