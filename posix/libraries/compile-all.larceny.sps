;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: compile script
;;;Date: Fri Dec 26, 2008
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

(import (rnrs)
  (larceny compiler))

(compile-library "posix/sizeof.sls"
		 "posix/sizeof.larceny.slfasl")

(compile-library "posix/working-directory.sls"
		 "posix/working-directory.larceny.slfasl")

(compile-library "posix/time/stub.sls"
		 "posix/time/stub.larceny.slfasl")

(compile-library "posix/time.sls"
		 "posix/time.larceny.slfasl")

(compile-library "posix/process/stub.sls"
		 "posix/process/stub.larceny.slfasl")

(compile-library "posix/process.sls"
		 "posix/process.larceny.slfasl")

(compile-library "posix/job.sls"
		 "posix/job.larceny.slfasl")

(compile-library "posix/fd.sls"
		 "posix/fd.larceny.slfasl")

(compile-library "posix/environment/compat.larceny.sls"
		 "posix/environment/compat.larceny.slfasl")

(compile-library "posix/environment.sls"
		 "posix/environment.larceny.slfasl")

;;; end of file
