;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: compile script for Ypsilon Scheme
;;;Date: Tue Nov  3, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (only (foreign posix environment))
  (only (foreign posix time))
  (only (foreign posix file))
  (only (foreign posix stat))
  (only (foreign posix fd))
  (only (foreign posix job))
  (only (foreign posix process))
  (only (foreign posix users))
  (only (foreign posix system))

  (only (foreign glibc environment))
  (only (foreign glibc streams))
  (only (foreign glibc time))
  (only (foreign glibc file))

  )

;;; end of file
