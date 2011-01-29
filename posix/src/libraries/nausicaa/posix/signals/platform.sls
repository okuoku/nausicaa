;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: callouts for interprocess signal functions
;;;Date: Fri Dec 18, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (nausicaa posix signals platform)
  (export
    signal_bub_init		signal_bub_final
    signal_bub_acquire		signal_bub_delivered

    signal_raise
    kill			killpg
    pause
    )
  (import (rnrs)
    (nausicaa ffi)
    (nausicaa ffi sizeof)
    (nausicaa posix sizeof)
    (nausicaa posix shared-object))

  (define-c-functions libnausicaa-posix
    (signal_bub_init		(void nausicaa_posix_signal_bub_init (void)))
    (signal_bub_final		(void nausicaa_posix_signal_bub_final (void)))
    (signal_bub_acquire		(void nausicaa_posix_signal_bub_acquire (void)))
    (signal_bub_delivered	(int nausicaa_posix_signal_bub_delivered (int)))
    )

  (define-c-functions libc-shared-object
    (signal_raise		(int raise (int)))
    )

  (define-c-functions/with-errno libc-shared-object
    (kill			(int kill (pid_t int)))
    (killpg			(int killpg (int int)))
    (pause			(int pause (void)))))

;;; end of file
