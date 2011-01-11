;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: R6RS source code parser
;;;Date: Fri Jan  7, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa r6rs parser)
  (export
    make-r6rs-parser

    ;; the following are reexported from (nausicaa r6rs datum-processing)
    <commented-datum>			<interlexeme-space>
    remove-interlexeme-space

    list-of-datums-maker		make-list-of-datums

    identifier-datum-maker		make-identifier-datum
    boolean-datum-maker			make-boolean-datum
    number-datum-maker			make-number-datum
    string-datum-maker			make-string-datum
    character-datum-maker		make-character-datum
    pair-datum-maker			make-pair-datum
    list-datum-maker			make-list-datum
    vector-datum-maker			make-vector-datum
    bytevector-datum-maker		make-bytevector-datum

    quoted-datum-maker			make-quoted-datum
    quasiquoted-datum-maker		make-quasiquoted-datum
    unquoted-datum-maker		make-unquoted-datum
    unquoted-splicing-datum-maker	make-unquoted-splicing-datum
    syntax-datum-maker			make-syntax-datum
    quasisyntax-datum-maker		make-quasisyntax-datum
    unsyntax-datum-maker		make-unsyntax-datum
    unsyntax-splicing-datum-maker	make-unsyntax-splicing-datum

    interlexeme-space-datum-maker	make-interlexeme-space-datum
    whitespace-datum-maker		make-whitespace-datum
    line-comment-datum-maker		make-line-comment-datum
    nested-comment-datum-maker		make-nested-comment-datum
    sharp-semicolon-datum-maker		make-sharp-semicolon-datum
    sharp-bang-datum-maker		make-sharp-bang-datum
    sharp-bang-r6rs-datum-maker		make-sharp-bang-r6rs-datum
    )
  (import (nausicaa)
    (nausicaa r6rs parser-table)
    (nausicaa r6rs datum-processing)))

;;; end of file
