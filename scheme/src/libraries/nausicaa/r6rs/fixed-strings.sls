;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: MMUX Home Directory
;;;Contents: collection of strings
;;;Date: Fri Jan  7, 2011
;;;
;;;Abstract
;;;
;;;	This library  defines and exports bindings to  strings which are
;;;	meant to be comparable using EQ?.
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
(library (nausicaa r6rs fixed-strings)
  (export
    comma-at
    sharp-paren
    sharp-vu8-paren
    sharp-tick
    sharp-back-tick
    sharp-comma-at
    sharp-comma
    sharp-semicolon
    sharp-bang-r6rs
    sharp-bang
    open-nested-comment
    true-small
    true-capital
    false-small
    false-capital

    named-character-nul
    named-character-alarm
    named-character-backspace
    named-character-tab
    named-character-linefeed
    named-character-newline
    named-character-vtab
    named-character-page
    named-character-return
    named-character-esc
    named-character-space
    named-character-delete
    )
  (import (rnrs))

  (define comma-at		",@")
  (define sharp-paren		"#(")
  (define sharp-vu8-paren	"#vu8(")
  (define sharp-tick		"#'")
  (define sharp-back-tick	"#`")
  (define sharp-comma-at	"#,@")
  (define sharp-comma		"#,")
  (define sharp-semicolon	"#;")
  (define sharp-bang-r6rs	"#!r6rs")
  (define sharp-bang		"#!")
  (define open-nested-comment	"#|")
  (define true-small		"#t")
  (define true-capital		"#T")
  (define false-small		"#f")
  (define false-capital		"#F")

  (define named-character-nul		"#\\nul")
  (define named-character-alarm		"#\\alarm")
  (define named-character-backspace	"#\\backspace")
  (define named-character-tab		"#\\tab")
  (define named-character-linefeed	"#\\linefeed")
  (define named-character-newline	"#\\newline")
  (define named-character-vtab		"#\\vtab")
  (define named-character-page		"#\\page")
  (define named-character-return	"#\\return")
  (define named-character-esc		"#\\esc")
  (define named-character-space		"#\\space")
  (define named-character-delete	"#\\delete"))

;;; end of file
