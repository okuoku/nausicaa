;;; (compression zlib structs) --
;;;
;;;Part of: Nausicaa
;;;Contents: foreign library structs fields identifier accessors
;;;Date: Fri Apr 23, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library
  (compression zlib structs)
  (export <struct-z_stream> <struct-gz_header>)
  (import
    (nausicaa)
    (ffi)
    (ffi utilities)
    (compression zlib sizeof))
  (define-class
    <struct-z_stream>
    (parent <c-struct>)
    (virtual-fields
      (mutable
        opaque
        <struct-z_stream>-opaque
        <struct-z_stream>-opaque-set!)
      (mutable
        zfree
        <struct-z_stream>-zfree
        <struct-z_stream>-zfree-set!)
      (mutable
        zalloc
        <struct-z_stream>-zalloc
        <struct-z_stream>-zalloc-set!)
      (mutable
        adler
        <struct-z_stream>-adler
        <struct-z_stream>-adler-set!)
      (mutable
        data_type
        <struct-z_stream>-data_type
        <struct-z_stream>-data_type-set!)
      (mutable
        msg
        <struct-z_stream>-msg
        <struct-z_stream>-msg-set!)
      (mutable
        total_out
        <struct-z_stream>-total_out
        <struct-z_stream>-total_out-set!)
      (mutable
        avail_out
        <struct-z_stream>-avail_out
        <struct-z_stream>-avail_out-set!)
      (mutable
        next_out
        <struct-z_stream>-next_out
        <struct-z_stream>-next_out-set!)
      (mutable
        total_in
        <struct-z_stream>-total_in
        <struct-z_stream>-total_in-set!)
      (mutable
        avail_in
        <struct-z_stream>-avail_in
        <struct-z_stream>-avail_in-set!)
      (mutable
        next_in
        <struct-z_stream>-next_in
        <struct-z_stream>-next_in-set!))
    (protocol
      (lambda
        (make-c-struct)
        (lambda (pointer) ((make-c-struct pointer)))))
    (nongenerative nausicaa:zlib:))
  (define
    (<struct-z_stream>-opaque-set!
      (o <c-struct>)
      new-value)
    (struct-z_stream-opaque-set! o.pointer new-value))
  (define
    (<struct-z_stream>-opaque (o <c-struct>))
    (struct-z_stream-opaque-ref o.pointer))
  (define
    (<struct-z_stream>-zfree-set!
      (o <c-struct>)
      new-value)
    (struct-z_stream-zfree-set! o.pointer new-value))
  (define
    (<struct-z_stream>-zfree (o <c-struct>))
    (struct-z_stream-zfree-ref o.pointer))
  (define
    (<struct-z_stream>-zalloc-set!
      (o <c-struct>)
      new-value)
    (struct-z_stream-zalloc-set! o.pointer new-value))
  (define
    (<struct-z_stream>-zalloc (o <c-struct>))
    (struct-z_stream-zalloc-ref o.pointer))
  (define
    (<struct-z_stream>-adler-set!
      (o <c-struct>)
      new-value)
    (struct-z_stream-adler-set! o.pointer new-value))
  (define
    (<struct-z_stream>-adler (o <c-struct>))
    (struct-z_stream-adler-ref o.pointer))
  (define
    (<struct-z_stream>-data_type-set!
      (o <c-struct>)
      new-value)
    (struct-z_stream-data_type-set!
      o.pointer
      new-value))
  (define
    (<struct-z_stream>-data_type (o <c-struct>))
    (struct-z_stream-data_type-ref o.pointer))
  (define
    (<struct-z_stream>-msg-set!
      (o <c-struct>)
      new-value)
    (struct-z_stream-msg-set! o.pointer new-value))
  (define
    (<struct-z_stream>-msg (o <c-struct>))
    (struct-z_stream-msg-ref o.pointer))
  (define
    (<struct-z_stream>-total_out-set!
      (o <c-struct>)
      new-value)
    (struct-z_stream-total_out-set!
      o.pointer
      new-value))
  (define
    (<struct-z_stream>-total_out (o <c-struct>))
    (struct-z_stream-total_out-ref o.pointer))
  (define
    (<struct-z_stream>-avail_out-set!
      (o <c-struct>)
      new-value)
    (struct-z_stream-avail_out-set!
      o.pointer
      new-value))
  (define
    (<struct-z_stream>-avail_out (o <c-struct>))
    (struct-z_stream-avail_out-ref o.pointer))
  (define
    (<struct-z_stream>-next_out-set!
      (o <c-struct>)
      new-value)
    (struct-z_stream-next_out-set!
      o.pointer
      new-value))
  (define
    (<struct-z_stream>-next_out (o <c-struct>))
    (struct-z_stream-next_out-ref o.pointer))
  (define
    (<struct-z_stream>-total_in-set!
      (o <c-struct>)
      new-value)
    (struct-z_stream-total_in-set!
      o.pointer
      new-value))
  (define
    (<struct-z_stream>-total_in (o <c-struct>))
    (struct-z_stream-total_in-ref o.pointer))
  (define
    (<struct-z_stream>-avail_in-set!
      (o <c-struct>)
      new-value)
    (struct-z_stream-avail_in-set!
      o.pointer
      new-value))
  (define
    (<struct-z_stream>-avail_in (o <c-struct>))
    (struct-z_stream-avail_in-ref o.pointer))
  (define
    (<struct-z_stream>-next_in-set!
      (o <c-struct>)
      new-value)
    (struct-z_stream-next_in-set!
      o.pointer
      new-value))
  (define
    (<struct-z_stream>-next_in (o <c-struct>))
    (struct-z_stream-next_in-ref o.pointer))
  (define-class
    <struct-gz_header>
    (parent <c-struct>)
    (virtual-fields
      (mutable
        done
        <struct-gz_header>-done
        <struct-gz_header>-done-set!)
      (mutable
        hcrc
        <struct-gz_header>-hcrc
        <struct-gz_header>-hcrc-set!)
      (mutable
        comm_max
        <struct-gz_header>-comm_max
        <struct-gz_header>-comm_max-set!)
      (mutable
        comment
        <struct-gz_header>-comment
        <struct-gz_header>-comment-set!)
      (mutable
        name_max
        <struct-gz_header>-name_max
        <struct-gz_header>-name_max-set!)
      (mutable
        name
        <struct-gz_header>-name
        <struct-gz_header>-name-set!)
      (mutable
        extra_max
        <struct-gz_header>-extra_max
        <struct-gz_header>-extra_max-set!)
      (mutable
        extra_len
        <struct-gz_header>-extra_len
        <struct-gz_header>-extra_len-set!)
      (mutable
        extra
        <struct-gz_header>-extra
        <struct-gz_header>-extra-set!)
      (mutable
        os
        <struct-gz_header>-os
        <struct-gz_header>-os-set!)
      (mutable
        xflags
        <struct-gz_header>-xflags
        <struct-gz_header>-xflags-set!)
      (mutable
        time
        <struct-gz_header>-time
        <struct-gz_header>-time-set!)
      (mutable
        text
        <struct-gz_header>-text
        <struct-gz_header>-text-set!))
    (protocol
      (lambda
        (make-c-struct)
        (lambda (pointer) ((make-c-struct pointer)))))
    (nongenerative nausicaa:zlib:))
  (define
    (<struct-gz_header>-done-set!
      (o <c-struct>)
      new-value)
    (struct-gz_header-done-set! o.pointer new-value))
  (define
    (<struct-gz_header>-done (o <c-struct>))
    (struct-gz_header-done-ref o.pointer))
  (define
    (<struct-gz_header>-hcrc-set!
      (o <c-struct>)
      new-value)
    (struct-gz_header-hcrc-set! o.pointer new-value))
  (define
    (<struct-gz_header>-hcrc (o <c-struct>))
    (struct-gz_header-hcrc-ref o.pointer))
  (define
    (<struct-gz_header>-comm_max-set!
      (o <c-struct>)
      new-value)
    (struct-gz_header-comm_max-set!
      o.pointer
      new-value))
  (define
    (<struct-gz_header>-comm_max (o <c-struct>))
    (struct-gz_header-comm_max-ref o.pointer))
  (define
    (<struct-gz_header>-comment-set!
      (o <c-struct>)
      new-value)
    (struct-gz_header-comment-set!
      o.pointer
      new-value))
  (define
    (<struct-gz_header>-comment (o <c-struct>))
    (struct-gz_header-comment-ref o.pointer))
  (define
    (<struct-gz_header>-name_max-set!
      (o <c-struct>)
      new-value)
    (struct-gz_header-name_max-set!
      o.pointer
      new-value))
  (define
    (<struct-gz_header>-name_max (o <c-struct>))
    (struct-gz_header-name_max-ref o.pointer))
  (define
    (<struct-gz_header>-name-set!
      (o <c-struct>)
      new-value)
    (struct-gz_header-name-set! o.pointer new-value))
  (define
    (<struct-gz_header>-name (o <c-struct>))
    (struct-gz_header-name-ref o.pointer))
  (define
    (<struct-gz_header>-extra_max-set!
      (o <c-struct>)
      new-value)
    (struct-gz_header-extra_max-set!
      o.pointer
      new-value))
  (define
    (<struct-gz_header>-extra_max (o <c-struct>))
    (struct-gz_header-extra_max-ref o.pointer))
  (define
    (<struct-gz_header>-extra_len-set!
      (o <c-struct>)
      new-value)
    (struct-gz_header-extra_len-set!
      o.pointer
      new-value))
  (define
    (<struct-gz_header>-extra_len (o <c-struct>))
    (struct-gz_header-extra_len-ref o.pointer))
  (define
    (<struct-gz_header>-extra-set!
      (o <c-struct>)
      new-value)
    (struct-gz_header-extra-set! o.pointer new-value))
  (define
    (<struct-gz_header>-extra (o <c-struct>))
    (struct-gz_header-extra-ref o.pointer))
  (define
    (<struct-gz_header>-os-set!
      (o <c-struct>)
      new-value)
    (struct-gz_header-os-set! o.pointer new-value))
  (define
    (<struct-gz_header>-os (o <c-struct>))
    (struct-gz_header-os-ref o.pointer))
  (define
    (<struct-gz_header>-xflags-set!
      (o <c-struct>)
      new-value)
    (struct-gz_header-xflags-set!
      o.pointer
      new-value))
  (define
    (<struct-gz_header>-xflags (o <c-struct>))
    (struct-gz_header-xflags-ref o.pointer))
  (define
    (<struct-gz_header>-time-set!
      (o <c-struct>)
      new-value)
    (struct-gz_header-time-set! o.pointer new-value))
  (define
    (<struct-gz_header>-time (o <c-struct>))
    (struct-gz_header-time-ref o.pointer))
  (define
    (<struct-gz_header>-text-set!
      (o <c-struct>)
      new-value)
    (struct-gz_header-text-set! o.pointer new-value))
  (define
    (<struct-gz_header>-text (o <c-struct>))
    (struct-gz_header-text-ref o.pointer)))


;;; end of file
