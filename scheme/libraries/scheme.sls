;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: augmented Scheme language around (rnrs)
;;;Date: Tue Dec  9, 2008
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


;;;; copyright notice for some SRFI definitions
;;;
;;;Copyright (c) 2008 Derick Eddington
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;Except  as  contained  in  this  notice, the  name(s)  of  the  above
;;;copyright holders  shall not be  used in advertising or  otherwise to
;;;promote  the sale,  use or  other dealings  in this  Software without
;;;prior written authorization.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


;;;; copyright notice for the REC definition, renamed to RECURSIION
;;;
;;;Copyright (c) 2002 Dr. Mirko Luedde <Mirko.Luedde@SAP.com>
;;;All Rights Reserved.
;;;
;;;Modified by Derick Eddington as port to R6RS.
;;;Modified by Marco Maggi upon inclusion in Nausicaa.
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;``Software''), to deal in the Software without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;THE SOFTWARE  IS PROVIDED  ``AS IS'', WITHOUT  WARRANTY OF  ANY KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


;;;; copyright notice for the CUT and CUTE definitions
;;;
;;;Reference implementation for SRFI-26 "cut"
;;;
;;;Copyright (c) 2002 Sebastian.Egner@philips.com, 5-Jun-2002.
;;;Copyright (c) 2002 Al Petrofsky <al@petrofsky.org>
;;;Copyright (c) 2008 Derick Eddington
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;Except  as  contained  in  this  notice, the  name(s)  of  the  above
;;;copyright holders  shall not be  used in advertising or  otherwise to
;;;promote  the sale,  use or  other dealings  in this  Software without
;;;prior written authorization.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


;;;; copyright notice for the shared structures SRFI
;;;
;;;Copyright (C) Ray Dillinger 2003.  All Rights Reserved.
;;;
;;;This document and  translations of it may be  copied and furnished to
;;;others, and derivative works that  comment on or otherwise explain it
;;;or assist  in its implementation  may be prepared,  copied, published
;;;and  distributed, in  whole or  in part,  without restriction  of any
;;;kind, provided that the above copyright notice and this paragraph are
;;;included  on all  such  copies and  derivative  works. However,  this
;;;document itself may  not be modified in any way,  such as by removing
;;;the  copyright  notice  or  references  to  the  Scheme  Request  For
;;;Implementation process  or editors, except as needed  for the purpose
;;;of  developing SRFIs  in  which case  the  procedures for  copyrights
;;;defined  in the  SRFI process  must be  followed, or  as  required to
;;;translate it into languages other than English.
;;;
;;;The limited permissions  granted above are perpetual and  will not be
;;;revoked by the authors or their successors or assigns.
;;;
;;;This document and the information  contained herein is provided on an
;;;"AS  IS" basis  and  THE AUTHOR  AND  THE SRFI  EDITORS DISCLAIM  ALL
;;;WARRANTIES,  EXPRESS OR  IMPLIED, INCLUDING  BUT NOT  LIMITED  TO ANY
;;;WARRANTY THAT THE USE OF THE INFORMATION HEREIN WILL NOT INFRINGE ANY
;;;RIGHTS OR ANY IMPLIED WARRANTIES  OF MERCHANTABILITY OR FITNESS FOR A
;;;PARTICULAR PURPOSE.
;;;
;;;A printer that  shows all sharing of substructures.   Uses the Common
;;;Lisp  print-circle notation:  #n# refers  to a  previous substructure
;;;labeled with #n=.  Takes O(n^2) time.
;;;
;;;Code attributed to Al* Petrofsky, modified by Dillinger.
;;;
;;;Minor tweaks for ERR5RS/R6RS by Ken Dickey
;;;
;;;NOTE: This pre-R6RS  code does not support full  R6RS lexical syntax.
;;;This   library  is  a   last  resort   fall-back  because   the  R6RS
;;;implementation should supply this functionality.



(library (scheme)
  (export
    &assertion
    &condition
    &error
    &i/o
    &i/o-decoding
    &i/o-encoding
    &i/o-file-already-exists
    &i/o-file-does-not-exist
    &i/o-file-is-read-only
    &i/o-file-protection
    &i/o-filename
    &i/o-invalid-position
    &i/o-port
    &i/o-read
    &i/o-write
    &implementation-restriction
    &irritants
    &lexical
    &message
    &no-infinities
    &no-nans
    &non-continuable
    &serious
    &syntax
    &undefined
    &violation
    &warning
    &who
    *
    +
    -
    ...
    /
    <
    <=
    =
    =>
    >
    >=
    _
    abs
    acos
    and
    angle
    append
    apply
    asin
    assert
    assertion-violation
    assertion-violation?
    assoc
    assp
    assq
    assv
    atan
    begin
    binary-port?
    bitwise-and
    bitwise-arithmetic-shift
    bitwise-arithmetic-shift-left
    bitwise-arithmetic-shift-right
    bitwise-bit-count
    bitwise-bit-field
    bitwise-bit-set?
    bitwise-copy-bit
    bitwise-copy-bit-field
    bitwise-first-bit-set
    bitwise-if
    bitwise-ior
    bitwise-length
    bitwise-not
    bitwise-reverse-bit-field
    bitwise-rotate-bit-field
    bitwise-xor
    boolean=?
    boolean?
    bound-identifier=?
    buffer-mode
    buffer-mode?
    bytevector->sint-list
    bytevector->string
    bytevector->u8-list
    bytevector->uint-list
    bytevector-copy
    bytevector-copy!
    bytevector-fill!
    bytevector-ieee-double-native-ref
    bytevector-ieee-double-native-set!
    bytevector-ieee-double-ref
    bytevector-ieee-double-set!
    bytevector-ieee-single-native-ref
    bytevector-ieee-single-native-set!
    bytevector-ieee-single-ref
    bytevector-ieee-single-set!
    bytevector-length
    bytevector-s16-native-ref
    bytevector-s16-native-set!
    bytevector-s16-ref
    bytevector-s16-set!
    bytevector-s32-native-ref
    bytevector-s32-native-set!
    bytevector-s32-ref
    bytevector-s32-set!
    bytevector-s64-native-ref
    bytevector-s64-native-set!
    bytevector-s64-ref
    bytevector-s64-set!
    bytevector-s8-ref
    bytevector-s8-set!
    bytevector-sint-ref
    bytevector-sint-set!
    bytevector-u16-native-ref
    bytevector-u16-native-set!
    bytevector-u16-ref
    bytevector-u16-set!
    bytevector-u32-native-ref
    bytevector-u32-native-set!
    bytevector-u32-ref
    bytevector-u32-set!
    bytevector-u64-native-ref
    bytevector-u64-native-set!
    bytevector-u64-ref
    bytevector-u64-set!
    bytevector-u8-ref
    bytevector-u8-set!
    bytevector-uint-ref
    bytevector-uint-set!
    bytevector=?
    bytevector?
    caaaar
    caaadr
    caaar
    caadar
    caaddr
    caadr
    caar
    cadaar
    cadadr
    cadar
    caddar
    cadddr
    caddr
    cadr
    call-with-bytevector-output-port
    call-with-current-continuation
    call-with-input-file
    call-with-output-file
    call-with-port
    call-with-string-output-port
    call-with-values
    call/cc
    car
    case
    case-lambda
    cdaaar
    cdaadr
    cdaar
    cdadar
    cdaddr
    cdadr
    cdar
    cddaar
    cddadr
    cddar
    cdddar
    cddddr
    cdddr
    cddr
    cdr
    ceiling
    char->integer
    char-alphabetic?
    char-ci<=?
    char-ci<?
    char-ci=?
    char-ci>=?
    char-ci>?
    char-downcase
    char-foldcase
    char-general-category
    char-lower-case?
    char-numeric?
    char-title-case?
    char-titlecase
    char-upcase
    char-upper-case?
    char-whitespace?
    char<=?
    char<?
    char=?
    char>=?
    char>?
    char?
    close-input-port
    close-output-port
    close-port
    command-line
    complex?
    cond
    condition
    condition-accessor
    condition-irritants
    condition-message
    condition-predicate
    condition-who
    condition?
    cons
    cons*
    cos
    current-error-port
    current-input-port
    current-output-port
    datum->syntax
    define
    define-condition-type
    define-enumeration
    define-record-type
    define-syntax
;;; this is in (rnrs r5rs (6))
;;;    delay
    delete-file
    denominator
    display
    div
    div-and-mod
    div0
    div0-and-mod0
    do
    dynamic-wind
    else
    endianness
    enum-set->list
    enum-set-complement
    enum-set-constructor
    enum-set-difference
    enum-set-indexer
    enum-set-intersection
    enum-set-member?
    enum-set-projection
    enum-set-subset?
    enum-set-union
    enum-set-universe
    enum-set=?
;;; this is in (rnrs eval (6))
;;; environment
    eof-object
    eof-object?
    eol-style
    eq?
    equal-hash
    equal?
    eqv?
    error
    error-handling-mode
    error?
;;; this is in (rnrs eval (6))
;;;    eval
    even?
    exact
;;; this is in (rnrs r5rs (6))
;;;    exact->inexact
    exact-integer-sqrt
    exact?
    exists
    exit
    exp
    expt
;;; this is an auxiliary syntax
;;;    fields
    file-exists?
    file-options
    filter
    find
    finite?
    fixnum->flonum
    fixnum-width
    fixnum?
    fl*
    fl+
    fl-
    fl/
    fl<=?
    fl<?
    fl=?
    fl>=?
    fl>?
    flabs
    flacos
    flasin
    flatan
    flceiling
    flcos
    fldenominator
    fldiv
    fldiv-and-mod
    fldiv0
    fldiv0-and-mod0
    fleven?
    flexp
    flexpt
    flfinite?
    flfloor
    flinfinite?
    flinteger?
    fllog
    flmax
    flmin
    flmod
    flmod0
    flnan?
    flnegative?
    flnumerator
    flodd?
    flonum?
    floor
    flpositive?
    flround
    flsin
    flsqrt
    fltan
    fltruncate
    flush-output-port
    flzero?
    fold-left
    fold-right
    for-all
    for-each
;;; this is in (rnrs r5rs (6))
;;;    force
    free-identifier=?
    fx*
    fx*/carry
    fx+
    fx+/carry
    fx-
    fx-/carry
    fx<=?
    fx<?
    fx=?
    fx>=?
    fx>?
    fxand
    fxarithmetic-shift
    fxarithmetic-shift-left
    fxarithmetic-shift-right
    fxbit-count
    fxbit-field
    fxbit-set?
    fxcopy-bit
    fxcopy-bit-field
    fxdiv
    fxdiv-and-mod
    fxdiv0
    fxdiv0-and-mod0
    fxeven?
    fxfirst-bit-set
    fxif
    fxior
    fxlength
    fxmax
    fxmin
    fxmod
    fxmod0
    fxnegative?
    fxnot
    fxodd?
    fxpositive?
    fxreverse-bit-field
    fxrotate-bit-field
    fxxor
    fxzero?
    gcd
    generate-temporaries
    get-bytevector-all
    get-bytevector-n
    get-bytevector-n!
    get-bytevector-some
    get-char
    get-datum
    get-line
    get-string-all
    get-string-n
    get-string-n!
    get-u8
    greatest-fixnum
    guard
    hashtable-clear!
    hashtable-contains?
    hashtable-copy
    hashtable-delete!
    hashtable-entries
    hashtable-equivalence-function
    hashtable-hash-function
    hashtable-keys
    hashtable-mutable?
    hashtable-ref
    hashtable-set!
    hashtable-size
    hashtable-update!
    hashtable?
    i/o-decoding-error?
    i/o-encoding-error-char
    i/o-encoding-error?
    i/o-error-filename
    i/o-error-port
    i/o-error-position
    i/o-error?
    i/o-file-already-exists-error?
    i/o-file-does-not-exist-error?
    i/o-file-is-read-only-error?
    i/o-file-protection-error?
    i/o-filename-error?
    i/o-invalid-position-error?
    i/o-port-error?
    i/o-read-error?
    i/o-write-error?
    identifier-syntax
    identifier?
    if
    imag-part
;;; this is an auxiliary syntax
;;;    immutable
    implementation-restriction-violation?
    inexact
;;; this is in (rnrs r5rs (6))
;;; inexact->exact
    inexact?
    infinite?
    input-port?
    integer->char
    integer-valued?
    integer?
    irritants-condition?
    lambda
    latin-1-codec
    lcm
    least-fixnum
    length
    let
    let*
    let*-values
    let-syntax
    let-values
    letrec
    letrec*
    letrec-syntax
    lexical-violation?
    list
    list->string
    list->vector
    list-ref
    list-sort
    list-tail
    list?
    log
    lookahead-char
    lookahead-u8
    magnitude
    make-assertion-violation
    make-bytevector
    make-custom-binary-input-port
    make-custom-binary-input/output-port
    make-custom-binary-output-port
    make-custom-textual-input-port
    make-custom-textual-input/output-port
    make-custom-textual-output-port
    make-enumeration
    make-eq-hashtable
    make-eqv-hashtable
    make-error
    make-hashtable
    make-i/o-decoding-error
    make-i/o-encoding-error
    make-i/o-error
    make-i/o-file-already-exists-error
    make-i/o-file-does-not-exist-error
    make-i/o-file-is-read-only-error
    make-i/o-file-protection-error
    make-i/o-filename-error
    make-i/o-invalid-position-error
    make-i/o-port-error
    make-i/o-read-error
    make-i/o-write-error
    make-implementation-restriction-violation
    make-irritants-condition
    make-lexical-violation
    make-message-condition
    make-no-infinities-violation
    make-no-nans-violation
    make-non-continuable-violation
    make-polar
    make-record-constructor-descriptor
    make-record-type-descriptor
    make-rectangular
    make-serious-condition
    make-string
    make-syntax-violation
    make-transcoder
    make-undefined-violation
    make-variable-transformer
    make-vector
    make-violation
    make-warning
    make-who-condition
    map
    max
    member
    memp
    memq
    memv
    message-condition?
    min
    mod
    mod0
;;; this is in (rnrs r5rs (6))
;;;    modulo
;;; this is an auxiliary syntax
;;;    mutable
    nan?
    native-endianness
    native-eol-style
    native-transcoder
    negative?
    newline
    no-infinities-violation?
    no-nans-violation?
    non-continuable-violation?
;;; this is an auxiliary syntax
;;;    nongenerative
    not
;;; this is in (rnrs r5rs (6))
;;;    null-environment
    null?
    number->string
    number?
    numerator
    odd?
;;; this is an auxiliary syntax
;;;    opaque
    open-bytevector-input-port
    open-bytevector-output-port
    open-file-input-port
    open-file-input/output-port
    open-file-output-port
    open-input-file
    open-output-file
    open-string-input-port
    open-string-output-port
    or
    output-port-buffer-mode
    output-port?
    pair?
;;; this is an auxiliary syntax
;;;    parent
;;; this is an auxiliary syntax
;;;    parent-rtd
    partition
    peek-char
    port-eof?
    port-has-port-position?
    port-has-set-port-position!?
    port-position
    port-transcoder
    port?
    positive?
    procedure?
;;; this is an auxiliary syntax
;;;    protocol
    put-bytevector
    put-char
    put-datum
    put-string
    put-u8
    quasiquote
    quasisyntax
    quote
;;; this is in (rnrs r5rs (6))
;;;    quotient
    raise
    raise-continuable
    rational-valued?
    rational?
    rationalize
    read
    read-char
    real->flonum
    real-part
    real-valued?
    real?
    record-accessor
    record-constructor
    record-constructor-descriptor
    record-field-mutable?
    record-mutator
    record-predicate
    record-rtd
    record-type-descriptor
    record-type-descriptor?
    record-type-field-names
    record-type-generative?
    record-type-name
    record-type-opaque?
    record-type-parent
    record-type-sealed?
    record-type-uid
    record?
;;; this is in (rnrs r5rs (6))
;;; remainder
    remove
    remp
    remq
    remv
    reverse
    round
;;; this is in (rnrs r5rs (6))
;;;    scheme-report-environment
;;; this is an auxiliary syntax
;;;    sealed
    serious-condition?
    set!
;;; these are in (rnrs mutable-pairs (6))
;;;    set-car!
;;;    set-cdr!
    set-port-position!
    simple-conditions
    sin
    sint-list->bytevector
    sqrt
    standard-error-port
    standard-input-port
    standard-output-port
    string
    string->bytevector
    string->list
    string->number
    string->symbol
    string->utf16
    string->utf32
    string->utf8
    string-append
    string-ci-hash
    string-ci<=?
    string-ci<?
    string-ci=?
    string-ci>=?
    string-ci>?
    string-copy
    string-downcase
;;; this is in (rnrs mutable-strings (6))
;;;    string-fill!
    string-foldcase
    string-for-each
    string-hash
    string-length
    string-normalize-nfc
    string-normalize-nfd
    string-normalize-nfkc
    string-normalize-nfkd
    string-ref
;;; this is in (rnrs mutable-strings (6))
;;;    string-set!
    string-titlecase
    string-upcase
    string<=?
    string<?
    string=?
    string>=?
    string>?
    string?
    substring
    symbol->string
    symbol-hash
    symbol=?
    symbol?
    syntax
    syntax->datum
    syntax-case
    syntax-rules
    syntax-violation
    syntax-violation-form
    syntax-violation-subform
    syntax-violation?
    tan
    textual-port?
    transcoded-port
    transcoder-codec
    transcoder-eol-style
    transcoder-error-handling-mode
    truncate
    u8-list->bytevector
    uint-list->bytevector
    undefined-violation?
    unless
    unquote
    unquote-splicing
    unsyntax
    unsyntax-splicing
    utf-16-codec
    utf-8-codec
    utf16->string
    utf32->string
    utf8->string
    values
    vector
    vector->list
    vector-fill!
    vector-for-each
    vector-length
    vector-map
    vector-ref
    vector-set!
    vector-sort
    vector-sort!
    vector?
    violation?
    warning?
    when
    who-condition?
    with-exception-handler
    with-input-from-file
    with-output-to-file
    with-syntax
    write
    write-char
    zero?

;;; --------------------------------------------------------------------

    ;; stuff from the SRFIs
    cond-expand and-let* receive recursion cut cute
    parameterize make-parameter
    (rename (parameterize parameterise))

    get-environment-variable get-environment-variables

    write-with-shared-structure
    read-with-shared-structure
    (rename (write-with-shared-structure write/ss))
    (rename (read-with-shared-structure  read/ss))

    ;; unimplemented condition
    &unimplemented unimplemented-condition?
    make-unimplemented-condition raise-unimplemented-error

    ;; other stuff
    pretty-print)
  (import (except (rnrs) equal-hash)
    (rnrs mutable-pairs)
    (rnrs mutable-strings)
    (scheme compat)
    (scheme unimplemented)
    (for (scheme registry) expand))


;;;; additional definitions

(define-syntax and-let*
  (lambda (stx)
    (define (get-id c)
      (syntax-case c () [(var expr) #'var] [_ #f]))
    (syntax-case stx ()
      [(_ (clause* ...) body* ...)
       (for-all identifier? (filter values (map get-id #'(clause* ...))))
       #'(and-let*-core #t (clause* ...) body* ...)])))

(define-syntax and-let*-core
  (lambda (stx)
    (syntax-case stx ()
      [(kw _ ([var expr] clause* ...) body* ...)
       #'(let ([var expr])
	   (if var
               (kw var (clause* ...) body* ...)
	     #f))]
      [(kw _ ([expr] clause* ...) body* ...)
       #'(let ([t expr])
	   (if t
               (kw t (clause* ...) body* ...)
	     #f))]
      [(kw _ (id clause* ...) body* ...)
       (or (identifier? #'id)
	   (syntax-violation #f "invalid clause" stx #'id))
       #'(if id
             (kw id (clause* ...) body* ...)
	   #f)]
      [(kw last () body* ...)
       (if (positive? (length #'(body* ...)))
           #'(begin body* ...)
	 #'last)])))

;;; --------------------------------------------------------------------

;;;This  syntax  comes  from  the  R6RS original  document,  Appendix  A
;;;``Formal semantics''.
(define-syntax begin0
  (syntax-rules ()
    ((_ ?expr0 ?expr ...)
     (call-with-values
	 (lambda () ?expr0)
       (lambda x
	 ?expr ...
	 (apply values x))))))

;;; --------------------------------------------------------------------

(define-syntax receive
  (syntax-rules ()
    [(_ formals expression b b* ...)
     (call-with-values
         (lambda () expression)
       (lambda formals b b* ...))]))

;;; --------------------------------------------------------------------

(define-syntax recursion
  (syntax-rules ()
    ((_ (?name . ?variables) . ?body)
     (letrec ((?name (lambda ?variables . ?body))) ?name))
    ((_ ?name ?expr)
     (letrec ((?name ?expr)) ?name))))

;;; --------------------------------------------------------------------

(define-syntax internal-cut
  (syntax-rules (<> <...>)
    ((internal-cut (?slot-name ...) (?proc ?arg ...))
     (lambda (?slot-name ...) ((begin ?proc) ?arg ...)))
    ((internal-cut (?slot-name ...) (?proc ?arg ...) <...>)
     (lambda (?slot-name ... . rest-slot) (apply ?proc ?arg ... rest-slot)))
    ((internal-cut (?slot-name ...)	(?position ...)		<>   . ?se)
     (internal-cut (?slot-name ... x)	(?position ... x)	     . ?se))
    ((internal-cut (?slot-name ...)	(?position ...)		?nse . ?se)
     (internal-cut (?slot-name ...)	(?position ... ?nse)	     . ?se))))

(define-syntax internal-cute
  (syntax-rules (<> <...>)
    ((internal-cute (?slot-name ...) ?nse-bindings (?proc ?arg ...))
     (let ?nse-bindings (lambda (?slot-name ...) (?proc ?arg ...))))
    ((internal-cute (?slot-name ...) ?nse-bindings (?proc ?arg ...) <...>)
     (let ?nse-bindings (lambda (?slot-name ... . x) (apply ?proc ?arg ... x))))
    ((internal-cute (?slot-name ...)   ?nse-bindings  (?position ...)   <>  . se)
     (internal-cute (?slot-name ... x) ?nse-bindings  (?position ... x)     . se))
    ((internal-cute ?slot-names        ?nse-bindings  (?position ...)   nse . se)
     (internal-cute ?slot-names ((x nse) . ?nse-bindings) (?position ... x) . se))))

(define-syntax cut
  (syntax-rules ()
    ((cut . slots-or-exprs)
     (internal-cut () () . slots-or-exprs))))

(define-syntax cute
  (syntax-rules ()
    ((cute . slots-or-exprs)
     (internal-cute () () () . slots-or-exprs))))


;;;; feature--based conditional expansion

(define-syntax cond-expand
  (lambda (stx)
    (syntax-case stx (and or not else)
      ((_)
       (syntax-violation #f "unfulfilled cond-expand" stx))

      ((_ (else ?body ...))
       (syntax (begin ?body ...)))

      ((_ ((and) ?body ...) ?more-clauses ...)
       (syntax (begin ?body ...)))

      ((_ ((and ?req1 ?req2 ...) ?body ...) ?more-clauses ...)
       (syntax (cond-expand
		;;It is right to put ?more-clauses in both the branches,
		;;because only one will be evaluated: the inner if ?req1
		;;is found, the outer if ?req1 is not found.
		(?req1 (cond-expand
			((and ?req2 ...) ?body ...)
			?more-clauses ...))
		?more-clauses ...)))

      ((_ ((or) ?body ...) ?more-clauses ...)
       (syntax (cond-expand ?more-clauses ...)))

      ((_ ((or ?req1 ?req2 ...) ?body ...) ?more-clauses ...)
       (syntax (cond-expand
		(?req1	(begin ?body ...))
		(else	(cond-expand
			 ((or ?req2 ...) ?body ...)
			 ?more-clauses ...)))))

      ((_ ((not ?req) ?body ...) ?more-clauses ...)
       (syntax (cond-expand
		(?req	(cond-expand ?more-clauses ...))
		(else	?body ...))))

      ((_ (?feature-id ?body ...) ?more-clauses ...)
       (if (member (syntax->datum (syntax ?feature-id)) available-features)
           (syntax (begin ?body ...))
	 (syntax (cond-expand ?more-clauses ...)))))))



;;;; shared structures

(define (write-with-shared-structure obj . optional-port)
  (define (acons key val alist)
    (cons (cons key val) alist))
  (define outport (if (eq? '() optional-port)
		      (current-output-port)
		    (car optional-port)))
  (define (interesting? obj)
    (or (pair? obj)
	(and (vector? obj) (not (zero? (vector-length obj))))
	(and (string? obj) (not (zero? (string-length obj))))))
  (define (write-obj obj alist)
    (define (write-interesting alist)
      (cond ((pair? obj)
	     (display "(" outport)
	     (let write-cdr ((obj (cdr obj)) (alist (write-obj (car obj) alist)))
	       (cond ((and (pair? obj) (not (cdr (assq obj alist))))
		      (display " " outport)
		      (write-cdr (cdr obj) (write-obj (car obj) alist)))
		     ((null? obj)
		      (display ")" outport)
		      alist)
		     (else
		      (display " . " outport)
		      (let ((alist (write-obj obj alist)))
			(display ")" outport)
			alist)))))
	    ((vector? obj)
	     (display "#(" outport)
	     (let ((len (vector-length obj)))
	       (let write-vec ((i 1) (alist (write-obj (vector-ref obj 0) alist)))
		 (cond ((= i len) (display ")" outport) alist)
		       (else (display " " outport)
			     (write-vec (+ i 1)
					(write-obj (vector-ref obj i) alist)))))))
	    (else (write obj outport) alist)))
    (cond ((interesting? obj)
	   (let ((val (cdr (assq obj alist))))
	     (cond ((not val) (write-interesting alist))
		   ((number? val)
		    (begin (display "#" outport)
			   (write val outport)
			   (display "#" outport) alist))
		   (else
		    (let ((n (+ 1 (cdar alist))))
		      (begin (display "#" outport)
			     (write n outport)
			     (display "=" outport))
		      (write-interesting (acons obj n alist)))))))
	  (else (write obj outport) alist)))
  (define (scan obj alist)
    (cond ((not (interesting? obj)) alist)
	  ((assq obj alist)
	   => (lambda (p) (if (cdr p) alist (acons obj #t alist))))
	  (else
	   (let ((alist (acons obj #f alist)))
	     (cond ((pair? obj) (scan (car obj) (scan (cdr obj) alist)))
		   ((vector? obj)
		    (let ((len (vector-length obj)))
		      (do ((i 0 (+ 1 i))
			   (alist alist (scan (vector-ref obj i) alist)))
			  ((= i len) alist))))
		   (else alist))))))
  (write-obj obj (acons 'dummy -1 (scan obj '())))
  (if #f #f))

(define (read-with-shared-structure . optional-port)
  (define port
    (if (null? optional-port) (current-input-port) (car optional-port)))
  (define (read-char*) (read-char port))
  (define (peek-char*) (peek-char port))
  (define (looking-at? c)
    (eqv? c (peek-char*)))
  (define (delimiter? c)
    (case c
      ((#\( #\) #\" #\;) #t)
      (else (or (eof-object? c)
		(char-whitespace? c)))))
  (define (not-delimiter? c) (not (delimiter? c)))
  (define (eat-intertoken-space)
    (define c (peek-char*))
    (cond ((eof-object? c))
	  ((char-whitespace? c) (read-char*) (eat-intertoken-space))
	  ((char=? c #\;)
	   (do ((c (read-char*) (read-char*)))
	       ((or (eof-object? c) (char=? c #\newline))))
	   (eat-intertoken-space))))
  (define (read-string)
    (read-char*)
    (let read-it ((chars '()))
      (let ((c (read-char*)))
	(if (eof-object? c)
	    (error 'read-string "EOF inside a string")
	  (case c
	    ((#\") (list->string (reverse chars)))
	    ((#\\) (read-it (cons (read-char*) chars)))
	    (else (read-it (cons c chars))))))))
  (define (read-some-chars pred)
    (let iter ((chars '()))
      (let ((c (peek-char*)))
	(if (or (eof-object? c) (not (pred c)))
	    (list->string (reverse chars))
	  (iter (cons (read-char*) chars))))))
  (define (read-character)
    (let ((c (peek-char*)))
      (cond ((eof-object? c) (error 'read-character "EOF inside a character"))
	    ((char-alphabetic? c)
	     (let ((name (read-some-chars char-alphabetic?)))
	       (cond ((= 1 (string-length name)) (string-ref name 0))
		     ((string-ci=? name "space") #\space)
		     ((string-ci=? name "newline") #\newline)
		     (else (error 'read-character "Unknown named character" name)))))
	    (else (read-char*)))))
  (define (read-number first-char)
    (let ((str (string-append (string first-char)
			      (read-some-chars not-delimiter?))))
      (or (string->number str)
	  (error 'read-number "Malformed number" str))))
  (define char-standard-case
    (if (char=? #\a (string-ref (symbol->string 'a) 0))
	char-downcase
      char-upcase))
  (define (string-standard-case str)
    (let* ((len (string-length str))
	   (new (make-string len)))
      (do ((i 0 (+ i 1)))
	  ((= i len) new)
	(string-set! new i (char-standard-case (string-ref str i))))))
  (define (read-identifier)
    (string->symbol (string-standard-case (read-some-chars not-delimiter?))))
  (define (read-part-spec)
    (let ((n (string->number (read-some-chars char-numeric?))))
      (let ((c (read-char*)))
	(case c
	  ((#\=) (cons 'decl n))
	  ((#\#) (cons 'use n))
	  (else (error 'read-part-spec "Malformed shared part specifier"))))))
  (define (read-optional-token)
    (eat-intertoken-space)
    (let ((c (peek-char*)))
      (case c
	((#\( #\) #\' #\`) (read-char*) (list c))
	((#\,)
	 (read-char*)
	 (if (looking-at? #\@)
	     (begin (read-char*) '(#\@))
	   '(#\,)))
	((#\") (read-string))
	((#\.)
	 (read-char*)
	 (cond ((delimiter? (peek-char*)) '(#\.))
	       ((not (looking-at? #\.)) (read-number #\.))
	       ((begin (read-char*) (looking-at? #\.)) (read-char*) '...)
	       (else (error 'read-char* "Malformed token starting with \"..\""))))
	((#\+) (read-char*) (if (delimiter? (peek-char*)) '+ (read-number c)))
	((#\-) (read-char*) (if (delimiter? (peek-char*)) '- (read-number c)))
	((#\#)
	 (read-char*)
	 (let ((c (peek-char*)))
	   (case c
	     ((#\() (read-char*) '(#\#))
	     ((#\\) (read-char*) (read-character))
	     ((#\t #\T) (read-char*) #t)
	     ((#\f #\F) (read-char*) #f)
	     (else (cond ((eof-object? c) (error 'read-char* "EOF inside a # token"))
			 ((char-numeric? c) (read-part-spec))
			 (else (read-number #\#)))))))
	(else (cond ((eof-object? c) c)
		    ((char-numeric? c) (read-char*) (read-number c))
		    (else (read-identifier)))))))
  (define (read-token)
    (let ((tok (read-optional-token)))
      (if (eof-object? tok)
	  (error 'read-token "EOF where token was required")
	tok)))
  (define parts-alist '())
  (define (add-part-to-alist! n thunk)
    (set! parts-alist (cons (cons n thunk) parts-alist)))
  (define (read-object)
    (finish-reading-object (read-token)))
  (define (read-optional-object)
    (finish-reading-object (read-optional-token)))
  (define (finish-reading-object first-token)
    (if (not (pair? first-token))
	first-token
      (if (char? (car first-token))
	  (case (car first-token)
	    ((#\() (read-list-tail))
	    ((#\#) (list->vector (read-list-tail)))
	    ((#\. #\)) (error 'read-with-shared-structure
			 (string-append "Unexpected \"" first-token "\"")))
	    (else
	     (list (caadr (assv (car first-token)
				'((#\' 'x) (#\, ,x) (#\` `x) (#\@ ,@x))))
		   (read-object))))
	(let ((starting-alist parts-alist))
	  (let read-decls ((token first-token))
	    (if (and (pair? token) (symbol? (car token)))
		(let ((n (cdr token)))
		  (case (car token)
		    ((use)
		     (cond ((assv n starting-alist) => cdr)
			   (else (error 'read-with-shared-structure
				   "Use of undeclared part " n))))
		    ((decl)
		     (if (assv n parts-alist)
			 (error 'read-with-shared-structure
			   "Double declaration of part" n))
		     (letrec ((obj (begin
				     (add-part-to-alist! n (lambda () obj))
				     (read-decls (read-token)))))
		       obj))))
	      (finish-reading-object token)))))))
  (define (read-list-tail)
    (let ((token (read-token)))
      (if (not (pair? token))
	  (cons token (read-list-tail))
	(case (car token)
	  ((#\)) '())
	  ((#\.) (let* ((obj (read-object))
			(tok (read-token)))
		   (if (and (pair? tok) (char=? #\) (car tok)))
		       obj
		     (error 'read-list-tail "Extra junk after a dot" token))))
	  (else (let ((obj (finish-reading-object token)))
		  (cons obj (read-list-tail))))))))
  (define (unthunk thunk)
    (let ((x (thunk)))
      (if (procedure? x) (unthunk x) x)))
  (let ((obj (read-optional-object)))
    (let fill-in-parts ((obj obj))
      (cond ((pair? obj)
	     (if (procedure? (car obj))
		 (set-car! obj (unthunk (car obj)))
	       (fill-in-parts (car obj)))
	     (if (procedure? (cdr obj))
		 (set-cdr! obj (unthunk (cdr obj)))
	       (fill-in-parts (cdr obj))))
	    ((vector? obj)
	     (let ((len (vector-length obj)))
	       (do ((i 0 (+ i 1)))
		   ((= i len))
		 (let ((elt (vector-ref obj i)))
		   (if (procedure? elt)
		       (vector-set! obj i (unthunk elt))
		     (fill-in-parts elt))))))))
    obj))


;;;; done

)

;;; end of file
