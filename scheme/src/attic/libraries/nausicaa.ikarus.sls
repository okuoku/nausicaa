;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: augmented Scheme language around (rnrs)
;;;Date: Tue Dec  9, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa)
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
;;;replaced with case-lambda/with-class from (classes)
;;;    case-lambda
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
;;;replaced by define/with from (classes)
;;; define
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
    fields
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
    immutable
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
;;;replaced with lambda/with-class from (classes)
;;;    lambda
    latin-1-codec
    lcm
    least-fixnum
    length
;;;replaced with the analogous from (classes)
;;;    let
;;;    let*
;;;    letrec
;;;    letrec*
    let*-values
    let-syntax
    let-values
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
    mutable
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
    nongenerative
    not
;;; this is in (rnrs r5rs (6))
;;;    null-environment
    null?
    number->string
    number?
    numerator
    odd?
;;; this is an auxiliary syntax
    opaque
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
    parent
;;; this is an auxiliary syntax
    parent-rtd
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
    protocol
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
    sealed
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



    ;; predicates
    non-negative? non-positive?

    ;; stuff from the SRFIs
    cond-expand and-let* recursion cut cute begin0 begin0-let
    set-cons! incr! decr! while while* do-while do-while*

    ;; macro definition helpers
    define-identifier-accessor-mutator define-inline
    identifier-syntax-accessor-mutator with-accessor-and-mutator

    ;; shared structures
    write-with-shared-structure write/ss
    read-with-shared-structure  read/ss

    ;; environment variables
    (rename (getenv get-environment-variable))
    get-environment-variables

    ;; parameters
    parameterize parameterise parametrise make-parameter

    ;; simple syntaxes
    dotimes dolist loop-upon-list ensure
    define-values define-constant

    ;; miscellaneous
    symbol*->string symbol->string/maybe do*

    ;; other stuff
    pretty-print define-maker

;;;; bindings from (conditions)

    define-condition

    ;; mismatch
    &mismatch make-mismatch-condition mismatch-condition?

    ;; wrong num args
    &wrong-num-args make-wrong-num-args-condition wrong-num-args-condition?
    condition-wrong-num-args/procname
    condition-wrong-num-args/expected
    condition-wrong-num-args/given
    raise-wrong-num-args-error

    ;; unimplemented
    &unimplemented make-unimplemented-condition unimplemented-condition?
    raise-unimplemented-error


;;;; bindings from (classes)

    (rename
     (let/with-class		let)
     (let*/with-class		let*)
     (letrec/with-class		letrec)
     (letrec*/with-class	letrec*)
     (define/with-class*	define)
     (lambda/with-class*	lambda)
     (case-lambda/with-class*	case-lambda)
     (receive/with-class	receive))

    ;; usage macros
    define-class			define-foreign-class
    define-label			is-a?
    make				make-from-fields
    make*
    define-virtual-method

    ;; inspection macros
    class-record-type-descriptor
    class-public-constructor-descriptor	class-superclass-constructor-descriptor
    class-from-fields-constructor-descriptor
    class-type-uid			class-uid-list
    class-parent-rtd-list

    ;; inspection functions
    record-type-parent?
    class-uid-equal-or-parent?
    record-type-of
    record-parent-list
    class-uid-list-of

    ;; dot notation syntaxes
    with-class
    setf				getf
    define/with-class			define/with-class*
    lambda/with-class			lambda/with-class*
    case-lambda/with-class		case-lambda/with-class*
    receive/with-class
    let/with-class			let*/with-class
    letrec/with-class			letrec*/with-class

    ;; auxiliary syntaxes
    inherit predicate maker setter getter bindings
    public-protocol maker-protocol superclass-protocol virtual-fields
    methods method method-syntax

    ;; builtin classes
    <top> <builtin> <pair> <list>
    <char> <string> <vector> <bytevector> <hashtable> <record> <condition>
    <port> <binary-port> <input-port> <output-port> <textual-port>
    <fixnum> <flonum> <integer> <integer-valued> <rational> <rational-valued>
    <real> <real-valued> <complex> <number>

;;;; bindings from (compensations)

    with-compensations with-compensations/on-error
    compensate run-compensations push-compensation

;;;; bindings from (deferred-exceptions)

    with-deferred-exceptions-handler
    defer-exceptions run-deferred-exceptions-handler

  )


  (import (for (except (rnrs) equal-hash finite? infinite? nan? = max) expand run)
    (for (only (ikarus) getenv) expand run)
    (for (cond-expand) expand run)
    (for (conditions) expand run)
    (for (language-extensions) expand run)
    (for (parameters) expand run)
    (for (pretty-print) expand run)
    (for (shared-structures) expand run)
    (for (classes) expand run)
    (for (compensations) expand run)
    (for (deferred-exceptions) expand run)
    (for (makers) expand run)
    (for (nausicaa common) expand run))


;;;; Ikarus specific stuff

(define (get-environment-variables)
  (raise-unimplemented-error 'get-environment-variables))

(define (equal-hash obj)
  (string-hash
   (call-with-string-output-port
       (lambda (port) (write obj port)))))

(define max
  (case-lambda
   ((n)
    n)
   ((n m)
    (cond ((nan? n)
	   +nan.0)
	  ((nan? m)
	   +nan.0)
	  (else
	   (if (< n m) m n))))
   ((n m . args)
    (max n (apply max m args)))))


;;;; done

)

;;; end of file
