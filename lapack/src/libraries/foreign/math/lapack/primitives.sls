;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/LAPACK
;;;Contents: primitive functions
;;;Date: Mon Feb  1, 2010
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


(library (foreign math lapack primitives)
  (export

    all*
    lower*
    none*
    oscar*
    sierra*
    upper*

    cbdsqr
    cgbbrd
    cgbcon
    cgbequ
    cgbrfs
    cgbsv
    cgbsvx
    cgbtf2
    cgbtrf
    cgbtrs
    cgebak
    cgebal
    cgebd2
    cgebrd
    cgecon
    cgeequ
    cgees
    cgeesx
    cgeev
    cgeevx
    cgegs
    cgegv
    cgehd2
    cgehrd
    cgelq2
    cgelqf
    cgels
    cgelsx
    cgelsy
    cgeql2
    cgeqlf
    cgeqp3
    cgeqpf
    cgeqr2
    cgeqrf
    cgerfs
    cgerq2
    cgerqf
    cgesc2
    cgesv
    cgesvx
    cgetc2
    cgetf2
    cgetrf
    cgetri
    cgetrs
    cggbak
    cggbal
    cgges
    cggesx
    cggev
    cggevx
    cggglm
    cgghrd
    cgglse
    cggqrf
    cggrqf
    cggsvd
    cggsvp
    cgtcon
    cgtrfs
    cgtsv
    cgtsvx
    cgttrf
    cgttrs
    cgtts2
    chbev
    chbevd
    chbevx
    chbgst
    chbgv
    chbgvx
    chbtrd
    checon
    cheev
    cheevd
    cheevr
    cheevx
    chegs2
    chegst
    chegv
    chegvd
    chegvx
    cherfs
    chesv
    chesvx
    chetf2
    chetrd
    chetrf
    chetri
    chetrs
    chgeqz
    chpcon
    chpev
    chpevd
    chpevx
    chpgst
    chpgv
    chpgvd
    chpgvx
    chprfs
    chpsv
    chpsvx
    chptrd
    chptrf
    chptri
    chptrs
    chsein
    chseqr
    clabrd
    clacgv
    clacon
    clacp2
    clacpy
    clacrm
    clacrt
    claed0
    claed7
    claed8
    claein
    claesy
    claev2
    clags2
    clagtm
    clahef
    clahqr
    clahrd
    claic1
    clals0
    clalsa
    clapll
    clapmt
    claqgb
    claqge
    claqhb
    claqhe
    claqhp
    claqp2
    claqps
    claqsb
    claqsp
    claqsy
    clar1v
    clar2v
    clarcm
    clarf
    clarfb
    clarfg
    clarft
    clarfx
    clargv
    clarnv
    clarrv
    clartg
    clartv
    clarz
    clarzb
    clarzt
    clascl
    claset
    clasr
    classq
    claswp
    clasyf
    clatbs
    clatdf
    clatps
    clatrd
    clatrs
    clatrz
    clatzm
    clauu2
    clauum
    cpbcon
    cpbequ
    cpbrfs
    cpbstf
    cpbsv
    cpbsvx
    cpbtf2
    cpbtrf
    cpbtrs
    cpocon
    cpoequ
    cporfs
    cposv
    cposvx
    cpotf2
    cpotrf
    cpotri
    cpotrs
    cppcon
    cppequ
    cpprfs
    cppsv
    cppsvx
    cpptrf
    cpptri
    cpptrs
    cptcon
    cptrfs
    cptsv
    cptsvx
    cpttrf
    cpttrs
    cptts2
    crot
    cspcon
    cspmv
    cspr
    csprfs
    cspsv
    cspsvx
    csptrf
    csptri
    csptrs
    csrot
    csrscl
    cstedc
    cstein
    csteqr
    csycon
    csymv
    csyr
    csyrfs
    csysv
    csysvx
    csytf2
    csytrf
    csytri
    csytrs
    ctbcon
    ctbrfs
    ctbtrs
    ctgevc
    ctgex2
    ctgexc
    ctgsen
    ctgsja
    ctgsna
    ctgsy2
    ctgsyl
    ctpcon
    ctprfs
    ctptri
    ctptrs
    ctrcon
    ctrevc
    ctrexc
    ctrrfs
    ctrsen
    ctrsna
    ctrsyl
    ctrti2
    ctrtri
    ctrtrs
    ctzrqf
    ctzrzf
    cung2l
    cung2r
    cungbr
    cunghr
    cungl2
    cunglq
    cungql
    cungqr
    cungr2
    cungrq
    cungtr
    cunm2l
    cunm2r
    cunmbr
    cunmhr
    cunml2
    cunmlq
    cunmql
    cunmqr
    cunmr2
    cunmr3
    cunmrq
    cunmrz
    cunmtr
    cupgtr
    cupmtr
    dbdsdc
    dbdsqr
    ddisna
    dgbbrd
    dgbcon
    dgbequ
    dgbrfs
    dgbsv
    dgbsvx
    dgbtf2
    dgbtrf
    dgbtrs
    dgebak
    dgebal
    dgebd2
    dgebrd
    dgecon
    dgeequ
    dgees
    dgeesx
    dgeev
    dgeevx
    dgegs
    dgegv
    dgehd2
    dgehrd
    dgelq2
    dgelqf
    dgels
    dgelsd
    dgelss
    dgelsx
    dgelsy
    dgeql2
    dgeqlf
    dgeqp3
    dgeqpf
    dgeqr2
    dgeqrf
    dgerfs
    dgerq2
    dgerqf
    dgesc2
    dgesdd
    dgesv
    dgesvd
    dgesvx
    dgetc2
    dgetf2
    dgetrf
    dgetri
    dgetrs
    dggbak
    dggbal
    dgges
    dggesx
    dggev
    dggevx
    dggglm
    dgghrd
    dgglse
    dggqrf
    dggrqf
    dggsvd
    dggsvp
    dgtcon
    dgtrfs
    dgtsv
    dgtsvx
    dgttrf
    dgttrs
    dgtts2
    dhgeqz
    dhsein
    dhseqr
    dlabad
    dlabrd
    dlacon
    dlacpy
    dladiv
    dlae2
    dlaebz
    dlaed0
    dlaed1
    dlaed2
    dlaed3
    dlaed4
    dlaed5
    dlaed6
    dlaed7
    dlaed8
    dlaed9
    dlaeda
    dlaein
    dlaev2
    dlaexc
    dlag2
    dlags2
    dlagtf
    dlagtm
    dlagts
    dlagv2
    dlahqr
    dlahrd
    dlaic1
    dlaln2
    dlals0
    dlalsa
    dlalsd
    dlamc1
    dlamc2
    dlamc4
    dlamc5
    dlamrg
    dlanv2
    dlapll
    dlapmt
    dlaqgb
    dlaqge
    dlaqp2
    dlaqps
    dlaqsb
    dlaqsp
    dlaqsy
    dlaqtr
    dlar1v
    dlar2v
    dlarf
    dlarfb
    dlarfg
    dlarft
    dlarfx
    dlargv
    dlarnv
    dlarrb
    dlarre
    dlarrf
    dlarrv
    dlartg
    dlartv
    dlaruv
    dlarz
    dlarzb
    dlarzt
    dlas2
    dlascl
    dlasd0
    dlasd1
    dlasd2
    dlasd3
    dlasd4
    dlasd5
    dlasd6
    dlasd7
    dlasd8
    dlasd9
    dlasda
    dlasdq
    dlasdt
    dlaset
    dlasq1
    dlasq2
    dlasq3
    dlasq4
    dlasq5
    dlasq6
    dlasr
    dlasrt
    dlassq
    dlasv2
    dlaswp
    dlasy2
    dlasyf
    dlatbs
    dlatdf
    dlatps
    dlatrd
    dlatrs
    dlatrz
    dlatzm
    dlauu2
    dlauum
    dopgtr
    dopmtr
    dorg2l
    dorg2r
    dorgbr
    dorghr
    dorgl2
    dorglq
    dorgql
    dorgqr
    dorgr2
    dorgrq
    dorgtr
    dorm2l
    dorm2r
    dormbr
    dormhr
    dorml2
    dormlq
    dormql
    dormqr
    dormr2
    dormr3
    dormrq
    dormrz
    dormtr
    dpbcon
    dpbequ
    dpbrfs
    dpbstf
    dpbsv
    dpbsvx
    dpbtf2
    dpbtrf
    dpbtrs
    dpocon
    dpoequ
    dporfs
    dposv
    dposvx
    dpotf2
    dpotrf
    dpotri
    dpotrs
    dppcon
    dppequ
    dpprfs
    dppsv
    dppsvx
    dpptrf
    dpptri
    dpptrs
    dptcon
    dpteqr
    dptrfs
    dptsv
    dptsvx
    dpttrf
    dpttrs
    dptts2
    drscl
    dsbev
    dsbevd
    dsbevx
    dsbgst
    dsbgv
    dsbgvd
    dsbgvx
    dsbtrd
    dspcon
    dspev
    dspevd
    dspevx
    dspgst
    dspgv
    dspgvd
    dspgvx
    dsprfs
    dspsv
    dspsvx
    dsptrd
    dsptrf
    dsptri
    dsptrs
    dstebz
    dstedc
    dstegr
    dstein
    dsteqr
    dsterf
    dstev
    dstevd
    dstevr
    dstevx
    dsycon
    dsyev
    dsyevd
    dsyevr
    dsyevx
    dsygs2
    dsygst
    dsygv
    dsygvd
    dsygvx
    dsyrfs
    dsysv
    dsysvx
    dsytd2
    dsytf2
    dsytrd
    dsytrf
    dsytri
    dsytrs
    dtbcon
    dtbrfs
    dtbtrs
    dtgevc
    dtgex2
    dtgexc
    dtgsen
    dtgsja
    dtgsna
    dtgsy2
    dtgsyl
    dtpcon
    dtprfs
    dtptri
    dtptrs
    dtrcon
    dtrevc
    dtrexc
    dtrrfs
    dtrsen
    dtrsna
    dtrsyl
    dtrti2
    dtrtri
    dtrtrs
    dtzrqf
    dtzrzf
    icmax1
    ieeeck
    ilaenv
    izmax1
    sbdsdc
    sbdsqr
    sdisna
    sgbbrd
    sgbcon
    sgbequ
    sgbrfs
    sgbsv
    sgbsvx
    sgbtf2
    sgbtrf
    sgbtrs
    sgebak
    sgebal
    sgebd2
    sgebrd
    sgecon
    sgeequ
    sgees
    sgeesx
    sgeev
    sgeevx
    sgegs
    sgegv
    sgehd2
    sgehrd
    sgelq2
    sgelqf
    sgels
    sgelsd
    sgelss
    sgelsx
    sgelsy
    sgeql2
    sgeqlf
    sgeqp3
    sgeqpf
    sgeqr2
    sgeqrf
    sgerfs
    sgerq2
    sgerqf
    sgesc2
    sgesdd
    sgesv
    sgesvd
    sgesvx
    sgetc2
    sgetf2
    sgetrf
    sgetri
    sgetrs
    sggbak
    sggbal
    sgges
    sggesx
    sggev
    sggevx
    sggglm
    sgghrd
    sgglse
    sggqrf
    sggrqf
    sggsvd
    sggsvp
    sgtcon
    sgtrfs
    sgtsv
    sgtsvx
    sgttrf
    sgttrs
    sgtts2
    shgeqz
    shsein
    shseqr
    slabad
    slabrd
    slacon
    slacpy
    sladiv
    slae2
    slaebz
    slaed0
    slaed1
    slaed2
    slaed3
    slaed4
    slaed5
    slaed6
    slaed7
    slaed8
    slaed9
    slaeda
    slaein
    slaev2
    slaexc
    slag2
    slags2
    slagtf
    slagtm
    slagts
    slagv2
    slahqr
    slahrd
    slaic1
    slaln2
    slals0
    slalsa
    slalsd
    slamc1
    slamc2
    slamc4
    slamc5
    slamrg
    slanv2
    slapll
    slapmt
    slaqgb
    slaqge
    slaqp2
    slaqps
    slaqsb
    slaqsp
    slaqsy
    slaqtr
    slar1v
    slar2v
    slarf
    slarfb
    slarfg
    slarft
    slarfx
    slargv
    slarnv
    slarrb
    slarre
    slarrf
    slarrv
    slartg
    slartv
    slaruv
    slarz
    slarzb
    slarzt
    slas2
    slascl
    slasd0
    slasd1
    slasd2
    slasd3
    slasd4
    slasd5
    slasd6
    slasd7
    slasd8
    slasd9
    slasda
    slasdq
    slasdt
    slaset
    slasq1
    slasq2
    slasq3
    slasq4
    slasq5
    slasq6
    slasr
    slasrt
    slassq
    slasv2
    slaswp
    slasy2
    slasyf
    slatbs
    slatdf
    slatps
    slatrd
    slatrs
    slatrz
    slatzm
    slauu2
    slauum
    sopgtr
    sopmtr
    sorg2l
    sorg2r
    sorgbr
    sorghr
    sorgl2
    sorglq
    sorgql
    sorgqr
    sorgr2
    sorgrq
    sorgtr
    sorm2l
    sorm2r
    sormbr
    sormhr
    sorml2
    sormlq
    sormql
    sormqr
    sormr2
    sormr3
    sormrq
    sormrz
    sormtr
    spbcon
    spbequ
    spbrfs
    spbstf
    spbsv
    spbsvx
    spbtf2
    spbtrf
    spbtrs
    spocon
    spoequ
    sporfs
    sposv
    sposvx
    spotf2
    spotrf
    spotri
    spotrs
    sppcon
    sppequ
    spprfs
    sppsv
    sppsvx
    spptrf
    spptri
    spptrs
    sptcon
    spteqr
    sptrfs
    sptsv
    sptsvx
    spttrf
    spttrs
    sptts2
    srscl
    ssbev
    ssbevd
    ssbevx
    ssbgst
    ssbgv
    ssbgvd
    ssbgvx
    ssbtrd
    sspcon
    sspev
    sspevd
    sspevx
    sspgst
    sspgv
    sspgvd
    sspgvx
    ssprfs
    sspsv
    sspsvx
    ssptrd
    ssptrf
    ssptri
    ssptrs
    sstebz
    sstedc
    sstegr
    sstein
    ssteqr
    ssterf
    sstev
    sstevd
    sstevr
    sstevx
    ssycon
    ssyev
    ssyevd
    ssyevr
    ssyevx
    ssygs2
    ssygst
    ssygv
    ssygvd
    ssygvx
    ssyrfs
    ssysv
    ssysvx
    ssytd2
    ssytf2
    ssytrd
    ssytrf
    ssytri
    ssytrs
    stbcon
    stbrfs
    stbtrs
    stgevc
    stgex2
    stgexc
    stgsen
    stgsja
    stgsna
    stgsy2
    stgsyl
    stpcon
    stprfs
    stptri
    stptrs
    strcon
    strevc
    strexc
    strrfs
    strsen
    strsna
    strsyl
    strti2
    strtri
    strtrs
    stzrqf
    stzrzf
    xerbla
    zbdsqr
    zdrot
    zdrscl
    zgbbrd
    zgbcon
    zgbequ
    zgbrfs
    zgbsv
    zgbsvx
    zgbtf2
    zgbtrf
    zgbtrs
    zgebak
    zgebal
    zgebd2
    zgebrd
    zgecon
    zgeequ
    zgees
    zgeesx
    zgeev
    zgeevx
    zgegs
    zgegv
    zgehd2
    zgehrd
    zgelq2
    zgelqf
    zgels
    zgelsx
    zgelsy
    zgeql2
    zgeqlf
    zgeqp3
    zgeqpf
    zgeqr2
    zgeqrf
    zgerfs
    zgerq2
    zgerqf
    zgesc2
    zgesv
    zgesvx
    zgetc2
    zgetf2
    zgetrf
    zgetri
    zgetrs
    zggbak
    zggbal
    zgges
    zggesx
    zggev
    zggevx
    zggglm
    zgghrd
    zgglse
    zggqrf
    zggrqf
    zggsvd
    zggsvp
    zgtcon
    zgtrfs
    zgtsv
    zgtsvx
    zgttrf
    zgttrs
    zgtts2
    zhbev
    zhbevd
    zhbevx
    zhbgst
    zhbgv
    zhbgvx
    zhbtrd
    zhecon
    zheev
    zheevd
    zheevr
    zheevx
    zhegs2
    zhegst
    zhegv
    zhegvd
    zhegvx
    zherfs
    zhesv
    zhesvx
    zhetf2
    zhetrd
    zhetrf
    zhetri
    zhetrs
    zhgeqz
    zhpcon
    zhpev
    zhpevd
    zhpevx
    zhpgst
    zhpgv
    zhpgvd
    zhpgvx
    zhprfs
    zhpsv
    zhpsvx
    zhptrd
    zhptrf
    zhptri
    zhptrs
    zhsein
    zhseqr
    zlabrd
    zlacgv
    zlacon
    zlacp2
    zlacpy
    zlacrm
    zlacrt
    zlaed0
    zlaed7
    zlaed8
    zlaein
    zlaesy
    zlaev2
    zlags2
    zlagtm
    zlahef
    zlahqr
    zlahrd
    zlaic1
    zlals0
    zlalsa
    zlapll
    zlapmt
    zlaqgb
    zlaqge
    zlaqhb
    zlaqhe
    zlaqhp
    zlaqp2
    zlaqps
    zlaqsb
    zlaqsp
    zlaqsy
    zlar1v
    zlar2v
    zlarcm
    zlarf
    zlarfb
    zlarfg
    zlarft
    zlarfx
    zlargv
    zlarnv
    zlarrv
    zlartg
    zlartv
    zlarz
    zlarzb
    zlarzt
    zlascl
    zlaset
    zlasr
    zlassq
    zlaswp
    zlasyf
    zlatbs
    zlatdf
    zlatps
    zlatrd
    zlatrs
    zlatrz
    zlatzm
    zlauu2
    zlauum
    zpbcon
    zpbequ
    zpbrfs
    zpbstf
    zpbsv
    zpbsvx
    zpbtf2
    zpbtrf
    zpbtrs
    zpocon
    zpoequ
    zporfs
    zposv
    zposvx
    zpotf2
    zpotrf
    zpotri
    zpotrs
    zppcon
    zppequ
    zpprfs
    zppsv
    zppsvx
    zpptrf
    zpptri
    zpptrs
    zptcon
    zptrfs
    zptsv
    zptsvx
    zpttrf
    zpttrs
    zptts2
    zrot
    zspcon
    zspmv
    zspr
    zsprfs
    zspsv
    zspsvx
    zsptrf
    zsptri
    zsptrs
    zstedc
    zstein
    zsteqr
    zsycon
    zsymv
    zsyr
    zsyrfs
    zsysv
    zsysvx
    zsytf2
    zsytrf
    zsytri
    zsytrs
    ztbcon
    ztbrfs
    ztbtrs
    ztgevc
    ztgex2
    ztgexc
    ztgsen
    ztgsja
    ztgsna
    ztgsy2
    ztgsyl
    ztpcon
    ztprfs
    ztptri
    ztptrs
    ztrcon
    ztrevc
    ztrexc
    ztrrfs
    ztrsen
    ztrsna
    ztrsyl
    ztrti2
    ztrtri
    ztrtrs
    ztzrqf
    ztzrzf
    zung2l
    zung2r
    zungbr
    zunghr
    zungl2
    zunglq
    zungql
    zungqr
    zungr2
    zungrq
    zungtr
    zunm2l
    zunm2r
    zunmbr
    zunmhr
    zunml2
    zunmlq
    zunmql
    zunmqr
    zunmr2
    zunmr3
    zunmrq
    zunmrz
    zunmtr
    zupgtr
    zupmtr)
  (import (rnrs)
;;;    (pretty-print)
    (begin0)
    (compensations)
    (parameters)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign memory)
    (foreign memory mempool)
    (foreign cstrings)
    (foreign math lapack platform)
    (foreign math lapack conditions)
    (foreign math lapack sizeof))


;;;; constant argument values
;;
;;The following  values are never mutated  and can be reused  at will in
;;all the function calls.  Their usage is thread-safe.
;;

(define %constants-pool
  (malloc (* 6 strideof-int)))

(define %constants-cursor
  %constants-pool)

(define-syntax define-char-pointer
  (syntax-rules ()
    ((_ ?name ?char)
     (define ?name
       (begin0-let ((p %constants-cursor))
	 (pointer-incr! %constants-cursor strideof-int)
	 (pointer-set-c-signed-char! p 0 (char->integer ?char)))))))

(define-char-pointer all*	#\A)
(define-char-pointer lower*	#\L)
(define-char-pointer none*	#\N)
(define-char-pointer oscar*	#\O)
(define-char-pointer sierra*	#\S)
(define-char-pointer upper*	#\U)


;;;; pointer to integer arguments
;;
;;Preallocated  memory  locations  meant  to  be  used  to  store  boxed
;;arguments to  CLAPACK callout functions.   Each use mutate  the stored
;;value, so the usage of these locations is NOT thread-safe.
;;

(define %number-of-preallocated-locations 32)

(define %integer-pointers-pool
  (malloc (* %number-of-preallocated-locations (max strideof-integer
						    strideof-real
						    strideof-doublereal
						    strideof-complex
						    strideof-doublecomplex))))

(define %integer-pointers-cursor
  %integer-pointers-pool)

(define-syntax define-integer-pointer
  ;;Define a new named location and reserve the used memory.
  ;;
  (syntax-rules ()
    ((_ ?name)
     (define ?name
       (begin0-let ((p %integer-pointers-cursor))
	 (pointer-incr! %integer-pointers-cursor strideof-integer))))))

;;Predefined locations for boxed arguments.
;;
(define-integer-pointer info*)
(define-integer-pointer box0)
(define-integer-pointer box1)
(define-integer-pointer box2)
(define-integer-pointer box3)
(define-integer-pointer box4)
(define-integer-pointer box5)
(define-integer-pointer box6)
(define-integer-pointer box7)
(define-integer-pointer box8)
(define-integer-pointer box9)
(define-integer-pointer box10)
(define-integer-pointer box11)
(define-integer-pointer box12)
(define-integer-pointer box13)
(define-integer-pointer box14)
(define-integer-pointer box15)


(define (%process-result who return-value info arg-names arg-values)
  ;;Examine the value  stored in the INFO argument  to CLAPACK routines;
  ;;if an error is detected, raise an appropriate exception.
  ;;
  ;;WHO must be the value  for the &who condition.  RETURN-VALUE must be
  ;;the return value from the CLAPACK routine.
  ;;
  ;;ARG-NAMES must be the list of  names of the arguments to the calling
  ;;function.  ARG-VALUES must  be the list of arguments  to the calling
  ;;function.
  ;;
  (cond ((< info 0)
	 (let* ((index  (- info))
		(index1 (- index 1)) ;Fortran indexing starts at 1.
		(name	(list-ref arg-names  index1))
		(value	(list-ref arg-values index1)))
	   (raise
	    (condition
	     (make-who-condition who)
	     (make-message-condition
	      (string-append "invalid argument '"
			     (symbol->string name)
			     "' (position " (number->string index) ")"))
	     (make-lapack-invalid-argument-condition index name value)))))
	((> info 0)
	 (raise
	  (condition
	   (make-who-condition who)
	   (make-message-condition
	    (string-append "error in the course of computation at step " (number->string info)))
	   (make-lapack-failed-step-condition info))))
	(else return-value)))


(define-syntax define-wrapper
  ;;Define a standard wrapper for each callout.
  ;;
  (lambda (stx)

    (define (%name->callout-name name)
      ;;Strip the leading #\% if there is one.
      ;;
      (let ((n (symbol->string name)))
	(string->symbol (if (char=? #\% (string-ref n 0))
			    (substring n 1 (string-length n))
			  n))))
    (define (%name->wrapper-name name)
      ;;Strip the trailing #\_.
      ;;
      (let ((n (symbol->string name)))
	(string->symbol (substring n 0 (- (string-length n) 1)))))

    (define (%suffix-star name)
      ;;Append #\* to NAME.
      ;;
      (string->symbol (string-append (symbol->string name) "*")))

    (define (%arg-name-is-non-boxed? arg-name)
      ;;Return true  if ARG-NAME is an  argument name which  is known to
      ;;reference an array  of values; such arguments must  not be boxed
      ;;even if their type is equal to the type of boxed arguments.
      ;;
      ;;Usually arguments with type "integer*" are boxed, so we need the
      ;;list  of  argument names  with  type  "integer*"  which need  no
      ;;boxing.
      ;;
      ;;For example:  "integer* lda" and "integer* ipiv"  both have type
      ;;"integer*", but "lda" must be boxed while "ipiv" must not.
      ;;
      (memq arg-name '(ipiv jpiv)))

    (define (%arg-name-is-boxed? arg-name)
      ;;Return true  if ARG-NAME is an  argument name which  is known to
      ;;reference a  non-array value; such arguments must  be boxed even
      ;;if their type is equal to the type of non-boxed arguments.
      ;;
      ;;Usually arguments with  type "real*", "doublereal*", "complex*",
      ;;"doublecomplex*" and  "logical*" are  non-boxed, so we  need the
      ;;list of argument names with those types which need boxing.
      ;;
      ;;For example: "real* a" and "real* scale" both have type "real*",
      ;;but "a" must be boxed while "scale" must not.
      ;;
      (memq arg-name '(scale lscale rscale c s
			     rightv noinit)))

    (define (%args->callout-arg-names arg-types arg-names)
      ;;Return a list of symbols  representing the arguments names to be
      ;;handed to  the callout function.   Argument names whose  type is
      ;;"char*"  or "integer*"  are replaced  with names  having  a star
      ;;suffix, unless they are known to be non-boxed arguments.
      ;;
      (reverse
       (fold-left (lambda (knil type name)
		    (case type
		      ((char*)
		       (cons (%suffix-star name) knil))
		      ((integer*)
		       (if (%arg-name-is-non-boxed? name)
			   (cons name knil)
			 (cons (%suffix-star name) knil)))
		      ((real* doublereal* complex* doublecomplex* logical*)
		       (if (%arg-name-is-boxed? name)
			   (cons (%suffix-star name) knil)
			 (cons name knil)))
		      (else
		       (cons name knil))))
		  '() arg-types arg-names)))

    (define (%args->boxed-arg-names arg-types arg-names)
      ;;Return a list of symbols representing the names of the arguments
      ;;to the callout functions  which are boxed.  Argument names whose
      ;;type is "char*"  or "integer*" are replaced with  names having a
      ;;star suffix,  unless they are  known to be non  boxed arguments;
      ;;argument names having other types are discarded.
      ;;
      (reverse
       (fold-left (lambda (knil type name)
		    (case type
		      ((char*)
		       (cons (%suffix-star name) knil))
		      ((integer*)
		       (if (%arg-name-is-non-boxed? name)
			   knil
			 (cons (%suffix-star name) knil)))
		      ((real* doublereal* complex* doublecomplex* logical*)
		       (if (%arg-name-is-boxed? name)
			   (cons (%suffix-star name) knil)
			 knil))
		      (else
		       knil)))
		  '() arg-types arg-names)))

    (define (%args->box-names arg-types arg-names)
      ;;Return  a  list of  symbols  representing  the  bindings of  the
      ;;preallocated  boxes.   There  is  one  of them  for  each  boxed
      ;;argument.
      ;;
      (let ((%index->box-name	(lambda (i) (string->symbol (string-append "box" (number->string i)))))
	    (len		(length (%args->boxed-arg-names arg-types arg-names))))
	(let loop ((i 0) (box-names '()))
	  (if (= i len)
	      (reverse box-names)
	    (loop (+ 1 i) (cons (%index->box-name i) box-names))))))

    (define (%args->boxing arg-types arg-names)
      ;;Return a list of Scheme expressions being the boxing operations.
      ;;Arguments whose type is  "char*" or "integer*" are boxed, unless
      ;;they are known to  be non-boxed arguments; argument names having
      ;;other types are discarded.
      ;;
      ;;Boxing the argument "char* uplo" means:
      ;;
      ;;   (pointer-set-c-signed-char uplo* 0 uplo)
      ;;
      ;;boxing the argument "integer* lda" means:
      ;;
      ;;   (pointer-set-c-integer! lda* 0 lda)
      ;;
      (reverse
       (fold-left (lambda (knil type name)
		    (case type
		      ((char*)
		       (cons `(pointer-set-c-signed-char! ,(%suffix-star name) 0 (char->integer ,name))
			     knil))
		      ((integer*)
		       (if (%arg-name-is-non-boxed? name)
			   knil
			 (cons `(pointer-set-c-integer! ,(%suffix-star name) 0 ,name) knil)))
		      ((real* doublereal*)
		       (if (%arg-name-is-non-boxed? name)
			   (cons `(pointer-set-c-real! ,(%suffix-star name) 0 (inexact ,name)) knil)
			 knil))
		      ((complex* doublecomplex*)
		       (if (%arg-name-is-non-boxed? name)
			   (cons `(begin
				    (array-set-c-real! ,(%suffix-star name) 0
						       (inexact (real-part ,name)))
				    (array-set-c-real! ,(%suffix-star name) 1
						       (inexact (imag-part ,name))))
				 knil)
			 knil))
		      ((logical*)
		       (if (%arg-name-is-non-boxed? name)
			   (cons `(pointer-set-c-logical! ,(%suffix-star name) 0 ,name) knil)
			 knil))
		      (else
		       knil)))
		  '() arg-types arg-names)))

    (syntax-case stx (info)

      ((_ ?ret-type ?name ((?arg-type ?arg-name) ... (?info-type info)))
       ;;The  purpose  of this  rule  is  to  rewrite (notice  the  last
       ;;argument INFO):
       ;;
       ;;   (define-wrapper int dgesv_ ((integer* n) (integer* nrhs)
       ;;				(doublereal* a) (integer* lda)
       ;;				(integer* ipiv)
       ;;				(doublereal* b) (integer* ldb)
       ;;				(integer* info)))
       ;;
       ;;to:
       ;;
       ;;   (define (dgesv n nrhs a lda ipiv b ldb)
       ;;     (define n*	box0)
       ;;     (define nrhs*	box1)
       ;;     (define lda*	box2)
       ;;     (define ldb*	box3)
       ;;     (pointer-set-c-integer! n*    0 n)
       ;;     (pointer-set-c-integer! nrhs* 0 nrhs)
       ;;     (pointer-set-c-integer! lda*  0 lda)
       ;;     (pointer-set-c-integer! ldb*  0 ldb)
       ;;     (pointer-set-c-integer! info* 0 0)
       ;;     (let ((result (dgesv_ n nrhs a lda ipiv b ldb info*)))
       ;;       (%process-result (quote dgesv)
       ;;                         result (pointer-ref-c-integer info* 0)
       ;;                         '(n nrhs a lda ipiv b ldb)
       ;;                         (list n nrhs a lda ipiv b ldb))))
       ;;
       (let ((name	(syntax->datum #'?name))
	     (arg-types	(syntax->datum #'(?arg-type ...)))
	     (arg-names	(syntax->datum #'(?arg-name ...))))
	 (with-syntax
	     ((WRAPPER-NAME		(datum->syntax #'?name (%name->wrapper-name name)))
	      (CALLOUT-NAME		(datum->syntax #'?name (%name->callout-name name)))
	      ((BOXING ...)		(datum->syntax #'?name (%args->boxing arg-types arg-names)))
	      ((ARG-NAME ...)		(datum->syntax #'?name (%args->callout-arg-names arg-types arg-names)))
	      ((BOX-NAME ...)		(datum->syntax #'?name (%args->box-names arg-types arg-names)))
	      ((BOXED-ARG-NAME ...)	(datum->syntax #'?name (%args->boxed-arg-names arg-types
										       arg-names))))
	   (let ((output-stx #'(define (WRAPPER-NAME ?arg-name ...)
				 (define BOXED-ARG-NAME BOX-NAME) ...
				 BOXING ...
				 (pointer-set-c-integer! info* 0 0)
				 (let ((result (CALLOUT-NAME ARG-NAME ... info*)))
				   (%process-result (quote WRAPPER-NAME)
						    result (pointer-ref-c-integer info* 0)
						    '(?arg-name ...) (list ?arg-name ...))))))
	     ;; (when (eq? name 'dgesvd_)
	     ;;   (pretty-print (syntax->datum output-stx)))
	     output-stx))))

      ((_ ?ret-type ?name ((?arg-type ?arg-name) ...))
       ;;The purpose of this rule is to rewrite:
       ;;
       ;;   (define-wrapper int cgesc2_ ((integer* n)
       ;;                                (complex* a) (integer* lda)
       ;;                                (complex* rhs) (integer* ipiv)
       ;;                                (integer* jpiv) (real* scale)))
       ;;
       ;;to:
       ;;
       ;;   (define (cgesc2 n a lda rhs ipiv jpiv scale)
       ;;     (define n*	box0)
       ;;     (define lda*	box1)
       ;;     (define scale*	box3)
       ;;     (pointer-set-c-integer! n*     0 n)
       ;;     (pointer-set-c-integer! lda*   0 lda)
       ;;     (pointer-set-c-real!    scale* 0 scale)
       ;;     (cgesc2 n* a lda* rhs ipiv jpiv scale*))
       ;;
       (let ((name	(syntax->datum #'?name))
	     (arg-types	(syntax->datum #'(?arg-type ...)))
	     (arg-names	(syntax->datum #'(?arg-name ...))))
	 (with-syntax
	     ((WRAPPER-NAME		(datum->syntax #'?name (%name->wrapper-name name)))
	      (CALLOUT-NAME		(datum->syntax #'?name (%name->callout-name name)))
	      ((BOXING ...)		(datum->syntax #'?name (%args->boxing arg-types arg-names)))
	      ((ARG-NAME ...)		(datum->syntax #'?name (%args->callout-arg-names arg-types
											 arg-names)))
	      ((BOX-NAME ...)		(datum->syntax #'?name (%args->box-names arg-types arg-names)))
	      ((BOXED-ARG-NAME ...)	(datum->syntax #'?name (%args->boxed-arg-names arg-types
										       arg-names))))
	   (let ((output-stx #'(define (WRAPPER-NAME ?arg-name ...)
				 (define BOXED-ARG-NAME BOX-NAME) ...
				 BOXING ...
				 (CALLOUT-NAME ARG-NAME ...))))
	     ;; (when (eq? name 'dgesv_)
	     ;;   (pretty-print (syntax->datum output-stx)))
	     output-stx)))))))


(define-wrapper int cbdsqr_ ((char* uplo) (integer* n) (integer* ncvt) (integer* nru) (integer* ncc) (real* d__) (real* e) (complex* vt) (integer* ldvt) (complex* u) (integer* ldu) (complex* c__) (integer* ldc) (real* rwork) (integer* info)))

(define-wrapper int cgbbrd_ ((char* vect) (integer* m) (integer* n) (integer* ncc) (integer* kl) (integer* ku) (complex* ab) (integer* ldab) (real* d__) (real* e) (complex* q) (integer* ldq) (complex* pt) (integer* ldpt) (complex* c__) (integer* ldc) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int cgbcon_ ((char* norm) (integer* n) (integer* kl) (integer* ku) (complex* ab) (integer* ldab) (integer* ipiv) (real* anorm) (real* rcond) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int cgbequ_ ((integer* m) (integer* n) (integer* kl) (integer* ku) (complex* ab) (integer* ldab) (real* r__) (real* c__) (real* rowcnd) (real* colcnd) (real* amax) (integer* info)))

(define-wrapper int cgbrfs_ ((char* trans) (integer* n) (integer* kl) (integer* ku) (integer* nrhs) (complex* ab) (integer* ldab) (complex* afb) (integer* ldafb) (integer* ipiv) (complex* b) (integer* ldb) (complex* x) (integer* ldx) (real* ferr) (real* berr) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int cgbsv_ ((integer* n) (integer* kl) (integer* ku) (integer* nrhs) (complex* ab) (integer* ldab) (integer* ipiv) (complex* b) (integer* ldb) (integer* info)))

(define-wrapper int cgbsvx_ ((char* fact) (char* trans) (integer* n) (integer* kl) (integer* ku) (integer* nrhs) (complex* ab) (integer* ldab) (complex* afb) (integer* ldafb) (integer* ipiv) (char* equed) (real* r__) (real* c__) (complex* b) (integer* ldb) (complex* x) (integer* ldx) (real* rcond) (real* ferr) (real* berr) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int cgbtf2_ ((integer* m) (integer* n) (integer* kl) (integer* ku) (complex* ab) (integer* ldab) (integer* ipiv) (integer* info)))

(define-wrapper int cgbtrf_ ((integer* m) (integer* n) (integer* kl) (integer* ku) (complex* ab) (integer* ldab) (integer* ipiv) (integer* info)))

(define-wrapper int cgbtrs_ ((char* trans) (integer* n) (integer* kl) (integer* ku) (integer* nrhs) (complex* ab) (integer* ldab) (integer* ipiv) (complex* b) (integer* ldb) (integer* info)))

(define-wrapper int cgebak_ ((char* job) (char* side) (integer* n) (integer* ilo) (integer* ihi) (real* scale) (integer* m) (complex* v) (integer* ldv) (integer* info)))

(define-wrapper int cgebal_ ((char* job) (integer* n) (complex* a) (integer* lda) (integer* ilo) (integer* ihi) (real* scale) (integer* info)))

(define-wrapper int cgebd2_ ((integer* m) (integer* n) (complex* a) (integer* lda) (real* d__) (real* e) (complex* tauq) (complex* taup) (complex* work) (integer* info)))

(define-wrapper int cgebrd_ ((integer* m) (integer* n) (complex* a) (integer* lda) (real* d__) (real* e) (complex* tauq) (complex* taup) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int cgecon_ ((char* norm) (integer* n) (complex* a) (integer* lda) (real* anorm) (real* rcond) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int cgeequ_ ((integer* m) (integer* n) (complex* a) (integer* lda) (real* r__) (real* c__) (real* rowcnd) (real* colcnd) (real* amax) (integer* info)))

(define-wrapper int cgees_ ((char* jobvs) (char* sort) (L_fp select) (integer* n) (complex* a) (integer* lda) (integer* sdim) (complex* w) (complex* vs) (integer* ldvs) (complex* work) (integer* lwork) (real* rwork) (logical* bwork) (integer* info)))

(define-wrapper int cgeesx_ ((char* jobvs) (char* sort) (L_fp select) (char* sense) (integer* n) (complex* a) (integer* lda) (integer* sdim) (complex* w) (complex* vs) (integer* ldvs) (real* rconde) (real* rcondv) (complex* work) (integer* lwork) (real* rwork) (logical* bwork) (integer* info)))

(define-wrapper int cgeev_ ((char* jobvl) (char* jobvr) (integer* n) (complex* a) (integer* lda) (complex* w) (complex* vl) (integer* ldvl) (complex* vr) (integer* ldvr) (complex* work) (integer* lwork) (real* rwork) (integer* info)))

(define-wrapper int cgeevx_ ((char* balanc) (char* jobvl) (char* jobvr) (char* sense) (integer* n) (complex* a) (integer* lda) (complex* w) (complex* vl) (integer* ldvl) (complex* vr) (integer* ldvr) (integer* ilo) (integer* ihi) (real* scale) (real* abnrm) (real* rconde) (real* rcondv) (complex* work) (integer* lwork) (real* rwork) (integer* info)))

(define-wrapper int cgegs_ ((char* jobvsl) (char* jobvsr) (integer* n) (complex* a) (integer* lda) (complex* b) (integer* ldb) (complex* alpha) (complex* beta) (complex* vsl) (integer* ldvsl) (complex* vsr) (integer* ldvsr) (complex* work) (integer* lwork) (real* rwork) (integer* info)))

(define-wrapper int cgegv_ ((char* jobvl) (char* jobvr) (integer* n) (complex* a) (integer* lda) (complex* b) (integer* ldb) (complex* alpha) (complex* beta) (complex* vl) (integer* ldvl) (complex* vr) (integer* ldvr) (complex* work) (integer* lwork) (real* rwork) (integer* info)))

(define-wrapper int cgehd2_ ((integer* n) (integer* ilo) (integer* ihi) (complex* a) (integer* lda) (complex* tau) (complex* work) (integer* info)))

(define-wrapper int cgehrd_ ((integer* n) (integer* ilo) (integer* ihi) (complex* a) (integer* lda) (complex* tau) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int cgelq2_ ((integer* m) (integer* n) (complex* a) (integer* lda) (complex* tau) (complex* work) (integer* info)))

(define-wrapper int cgelqf_ ((integer* m) (integer* n) (complex* a) (integer* lda) (complex* tau) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int cgels_ ((char* trans) (integer* m) (integer* n) (integer* nrhs) (complex* a) (integer* lda) (complex* b) (integer* ldb) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int cgelsx_ ((integer* m) (integer* n) (integer* nrhs) (complex* a) (integer* lda) (complex* b) (integer* ldb) (integer* jpvt) (real* rcond) (integer* rank) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int cgelsy_ ((integer* m) (integer* n) (integer* nrhs) (complex* a) (integer* lda) (complex* b) (integer* ldb) (integer* jpvt) (real* rcond) (integer* rank) (complex* work) (integer* lwork) (real* rwork) (integer* info)))

(define-wrapper int cgeql2_ ((integer* m) (integer* n) (complex* a) (integer* lda) (complex* tau) (complex* work) (integer* info)))

(define-wrapper int cgeqlf_ ((integer* m) (integer* n) (complex* a) (integer* lda) (complex* tau) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int cgeqp3_ ((integer* m) (integer* n) (complex* a) (integer* lda) (integer* jpvt) (complex* tau) (complex* work) (integer* lwork) (real* rwork) (integer* info)))

(define-wrapper int cgeqpf_ ((integer* m) (integer* n) (complex* a) (integer* lda) (integer* jpvt) (complex* tau) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int cgeqr2_ ((integer* m) (integer* n) (complex* a) (integer* lda) (complex* tau) (complex* work) (integer* info)))

(define-wrapper int cgeqrf_ ((integer* m) (integer* n) (complex* a) (integer* lda) (complex* tau) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int cgerfs_ ((char* trans) (integer* n) (integer* nrhs) (complex* a) (integer* lda) (complex* af) (integer* ldaf) (integer* ipiv) (complex* b) (integer* ldb) (complex* x) (integer* ldx) (real* ferr) (real* berr) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int cgerq2_ ((integer* m) (integer* n) (complex* a) (integer* lda) (complex* tau) (complex* work) (integer* info)))

(define-wrapper int cgerqf_ ((integer* m) (integer* n) (complex* a) (integer* lda) (complex* tau) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int cgesc2_ ((integer* n) (complex* a) (integer* lda) (complex* rhs) (integer* ipiv) (integer* jpiv) (real* scale)))

(define-wrapper int cgesv_ ((integer* n) (integer* nrhs) (complex* a) (integer* lda) (integer* ipiv) (complex* b) (integer* ldb) (integer* info)))

(define-wrapper int cgesvx_ ((char* fact) (char* trans) (integer* n) (integer* nrhs) (complex* a) (integer* lda) (complex* af) (integer* ldaf) (integer* ipiv) (char* equed) (real* r__) (real* c__) (complex* b) (integer* ldb) (complex* x) (integer* ldx) (real* rcond) (real* ferr) (real* berr) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int cgetc2_ ((integer* n) (complex* a) (integer* lda) (integer* ipiv) (integer* jpiv) (integer* info)))

(define-wrapper int cgetf2_ ((integer* m) (integer* n) (complex* a) (integer* lda) (integer* ipiv) (integer* info)))

(define-wrapper int cgetrf_ ((integer* m) (integer* n) (complex* a) (integer* lda) (integer* ipiv) (integer* info)))

(define-wrapper int cgetri_ ((integer* n) (complex* a) (integer* lda) (integer* ipiv) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int cgetrs_ ((char* trans) (integer* n) (integer* nrhs) (complex* a) (integer* lda) (integer* ipiv) (complex* b) (integer* ldb) (integer* info)))

(define-wrapper int cggbak_ ((char* job) (char* side) (integer* n) (integer* ilo) (integer* ihi) (real* lscale) (real* rscale) (integer* m) (complex* v) (integer* ldv) (integer* info)))

(define-wrapper int cggbal_ ((char* job) (integer* n) (complex* a) (integer* lda) (complex* b) (integer* ldb) (integer* ilo) (integer* ihi) (real* lscale) (real* rscale) (real* work) (integer* info)))

(define-wrapper int cgges_ ((char* jobvsl) (char* jobvsr) (char* sort) (L_fp selctg) (integer* n) (complex* a) (integer* lda) (complex* b) (integer* ldb) (integer* sdim) (complex* alpha) (complex* beta) (complex* vsl) (integer* ldvsl) (complex* vsr) (integer* ldvsr) (complex* work) (integer* lwork) (real* rwork) (logical* bwork) (integer* info)))

(define-wrapper int cggesx_ ((char* jobvsl) (char* jobvsr) (char* sort) (L_fp selctg) (char* sense) (integer* n) (complex* a) (integer* lda) (complex* b) (integer* ldb) (integer* sdim) (complex* alpha) (complex* beta) (complex* vsl) (integer* ldvsl) (complex* vsr) (integer* ldvsr) (real* rconde) (real* rcondv) (complex* work) (integer* lwork) (real* rwork) (integer* iwork) (integer* liwork) (logical* bwork) (integer* info)))

(define-wrapper int cggev_ ((char* jobvl) (char* jobvr) (integer* n) (complex* a) (integer* lda) (complex* b) (integer* ldb) (complex* alpha) (complex* beta) (complex* vl) (integer* ldvl) (complex* vr) (integer* ldvr) (complex* work) (integer* lwork) (real* rwork) (integer* info)))

(define-wrapper int cggevx_ ((char* balanc) (char* jobvl) (char* jobvr) (char* sense) (integer* n) (complex* a) (integer* lda) (complex* b) (integer* ldb) (complex* alpha) (complex* beta) (complex* vl) (integer* ldvl) (complex* vr) (integer* ldvr) (integer* ilo) (integer* ihi) (real* lscale) (real* rscale) (real* abnrm) (real* bbnrm) (real* rconde) (real* rcondv) (complex* work) (integer* lwork) (real* rwork) (integer* iwork) (logical* bwork) (integer* info)))

(define-wrapper int cggglm_ ((integer* n) (integer* m) (integer* p) (complex* a) (integer* lda) (complex* b) (integer* ldb) (complex* d__) (complex* x) (complex* y) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int cgghrd_ ((char* compq) (char* compz) (integer* n) (integer* ilo) (integer* ihi) (complex* a) (integer* lda) (complex* b) (integer* ldb) (complex* q) (integer* ldq) (complex* z__) (integer* ldz) (integer* info)))

(define-wrapper int cgglse_ ((integer* m) (integer* n) (integer* p) (complex* a) (integer* lda) (complex* b) (integer* ldb) (complex* c__) (complex* d__) (complex* x) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int cggqrf_ ((integer* n) (integer* m) (integer* p) (complex* a) (integer* lda) (complex* taua) (complex* b) (integer* ldb) (complex* taub) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int cggrqf_ ((integer* m) (integer* p) (integer* n) (complex* a) (integer* lda) (complex* taua) (complex* b) (integer* ldb) (complex* taub) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int cggsvd_ ((char* jobu) (char* jobv) (char* jobq) (integer* m) (integer* n) (integer* p) (integer* k) (integer* l) (complex* a) (integer* lda) (complex* b) (integer* ldb) (real* alpha) (real* beta) (complex* u) (integer* ldu) (complex* v) (integer* ldv) (complex* q) (integer* ldq) (complex* work) (real* rwork) (integer* iwork) (integer* info)))

(define-wrapper int cggsvp_ ((char* jobu) (char* jobv) (char* jobq) (integer* m) (integer* p) (integer* n) (complex* a) (integer* lda) (complex* b) (integer* ldb) (real* tola) (real* tolb) (integer* k) (integer* l) (complex* u) (integer* ldu) (complex* v) (integer* ldv) (complex* q) (integer* ldq) (integer* iwork) (real* rwork) (complex* tau) (complex* work) (integer* info)))

(define-wrapper int cgtcon_ ((char* norm) (integer* n) (complex* dl) (complex* d__) (complex* du) (complex* du2) (integer* ipiv) (real* anorm) (real* rcond) (complex* work) (integer* info)))

(define-wrapper int cgtrfs_ ((char* trans) (integer* n) (integer* nrhs) (complex* dl) (complex* d__) (complex* du) (complex* dlf) (complex* df) (complex* duf) (complex* du2) (integer* ipiv) (complex* b) (integer* ldb) (complex* x) (integer* ldx) (real* ferr) (real* berr) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int cgtsv_ ((integer* n) (integer* nrhs) (complex* dl) (complex* d__) (complex* du) (complex* b) (integer* ldb) (integer* info)))

(define-wrapper int cgtsvx_ ((char* fact) (char* trans) (integer* n) (integer* nrhs) (complex* dl) (complex* d__) (complex* du) (complex* dlf) (complex* df) (complex* duf) (complex* du2) (integer* ipiv) (complex* b) (integer* ldb) (complex* x) (integer* ldx) (real* rcond) (real* ferr) (real* berr) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int cgttrf_ ((integer* n) (complex* dl) (complex* d__) (complex* du) (complex* du2) (integer* ipiv) (integer* info)))

(define-wrapper int cgttrs_ ((char* trans) (integer* n) (integer* nrhs) (complex* dl) (complex* d__) (complex* du) (complex* du2) (integer* ipiv) (complex* b) (integer* ldb) (integer* info)))

(define-wrapper int cgtts2_ ((integer* itrans) (integer* n) (integer* nrhs) (complex* dl) (complex* d__) (complex* du) (complex* du2) (integer* ipiv) (complex* b) (integer* ldb)))

(define-wrapper int chbev_ ((char* jobz) (char* uplo) (integer* n) (integer* kd) (complex* ab) (integer* ldab) (real* w) (complex* z__) (integer* ldz) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int chbevd_ ((char* jobz) (char* uplo) (integer* n) (integer* kd) (complex* ab) (integer* ldab) (real* w) (complex* z__) (integer* ldz) (complex* work) (integer* lwork) (real* rwork) (integer* lrwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int chbevx_ ((char* jobz) (char* range) (char* uplo) (integer* n) (integer* kd) (complex* ab) (integer* ldab) (complex* q) (integer* ldq) (real* vl) (real* vu) (integer* il) (integer* iu) (real* abstol) (integer* m) (real* w) (complex* z__) (integer* ldz) (complex* work) (real* rwork) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int chbgst_ ((char* vect) (char* uplo) (integer* n) (integer* ka) (integer* kb) (complex* ab) (integer* ldab) (complex* bb) (integer* ldbb) (complex* x) (integer* ldx) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int chbgv_ ((char* jobz) (char* uplo) (integer* n) (integer* ka) (integer* kb) (complex* ab) (integer* ldab) (complex* bb) (integer* ldbb) (real* w) (complex* z__) (integer* ldz) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int chbgvx_ ((char* jobz) (char* range) (char* uplo) (integer* n) (integer* ka) (integer* kb) (complex* ab) (integer* ldab) (complex* bb) (integer* ldbb) (complex* q) (integer* ldq) (real* vl) (real* vu) (integer* il) (integer* iu) (real* abstol) (integer* m) (real* w) (complex* z__) (integer* ldz) (complex* work) (real* rwork) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int chbtrd_ ((char* vect) (char* uplo) (integer* n) (integer* kd) (complex* ab) (integer* ldab) (real* d__) (real* e) (complex* q) (integer* ldq) (complex* work) (integer* info)))

(define-wrapper int checon_ ((char* uplo) (integer* n) (complex* a) (integer* lda) (integer* ipiv) (real* anorm) (real* rcond) (complex* work) (integer* info)))

(define-wrapper int cheev_ ((char* jobz) (char* uplo) (integer* n) (complex* a) (integer* lda) (real* w) (complex* work) (integer* lwork) (real* rwork) (integer* info)))

(define-wrapper int cheevd_ ((char* jobz) (char* uplo) (integer* n) (complex* a) (integer* lda) (real* w) (complex* work) (integer* lwork) (real* rwork) (integer* lrwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int cheevr_ ((char* jobz) (char* range) (char* uplo) (integer* n) (complex* a) (integer* lda) (real* vl) (real* vu) (integer* il) (integer* iu) (real* abstol) (integer* m) (real* w) (complex* z__) (integer* ldz) (integer* isuppz) (complex* work) (integer* lwork) (real* rwork) (integer* lrwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int cheevx_ ((char* jobz) (char* range) (char* uplo) (integer* n) (complex* a) (integer* lda) (real* vl) (real* vu) (integer* il) (integer* iu) (real* abstol) (integer* m) (real* w) (complex* z__) (integer* ldz) (complex* work) (integer* lwork) (real* rwork) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int chegs2_ ((integer* itype) (char* uplo) (integer* n) (complex* a) (integer* lda) (complex* b) (integer* ldb) (integer* info)))

(define-wrapper int chegst_ ((integer* itype) (char* uplo) (integer* n) (complex* a) (integer* lda) (complex* b) (integer* ldb) (integer* info)))

(define-wrapper int chegv_ ((integer* itype) (char* jobz) (char* uplo) (integer* n) (complex* a) (integer* lda) (complex* b) (integer* ldb) (real* w) (complex* work) (integer* lwork) (real* rwork) (integer* info)))

(define-wrapper int chegvd_ ((integer* itype) (char* jobz) (char* uplo) (integer* n) (complex* a) (integer* lda) (complex* b) (integer* ldb) (real* w) (complex* work) (integer* lwork) (real* rwork) (integer* lrwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int chegvx_ ((integer* itype) (char* jobz) (char* range) (char* uplo) (integer* n) (complex* a) (integer* lda) (complex* b) (integer* ldb) (real* vl) (real* vu) (integer* il) (integer* iu) (real* abstol) (integer* m) (real* w) (complex* z__) (integer* ldz) (complex* work) (integer* lwork) (real* rwork) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int cherfs_ ((char* uplo) (integer* n) (integer* nrhs) (complex* a) (integer* lda) (complex* af) (integer* ldaf) (integer* ipiv) (complex* b) (integer* ldb) (complex* x) (integer* ldx) (real* ferr) (real* berr) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int chesv_ ((char* uplo) (integer* n) (integer* nrhs) (complex* a) (integer* lda) (integer* ipiv) (complex* b) (integer* ldb) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int chesvx_ ((char* fact) (char* uplo) (integer* n) (integer* nrhs) (complex* a) (integer* lda) (complex* af) (integer* ldaf) (integer* ipiv) (complex* b) (integer* ldb) (complex* x) (integer* ldx) (real* rcond) (real* ferr) (real* berr) (complex* work) (integer* lwork) (real* rwork) (integer* info)))

(define-wrapper int chetf2_ ((char* uplo) (integer* n) (complex* a) (integer* lda) (integer* ipiv) (integer* info)))

(define-wrapper int chetrd_ ((char* uplo) (integer* n) (complex* a) (integer* lda) (real* d__) (real* e) (complex* tau) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int chetrf_ ((char* uplo) (integer* n) (complex* a) (integer* lda) (integer* ipiv) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int chetri_ ((char* uplo) (integer* n) (complex* a) (integer* lda) (integer* ipiv) (complex* work) (integer* info)))

(define-wrapper int chetrs_ ((char* uplo) (integer* n) (integer* nrhs) (complex* a) (integer* lda) (integer* ipiv) (complex* b) (integer* ldb) (integer* info)))

(define-wrapper int chgeqz_ ((char* job) (char* compq) (char* compz) (integer* n) (integer* ilo) (integer* ihi) (complex* a) (integer* lda) (complex* b) (integer* ldb) (complex* alpha) (complex* beta) (complex* q) (integer* ldq) (complex* z__) (integer* ldz) (complex* work) (integer* lwork) (real* rwork) (integer* info)))

(define-wrapper int chpcon_ ((char* uplo) (integer* n) (complex* ap) (integer* ipiv) (real* anorm) (real* rcond) (complex* work) (integer* info)))

(define-wrapper int chpev_ ((char* jobz) (char* uplo) (integer* n) (complex* ap) (real* w) (complex* z__) (integer* ldz) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int chpevd_ ((char* jobz) (char* uplo) (integer* n) (complex* ap) (real* w) (complex* z__) (integer* ldz) (complex* work) (integer* lwork) (real* rwork) (integer* lrwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int chpevx_ ((char* jobz) (char* range) (char* uplo) (integer* n) (complex* ap) (real* vl) (real* vu) (integer* il) (integer* iu) (real* abstol) (integer* m) (real* w) (complex* z__) (integer* ldz) (complex* work) (real* rwork) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int chpgst_ ((integer* itype) (char* uplo) (integer* n) (complex* ap) (complex* bp) (integer* info)))

(define-wrapper int chpgv_ ((integer* itype) (char* jobz) (char* uplo) (integer* n) (complex* ap) (complex* bp) (real* w) (complex* z__) (integer* ldz) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int chpgvd_ ((integer* itype) (char* jobz) (char* uplo) (integer* n) (complex* ap) (complex* bp) (real* w) (complex* z__) (integer* ldz) (complex* work) (integer* lwork) (real* rwork) (integer* lrwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int chpgvx_ ((integer* itype) (char* jobz) (char* range) (char* uplo) (integer* n) (complex* ap) (complex* bp) (real* vl) (real* vu) (integer* il) (integer* iu) (real* abstol) (integer* m) (real* w) (complex* z__) (integer* ldz) (complex* work) (real* rwork) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int chprfs_ ((char* uplo) (integer* n) (integer* nrhs) (complex* ap) (complex* afp) (integer* ipiv) (complex* b) (integer* ldb) (complex* x) (integer* ldx) (real* ferr) (real* berr) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int chpsv_ ((char* uplo) (integer* n) (integer* nrhs) (complex* ap) (integer* ipiv) (complex* b) (integer* ldb) (integer* info)))

(define-wrapper int chpsvx_ ((char* fact) (char* uplo) (integer* n) (integer* nrhs) (complex* ap) (complex* afp) (integer* ipiv) (complex* b) (integer* ldb) (complex* x) (integer* ldx) (real* rcond) (real* ferr) (real* berr) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int chptrd_ ((char* uplo) (integer* n) (complex* ap) (real* d__) (real* e) (complex* tau) (integer* info)))

(define-wrapper int chptrf_ ((char* uplo) (integer* n) (complex* ap) (integer* ipiv) (integer* info)))

(define-wrapper int chptri_ ((char* uplo) (integer* n) (complex* ap) (integer* ipiv) (complex* work) (integer* info)))

(define-wrapper int chptrs_ ((char* uplo) (integer* n) (integer* nrhs) (complex* ap) (integer* ipiv) (complex* b) (integer* ldb) (integer* info)))

(define-wrapper int chsein_ ((char* side) (char* eigsrc) (char* initv) (logical* select) (integer* n) (complex* h__) (integer* ldh) (complex* w) (complex* vl) (integer* ldvl) (complex* vr) (integer* ldvr) (integer* mm) (integer* m) (complex* work) (real* rwork) (integer* ifaill) (integer* ifailr) (integer* info)))

(define-wrapper int chseqr_ ((char* job) (char* compz) (integer* n) (integer* ilo) (integer* ihi) (complex* h__) (integer* ldh) (complex* w) (complex* z__) (integer* ldz) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int clabrd_ ((integer* m) (integer* n) (integer* nb) (complex* a) (integer* lda) (real* d__) (real* e) (complex* tauq) (complex* taup) (complex* x) (integer* ldx) (complex* y) (integer* ldy)))

(define-wrapper int clacgv_ ((integer* n) (complex* x) (integer* incx)))

(define-wrapper int clacon_ ((integer* n) (complex* v) (complex* x) (real* est) (integer* kase)))

(define-wrapper int clacp2_ ((char* uplo) (integer* m) (integer* n) (real* a) (integer* lda) (complex* b) (integer* ldb)))

(define-wrapper int clacpy_ ((char* uplo) (integer* m) (integer* n) (complex* a) (integer* lda) (complex* b) (integer* ldb)))

(define-wrapper int clacrm_ ((integer* m) (integer* n) (complex* a) (integer* lda) (real* b) (integer* ldb) (complex* c__) (integer* ldc) (real* rwork)))

(define-wrapper int clacrt_ ((integer* n) (complex* cx) (integer* incx) (complex* cy) (integer* incy) (complex* c__) (complex* s)))

(define-wrapper int claed0_ ((integer* qsiz) (integer* n) (real* d__) (real* e) (complex* q) (integer* ldq) (complex* qstore) (integer* ldqs) (real* rwork) (integer* iwork) (integer* info)))

(define-wrapper int claed7_ ((integer* n) (integer* cutpnt) (integer* qsiz) (integer* tlvls) (integer* curlvl) (integer* curpbm) (real* d__) (complex* q) (integer* ldq) (real* rho) (integer* indxq) (real* qstore) (integer* qptr) (integer* prmptr) (integer* perm) (integer* givptr) (integer* givcol) (real* givnum) (complex* work) (real* rwork) (integer* iwork) (integer* info)))

(define-wrapper int claed8_ ((integer* k) (integer* n) (integer* qsiz) (complex* q) (integer* ldq) (real* d__) (real* rho) (integer* cutpnt) (real* z__) (real* dlamda) (complex* q2) (integer* ldq2) (real* w) (integer* indxp) (integer* indx) (integer* indxq) (integer* perm) (integer* givptr) (integer* givcol) (real* givnum) (integer* info)))

(define-wrapper int claein_ ((logical* rightv) (logical* noinit) (integer* n) (complex* h__) (integer* ldh) (complex* w) (complex* v) (complex* b) (integer* ldb) (real* rwork) (real* eps3) (real* smlnum) (integer* info)))

(define-wrapper int claesy_ ((complex* a) (complex* b) (complex* c__) (complex* rt1) (complex* rt2) (complex* evscal) (complex* cs1) (complex* sn1)))

(define-wrapper int claev2_ ((complex* a) (complex* b) (complex* c__) (real* rt1) (real* rt2) (real* cs1) (complex* sn1)))

(define-wrapper int clags2_ ((logical* upper) (real* a1) (complex* a2) (real* a3) (real* b1) (complex* b2) (real* b3) (real* csu) (complex* snu) (real* csv) (complex* snv) (real* csq) (complex* snq)))

(define-wrapper int clagtm_ ((char* trans) (integer* n) (integer* nrhs) (real* alpha) (complex* dl) (complex* d__) (complex* du) (complex* x) (integer* ldx) (real* beta) (complex* b) (integer* ldb)))

(define-wrapper int clahef_ ((char* uplo) (integer* n) (integer* nb) (integer* kb) (complex* a) (integer* lda) (integer* ipiv) (complex* w) (integer* ldw) (integer* info)))

(define-wrapper int clahqr_ ((logical* wantt) (logical* wantz) (integer* n) (integer* ilo) (integer* ihi) (complex* h__) (integer* ldh) (complex* w) (integer* iloz) (integer* ihiz) (complex* z__) (integer* ldz) (integer* info)))

(define-wrapper int clahrd_ ((integer* n) (integer* k) (integer* nb) (complex* a) (integer* lda) (complex* tau) (complex* t) (integer* ldt) (complex* y) (integer* ldy)))

(define-wrapper int claic1_ ((integer* job) (integer* j) (complex* x) (real* sest) (complex* w) (complex* gamma) (real* sestpr) (complex* s) (complex* c__)))

(define-wrapper int clals0_ ((integer* icompq) (integer* nl) (integer* nr) (integer* sqre) (integer* nrhs) (complex* b) (integer* ldb) (complex* bx) (integer* ldbx) (integer* perm) (integer* givptr) (integer* givcol) (integer* ldgcol) (real* givnum) (integer* ldgnum) (real* poles) (real* difl) (real* difr) (real* z__) (integer* k) (real* c__) (real* s) (real* rwork) (integer* info)))

(define-wrapper int clalsa_ ((integer* icompq) (integer* smlsiz) (integer* n) (integer* nrhs) (complex* b) (integer* ldb) (complex* bx) (integer* ldbx) (real* u) (integer* ldu) (real* vt) (integer* k) (real* difl) (real* difr) (real* z__) (real* poles) (integer* givptr) (integer* givcol) (integer* ldgcol) (integer* perm) (real* givnum) (real* c__) (real* s) (real* rwork) (integer* iwork) (integer* info)))

(define-wrapper int clapll_ ((integer* n) (complex* x) (integer* incx) (complex* y) (integer* incy) (real* ssmin)))

(define-wrapper int clapmt_ ((logical* forwrd) (integer* m) (integer* n) (complex* x) (integer* ldx) (integer* k)))

(define-wrapper int claqgb_ ((integer* m) (integer* n) (integer* kl) (integer* ku) (complex* ab) (integer* ldab) (real* r__) (real* c__) (real* rowcnd) (real* colcnd) (real* amax) (char* equed)))

(define-wrapper int claqge_ ((integer* m) (integer* n) (complex* a) (integer* lda) (real* r__) (real* c__) (real* rowcnd) (real* colcnd) (real* amax) (char* equed)))

(define-wrapper int claqhb_ ((char* uplo) (integer* n) (integer* kd) (complex* ab) (integer* ldab) (real* s) (real* scond) (real* amax) (char* equed)))

(define-wrapper int claqhe_ ((char* uplo) (integer* n) (complex* a) (integer* lda) (real* s) (real* scond) (real* amax) (char* equed)))

(define-wrapper int claqhp_ ((char* uplo) (integer* n) (complex* ap) (real* s) (real* scond) (real* amax) (char* equed)))

(define-wrapper int claqp2_ ((integer* m) (integer* n) (integer* offset) (complex* a) (integer* lda) (integer* jpvt) (complex* tau) (real* vn1) (real* vn2) (complex* work)))

(define-wrapper int claqps_ ((integer* m) (integer* n) (integer* offset) (integer* nb) (integer* kb) (complex* a) (integer* lda) (integer* jpvt) (complex* tau) (real* vn1) (real* vn2) (complex* auxv) (complex* f) (integer* ldf)))

(define-wrapper int claqsb_ ((char* uplo) (integer* n) (integer* kd) (complex* ab) (integer* ldab) (real* s) (real* scond) (real* amax) (char* equed)))

(define-wrapper int claqsp_ ((char* uplo) (integer* n) (complex* ap) (real* s) (real* scond) (real* amax) (char* equed)))

(define-wrapper int claqsy_ ((char* uplo) (integer* n) (complex* a) (integer* lda) (real* s) (real* scond) (real* amax) (char* equed)))

(define-wrapper int clar1v_ ((integer* n) (integer* b1) (integer* bn) (real* sigma) (real* d__) (real* l) (real* ld) (real* lld) (real* gersch) (complex* z__) (real* ztz) (real* mingma) (integer* r__) (integer* isuppz) (real* work)))

(define-wrapper int clar2v_ ((integer* n) (complex* x) (complex* y) (complex* z__) (integer* incx) (real* c__) (complex* s) (integer* incc)))

(define-wrapper int clarcm_ ((integer* m) (integer* n) (real* a) (integer* lda) (complex* b) (integer* ldb) (complex* c__) (integer* ldc) (real* rwork)))

(define-wrapper int clarf_ ((char* side) (integer* m) (integer* n) (complex* v) (integer* incv) (complex* tau) (complex* c__) (integer* ldc) (complex* work)))

(define-wrapper int clarfb_ ((char* side) (char* trans) (char* direct) (char* storev) (integer* m) (integer* n) (integer* k) (complex* v) (integer* ldv) (complex* t) (integer* ldt) (complex* c__) (integer* ldc) (complex* work) (integer* ldwork)))

(define-wrapper int clarfg_ ((integer* n) (complex* alpha) (complex* x) (integer* incx) (complex* tau)))

(define-wrapper int clarft_ ((char* direct) (char* storev) (integer* n) (integer* k) (complex* v) (integer* ldv) (complex* tau) (complex* t) (integer* ldt)))

(define-wrapper int clarfx_ ((char* side) (integer* m) (integer* n) (complex* v) (complex* tau) (complex* c__) (integer* ldc) (complex* work)))

(define-wrapper int clargv_ ((integer* n) (complex* x) (integer* incx) (complex* y) (integer* incy) (real* c__) (integer* incc)))

(define-wrapper int clarnv_ ((integer* idist) (integer* iseed) (integer* n) (complex* x)))

(define-wrapper int clarrv_ ((integer* n) (real* d__) (real* l) (integer* isplit) (integer* m) (real* w) (integer* iblock) (real* gersch) (real* tol) (complex* z__) (integer* ldz) (integer* isuppz) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int clartg_ ((complex* f) (complex* g) (real* cs) (complex* sn) (complex* r__)))

(define-wrapper int clartv_ ((integer* n) (complex* x) (integer* incx) (complex* y) (integer* incy) (real* c__) (complex* s) (integer* incc)))

(define-wrapper int clarz_ ((char* side) (integer* m) (integer* n) (integer* l) (complex* v) (integer* incv) (complex* tau) (complex* c__) (integer* ldc) (complex* work)))

(define-wrapper int clarzb_ ((char* side) (char* trans) (char* direct) (char* storev) (integer* m) (integer* n) (integer* k) (integer* l) (complex* v) (integer* ldv) (complex* t) (integer* ldt) (complex* c__) (integer* ldc) (complex* work) (integer* ldwork)))

(define-wrapper int clarzt_ ((char* direct) (char* storev) (integer* n) (integer* k) (complex* v) (integer* ldv) (complex* tau) (complex* t) (integer* ldt)))

(define-wrapper int clascl_ ((char* type__) (integer* kl) (integer* ku) (real* cfrom) (real* cto) (integer* m) (integer* n) (complex* a) (integer* lda) (integer* info)))

(define-wrapper int claset_ ((char* uplo) (integer* m) (integer* n) (complex* alpha) (complex* beta) (complex* a) (integer* lda)))

(define-wrapper int clasr_ ((char* side) (char* pivot) (char* direct) (integer* m) (integer* n) (real* c__) (real* s) (complex* a) (integer* lda)))

(define-wrapper int classq_ ((integer* n) (complex* x) (integer* incx) (real* scale) (real* sumsq)))

(define-wrapper int claswp_ ((integer* n) (complex* a) (integer* lda) (integer* k1) (integer* k2) (integer* ipiv) (integer* incx)))

(define-wrapper int clasyf_ ((char* uplo) (integer* n) (integer* nb) (integer* kb) (complex* a) (integer* lda) (integer* ipiv) (complex* w) (integer* ldw) (integer* info)))

(define-wrapper int clatbs_ ((char* uplo) (char* trans) (char* diag) (char* normin) (integer* n) (integer* kd) (complex* ab) (integer* ldab) (complex* x) (real* scale) (real* cnorm) (integer* info)))

(define-wrapper int clatdf_ ((integer* ijob) (integer* n) (complex* z__) (integer* ldz) (complex* rhs) (real* rdsum) (real* rdscal) (integer* ipiv) (integer* jpiv)))

(define-wrapper int clatps_ ((char* uplo) (char* trans) (char* diag) (char* normin) (integer* n) (complex* ap) (complex* x) (real* scale) (real* cnorm) (integer* info)))

(define-wrapper int clatrd_ ((char* uplo) (integer* n) (integer* nb) (complex* a) (integer* lda) (real* e) (complex* tau) (complex* w) (integer* ldw)))

(define-wrapper int clatrs_ ((char* uplo) (char* trans) (char* diag) (char* normin) (integer* n) (complex* a) (integer* lda) (complex* x) (real* scale) (real* cnorm) (integer* info)))

(define-wrapper int clatrz_ ((integer* m) (integer* n) (integer* l) (complex* a) (integer* lda) (complex* tau) (complex* work)))

(define-wrapper int clatzm_ ((char* side) (integer* m) (integer* n) (complex* v) (integer* incv) (complex* tau) (complex* c1) (complex* c2) (integer* ldc) (complex* work)))

(define-wrapper int clauu2_ ((char* uplo) (integer* n) (complex* a) (integer* lda) (integer* info)))

(define-wrapper int clauum_ ((char* uplo) (integer* n) (complex* a) (integer* lda) (integer* info)))

(define-wrapper int cpbcon_ ((char* uplo) (integer* n) (integer* kd) (complex* ab) (integer* ldab) (real* anorm) (real* rcond) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int cpbequ_ ((char* uplo) (integer* n) (integer* kd) (complex* ab) (integer* ldab) (real* s) (real* scond) (real* amax) (integer* info)))

(define-wrapper int cpbrfs_ ((char* uplo) (integer* n) (integer* kd) (integer* nrhs) (complex* ab) (integer* ldab) (complex* afb) (integer* ldafb) (complex* b) (integer* ldb) (complex* x) (integer* ldx) (real* ferr) (real* berr) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int cpbstf_ ((char* uplo) (integer* n) (integer* kd) (complex* ab) (integer* ldab) (integer* info)))

(define-wrapper int cpbsv_ ((char* uplo) (integer* n) (integer* kd) (integer* nrhs) (complex* ab) (integer* ldab) (complex* b) (integer* ldb) (integer* info)))

(define-wrapper int cpbsvx_ ((char* fact) (char* uplo) (integer* n) (integer* kd) (integer* nrhs) (complex* ab) (integer* ldab) (complex* afb) (integer* ldafb) (char* equed) (real* s) (complex* b) (integer* ldb) (complex* x) (integer* ldx) (real* rcond) (real* ferr) (real* berr) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int cpbtf2_ ((char* uplo) (integer* n) (integer* kd) (complex* ab) (integer* ldab) (integer* info)))

(define-wrapper int cpbtrf_ ((char* uplo) (integer* n) (integer* kd) (complex* ab) (integer* ldab) (integer* info)))

(define-wrapper int cpbtrs_ ((char* uplo) (integer* n) (integer* kd) (integer* nrhs) (complex* ab) (integer* ldab) (complex* b) (integer* ldb) (integer* info)))

(define-wrapper int cpocon_ ((char* uplo) (integer* n) (complex* a) (integer* lda) (real* anorm) (real* rcond) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int cpoequ_ ((integer* n) (complex* a) (integer* lda) (real* s) (real* scond) (real* amax) (integer* info)))

(define-wrapper int cporfs_ ((char* uplo) (integer* n) (integer* nrhs) (complex* a) (integer* lda) (complex* af) (integer* ldaf) (complex* b) (integer* ldb) (complex* x) (integer* ldx) (real* ferr) (real* berr) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int cposv_ ((char* uplo) (integer* n) (integer* nrhs) (complex* a) (integer* lda) (complex* b) (integer* ldb) (integer* info)))

(define-wrapper int cposvx_ ((char* fact) (char* uplo) (integer* n) (integer* nrhs) (complex* a) (integer* lda) (complex* af) (integer* ldaf) (char* equed) (real* s) (complex* b) (integer* ldb) (complex* x) (integer* ldx) (real* rcond) (real* ferr) (real* berr) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int cpotf2_ ((char* uplo) (integer* n) (complex* a) (integer* lda) (integer* info)))

(define-wrapper int cpotrf_ ((char* uplo) (integer* n) (complex* a) (integer* lda) (integer* info)))

(define-wrapper int cpotri_ ((char* uplo) (integer* n) (complex* a) (integer* lda) (integer* info)))

(define-wrapper int cpotrs_ ((char* uplo) (integer* n) (integer* nrhs) (complex* a) (integer* lda) (complex* b) (integer* ldb) (integer* info)))

(define-wrapper int cppcon_ ((char* uplo) (integer* n) (complex* ap) (real* anorm) (real* rcond) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int cppequ_ ((char* uplo) (integer* n) (complex* ap) (real* s) (real* scond) (real* amax) (integer* info)))

(define-wrapper int cpprfs_ ((char* uplo) (integer* n) (integer* nrhs) (complex* ap) (complex* afp) (complex* b) (integer* ldb) (complex* x) (integer* ldx) (real* ferr) (real* berr) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int cppsv_ ((char* uplo) (integer* n) (integer* nrhs) (complex* ap) (complex* b) (integer* ldb) (integer* info)))

(define-wrapper int cppsvx_ ((char* fact) (char* uplo) (integer* n) (integer* nrhs) (complex* ap) (complex* afp) (char* equed) (real* s-arry) (complex* b) (integer* ldb) (complex* x) (integer* ldx) (real* rcond) (real* ferr) (real* berr) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int cpptrf_ ((char* uplo) (integer* n) (complex* ap) (integer* info)))

(define-wrapper int cpptri_ ((char* uplo) (integer* n) (complex* ap) (integer* info)))

(define-wrapper int cpptrs_ ((char* uplo) (integer* n) (integer* nrhs) (complex* ap) (complex* b) (integer* ldb) (integer* info)))

(define-wrapper int cptcon_ ((integer* n) (real* d__) (complex* e) (real* anorm) (real* rcond) (real* rwork) (integer* info)))

(define-wrapper int cptrfs_ ((char* uplo) (integer* n) (integer* nrhs) (real* d__) (complex* e) (real* df) (complex* ef) (complex* b) (integer* ldb) (complex* x) (integer* ldx) (real* ferr) (real* berr) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int cptsv_ ((integer* n) (integer* nrhs) (real* d__) (complex* e) (complex* b) (integer* ldb) (integer* info)))

(define-wrapper int cptsvx_ ((char* fact) (integer* n) (integer* nrhs) (real* d__) (complex* e) (real* df) (complex* ef) (complex* b) (integer* ldb) (complex* x) (integer* ldx) (real* rcond) (real* ferr) (real* berr) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int cpttrf_ ((integer* n) (real* d__) (complex* e) (integer* info)))

(define-wrapper int cpttrs_ ((char* uplo) (integer* n) (integer* nrhs) (real* d__) (complex* e) (complex* b) (integer* ldb) (integer* info)))

(define-wrapper int cptts2_ ((integer* iuplo) (integer* n) (integer* nrhs) (real* d__) (complex* e) (complex* b) (integer* ldb)))

(define-wrapper int crot_ ((integer* n) (complex* cx) (integer* incx) (complex* cy) (integer* incy) (real* c__) (complex* s)))

(define-wrapper int cspcon_ ((char* uplo) (integer* n) (complex* ap) (integer* ipiv) (real* anorm) (real* rcond) (complex* work) (integer* info)))

(define-wrapper int cspmv_ ((char* uplo) (integer* n) (complex* alpha) (complex* ap) (complex* x) (integer* incx) (complex* beta) (complex* y) (integer* incy)))

(define-wrapper int cspr_ ((char* uplo) (integer* n) (complex* alpha) (complex* x) (integer* incx) (complex* ap)))

(define-wrapper int csprfs_ ((char* uplo) (integer* n) (integer* nrhs) (complex* ap) (complex* afp) (integer* ipiv) (complex* b) (integer* ldb) (complex* x) (integer* ldx) (real* ferr) (real* berr) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int cspsv_ ((char* uplo) (integer* n) (integer* nrhs) (complex* ap) (integer* ipiv) (complex* b) (integer* ldb) (integer* info)))

(define-wrapper int cspsvx_ ((char* fact) (char* uplo) (integer* n) (integer* nrhs) (complex* ap) (complex* afp) (integer* ipiv) (complex* b) (integer* ldb) (complex* x) (integer* ldx) (real* rcond) (real* ferr) (real* berr) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int csptrf_ ((char* uplo) (integer* n) (complex* ap) (integer* ipiv) (integer* info)))

(define-wrapper int csptri_ ((char* uplo) (integer* n) (complex* ap) (integer* ipiv) (complex* work) (integer* info)))

(define-wrapper int csptrs_ ((char* uplo) (integer* n) (integer* nrhs) (complex* ap) (integer* ipiv) (complex* b) (integer* ldb) (integer* info)))

(define-wrapper int csrot_ ((integer* n) (complex* cx) (integer* incx) (complex* cy) (integer* incy) (real* c__) (real* s)))

(define-wrapper int csrscl_ ((integer* n) (real* sa) (complex* sx) (integer* incx)))

(define-wrapper int cstedc_ ((char* compz) (integer* n) (real* d__) (real* e) (complex* z__) (integer* ldz) (complex* work) (integer* lwork) (real* rwork) (integer* lrwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int cstein_ ((integer* n) (real* d__) (real* e) (integer* m) (real* w) (integer* iblock) (integer* isplit) (complex* z__) (integer* ldz) (real* work) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int csteqr_ ((char* compz) (integer* n) (real* d__) (real* e) (complex* z__) (integer* ldz) (real* work) (integer* info)))

(define-wrapper int csycon_ ((char* uplo) (integer* n) (complex* a) (integer* lda) (integer* ipiv) (real* anorm) (real* rcond) (complex* work) (integer* info)))

(define-wrapper int csymv_ ((char* uplo) (integer* n) (complex* alpha) (complex* a) (integer* lda) (complex* x) (integer* incx) (complex* beta) (complex* y) (integer* incy)))

(define-wrapper int csyr_ ((char* uplo) (integer* n) (complex* alpha) (complex* x) (integer* incx) (complex* a) (integer* lda)))

(define-wrapper int csyrfs_ ((char* uplo) (integer* n) (integer* nrhs) (complex* a) (integer* lda) (complex* af) (integer* ldaf) (integer* ipiv) (complex* b) (integer* ldb) (complex* x) (integer* ldx) (real* ferr) (real* berr) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int csysv_ ((char* uplo) (integer* n) (integer* nrhs) (complex* a) (integer* lda) (integer* ipiv) (complex* b) (integer* ldb) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int csysvx_ ((char* fact) (char* uplo) (integer* n) (integer* nrhs) (complex* a) (integer* lda) (complex* af) (integer* ldaf) (integer* ipiv) (complex* b) (integer* ldb) (complex* x) (integer* ldx) (real* rcond) (real* ferr) (real* berr) (complex* work) (integer* lwork) (real* rwork) (integer* info)))

(define-wrapper int csytf2_ ((char* uplo) (integer* n) (complex* a) (integer* lda) (integer* ipiv) (integer* info)))

(define-wrapper int csytrf_ ((char* uplo) (integer* n) (complex* a) (integer* lda) (integer* ipiv) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int csytri_ ((char* uplo) (integer* n) (complex* a) (integer* lda) (integer* ipiv) (complex* work) (integer* info)))

(define-wrapper int csytrs_ ((char* uplo) (integer* n) (integer* nrhs) (complex* a) (integer* lda) (integer* ipiv) (complex* b) (integer* ldb) (integer* info)))

(define-wrapper int ctbcon_ ((char* norm) (char* uplo) (char* diag) (integer* n) (integer* kd) (complex* ab) (integer* ldab) (real* rcond) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int ctbrfs_ ((char* uplo) (char* trans) (char* diag) (integer* n) (integer* kd) (integer* nrhs) (complex* ab) (integer* ldab) (complex* b) (integer* ldb) (complex* x) (integer* ldx) (real* ferr) (real* berr) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int ctbtrs_ ((char* uplo) (char* trans) (char* diag) (integer* n) (integer* kd) (integer* nrhs) (complex* ab) (integer* ldab) (complex* b) (integer* ldb) (integer* info)))

(define-wrapper int ctgevc_ ((char* side) (char* howmny) (logical* select) (integer* n) (complex* a) (integer* lda) (complex* b) (integer* ldb) (complex* vl) (integer* ldvl) (complex* vr) (integer* ldvr) (integer* mm) (integer* m) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int ctgex2_ ((logical* wantq) (logical* wantz) (integer* n) (complex* a) (integer* lda) (complex* b) (integer* ldb) (complex* q) (integer* ldq) (complex* z__) (integer* ldz) (integer* j1) (integer* info)))

(define-wrapper int ctgexc_ ((logical* wantq) (logical* wantz) (integer* n) (complex* a) (integer* lda) (complex* b) (integer* ldb) (complex* q) (integer* ldq) (complex* z__) (integer* ldz) (integer* ifst) (integer* ilst) (integer* info)))

(define-wrapper int ctgsen_ ((integer* ijob) (logical* wantq) (logical* wantz) (logical* select) (integer* n) (complex* a) (integer* lda) (complex* b) (integer* ldb) (complex* alpha) (complex* beta) (complex* q) (integer* ldq) (complex* z__) (integer* ldz) (integer* m) (real* pl) (real* pr) (real* dif) (complex* work) (integer* lwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int ctgsja_ ((char* jobu) (char* jobv) (char* jobq) (integer* m) (integer* p) (integer* n) (integer* k) (integer* l) (complex* a) (integer* lda) (complex* b) (integer* ldb) (real* tola) (real* tolb) (real* alpha) (real* beta) (complex* u) (integer* ldu) (complex* v) (integer* ldv) (complex* q) (integer* ldq) (complex* work) (integer* ncycle) (integer* info)))

(define-wrapper int ctgsna_ ((char* job) (char* howmny) (logical* select) (integer* n) (complex* a) (integer* lda) (complex* b) (integer* ldb) (complex* vl) (integer* ldvl) (complex* vr) (integer* ldvr) (real* s) (real* dif) (integer* mm) (integer* m) (complex* work) (integer* lwork) (integer* iwork) (integer* info)))

(define-wrapper int ctgsy2_ ((char* trans) (integer* ijob) (integer* m) (integer* n) (complex* a) (integer* lda) (complex* b) (integer* ldb) (complex* c__) (integer* ldc) (complex* d__) (integer* ldd) (complex* e) (integer* lde) (complex* f) (integer* ldf) (real* scale) (real* rdsum) (real* rdscal) (integer* info)))

(define-wrapper int ctgsyl_ ((char* trans) (integer* ijob) (integer* m) (integer* n) (complex* a) (integer* lda) (complex* b) (integer* ldb) (complex* c__) (integer* ldc) (complex* d__) (integer* ldd) (complex* e) (integer* lde) (complex* f) (integer* ldf) (real* scale) (real* dif) (complex* work) (integer* lwork) (integer* iwork) (integer* info)))

(define-wrapper int ctpcon_ ((char* norm) (char* uplo) (char* diag) (integer* n) (complex* ap) (real* rcond) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int ctprfs_ ((char* uplo) (char* trans) (char* diag) (integer* n) (integer* nrhs) (complex* ap) (complex* b) (integer* ldb) (complex* x) (integer* ldx) (real* ferr) (real* berr) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int ctptri_ ((char* uplo) (char* diag) (integer* n) (complex* ap) (integer* info)))

(define-wrapper int ctptrs_ ((char* uplo) (char* trans) (char* diag) (integer* n) (integer* nrhs) (complex* ap) (complex* b) (integer* ldb) (integer* info)))

(define-wrapper int ctrcon_ ((char* norm) (char* uplo) (char* diag) (integer* n) (complex* a) (integer* lda) (real* rcond) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int ctrevc_ ((char* side) (char* howmny) (logical* select) (integer* n) (complex* t) (integer* ldt) (complex* vl) (integer* ldvl) (complex* vr) (integer* ldvr) (integer* mm) (integer* m) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int ctrexc_ ((char* compq) (integer* n) (complex* t) (integer* ldt) (complex* q) (integer* ldq) (integer* ifst) (integer* ilst) (integer* info)))

(define-wrapper int ctrrfs_ ((char* uplo) (char* trans) (char* diag) (integer* n) (integer* nrhs) (complex* a) (integer* lda) (complex* b) (integer* ldb) (complex* x) (integer* ldx) (real* ferr) (real* berr) (complex* work) (real* rwork) (integer* info)))

(define-wrapper int ctrsen_ ((char* job) (char* compq) (logical* select) (integer* n) (complex* t) (integer* ldt) (complex* q) (integer* ldq) (complex* w) (integer* m) (real* s) (real* sep) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int ctrsna_ ((char* job) (char* howmny) (logical* select) (integer* n) (complex* t) (integer* ldt) (complex* vl) (integer* ldvl) (complex* vr) (integer* ldvr) (real* s) (real* sep) (integer* mm) (integer* m) (complex* work) (integer* ldwork) (real* rwork) (integer* info)))

(define-wrapper int ctrsyl_ ((char* trana) (char* tranb) (integer* isgn) (integer* m) (integer* n) (complex* a) (integer* lda) (complex* b) (integer* ldb) (complex* c__) (integer* ldc) (real* scale) (integer* info)))

(define-wrapper int ctrti2_ ((char* uplo) (char* diag) (integer* n) (complex* a) (integer* lda) (integer* info)))

(define-wrapper int ctrtri_ ((char* uplo) (char* diag) (integer* n) (complex* a) (integer* lda) (integer* info)))

(define-wrapper int ctrtrs_ ((char* uplo) (char* trans) (char* diag) (integer* n) (integer* nrhs) (complex* a) (integer* lda) (complex* b) (integer* ldb) (integer* info)))

(define-wrapper int ctzrqf_ ((integer* m) (integer* n) (complex* a) (integer* lda) (complex* tau) (integer* info)))

(define-wrapper int ctzrzf_ ((integer* m) (integer* n) (complex* a) (integer* lda) (complex* tau) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int cung2l_ ((integer* m) (integer* n) (integer* k) (complex* a) (integer* lda) (complex* tau) (complex* work) (integer* info)))

(define-wrapper int cung2r_ ((integer* m) (integer* n) (integer* k) (complex* a) (integer* lda) (complex* tau) (complex* work) (integer* info)))

(define-wrapper int cungbr_ ((char* vect) (integer* m) (integer* n) (integer* k) (complex* a) (integer* lda) (complex* tau) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int cunghr_ ((integer* n) (integer* ilo) (integer* ihi) (complex* a) (integer* lda) (complex* tau) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int cungl2_ ((integer* m) (integer* n) (integer* k) (complex* a) (integer* lda) (complex* tau) (complex* work) (integer* info)))

(define-wrapper int cunglq_ ((integer* m) (integer* n) (integer* k) (complex* a) (integer* lda) (complex* tau) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int cungql_ ((integer* m) (integer* n) (integer* k) (complex* a) (integer* lda) (complex* tau) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int cungqr_ ((integer* m) (integer* n) (integer* k) (complex* a) (integer* lda) (complex* tau) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int cungr2_ ((integer* m) (integer* n) (integer* k) (complex* a) (integer* lda) (complex* tau) (complex* work) (integer* info)))

(define-wrapper int cungrq_ ((integer* m) (integer* n) (integer* k) (complex* a) (integer* lda) (complex* tau) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int cungtr_ ((char* uplo) (integer* n) (complex* a) (integer* lda) (complex* tau) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int cunm2l_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (complex* a) (integer* lda) (complex* tau) (complex* c__) (integer* ldc) (complex* work) (integer* info)))

(define-wrapper int cunm2r_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (complex* a) (integer* lda) (complex* tau) (complex* c__) (integer* ldc) (complex* work) (integer* info)))

(define-wrapper int cunmbr_ ((char* vect) (char* side) (char* trans) (integer* m) (integer* n) (integer* k) (complex* a) (integer* lda) (complex* tau) (complex* c__) (integer* ldc) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int cunmhr_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* ilo) (integer* ihi) (complex* a) (integer* lda) (complex* tau) (complex* c__) (integer* ldc) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int cunml2_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (complex* a) (integer* lda) (complex* tau) (complex* c__) (integer* ldc) (complex* work) (integer* info)))

(define-wrapper int cunmlq_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (complex* a) (integer* lda) (complex* tau) (complex* c__) (integer* ldc) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int cunmql_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (complex* a) (integer* lda) (complex* tau) (complex* c__) (integer* ldc) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int cunmqr_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (complex* a) (integer* lda) (complex* tau) (complex* c__) (integer* ldc) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int cunmr2_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (complex* a) (integer* lda) (complex* tau) (complex* c__) (integer* ldc) (complex* work) (integer* info)))

(define-wrapper int cunmr3_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (integer* l) (complex* a) (integer* lda) (complex* tau) (complex* c__) (integer* ldc) (complex* work) (integer* info)))

(define-wrapper int cunmrq_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (complex* a) (integer* lda) (complex* tau) (complex* c__) (integer* ldc) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int cunmrz_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (integer* l) (complex* a) (integer* lda) (complex* tau) (complex* c__) (integer* ldc) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int cunmtr_ ((char* side) (char* uplo) (char* trans) (integer* m) (integer* n) (complex* a) (integer* lda) (complex* tau) (complex* c__) (integer* ldc) (complex* work) (integer* lwork) (integer* info)))

(define-wrapper int cupgtr_ ((char* uplo) (integer* n) (complex* ap) (complex* tau) (complex* q) (integer* ldq) (complex* work) (integer* info)))

(define-wrapper int cupmtr_ ((char* side) (char* uplo) (char* trans) (integer* m) (integer* n) (complex* ap) (complex* tau) (complex* c__) (integer* ldc) (complex* work) (integer* info)))

(define-wrapper int dbdsdc_ ((char* uplo) (char* compq) (integer* n) (doublereal* d__) (doublereal* e) (doublereal* u) (integer* ldu) (doublereal* vt) (integer* ldvt) (doublereal* q) (integer* iq) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dbdsqr_ ((char* uplo) (integer* n) (integer* ncvt) (integer* nru) (integer* ncc) (doublereal* d__) (doublereal* e) (doublereal* vt) (integer* ldvt) (doublereal* u) (integer* ldu) (doublereal* c__) (integer* ldc) (doublereal* work) (integer* info)))

(define-wrapper int ddisna_ ((char* job) (integer* m) (integer* n) (doublereal* d__) (doublereal* sep) (integer* info)))

(define-wrapper int dgbbrd_ ((char* vect) (integer* m) (integer* n) (integer* ncc) (integer* kl) (integer* ku) (doublereal* ab) (integer* ldab) (doublereal* d__) (doublereal* e) (doublereal* q) (integer* ldq) (doublereal* pt) (integer* ldpt) (doublereal* c__) (integer* ldc) (doublereal* work) (integer* info)))

(define-wrapper int dgbcon_ ((char* norm) (integer* n) (integer* kl) (integer* ku) (doublereal* ab) (integer* ldab) (integer* ipiv) (doublereal* anorm) (doublereal* rcond) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dgbequ_ ((integer* m) (integer* n) (integer* kl) (integer* ku) (doublereal* ab) (integer* ldab) (doublereal* r__) (doublereal* c__) (doublereal* rowcnd) (doublereal* colcnd) (doublereal* amax) (integer* info)))

(define-wrapper int dgbrfs_ ((char* trans) (integer* n) (integer* kl) (integer* ku) (integer* nrhs) (doublereal* ab) (integer* ldab) (doublereal* afb) (integer* ldafb) (integer* ipiv) (doublereal* b) (integer* ldb) (doublereal* x) (integer* ldx) (doublereal* ferr) (doublereal* berr) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dgbsv_ ((integer* n) (integer* kl) (integer* ku) (integer* nrhs) (doublereal* ab) (integer* ldab) (integer* ipiv) (doublereal* b) (integer* ldb) (integer* info)))

(define-wrapper int dgbsvx_ ((char* fact) (char* trans) (integer* n) (integer* kl) (integer* ku) (integer* nrhs) (doublereal* ab) (integer* ldab) (doublereal* afb) (integer* ldafb) (integer* ipiv) (char* equed) (doublereal* r__) (doublereal* c__) (doublereal* b) (integer* ldb) (doublereal* x) (integer* ldx) (doublereal* rcond) (doublereal* ferr) (doublereal* berr) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dgbtf2_ ((integer* m) (integer* n) (integer* kl) (integer* ku) (doublereal* ab) (integer* ldab) (integer* ipiv) (integer* info)))

(define-wrapper int dgbtrf_ ((integer* m) (integer* n) (integer* kl) (integer* ku) (doublereal* ab) (integer* ldab) (integer* ipiv) (integer* info)))

(define-wrapper int dgbtrs_ ((char* trans) (integer* n) (integer* kl) (integer* ku) (integer* nrhs) (doublereal* ab) (integer* ldab) (integer* ipiv) (doublereal* b) (integer* ldb) (integer* info)))

(define-wrapper int dgebak_ ((char* job) (char* side) (integer* n) (integer* ilo) (integer* ihi) (doublereal* scale) (integer* m) (doublereal* v) (integer* ldv) (integer* info)))

(define-wrapper int dgebal_ ((char* job) (integer* n) (doublereal* a) (integer* lda) (integer* ilo) (integer* ihi) (doublereal* scale) (integer* info)))

(define-wrapper int dgebd2_ ((integer* m) (integer* n) (doublereal* a) (integer* lda) (doublereal* d__) (doublereal* e) (doublereal* tauq) (doublereal* taup) (doublereal* work) (integer* info)))

(define-wrapper int dgebrd_ ((integer* m) (integer* n) (doublereal* a) (integer* lda) (doublereal* d__) (doublereal* e) (doublereal* tauq) (doublereal* taup) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dgecon_ ((char* norm) (integer* n) (doublereal* a) (integer* lda) (doublereal* anorm) (doublereal* rcond) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dgeequ_ ((integer* m) (integer* n) (doublereal* a) (integer* lda) (doublereal* r__) (doublereal* c__) (doublereal* rowcnd) (doublereal* colcnd) (doublereal* amax) (integer* info)))

(define-wrapper int dgees_ ((char* jobvs) (char* sort) (L_fp select) (integer* n) (doublereal* a) (integer* lda) (integer* sdim) (doublereal* wr) (doublereal* wi) (doublereal* vs) (integer* ldvs) (doublereal* work) (integer* lwork) (logical* bwork) (integer* info)))

(define-wrapper int dgeesx_ ((char* jobvs) (char* sort) (L_fp select) (char* sense) (integer* n) (doublereal* a) (integer* lda) (integer* sdim) (doublereal* wr) (doublereal* wi) (doublereal* vs) (integer* ldvs) (doublereal* rconde) (doublereal* rcondv) (doublereal* work) (integer* lwork) (integer* iwork) (integer* liwork) (logical* bwork) (integer* info)))

(define-wrapper int dgeev_ ((char* jobvl) (char* jobvr) (integer* n) (doublereal* a) (integer* lda) (doublereal* wr) (doublereal* wi) (doublereal* vl) (integer* ldvl) (doublereal* vr) (integer* ldvr) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dgeevx_ ((char* balanc) (char* jobvl) (char* jobvr) (char* sense) (integer* n) (doublereal* a) (integer* lda) (doublereal* wr) (doublereal* wi) (doublereal* vl) (integer* ldvl) (doublereal* vr) (integer* ldvr) (integer* ilo) (integer* ihi) (doublereal* scale) (doublereal* abnrm) (doublereal* rconde) (doublereal* rcondv) (doublereal* work) (integer* lwork) (integer* iwork) (integer* info)))

(define-wrapper int dgegs_ ((char* jobvsl) (char* jobvsr) (integer* n) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (doublereal* alphar) (doublereal* alphai) (doublereal* beta) (doublereal* vsl) (integer* ldvsl) (doublereal* vsr) (integer* ldvsr) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dgegv_ ((char* jobvl) (char* jobvr) (integer* n) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (doublereal* alphar) (doublereal* alphai) (doublereal* beta) (doublereal* vl) (integer* ldvl) (doublereal* vr) (integer* ldvr) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dgehd2_ ((integer* n) (integer* ilo) (integer* ihi) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* work) (integer* info)))

(define-wrapper int dgehrd_ ((integer* n) (integer* ilo) (integer* ihi) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dgelq2_ ((integer* m) (integer* n) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* work) (integer* info)))

(define-wrapper int dgelqf_ ((integer* m) (integer* n) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dgels_ ((char* trans) (integer* m) (integer* n) (integer* nrhs) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dgelsd_ ((integer* m) (integer* n) (integer* nrhs) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (doublereal* s) (doublereal* rcond) (integer* rank) (doublereal* work) (integer* lwork) (integer* iwork) (integer* info)))

(define-wrapper int dgelss_ ((integer* m) (integer* n) (integer* nrhs) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (doublereal* s) (doublereal* rcond) (integer* rank) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dgelsx_ ((integer* m) (integer* n) (integer* nrhs) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (integer* jpvt) (doublereal* rcond) (integer* rank) (doublereal* work) (integer* info)))

(define-wrapper int dgelsy_ ((integer* m) (integer* n) (integer* nrhs) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (integer* jpvt) (doublereal* rcond) (integer* rank) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dgeql2_ ((integer* m) (integer* n) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* work) (integer* info)))

(define-wrapper int dgeqlf_ ((integer* m) (integer* n) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dgeqp3_ ((integer* m) (integer* n) (doublereal* a) (integer* lda) (integer* jpvt) (doublereal* tau) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dgeqpf_ ((integer* m) (integer* n) (doublereal* a) (integer* lda) (integer* jpvt) (doublereal* tau) (doublereal* work) (integer* info)))

(define-wrapper int dgeqr2_ ((integer* m) (integer* n) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* work) (integer* info)))

(define-wrapper int dgeqrf_ ((integer* m) (integer* n) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dgerfs_ ((char* trans) (integer* n) (integer* nrhs) (doublereal* a) (integer* lda) (doublereal* af) (integer* ldaf) (integer* ipiv) (doublereal* b) (integer* ldb) (doublereal* x) (integer* ldx) (doublereal* ferr) (doublereal* berr) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dgerq2_ ((integer* m) (integer* n) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* work) (integer* info)))

(define-wrapper int dgerqf_ ((integer* m) (integer* n) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dgesc2_ ((integer* n) (doublereal* a) (integer* lda) (doublereal* rhs) (integer* ipiv) (integer* jpiv) (doublereal* scale)))

(define-wrapper int dgesdd_ ((char* jobz) (integer* m) (integer* n) (doublereal* a) (integer* lda) (doublereal* s) (doublereal* u) (integer* ldu) (doublereal* vt) (integer* ldvt) (doublereal* work) (integer* lwork) (integer* iwork) (integer* info)))

(define-wrapper int dgesv_ ((integer* n) (integer* nrhs) (doublereal* a) (integer* lda) (integer* ipiv) (doublereal* b) (integer* ldb) (integer* info)))

(define-wrapper int dgesvd_ ((char* jobu) (char* jobvt) (integer* m) (integer* n) (doublereal* a) (integer* lda) (doublereal* s-arry) (doublereal* u) (integer* ldu) (doublereal* vt) (integer* ldvt) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dgesvx_ ((char* fact) (char* trans) (integer* n) (integer* nrhs) (doublereal* a) (integer* lda) (doublereal* af) (integer* ldaf) (integer* ipiv) (char* equed) (doublereal* r__) (doublereal* c__) (doublereal* b) (integer* ldb) (doublereal* x) (integer* ldx) (doublereal* rcond) (doublereal* ferr) (doublereal* berr) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dgetc2_ ((integer* n) (doublereal* a) (integer* lda) (integer* ipiv) (integer* jpiv) (integer* info)))

(define-wrapper int dgetf2_ ((integer* m) (integer* n) (doublereal* a) (integer* lda) (integer* ipiv) (integer* info)))

(define-wrapper int dgetrf_ ((integer* m) (integer* n) (doublereal* a) (integer* lda) (integer* ipiv) (integer* info)))

(define-wrapper int dgetri_ ((integer* n) (doublereal* a) (integer* lda) (integer* ipiv) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dgetrs_ ((char* trans) (integer* n) (integer* nrhs) (doublereal* a) (integer* lda) (integer* ipiv) (doublereal* b) (integer* ldb) (integer* info)))

(define-wrapper int dggbak_ ((char* job) (char* side) (integer* n) (integer* ilo) (integer* ihi) (doublereal* lscale) (doublereal* rscale) (integer* m) (doublereal* v) (integer* ldv) (integer* info)))

(define-wrapper int dggbal_ ((char* job) (integer* n) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (integer* ilo) (integer* ihi) (doublereal* lscale) (doublereal* rscale) (doublereal* work) (integer* info)))

(define-wrapper int dgges_ ((char* jobvsl) (char* jobvsr) (char* sort) (L_fp delctg) (integer* n) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (integer* sdim) (doublereal* alphar) (doublereal* alphai) (doublereal* beta) (doublereal* vsl) (integer* ldvsl) (doublereal* vsr) (integer* ldvsr) (doublereal* work) (integer* lwork) (logical* bwork) (integer* info)))

(define-wrapper int dggesx_ ((char* jobvsl) (char* jobvsr) (char* sort) (L_fp delctg) (char* sense) (integer* n) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (integer* sdim) (doublereal* alphar) (doublereal* alphai) (doublereal* beta) (doublereal* vsl) (integer* ldvsl) (doublereal* vsr) (integer* ldvsr) (doublereal* rconde) (doublereal* rcondv) (doublereal* work) (integer* lwork) (integer* iwork) (integer* liwork) (logical* bwork) (integer* info)))

(define-wrapper int dggev_ ((char* jobvl) (char* jobvr) (integer* n) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (doublereal* alphar) (doublereal* alphai) (doublereal* beta) (doublereal* vl) (integer* ldvl) (doublereal* vr) (integer* ldvr) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dggevx_ ((char* balanc) (char* jobvl) (char* jobvr) (char* sense) (integer* n) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (doublereal* alphar) (doublereal* alphai) (doublereal* beta) (doublereal* vl) (integer* ldvl) (doublereal* vr) (integer* ldvr) (integer* ilo) (integer* ihi) (doublereal* lscale) (doublereal* rscale) (doublereal* abnrm) (doublereal* bbnrm) (doublereal* rconde) (doublereal* rcondv) (doublereal* work) (integer* lwork) (integer* iwork) (logical* bwork) (integer* info)))

(define-wrapper int dggglm_ ((integer* n) (integer* m) (integer* p) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (doublereal* d__) (doublereal* x) (doublereal* y) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dgghrd_ ((char* compq) (char* compz) (integer* n) (integer* ilo) (integer* ihi) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (doublereal* q) (integer* ldq) (doublereal* z__) (integer* ldz) (integer* info)))

(define-wrapper int dgglse_ ((integer* m) (integer* n) (integer* p) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (doublereal* c__) (doublereal* d__) (doublereal* x) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dggqrf_ ((integer* n) (integer* m) (integer* p) (doublereal* a) (integer* lda) (doublereal* taua) (doublereal* b) (integer* ldb) (doublereal* taub) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dggrqf_ ((integer* m) (integer* p) (integer* n) (doublereal* a) (integer* lda) (doublereal* taua) (doublereal* b) (integer* ldb) (doublereal* taub) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dggsvd_ ((char* jobu) (char* jobv) (char* jobq) (integer* m) (integer* n) (integer* p) (integer* k) (integer* l) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (doublereal* alpha) (doublereal* beta) (doublereal* u) (integer* ldu) (doublereal* v) (integer* ldv) (doublereal* q) (integer* ldq) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dggsvp_ ((char* jobu) (char* jobv) (char* jobq) (integer* m) (integer* p) (integer* n) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (doublereal* tola) (doublereal* tolb) (integer* k) (integer* l) (doublereal* u) (integer* ldu) (doublereal* v) (integer* ldv) (doublereal* q) (integer* ldq) (integer* iwork) (doublereal* tau) (doublereal* work) (integer* info)))

(define-wrapper int dgtcon_ ((char* norm) (integer* n) (doublereal* dl) (doublereal* d__) (doublereal* du) (doublereal* du2) (integer* ipiv) (doublereal* anorm) (doublereal* rcond) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dgtrfs_ ((char* trans) (integer* n) (integer* nrhs) (doublereal* dl) (doublereal* d__) (doublereal* du) (doublereal* dlf) (doublereal* df) (doublereal* duf) (doublereal* du2) (integer* ipiv) (doublereal* b) (integer* ldb) (doublereal* x) (integer* ldx) (doublereal* ferr) (doublereal* berr) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dgtsv_ ((integer* n) (integer* nrhs) (doublereal* dl) (doublereal* d__) (doublereal* du) (doublereal* b) (integer* ldb) (integer* info)))

(define-wrapper int dgtsvx_ ((char* fact) (char* trans) (integer* n) (integer* nrhs) (doublereal* dl) (doublereal* d__) (doublereal* du) (doublereal* dlf) (doublereal* df) (doublereal* duf) (doublereal* du2) (integer* ipiv) (doublereal* b) (integer* ldb) (doublereal* x) (integer* ldx) (doublereal* rcond) (doublereal* ferr) (doublereal* berr) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dgttrf_ ((integer* n) (doublereal* dl) (doublereal* d__) (doublereal* du) (doublereal* du2) (integer* ipiv) (integer* info)))

(define-wrapper int dgttrs_ ((char* trans) (integer* n) (integer* nrhs) (doublereal* dl) (doublereal* d__) (doublereal* du) (doublereal* du2) (integer* ipiv) (doublereal* b) (integer* ldb) (integer* info)))

(define-wrapper int dgtts2_ ((integer* itrans) (integer* n) (integer* nrhs) (doublereal* dl) (doublereal* d__) (doublereal* du) (doublereal* du2) (integer* ipiv) (doublereal* b) (integer* ldb)))

(define-wrapper int dhgeqz_ ((char* job) (char* compq) (char* compz) (integer* n) (integer* ilo) (integer* ihi) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (doublereal* alphar) (doublereal* alphai) (doublereal* beta) (doublereal* q) (integer* ldq) (doublereal* z__) (integer* ldz) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dhsein_ ((char* side) (char* eigsrc) (char* initv) (logical* select) (integer* n) (doublereal* h__) (integer* ldh) (doublereal* wr) (doublereal* wi) (doublereal* vl) (integer* ldvl) (doublereal* vr) (integer* ldvr) (integer* mm) (integer* m) (doublereal* work) (integer* ifaill) (integer* ifailr) (integer* info)))

(define-wrapper int dhseqr_ ((char* job) (char* compz) (integer* n) (integer* ilo) (integer* ihi) (doublereal* h__) (integer* ldh) (doublereal* wr) (doublereal* wi) (doublereal* z__) (integer* ldz) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dlabad_ ((doublereal* small) (doublereal* large)))

(define-wrapper int dlabrd_ ((integer* m) (integer* n) (integer* nb) (doublereal* a) (integer* lda) (doublereal* d__) (doublereal* e) (doublereal* tauq) (doublereal* taup) (doublereal* x) (integer* ldx) (doublereal* y) (integer* ldy)))

(define-wrapper int dlacon_ ((integer* n) (doublereal* v) (doublereal* x) (integer* isgn) (doublereal* est) (integer* kase)))

(define-wrapper int dlacpy_ ((char* uplo) (integer* m) (integer* n) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb)))

(define-wrapper int dladiv_ ((doublereal* a) (doublereal* b) (doublereal* c__) (doublereal* d__) (doublereal* p) (doublereal* q)))

(define-wrapper int dlae2_ ((doublereal* a) (doublereal* b) (doublereal* c__) (doublereal* rt1) (doublereal* rt2)))

(define-wrapper int dlaebz_ ((integer* ijob) (integer* nitmax) (integer* n) (integer* mmax) (integer* minp) (integer* nbmin) (doublereal* abstol) (doublereal* reltol) (doublereal* pivmin) (doublereal* d__) (doublereal* e) (doublereal* e2) (integer* nval) (doublereal* ab) (doublereal* c__) (integer* mout) (integer* nab) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dlaed0_ ((integer* icompq) (integer* qsiz) (integer* n) (doublereal* d__) (doublereal* e) (doublereal* q) (integer* ldq) (doublereal* qstore) (integer* ldqs) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dlaed1_ ((integer* n) (doublereal* d__) (doublereal* q) (integer* ldq) (integer* indxq) (doublereal* rho) (integer* cutpnt) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dlaed2_ ((integer* k) (integer* n) (integer* n1) (doublereal* d__) (doublereal* q) (integer* ldq) (integer* indxq) (doublereal* rho) (doublereal* z__) (doublereal* dlamda) (doublereal* w) (doublereal* q2) (integer* indx) (integer* indxc) (integer* indxp) (integer* coltyp) (integer* info)))

(define-wrapper int dlaed3_ ((integer* k) (integer* n) (integer* n1) (doublereal* d__) (doublereal* q) (integer* ldq) (doublereal* rho) (doublereal* dlamda) (doublereal* q2) (integer* indx) (integer* ctot) (doublereal* w) (doublereal* s) (integer* info)))

(define-wrapper int dlaed4_ ((integer* n) (integer* i__) (doublereal* d__) (doublereal* z__) (doublereal* delta) (doublereal* rho) (doublereal* dlam) (integer* info)))

(define-wrapper int dlaed5_ ((integer* i__) (doublereal* d__) (doublereal* z__) (doublereal* delta) (doublereal* rho) (doublereal* dlam)))

(define-wrapper int dlaed6_ ((integer* kniter) (logical* orgati) (doublereal* rho) (doublereal* d__) (doublereal* z__) (doublereal* finit) (doublereal* tau) (integer* info)))

(define-wrapper int dlaed7_ ((integer* icompq) (integer* n) (integer* qsiz) (integer* tlvls) (integer* curlvl) (integer* curpbm) (doublereal* d__) (doublereal* q) (integer* ldq) (integer* indxq) (doublereal* rho) (integer* cutpnt) (doublereal* qstore) (integer* qptr) (integer* prmptr) (integer* perm) (integer* givptr) (integer* givcol) (doublereal* givnum) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dlaed8_ ((integer* icompq) (integer* k) (integer* n) (integer* qsiz) (doublereal* d__) (doublereal* q) (integer* ldq) (integer* indxq) (doublereal* rho) (integer* cutpnt) (doublereal* z__) (doublereal* dlamda) (doublereal* q2) (integer* ldq2) (doublereal* w) (integer* perm) (integer* givptr) (integer* givcol) (doublereal* givnum) (integer* indxp) (integer* indx) (integer* info)))

(define-wrapper int dlaed9_ ((integer* k) (integer* kstart) (integer* kstop) (integer* n) (doublereal* d__) (doublereal* q) (integer* ldq) (doublereal* rho) (doublereal* dlamda) (doublereal* w) (doublereal* s) (integer* lds) (integer* info)))

(define-wrapper int dlaeda_ ((integer* n) (integer* tlvls) (integer* curlvl) (integer* curpbm) (integer* prmptr) (integer* perm) (integer* givptr) (integer* givcol) (doublereal* givnum) (doublereal* q) (integer* qptr) (doublereal* z__) (doublereal* ztemp) (integer* info)))

(define-wrapper int dlaein_ ((logical* rightv) (logical* noinit) (integer* n) (doublereal* h__) (integer* ldh) (doublereal* wr) (doublereal* wi) (doublereal* vr) (doublereal* vi) (doublereal* b) (integer* ldb) (doublereal* work) (doublereal* eps3) (doublereal* smlnum) (doublereal* bignum) (integer* info)))

(define-wrapper int dlaev2_ ((doublereal* a) (doublereal* b) (doublereal* c__) (doublereal* rt1) (doublereal* rt2) (doublereal* cs1) (doublereal* sn1)))

(define-wrapper int dlaexc_ ((logical* wantq) (integer* n) (doublereal* t) (integer* ldt) (doublereal* q) (integer* ldq) (integer* j1) (integer* n1) (integer* n2) (doublereal* work) (integer* info)))

(define-wrapper int dlag2_ ((doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (doublereal* safmin) (doublereal* scale1) (doublereal* scale2) (doublereal* wr1) (doublereal* wr2) (doublereal* wi)))

(define-wrapper int dlags2_ ((logical* upper) (doublereal* a1) (doublereal* a2) (doublereal* a3) (doublereal* b1) (doublereal* b2) (doublereal* b3) (doublereal* csu) (doublereal* snu) (doublereal* csv) (doublereal* snv) (doublereal* csq) (doublereal* snq)))

(define-wrapper int dlagtf_ ((integer* n) (doublereal* a) (doublereal* lambda) (doublereal* b) (doublereal* c__) (doublereal* tol) (doublereal* d__) (integer* in) (integer* info)))

(define-wrapper int dlagtm_ ((char* trans) (integer* n) (integer* nrhs) (doublereal* alpha) (doublereal* dl) (doublereal* d__) (doublereal* du) (doublereal* x) (integer* ldx) (doublereal* beta) (doublereal* b) (integer* ldb)))

(define-wrapper int dlagts_ ((integer* job) (integer* n) (doublereal* a) (doublereal* b) (doublereal* c__) (doublereal* d__) (integer* in) (doublereal* y) (doublereal* tol) (integer* info)))

(define-wrapper int dlagv2_ ((doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (doublereal* alphar) (doublereal* alphai) (doublereal* beta) (doublereal* csl) (doublereal* snl) (doublereal* csr) (doublereal* snr)))

(define-wrapper int dlahqr_ ((logical* wantt) (logical* wantz) (integer* n) (integer* ilo) (integer* ihi) (doublereal* h__) (integer* ldh) (doublereal* wr) (doublereal* wi) (integer* iloz) (integer* ihiz) (doublereal* z__) (integer* ldz) (integer* info)))

(define-wrapper int dlahrd_ ((integer* n) (integer* k) (integer* nb) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* t) (integer* ldt) (doublereal* y) (integer* ldy)))

(define-wrapper int dlaic1_ ((integer* job) (integer* j) (doublereal* x) (doublereal* sest) (doublereal* w) (doublereal* gamma) (doublereal* sestpr) (doublereal* s) (doublereal* c__)))

(define-wrapper int dlaln2_ ((logical* ltrans) (integer* na) (integer* nw) (doublereal* smin) (doublereal* ca) (doublereal* a) (integer* lda) (doublereal* d1) (doublereal* d2) (doublereal* b) (integer* ldb) (doublereal* wr) (doublereal* wi) (doublereal* x) (integer* ldx) (doublereal* scale) (doublereal* xnorm) (integer* info)))

(define-wrapper int dlals0_ ((integer* icompq) (integer* nl) (integer* nr) (integer* sqre) (integer* nrhs) (doublereal* b) (integer* ldb) (doublereal* bx) (integer* ldbx) (integer* perm) (integer* givptr) (integer* givcol) (integer* ldgcol) (doublereal* givnum) (integer* ldgnum) (doublereal* poles) (doublereal* difl) (doublereal* difr) (doublereal* z__) (integer* k) (doublereal* c__) (doublereal* s) (doublereal* work) (integer* info)))

(define-wrapper int dlalsa_ ((integer* icompq) (integer* smlsiz) (integer* n) (integer* nrhs) (doublereal* b) (integer* ldb) (doublereal* bx) (integer* ldbx) (doublereal* u) (integer* ldu) (doublereal* vt) (integer* k) (doublereal* difl) (doublereal* difr) (doublereal* z__) (doublereal* poles) (integer* givptr) (integer* givcol) (integer* ldgcol) (integer* perm) (doublereal* givnum) (doublereal* c__) (doublereal* s) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dlalsd_ ((char* uplo) (integer* smlsiz) (integer* n) (integer* nrhs) (doublereal* d__) (doublereal* e) (doublereal* b) (integer* ldb) (doublereal* rcond) (integer* rank) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dlamc1_ ((integer* beta) (integer* t) (logical* rnd) (logical* ieee1)))

(define-wrapper int dlamc2_ ((integer* beta) (integer* t) (logical* rnd) (doublereal* eps) (integer* emin) (doublereal* rmin) (integer* emax) (doublereal* rmax)))

(define-wrapper int dlamc4_ ((integer* emin) (doublereal* start) (integer* base)))

(define-wrapper int dlamc5_ ((integer* beta) (integer* p) (integer* emin) (logical* ieee) (integer* emax) (doublereal* rmax)))

(define-wrapper int dlamrg_ ((integer* n1) (integer* n2) (doublereal* a) (integer* dtrd1) (integer* dtrd2) (integer* index)))

(define-wrapper int dlanv2_ ((doublereal* a) (doublereal* b) (doublereal* c__) (doublereal* d__) (doublereal* rt1r) (doublereal* rt1i) (doublereal* rt2r) (doublereal* rt2i) (doublereal* cs) (doublereal* sn)))

(define-wrapper int dlapll_ ((integer* n) (doublereal* x) (integer* incx) (doublereal* y) (integer* incy) (doublereal* ssmin)))

(define-wrapper int dlapmt_ ((logical* forwrd) (integer* m) (integer* n) (doublereal* x) (integer* ldx) (integer* k)))

(define-wrapper int dlaqgb_ ((integer* m) (integer* n) (integer* kl) (integer* ku) (doublereal* ab) (integer* ldab) (doublereal* r__) (doublereal* c__) (doublereal* rowcnd) (doublereal* colcnd) (doublereal* amax) (char* equed)))

(define-wrapper int dlaqge_ ((integer* m) (integer* n) (doublereal* a) (integer* lda) (doublereal* r__) (doublereal* c__) (doublereal* rowcnd) (doublereal* colcnd) (doublereal* amax) (char* equed)))

(define-wrapper int dlaqp2_ ((integer* m) (integer* n) (integer* offset) (doublereal* a) (integer* lda) (integer* jpvt) (doublereal* tau) (doublereal* vn1) (doublereal* vn2) (doublereal* work)))

(define-wrapper int dlaqps_ ((integer* m) (integer* n) (integer* offset) (integer* nb) (integer* kb) (doublereal* a) (integer* lda) (integer* jpvt) (doublereal* tau) (doublereal* vn1) (doublereal* vn2) (doublereal* auxv) (doublereal* f) (integer* ldf)))

(define-wrapper int dlaqsb_ ((char* uplo) (integer* n) (integer* kd) (doublereal* ab) (integer* ldab) (doublereal* s) (doublereal* scond) (doublereal* amax) (char* equed)))

(define-wrapper int dlaqsp_ ((char* uplo) (integer* n) (doublereal* ap) (doublereal* s) (doublereal* scond) (doublereal* amax) (char* equed)))

(define-wrapper int dlaqsy_ ((char* uplo) (integer* n) (doublereal* a) (integer* lda) (doublereal* s) (doublereal* scond) (doublereal* amax) (char* equed)))

(define-wrapper int dlaqtr_ ((logical* ltran) (logical* lreal) (integer* n) (doublereal* t) (integer* ldt) (doublereal* b) (doublereal* w) (doublereal* scale) (doublereal* x) (doublereal* work) (integer* info)))

(define-wrapper int dlar1v_ ((integer* n) (integer* b1) (integer* bn) (doublereal* sigma) (doublereal* d__) (doublereal* l) (doublereal* ld) (doublereal* lld) (doublereal* gersch) (doublereal* z__) (doublereal* ztz) (doublereal* mingma) (integer* r__) (integer* isuppz) (doublereal* work)))

(define-wrapper int dlar2v_ ((integer* n) (doublereal* x) (doublereal* y) (doublereal* z__) (integer* incx) (doublereal* c__) (doublereal* s) (integer* incc)))

(define-wrapper int dlarf_ ((char* side) (integer* m) (integer* n) (doublereal* v) (integer* incv) (doublereal* tau) (doublereal* c__) (integer* ldc) (doublereal* work)))

(define-wrapper int dlarfb_ ((char* side) (char* trans) (char* direct) (char* storev) (integer* m) (integer* n) (integer* k) (doublereal* v) (integer* ldv) (doublereal* t) (integer* ldt) (doublereal* c__) (integer* ldc) (doublereal* work) (integer* ldwork)))

(define-wrapper int dlarfg_ ((integer* n) (doublereal* alpha) (doublereal* x) (integer* incx) (doublereal* tau)))

(define-wrapper int dlarft_ ((char* direct) (char* storev) (integer* n) (integer* k) (doublereal* v) (integer* ldv) (doublereal* tau) (doublereal* t) (integer* ldt)))

(define-wrapper int dlarfx_ ((char* side) (integer* m) (integer* n) (doublereal* v) (doublereal* tau) (doublereal* c__) (integer* ldc) (doublereal* work)))

(define-wrapper int dlargv_ ((integer* n) (doublereal* x) (integer* incx) (doublereal* y) (integer* incy) (doublereal* c__) (integer* incc)))

(define-wrapper int dlarnv_ ((integer* idist) (integer* iseed) (integer* n) (doublereal* x)))

(define-wrapper int dlarrb_ ((integer* n) (doublereal* d__) (doublereal* l) (doublereal* ld) (doublereal* lld) (integer* ifirst) (integer* ilast) (doublereal* sigma) (doublereal* reltol) (doublereal* w) (doublereal* wgap) (doublereal* werr) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dlarre_ ((integer* n) (doublereal* d__) (doublereal* e) (doublereal* tol) (integer* nsplit) (integer* isplit) (integer* m) (doublereal* w) (doublereal* woff) (doublereal* gersch) (doublereal* work) (integer* info)))

(define-wrapper int dlarrf_ ((integer* n) (doublereal* d__) (doublereal* l) (doublereal* ld) (doublereal* lld) (integer* ifirst) (integer* ilast) (doublereal* w) (doublereal* dplus) (doublereal* lplus) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dlarrv_ ((integer* n) (doublereal* d__) (doublereal* l) (integer* isplit) (integer* m) (doublereal* w) (integer* iblock) (doublereal* gersch) (doublereal* tol) (doublereal* z__) (integer* ldz) (integer* isuppz) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dlartg_ ((doublereal* f) (doublereal* g) (doublereal* cs) (doublereal* sn) (doublereal* r__)))

(define-wrapper int dlartv_ ((integer* n) (doublereal* x) (integer* incx) (doublereal* y) (integer* incy) (doublereal* c__) (doublereal* s) (integer* incc)))

(define-wrapper int dlaruv_ ((integer* iseed) (integer* n) (doublereal* x)))

(define-wrapper int dlarz_ ((char* side) (integer* m) (integer* n) (integer* l) (doublereal* v) (integer* incv) (doublereal* tau) (doublereal* c__) (integer* ldc) (doublereal* work)))

(define-wrapper int dlarzb_ ((char* side) (char* trans) (char* direct) (char* storev) (integer* m) (integer* n) (integer* k) (integer* l) (doublereal* v) (integer* ldv) (doublereal* t) (integer* ldt) (doublereal* c__) (integer* ldc) (doublereal* work) (integer* ldwork)))

(define-wrapper int dlarzt_ ((char* direct) (char* storev) (integer* n) (integer* k) (doublereal* v) (integer* ldv) (doublereal* tau) (doublereal* t) (integer* ldt)))

(define-wrapper int dlas2_ ((doublereal* f) (doublereal* g) (doublereal* h__) (doublereal* ssmin) (doublereal* ssmax)))

(define-wrapper int dlascl_ ((char* type__) (integer* kl) (integer* ku) (doublereal* cfrom) (doublereal* cto) (integer* m) (integer* n) (doublereal* a) (integer* lda) (integer* info)))

(define-wrapper int dlasd0_ ((integer* n) (integer* sqre) (doublereal* d__) (doublereal* e) (doublereal* u) (integer* ldu) (doublereal* vt) (integer* ldvt) (integer* smlsiz) (integer* iwork) (doublereal* work) (integer* info)))

(define-wrapper int dlasd1_ ((integer* nl) (integer* nr) (integer* sqre) (doublereal* d__) (doublereal* alpha) (doublereal* beta) (doublereal* u) (integer* ldu) (doublereal* vt) (integer* ldvt) (integer* idxq) (integer* iwork) (doublereal* work) (integer* info)))

(define-wrapper int dlasd2_ ((integer* nl) (integer* nr) (integer* sqre) (integer* k) (doublereal* d__) (doublereal* z__) (doublereal* alpha) (doublereal* beta) (doublereal* u) (integer* ldu) (doublereal* vt) (integer* ldvt) (doublereal* dsigma) (doublereal* u2) (integer* ldu2) (doublereal* vt2) (integer* ldvt2) (integer* idxp) (integer* idx) (integer* idxc) (integer* idxq) (integer* coltyp) (integer* info)))

(define-wrapper int dlasd3_ ((integer* nl) (integer* nr) (integer* sqre) (integer* k) (doublereal* d__) (doublereal* q) (integer* ldq) (doublereal* dsigma) (doublereal* u) (integer* ldu) (doublereal* u2) (integer* ldu2) (doublereal* vt) (integer* ldvt) (doublereal* vt2) (integer* ldvt2) (integer* idxc) (integer* ctot) (doublereal* z__) (integer* info)))

(define-wrapper int dlasd4_ ((integer* n) (integer* i__) (doublereal* d__) (doublereal* z__) (doublereal* delta) (doublereal* rho) (doublereal* sigma) (doublereal* work) (integer* info)))

(define-wrapper int dlasd5_ ((integer* i__) (doublereal* d__) (doublereal* z__) (doublereal* delta) (doublereal* rho) (doublereal* dsigma) (doublereal* work)))

(define-wrapper int dlasd6_ ((integer* icompq) (integer* nl) (integer* nr) (integer* sqre) (doublereal* d__) (doublereal* vf) (doublereal* vl) (doublereal* alpha) (doublereal* beta) (integer* idxq) (integer* perm) (integer* givptr) (integer* givcol) (integer* ldgcol) (doublereal* givnum) (integer* ldgnum) (doublereal* poles) (doublereal* difl) (doublereal* difr) (doublereal* z__) (integer* k) (doublereal* c__) (doublereal* s) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dlasd7_ ((integer* icompq) (integer* nl) (integer* nr) (integer* sqre) (integer* k) (doublereal* d__) (doublereal* z__) (doublereal* zw) (doublereal* vf) (doublereal* vfw) (doublereal* vl) (doublereal* vlw) (doublereal* alpha) (doublereal* beta) (doublereal* dsigma) (integer* idx) (integer* idxp) (integer* idxq) (integer* perm) (integer* givptr) (integer* givcol) (integer* ldgcol) (doublereal* givnum) (integer* ldgnum) (doublereal* c__) (doublereal* s) (integer* info)))

(define-wrapper int dlasd8_ ((integer* icompq) (integer* k) (doublereal* d__) (doublereal* z__) (doublereal* vf) (doublereal* vl) (doublereal* difl) (doublereal* difr) (integer* lddifr) (doublereal* dsigma) (doublereal* work) (integer* info)))

(define-wrapper int dlasd9_ ((integer* icompq) (integer* ldu) (integer* k) (doublereal* d__) (doublereal* z__) (doublereal* vf) (doublereal* vl) (doublereal* difl) (doublereal* difr) (doublereal* dsigma) (doublereal* work) (integer* info)))

(define-wrapper int dlasda_ ((integer* icompq) (integer* smlsiz) (integer* n) (integer* sqre) (doublereal* d__) (doublereal* e) (doublereal* u) (integer* ldu) (doublereal* vt) (integer* k) (doublereal* difl) (doublereal* difr) (doublereal* z__) (doublereal* poles) (integer* givptr) (integer* givcol) (integer* ldgcol) (integer* perm) (doublereal* givnum) (doublereal* c__) (doublereal* s) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dlasdq_ ((char* uplo) (integer* sqre) (integer* n) (integer* ncvt) (integer* nru) (integer* ncc) (doublereal* d__) (doublereal* e) (doublereal* vt) (integer* ldvt) (doublereal* u) (integer* ldu) (doublereal* c__) (integer* ldc) (doublereal* work) (integer* info)))

(define-wrapper int dlasdt_ ((integer* n) (integer* lvl) (integer* nd) (integer* inode) (integer* ndiml) (integer* ndimr) (integer* msub)))

(define-wrapper int dlaset_ ((char* uplo) (integer* m) (integer* n) (doublereal* alpha) (doublereal* beta) (doublereal* a) (integer* lda)))

(define-wrapper int dlasq1_ ((integer* n) (doublereal* d__) (doublereal* e) (doublereal* work) (integer* info)))

(define-wrapper int dlasq2_ ((integer* n) (doublereal* z__) (integer* info)))

(define-wrapper int dlasq3_ ((integer* i0) (integer* n0) (doublereal* z__) (integer* pp) (doublereal* dmin__) (doublereal* sigma) (doublereal* desig) (doublereal* qmax) (integer* nfail) (integer* iter) (integer* ndiv) (logical* ieee)))

(define-wrapper int dlasq4_ ((integer* i0) (integer* n0) (doublereal* z__) (integer* pp) (integer* n0in) (doublereal* dmin__) (doublereal* dmin1) (doublereal* dmin2) (doublereal* dn) (doublereal* dn1) (doublereal* dn2) (doublereal* tau) (integer* ttype)))

(define-wrapper int dlasq5_ ((integer* i0) (integer* n0) (doublereal* z__) (integer* pp) (doublereal* tau) (doublereal* dmin__) (doublereal* dmin1) (doublereal* dmin2) (doublereal* dn) (doublereal* dnm1) (doublereal* dnm2) (logical* ieee)))

(define-wrapper int dlasq6_ ((integer* i0) (integer* n0) (doublereal* z__) (integer* pp) (doublereal* dmin__) (doublereal* dmin1) (doublereal* dmin2) (doublereal* dn) (doublereal* dnm1) (doublereal* dnm2)))

(define-wrapper int dlasr_ ((char* side) (char* pivot) (char* direct) (integer* m) (integer* n) (doublereal* c__) (doublereal* s) (doublereal* a) (integer* lda)))

(define-wrapper int dlasrt_ ((char* id) (integer* n) (doublereal* d__) (integer* info)))

(define-wrapper int dlassq_ ((integer* n) (doublereal* x) (integer* incx) (doublereal* scale) (doublereal* sumsq)))

(define-wrapper int dlasv2_ ((doublereal* f) (doublereal* g) (doublereal* h__) (doublereal* ssmin) (doublereal* ssmax) (doublereal* snr) (doublereal* csr) (doublereal* snl) (doublereal* csl)))

(define-wrapper int dlaswp_ ((integer* n) (doublereal* a) (integer* lda) (integer* k1) (integer* k2) (integer* ipiv) (integer* incx)))

(define-wrapper int dlasy2_ ((logical* ltranl) (logical* ltranr) (integer* isgn) (integer* n1) (integer* n2) (doublereal* tl) (integer* ldtl) (doublereal* tr) (integer* ldtr) (doublereal* b) (integer* ldb) (doublereal* scale) (doublereal* x) (integer* ldx) (doublereal* xnorm) (integer* info)))

(define-wrapper int dlasyf_ ((char* uplo) (integer* n) (integer* nb) (integer* kb) (doublereal* a) (integer* lda) (integer* ipiv) (doublereal* w) (integer* ldw) (integer* info)))

(define-wrapper int dlatbs_ ((char* uplo) (char* trans) (char* diag) (char* normin) (integer* n) (integer* kd) (doublereal* ab) (integer* ldab) (doublereal* x) (doublereal* scale) (doublereal* cnorm) (integer* info)))

(define-wrapper int dlatdf_ ((integer* ijob) (integer* n) (doublereal* z__) (integer* ldz) (doublereal* rhs) (doublereal* rdsum) (doublereal* rdscal) (integer* ipiv) (integer* jpiv)))

(define-wrapper int dlatps_ ((char* uplo) (char* trans) (char* diag) (char* normin) (integer* n) (doublereal* ap) (doublereal* x) (doublereal* scale) (doublereal* cnorm) (integer* info)))

(define-wrapper int dlatrd_ ((char* uplo) (integer* n) (integer* nb) (doublereal* a) (integer* lda) (doublereal* e) (doublereal* tau) (doublereal* w) (integer* ldw)))

(define-wrapper int dlatrs_ ((char* uplo) (char* trans) (char* diag) (char* normin) (integer* n) (doublereal* a) (integer* lda) (doublereal* x) (doublereal* scale) (doublereal* cnorm) (integer* info)))

(define-wrapper int dlatrz_ ((integer* m) (integer* n) (integer* l) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* work)))

(define-wrapper int dlatzm_ ((char* side) (integer* m) (integer* n) (doublereal* v) (integer* incv) (doublereal* tau) (doublereal* c1) (doublereal* c2) (integer* ldc) (doublereal* work)))

(define-wrapper int dlauu2_ ((char* uplo) (integer* n) (doublereal* a) (integer* lda) (integer* info)))

(define-wrapper int dlauum_ ((char* uplo) (integer* n) (doublereal* a) (integer* lda) (integer* info)))

(define-wrapper int dopgtr_ ((char* uplo) (integer* n) (doublereal* ap) (doublereal* tau) (doublereal* q) (integer* ldq) (doublereal* work) (integer* info)))

(define-wrapper int dopmtr_ ((char* side) (char* uplo) (char* trans) (integer* m) (integer* n) (doublereal* ap) (doublereal* tau) (doublereal* c__) (integer* ldc) (doublereal* work) (integer* info)))

(define-wrapper int dorg2l_ ((integer* m) (integer* n) (integer* k) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* work) (integer* info)))

(define-wrapper int dorg2r_ ((integer* m) (integer* n) (integer* k) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* work) (integer* info)))

(define-wrapper int dorgbr_ ((char* vect) (integer* m) (integer* n) (integer* k) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dorghr_ ((integer* n) (integer* ilo) (integer* ihi) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dorgl2_ ((integer* m) (integer* n) (integer* k) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* work) (integer* info)))

(define-wrapper int dorglq_ ((integer* m) (integer* n) (integer* k) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dorgql_ ((integer* m) (integer* n) (integer* k) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dorgqr_ ((integer* m) (integer* n) (integer* k) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dorgr2_ ((integer* m) (integer* n) (integer* k) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* work) (integer* info)))

(define-wrapper int dorgrq_ ((integer* m) (integer* n) (integer* k) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dorgtr_ ((char* uplo) (integer* n) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dorm2l_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* c__) (integer* ldc) (doublereal* work) (integer* info)))

(define-wrapper int dorm2r_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* c__) (integer* ldc) (doublereal* work) (integer* info)))

(define-wrapper int dormbr_ ((char* vect) (char* side) (char* trans) (integer* m) (integer* n) (integer* k) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* c__) (integer* ldc) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dormhr_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* ilo) (integer* ihi) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* c__) (integer* ldc) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dorml2_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* c__) (integer* ldc) (doublereal* work) (integer* info)))

(define-wrapper int dormlq_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* c__) (integer* ldc) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dormql_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* c__) (integer* ldc) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dormqr_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* c__) (integer* ldc) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dormr2_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* c__) (integer* ldc) (doublereal* work) (integer* info)))

(define-wrapper int dormr3_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (integer* l) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* c__) (integer* ldc) (doublereal* work) (integer* info)))

(define-wrapper int dormrq_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* c__) (integer* ldc) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dormrz_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (integer* l) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* c__) (integer* ldc) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dormtr_ ((char* side) (char* uplo) (char* trans) (integer* m) (integer* n) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* c__) (integer* ldc) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dpbcon_ ((char* uplo) (integer* n) (integer* kd) (doublereal* ab) (integer* ldab) (doublereal* anorm) (doublereal* rcond) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dpbequ_ ((char* uplo) (integer* n) (integer* kd) (doublereal* ab) (integer* ldab) (doublereal* s) (doublereal* scond) (doublereal* amax) (integer* info)))

(define-wrapper int dpbrfs_ ((char* uplo) (integer* n) (integer* kd) (integer* nrhs) (doublereal* ab) (integer* ldab) (doublereal* afb) (integer* ldafb) (doublereal* b) (integer* ldb) (doublereal* x) (integer* ldx) (doublereal* ferr) (doublereal* berr) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dpbstf_ ((char* uplo) (integer* n) (integer* kd) (doublereal* ab) (integer* ldab) (integer* info)))

(define-wrapper int dpbsv_ ((char* uplo) (integer* n) (integer* kd) (integer* nrhs) (doublereal* ab) (integer* ldab) (doublereal* b) (integer* ldb) (integer* info)))

(define-wrapper int dpbsvx_ ((char* fact) (char* uplo) (integer* n) (integer* kd) (integer* nrhs) (doublereal* ab) (integer* ldab) (doublereal* afb) (integer* ldafb) (char* equed) (doublereal* s) (doublereal* b) (integer* ldb) (doublereal* x) (integer* ldx) (doublereal* rcond) (doublereal* ferr) (doublereal* berr) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dpbtf2_ ((char* uplo) (integer* n) (integer* kd) (doublereal* ab) (integer* ldab) (integer* info)))

(define-wrapper int dpbtrf_ ((char* uplo) (integer* n) (integer* kd) (doublereal* ab) (integer* ldab) (integer* info)))

(define-wrapper int dpbtrs_ ((char* uplo) (integer* n) (integer* kd) (integer* nrhs) (doublereal* ab) (integer* ldab) (doublereal* b) (integer* ldb) (integer* info)))

(define-wrapper int dpocon_ ((char* uplo) (integer* n) (doublereal* a) (integer* lda) (doublereal* anorm) (doublereal* rcond) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dpoequ_ ((integer* n) (doublereal* a) (integer* lda) (doublereal* s) (doublereal* scond) (doublereal* amax) (integer* info)))

(define-wrapper int dporfs_ ((char* uplo) (integer* n) (integer* nrhs) (doublereal* a) (integer* lda) (doublereal* af) (integer* ldaf) (doublereal* b) (integer* ldb) (doublereal* x) (integer* ldx) (doublereal* ferr) (doublereal* berr) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dposv_ ((char* uplo) (integer* n) (integer* nrhs) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (integer* info)))

(define-wrapper int dposvx_ ((char* fact) (char* uplo) (integer* n) (integer* nrhs) (doublereal* a) (integer* lda) (doublereal* af) (integer* ldaf) (char* equed) (doublereal* s) (doublereal* b) (integer* ldb) (doublereal* x) (integer* ldx) (doublereal* rcond) (doublereal* ferr) (doublereal* berr) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dpotf2_ ((char* uplo) (integer* n) (doublereal* a) (integer* lda) (integer* info)))

(define-wrapper int dpotrf_ ((char* uplo) (integer* n) (doublereal* a) (integer* lda) (integer* info)))

(define-wrapper int dpotri_ ((char* uplo) (integer* n) (doublereal* a) (integer* lda) (integer* info)))

(define-wrapper int dpotrs_ ((char* uplo) (integer* n) (integer* nrhs) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (integer* info)))

(define-wrapper int dppcon_ ((char* uplo) (integer* n) (doublereal* ap) (doublereal* anorm) (doublereal* rcond) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dppequ_ ((char* uplo) (integer* n) (doublereal* ap) (doublereal* s) (doublereal* scond) (doublereal* amax) (integer* info)))

(define-wrapper int dpprfs_ ((char* uplo) (integer* n) (integer* nrhs) (doublereal* ap) (doublereal* afp) (doublereal* b) (integer* ldb) (doublereal* x) (integer* ldx) (doublereal* ferr) (doublereal* berr) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dppsv_ ((char* uplo) (integer* n) (integer* nrhs) (doublereal* ap) (doublereal* b) (integer* ldb) (integer* info)))

(define-wrapper int dppsvx_ ((char* fact) (char* uplo) (integer* n) (integer* nrhs) (doublereal* ap) (doublereal* afp) (char* equed) (doublereal* s) (doublereal* b) (integer* ldb) (doublereal* x) (integer* ldx) (doublereal* rcond) (doublereal* ferr) (doublereal* berr) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dpptrf_ ((char* uplo) (integer* n) (doublereal* ap) (integer* info)))

(define-wrapper int dpptri_ ((char* uplo) (integer* n) (doublereal* ap) (integer* info)))

(define-wrapper int dpptrs_ ((char* uplo) (integer* n) (integer* nrhs) (doublereal* ap) (doublereal* b) (integer* ldb) (integer* info)))

(define-wrapper int dptcon_ ((integer* n) (doublereal* d__) (doublereal* e) (doublereal* anorm) (doublereal* rcond) (doublereal* work) (integer* info)))

(define-wrapper int dpteqr_ ((char* compz) (integer* n) (doublereal* d__) (doublereal* e) (doublereal* z__) (integer* ldz) (doublereal* work) (integer* info)))

(define-wrapper int dptrfs_ ((integer* n) (integer* nrhs) (doublereal* d__) (doublereal* e) (doublereal* df) (doublereal* ef) (doublereal* b) (integer* ldb) (doublereal* x) (integer* ldx) (doublereal* ferr) (doublereal* berr) (doublereal* work) (integer* info)))

(define-wrapper int dptsv_ ((integer* n) (integer* nrhs) (doublereal* d__) (doublereal* e) (doublereal* b) (integer* ldb) (integer* info)))

(define-wrapper int dptsvx_ ((char* fact) (integer* n) (integer* nrhs) (doublereal* d__) (doublereal* e) (doublereal* df) (doublereal* ef) (doublereal* b) (integer* ldb) (doublereal* x) (integer* ldx) (doublereal* rcond) (doublereal* ferr) (doublereal* berr) (doublereal* work) (integer* info)))

(define-wrapper int dpttrf_ ((integer* n) (doublereal* d__) (doublereal* e) (integer* info)))

(define-wrapper int dpttrs_ ((integer* n) (integer* nrhs) (doublereal* d__) (doublereal* e) (doublereal* b) (integer* ldb) (integer* info)))

(define-wrapper int dptts2_ ((integer* n) (integer* nrhs) (doublereal* d__) (doublereal* e) (doublereal* b) (integer* ldb)))

(define-wrapper int drscl_ ((integer* n) (doublereal* sa) (doublereal* sx) (integer* incx)))

(define-wrapper int dsbev_ ((char* jobz) (char* uplo) (integer* n) (integer* kd) (doublereal* ab) (integer* ldab) (doublereal* w) (doublereal* z__) (integer* ldz) (doublereal* work) (integer* info)))

(define-wrapper int dsbevd_ ((char* jobz) (char* uplo) (integer* n) (integer* kd) (doublereal* ab) (integer* ldab) (doublereal* w) (doublereal* z__) (integer* ldz) (doublereal* work) (integer* lwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int dsbevx_ ((char* jobz) (char* range) (char* uplo) (integer* n) (integer* kd) (doublereal* ab) (integer* ldab) (doublereal* q) (integer* ldq) (doublereal* vl) (doublereal* vu) (integer* il) (integer* iu) (doublereal* abstol) (integer* m) (doublereal* w) (doublereal* z__) (integer* ldz) (doublereal* work) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int dsbgst_ ((char* vect) (char* uplo) (integer* n) (integer* ka) (integer* kb) (doublereal* ab) (integer* ldab) (doublereal* bb) (integer* ldbb) (doublereal* x) (integer* ldx) (doublereal* work) (integer* info)))

(define-wrapper int dsbgv_ ((char* jobz) (char* uplo) (integer* n) (integer* ka) (integer* kb) (doublereal* ab) (integer* ldab) (doublereal* bb) (integer* ldbb) (doublereal* w) (doublereal* z__) (integer* ldz) (doublereal* work) (integer* info)))

(define-wrapper int dsbgvd_ ((char* jobz) (char* uplo) (integer* n) (integer* ka) (integer* kb) (doublereal* ab) (integer* ldab) (doublereal* bb) (integer* ldbb) (doublereal* w) (doublereal* z__) (integer* ldz) (doublereal* work) (integer* lwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int dsbgvx_ ((char* jobz) (char* range) (char* uplo) (integer* n) (integer* ka) (integer* kb) (doublereal* ab) (integer* ldab) (doublereal* bb) (integer* ldbb) (doublereal* q) (integer* ldq) (doublereal* vl) (doublereal* vu) (integer* il) (integer* iu) (doublereal* abstol) (integer* m) (doublereal* w) (doublereal* z__) (integer* ldz) (doublereal* work) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int dsbtrd_ ((char* vect) (char* uplo) (integer* n) (integer* kd) (doublereal* ab) (integer* ldab) (doublereal* d__) (doublereal* e) (doublereal* q) (integer* ldq) (doublereal* work) (integer* info)))

(define-wrapper int dspcon_ ((char* uplo) (integer* n) (doublereal* ap) (integer* ipiv) (doublereal* anorm) (doublereal* rcond) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dspev_ ((char* jobz) (char* uplo) (integer* n) (doublereal* ap) (doublereal* w) (doublereal* z__) (integer* ldz) (doublereal* work) (integer* info)))

(define-wrapper int dspevd_ ((char* jobz) (char* uplo) (integer* n) (doublereal* ap) (doublereal* w) (doublereal* z__) (integer* ldz) (doublereal* work) (integer* lwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int dspevx_ ((char* jobz) (char* range) (char* uplo) (integer* n) (doublereal* ap) (doublereal* vl) (doublereal* vu) (integer* il) (integer* iu) (doublereal* abstol) (integer* m) (doublereal* w) (doublereal* z__) (integer* ldz) (doublereal* work) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int dspgst_ ((integer* itype) (char* uplo) (integer* n) (doublereal* ap) (doublereal* bp) (integer* info)))

(define-wrapper int dspgv_ ((integer* itype) (char* jobz) (char* uplo) (integer* n) (doublereal* ap) (doublereal* bp) (doublereal* w) (doublereal* z__) (integer* ldz) (doublereal* work) (integer* info)))

(define-wrapper int dspgvd_ ((integer* itype) (char* jobz) (char* uplo) (integer* n) (doublereal* ap) (doublereal* bp) (doublereal* w) (doublereal* z__) (integer* ldz) (doublereal* work) (integer* lwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int dspgvx_ ((integer* itype) (char* jobz) (char* range) (char* uplo) (integer* n) (doublereal* ap) (doublereal* bp) (doublereal* vl) (doublereal* vu) (integer* il) (integer* iu) (doublereal* abstol) (integer* m) (doublereal* w) (doublereal* z__) (integer* ldz) (doublereal* work) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int dsprfs_ ((char* uplo) (integer* n) (integer* nrhs) (doublereal* ap) (doublereal* afp) (integer* ipiv) (doublereal* b) (integer* ldb) (doublereal* x) (integer* ldx) (doublereal* ferr) (doublereal* berr) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dspsv_ ((char* uplo) (integer* n) (integer* nrhs) (doublereal* ap) (integer* ipiv) (doublereal* b) (integer* ldb) (integer* info)))

(define-wrapper int dspsvx_ ((char* fact) (char* uplo) (integer* n) (integer* nrhs) (doublereal* ap) (doublereal* afp) (integer* ipiv) (doublereal* b) (integer* ldb) (doublereal* x) (integer* ldx) (doublereal* rcond) (doublereal* ferr) (doublereal* berr) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dsptrd_ ((char* uplo) (integer* n) (doublereal* ap) (doublereal* d__) (doublereal* e) (doublereal* tau) (integer* info)))

(define-wrapper int dsptrf_ ((char* uplo) (integer* n) (doublereal* ap) (integer* ipiv) (integer* info)))

(define-wrapper int dsptri_ ((char* uplo) (integer* n) (doublereal* ap) (integer* ipiv) (doublereal* work) (integer* info)))

(define-wrapper int dsptrs_ ((char* uplo) (integer* n) (integer* nrhs) (doublereal* ap) (integer* ipiv) (doublereal* b) (integer* ldb) (integer* info)))

(define-wrapper int dstebz_ ((char* range) (char* order) (integer* n) (doublereal* vl) (doublereal* vu) (integer* il) (integer* iu) (doublereal* abstol) (doublereal* d__) (doublereal* e) (integer* m) (integer* nsplit) (doublereal* w) (integer* iblock) (integer* isplit) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dstedc_ ((char* compz) (integer* n) (doublereal* d__) (doublereal* e) (doublereal* z__) (integer* ldz) (doublereal* work) (integer* lwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int dstegr_ ((char* jobz) (char* range) (integer* n) (doublereal* d__) (doublereal* e) (doublereal* vl) (doublereal* vu) (integer* il) (integer* iu) (doublereal* abstol) (integer* m) (doublereal* w) (doublereal* z__) (integer* ldz) (integer* isuppz) (doublereal* work) (integer* lwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int dstein_ ((integer* n) (doublereal* d__) (doublereal* e) (integer* m) (doublereal* w) (integer* iblock) (integer* isplit) (doublereal* z__) (integer* ldz) (doublereal* work) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int dsteqr_ ((char* compz) (integer* n) (doublereal* d__) (doublereal* e) (doublereal* z__) (integer* ldz) (doublereal* work) (integer* info)))

(define-wrapper int dsterf_ ((integer* n) (doublereal* d__) (doublereal* e) (integer* info)))

(define-wrapper int dstev_ ((char* jobz) (integer* n) (doublereal* d__) (doublereal* e) (doublereal* z__) (integer* ldz) (doublereal* work) (integer* info)))

(define-wrapper int dstevd_ ((char* jobz) (integer* n) (doublereal* d__) (doublereal* e) (doublereal* z__) (integer* ldz) (doublereal* work) (integer* lwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int dstevr_ ((char* jobz) (char* range) (integer* n) (doublereal* d__) (doublereal* e) (doublereal* vl) (doublereal* vu) (integer* il) (integer* iu) (doublereal* abstol) (integer* m) (doublereal* w) (doublereal* z__) (integer* ldz) (integer* isuppz) (doublereal* work) (integer* lwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int dstevx_ ((char* jobz) (char* range) (integer* n) (doublereal* d__) (doublereal* e) (doublereal* vl) (doublereal* vu) (integer* il) (integer* iu) (doublereal* abstol) (integer* m) (doublereal* w) (doublereal* z__) (integer* ldz) (doublereal* work) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int dsycon_ ((char* uplo) (integer* n) (doublereal* a) (integer* lda) (integer* ipiv) (doublereal* anorm) (doublereal* rcond) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dsyev_ ((char* jobz) (char* uplo) (integer* n) (doublereal* a) (integer* lda) (doublereal* w) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dsyevd_ ((char* jobz) (char* uplo) (integer* n) (doublereal* a) (integer* lda) (doublereal* w) (doublereal* work) (integer* lwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int dsyevr_ ((char* jobz) (char* range) (char* uplo) (integer* n) (doublereal* a) (integer* lda) (doublereal* vl) (doublereal* vu) (integer* il) (integer* iu) (doublereal* abstol) (integer* m) (doublereal* w) (doublereal* z__) (integer* ldz) (integer* isuppz) (doublereal* work) (integer* lwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int dsyevx_ ((char* jobz) (char* range) (char* uplo) (integer* n) (doublereal* a) (integer* lda) (doublereal* vl) (doublereal* vu) (integer* il) (integer* iu) (doublereal* abstol) (integer* m) (doublereal* w) (doublereal* z__) (integer* ldz) (doublereal* work) (integer* lwork) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int dsygs2_ ((integer* itype) (char* uplo) (integer* n) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (integer* info)))

(define-wrapper int dsygst_ ((integer* itype) (char* uplo) (integer* n) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (integer* info)))

(define-wrapper int dsygv_ ((integer* itype) (char* jobz) (char* uplo) (integer* n) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (doublereal* w) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dsygvd_ ((integer* itype) (char* jobz) (char* uplo) (integer* n) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (doublereal* w) (doublereal* work) (integer* lwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int dsygvx_ ((integer* itype) (char* jobz) (char* range) (char* uplo) (integer* n) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (doublereal* vl) (doublereal* vu) (integer* il) (integer* iu) (doublereal* abstol) (integer* m) (doublereal* w) (doublereal* z__) (integer* ldz) (doublereal* work) (integer* lwork) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int dsyrfs_ ((char* uplo) (integer* n) (integer* nrhs) (doublereal* a) (integer* lda) (doublereal* af) (integer* ldaf) (integer* ipiv) (doublereal* b) (integer* ldb) (doublereal* x) (integer* ldx) (doublereal* ferr) (doublereal* berr) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dsysv_ ((char* uplo) (integer* n) (integer* nrhs) (doublereal* a) (integer* lda) (integer* ipiv) (doublereal* b) (integer* ldb) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dsysvx_ ((char* fact) (char* uplo) (integer* n) (integer* nrhs) (doublereal* a) (integer* lda) (doublereal* af) (integer* ldaf) (integer* ipiv) (doublereal* b) (integer* ldb) (doublereal* x) (integer* ldx) (doublereal* rcond) (doublereal* ferr) (doublereal* berr) (doublereal* work) (integer* lwork) (integer* iwork) (integer* info)))

(define-wrapper int dsytd2_ ((char* uplo) (integer* n) (doublereal* a) (integer* lda) (doublereal* d__) (doublereal* e) (doublereal* tau) (integer* info)))

(define-wrapper int dsytf2_ ((char* uplo) (integer* n) (doublereal* a) (integer* lda) (integer* ipiv) (integer* info)))

(define-wrapper int dsytrd_ ((char* uplo) (integer* n) (doublereal* a) (integer* lda) (doublereal* d__) (doublereal* e) (doublereal* tau) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dsytrf_ ((char* uplo) (integer* n) (doublereal* a) (integer* lda) (integer* ipiv) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dsytri_ ((char* uplo) (integer* n) (doublereal* a) (integer* lda) (integer* ipiv) (doublereal* work) (integer* info)))

(define-wrapper int dsytrs_ ((char* uplo) (integer* n) (integer* nrhs) (doublereal* a) (integer* lda) (integer* ipiv) (doublereal* b) (integer* ldb) (integer* info)))

(define-wrapper int dtbcon_ ((char* norm) (char* uplo) (char* diag) (integer* n) (integer* kd) (doublereal* ab) (integer* ldab) (doublereal* rcond) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dtbrfs_ ((char* uplo) (char* trans) (char* diag) (integer* n) (integer* kd) (integer* nrhs) (doublereal* ab) (integer* ldab) (doublereal* b) (integer* ldb) (doublereal* x) (integer* ldx) (doublereal* ferr) (doublereal* berr) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dtbtrs_ ((char* uplo) (char* trans) (char* diag) (integer* n) (integer* kd) (integer* nrhs) (doublereal* ab) (integer* ldab) (doublereal* b) (integer* ldb) (integer* info)))

(define-wrapper int dtgevc_ ((char* side) (char* howmny) (logical* select) (integer* n) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (doublereal* vl) (integer* ldvl) (doublereal* vr) (integer* ldvr) (integer* mm) (integer* m) (doublereal* work) (integer* info)))

(define-wrapper int dtgex2_ ((logical* wantq) (logical* wantz) (integer* n) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (doublereal* q) (integer* ldq) (doublereal* z__) (integer* ldz) (integer* j1) (integer* n1) (integer* n2) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dtgexc_ ((logical* wantq) (logical* wantz) (integer* n) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (doublereal* q) (integer* ldq) (doublereal* z__) (integer* ldz) (integer* ifst) (integer* ilst) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper int dtgsen_ ((integer* ijob) (logical* wantq) (logical* wantz) (logical* select) (integer* n) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (doublereal* alphar) (doublereal* alphai) (doublereal* beta) (doublereal* q) (integer* ldq) (doublereal* z__) (integer* ldz) (integer* m) (doublereal* pl) (doublereal* pr) (doublereal* dif) (doublereal* work) (integer* lwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int dtgsja_ ((char* jobu) (char* jobv) (char* jobq) (integer* m) (integer* p) (integer* n) (integer* k) (integer* l) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (doublereal* tola) (doublereal* tolb) (doublereal* alpha) (doublereal* beta) (doublereal* u) (integer* ldu) (doublereal* v) (integer* ldv) (doublereal* q) (integer* ldq) (doublereal* work) (integer* ncycle) (integer* info)))

(define-wrapper int dtgsna_ ((char* job) (char* howmny) (logical* select) (integer* n) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (doublereal* vl) (integer* ldvl) (doublereal* vr) (integer* ldvr) (doublereal* s) (doublereal* dif) (integer* mm) (integer* m) (doublereal* work) (integer* lwork) (integer* iwork) (integer* info)))

(define-wrapper int dtgsy2_ ((char* trans) (integer* ijob) (integer* m) (integer* n) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (doublereal* c__) (integer* ldc) (doublereal* d__) (integer* ldd) (doublereal* e) (integer* lde) (doublereal* f) (integer* ldf) (doublereal* scale) (doublereal* rdsum) (doublereal* rdscal) (integer* iwork) (integer* pq) (integer* info)))

(define-wrapper int dtgsyl_ ((char* trans) (integer* ijob) (integer* m) (integer* n) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (doublereal* c__) (integer* ldc) (doublereal* d__) (integer* ldd) (doublereal* e) (integer* lde) (doublereal* f) (integer* ldf) (doublereal* scale) (doublereal* dif) (doublereal* work) (integer* lwork) (integer* iwork) (integer* info)))

(define-wrapper int dtpcon_ ((char* norm) (char* uplo) (char* diag) (integer* n) (doublereal* ap) (doublereal* rcond) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dtprfs_ ((char* uplo) (char* trans) (char* diag) (integer* n) (integer* nrhs) (doublereal* ap) (doublereal* b) (integer* ldb) (doublereal* x) (integer* ldx) (doublereal* ferr) (doublereal* berr) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dtptri_ ((char* uplo) (char* diag) (integer* n) (doublereal* ap) (integer* info)))

(define-wrapper int dtptrs_ ((char* uplo) (char* trans) (char* diag) (integer* n) (integer* nrhs) (doublereal* ap) (doublereal* b) (integer* ldb) (integer* info)))

(define-wrapper int dtrcon_ ((char* norm) (char* uplo) (char* diag) (integer* n) (doublereal* a) (integer* lda) (doublereal* rcond) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dtrevc_ ((char* side) (char* howmny) (logical* select) (integer* n) (doublereal* t) (integer* ldt) (doublereal* vl) (integer* ldvl) (doublereal* vr) (integer* ldvr) (integer* mm) (integer* m) (doublereal* work) (integer* info)))

(define-wrapper int dtrexc_ ((char* compq) (integer* n) (doublereal* t) (integer* ldt) (doublereal* q) (integer* ldq) (integer* ifst) (integer* ilst) (doublereal* work) (integer* info)))

(define-wrapper int dtrrfs_ ((char* uplo) (char* trans) (char* diag) (integer* n) (integer* nrhs) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (doublereal* x) (integer* ldx) (doublereal* ferr) (doublereal* berr) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int dtrsen_ ((char* job) (char* compq) (logical* select) (integer* n) (doublereal* t) (integer* ldt) (doublereal* q) (integer* ldq) (doublereal* wr) (doublereal* wi) (integer* m) (doublereal* s) (doublereal* sep) (doublereal* work) (integer* lwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int dtrsna_ ((char* job) (char* howmny) (logical* select) (integer* n) (doublereal* t) (integer* ldt) (doublereal* vl) (integer* ldvl) (doublereal* vr) (integer* ldvr) (doublereal* s) (doublereal* sep) (integer* mm) (integer* m) (doublereal* work) (integer* ldwork) (integer* iwork) (integer* info)))

(define-wrapper int dtrsyl_ ((char* trana) (char* tranb) (integer* isgn) (integer* m) (integer* n) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (doublereal* c__) (integer* ldc) (doublereal* scale) (integer* info)))

(define-wrapper int dtrti2_ ((char* uplo) (char* diag) (integer* n) (doublereal* a) (integer* lda) (integer* info)))

(define-wrapper int dtrtri_ ((char* uplo) (char* diag) (integer* n) (doublereal* a) (integer* lda) (integer* info)))

(define-wrapper int dtrtrs_ ((char* uplo) (char* trans) (char* diag) (integer* n) (integer* nrhs) (doublereal* a) (integer* lda) (doublereal* b) (integer* ldb) (integer* info)))

(define-wrapper int dtzrqf_ ((integer* m) (integer* n) (doublereal* a) (integer* lda) (doublereal* tau) (integer* info)))

(define-wrapper int dtzrzf_ ((integer* m) (integer* n) (doublereal* a) (integer* lda) (doublereal* tau) (doublereal* work) (integer* lwork) (integer* info)))

(define-wrapper integer icmax1_ ((integer* n) (complex* cx) (integer* incx)))

(define-wrapper integer ieeeck_ ((integer* ispec) (real* zero) (real* one)))

(define-wrapper integer ilaenv_ ((integer* ispec) (char* name__) (char* opts) (integer* n1) (integer* n2) (integer* n3) (integer* n4) (ftnlen name_len) (ftnlen opts_len)))

(define-wrapper integer izmax1_ ((integer* n) (doublecomplex* cx) (integer* incx)))

(define-wrapper int sbdsdc_ ((char* uplo) (char* compq) (integer* n) (real* d__) (real* e) (real* u) (integer* ldu) (real* vt) (integer* ldvt) (real* q) (integer* iq) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int sbdsqr_ ((char* uplo) (integer* n) (integer* ncvt) (integer* nru) (integer* ncc) (real* d__) (real* e) (real* vt) (integer* ldvt) (real* u) (integer* ldu) (real* c__) (integer* ldc) (real* work) (integer* info)))

(define-wrapper int sdisna_ ((char* job) (integer* m) (integer* n) (real* d__) (real* sep) (integer* info)))

(define-wrapper int sgbbrd_ ((char* vect) (integer* m) (integer* n) (integer* ncc) (integer* kl) (integer* ku) (real* ab) (integer* ldab) (real* d__) (real* e) (real* q) (integer* ldq) (real* pt) (integer* ldpt) (real* c__) (integer* ldc) (real* work) (integer* info)))

(define-wrapper int sgbcon_ ((char* norm) (integer* n) (integer* kl) (integer* ku) (real* ab) (integer* ldab) (integer* ipiv) (real* anorm) (real* rcond) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int sgbequ_ ((integer* m) (integer* n) (integer* kl) (integer* ku) (real* ab) (integer* ldab) (real* r__) (real* c__) (real* rowcnd) (real* colcnd) (real* amax) (integer* info)))

(define-wrapper int sgbrfs_ ((char* trans) (integer* n) (integer* kl) (integer* ku) (integer* nrhs) (real* ab) (integer* ldab) (real* afb) (integer* ldafb) (integer* ipiv) (real* b) (integer* ldb) (real* x) (integer* ldx) (real* ferr) (real* berr) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int sgbsv_ ((integer* n) (integer* kl) (integer* ku) (integer* nrhs) (real* ab) (integer* ldab) (integer* ipiv) (real* b) (integer* ldb) (integer* info)))

(define-wrapper int sgbsvx_ ((char* fact) (char* trans) (integer* n) (integer* kl) (integer* ku) (integer* nrhs) (real* ab) (integer* ldab) (real* afb) (integer* ldafb) (integer* ipiv) (char* equed) (real* r__) (real* c__) (real* b) (integer* ldb) (real* x) (integer* ldx) (real* rcond) (real* ferr) (real* berr) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int sgbtf2_ ((integer* m) (integer* n) (integer* kl) (integer* ku) (real* ab) (integer* ldab) (integer* ipiv) (integer* info)))

(define-wrapper int sgbtrf_ ((integer* m) (integer* n) (integer* kl) (integer* ku) (real* ab) (integer* ldab) (integer* ipiv) (integer* info)))

(define-wrapper int sgbtrs_ ((char* trans) (integer* n) (integer* kl) (integer* ku) (integer* nrhs) (real* ab) (integer* ldab) (integer* ipiv) (real* b) (integer* ldb) (integer* info)))

(define-wrapper int sgebak_ ((char* job) (char* side) (integer* n) (integer* ilo) (integer* ihi) (real* scale) (integer* m) (real* v) (integer* ldv) (integer* info)))

(define-wrapper int sgebal_ ((char* job) (integer* n) (real* a) (integer* lda) (integer* ilo) (integer* ihi) (real* scale) (integer* info)))

(define-wrapper int sgebd2_ ((integer* m) (integer* n) (real* a) (integer* lda) (real* d__) (real* e) (real* tauq) (real* taup) (real* work) (integer* info)))

(define-wrapper int sgebrd_ ((integer* m) (integer* n) (real* a) (integer* lda) (real* d__) (real* e) (real* tauq) (real* taup) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sgecon_ ((char* norm) (integer* n) (real* a) (integer* lda) (real* anorm) (real* rcond) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int sgeequ_ ((integer* m) (integer* n) (real* a) (integer* lda) (real* r__) (real* c__) (real* rowcnd) (real* colcnd) (real* amax) (integer* info)))

(define-wrapper int sgees_ ((char* jobvs) (char* sort) (L_fp select) (integer* n) (real* a) (integer* lda) (integer* sdim) (real* wr) (real* wi) (real* vs) (integer* ldvs) (real* work) (integer* lwork) (logical* bwork) (integer* info)))

(define-wrapper int sgeesx_ ((char* jobvs) (char* sort) (L_fp select) (char* sense) (integer* n) (real* a) (integer* lda) (integer* sdim) (real* wr) (real* wi) (real* vs) (integer* ldvs) (real* rconde) (real* rcondv) (real* work) (integer* lwork) (integer* iwork) (integer* liwork) (logical* bwork) (integer* info)))

(define-wrapper int sgeev_ ((char* jobvl) (char* jobvr) (integer* n) (real* a) (integer* lda) (real* wr) (real* wi) (real* vl) (integer* ldvl) (real* vr) (integer* ldvr) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sgeevx_ ((char* balanc) (char* jobvl) (char* jobvr) (char* sense) (integer* n) (real* a) (integer* lda) (real* wr) (real* wi) (real* vl) (integer* ldvl) (real* vr) (integer* ldvr) (integer* ilo) (integer* ihi) (real* scale) (real* abnrm) (real* rconde) (real* rcondv) (real* work) (integer* lwork) (integer* iwork) (integer* info)))

(define-wrapper int sgegs_ ((char* jobvsl) (char* jobvsr) (integer* n) (real* a) (integer* lda) (real* b) (integer* ldb) (real* alphar) (real* alphai) (real* beta) (real* vsl) (integer* ldvsl) (real* vsr) (integer* ldvsr) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sgegv_ ((char* jobvl) (char* jobvr) (integer* n) (real* a) (integer* lda) (real* b) (integer* ldb) (real* alphar) (real* alphai) (real* beta) (real* vl) (integer* ldvl) (real* vr) (integer* ldvr) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sgehd2_ ((integer* n) (integer* ilo) (integer* ihi) (real* a) (integer* lda) (real* tau) (real* work) (integer* info)))

(define-wrapper int sgehrd_ ((integer* n) (integer* ilo) (integer* ihi) (real* a) (integer* lda) (real* tau) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sgelq2_ ((integer* m) (integer* n) (real* a) (integer* lda) (real* tau) (real* work) (integer* info)))

(define-wrapper int sgelqf_ ((integer* m) (integer* n) (real* a) (integer* lda) (real* tau) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sgels_ ((char* trans) (integer* m) (integer* n) (integer* nrhs) (real* a) (integer* lda) (real* b) (integer* ldb) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sgelsd_ ((integer* m) (integer* n) (integer* nrhs) (real* a) (integer* lda) (real* b) (integer* ldb) (real* s) (real* rcond) (integer* rank) (real* work) (integer* lwork) (integer* iwork) (integer* info)))

(define-wrapper int sgelss_ ((integer* m) (integer* n) (integer* nrhs) (real* a) (integer* lda) (real* b) (integer* ldb) (real* s) (real* rcond) (integer* rank) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sgelsx_ ((integer* m) (integer* n) (integer* nrhs) (real* a) (integer* lda) (real* b) (integer* ldb) (integer* jpvt) (real* rcond) (integer* rank) (real* work) (integer* info)))

(define-wrapper int sgelsy_ ((integer* m) (integer* n) (integer* nrhs) (real* a) (integer* lda) (real* b) (integer* ldb) (integer* jpvt) (real* rcond) (integer* rank) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sgeql2_ ((integer* m) (integer* n) (real* a) (integer* lda) (real* tau) (real* work) (integer* info)))

(define-wrapper int sgeqlf_ ((integer* m) (integer* n) (real* a) (integer* lda) (real* tau) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sgeqp3_ ((integer* m) (integer* n) (real* a) (integer* lda) (integer* jpvt) (real* tau) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sgeqpf_ ((integer* m) (integer* n) (real* a) (integer* lda) (integer* jpvt) (real* tau) (real* work) (integer* info)))

(define-wrapper int sgeqr2_ ((integer* m) (integer* n) (real* a) (integer* lda) (real* tau) (real* work) (integer* info)))

(define-wrapper int sgeqrf_ ((integer* m) (integer* n) (real* a) (integer* lda) (real* tau) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sgerfs_ ((char* trans) (integer* n) (integer* nrhs) (real* a) (integer* lda) (real* af) (integer* ldaf) (integer* ipiv) (real* b) (integer* ldb) (real* x) (integer* ldx) (real* ferr) (real* berr) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int sgerq2_ ((integer* m) (integer* n) (real* a) (integer* lda) (real* tau) (real* work) (integer* info)))

(define-wrapper int sgerqf_ ((integer* m) (integer* n) (real* a) (integer* lda) (real* tau) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sgesc2_ ((integer* n) (real* a) (integer* lda) (real* rhs) (integer* ipiv) (integer* jpiv) (real* scale)))

(define-wrapper int sgesdd_ ((char* jobz) (integer* m) (integer* n) (real* a) (integer* lda) (real* s) (real* u) (integer* ldu) (real* vt) (integer* ldvt) (real* work) (integer* lwork) (integer* iwork) (integer* info)))

(define-wrapper int sgesv_ ((integer* n) (integer* nrhs) (real* a) (integer* lda) (integer* ipiv) (real* b) (integer* ldb) (integer* info)))

(define-wrapper int sgesvd_ ((char* jobu) (char* jobvt) (integer* m) (integer* n) (real* a) (integer* lda) (real* s) (real* u) (integer* ldu) (real* vt) (integer* ldvt) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sgesvx_ ((char* fact) (char* trans) (integer* n) (integer* nrhs) (real* a) (integer* lda) (real* af) (integer* ldaf) (integer* ipiv) (char* equed) (real* r__) (real* c__) (real* b) (integer* ldb) (real* x) (integer* ldx) (real* rcond) (real* ferr) (real* berr) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int sgetc2_ ((integer* n) (real* a) (integer* lda) (integer* ipiv) (integer* jpiv) (integer* info)))

(define-wrapper int sgetf2_ ((integer* m) (integer* n) (real* a) (integer* lda) (integer* ipiv) (integer* info)))

(define-wrapper int sgetrf_ ((integer* m) (integer* n) (real* a) (integer* lda) (integer* ipiv) (integer* info)))

(define-wrapper int sgetri_ ((integer* n) (real* a) (integer* lda) (integer* ipiv) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sgetrs_ ((char* trans) (integer* n) (integer* nrhs) (real* a) (integer* lda) (integer* ipiv) (real* b) (integer* ldb) (integer* info)))

(define-wrapper int sggbak_ ((char* job) (char* side) (integer* n) (integer* ilo) (integer* ihi) (real* lscale) (real* rscale) (integer* m) (real* v) (integer* ldv) (integer* info)))

(define-wrapper int sggbal_ ((char* job) (integer* n) (real* a) (integer* lda) (real* b) (integer* ldb) (integer* ilo) (integer* ihi) (real* lscale) (real* rscale) (real* work) (integer* info)))

(define-wrapper int sgges_ ((char* jobvsl) (char* jobvsr) (char* sort) (L_fp selctg) (integer* n) (real* a) (integer* lda) (real* b) (integer* ldb) (integer* sdim) (real* alphar) (real* alphai) (real* beta) (real* vsl) (integer* ldvsl) (real* vsr) (integer* ldvsr) (real* work) (integer* lwork) (logical* bwork) (integer* info)))

(define-wrapper int sggesx_ ((char* jobvsl) (char* jobvsr) (char* sort) (L_fp selctg) (char* sense) (integer* n) (real* a) (integer* lda) (real* b) (integer* ldb) (integer* sdim) (real* alphar) (real* alphai) (real* beta) (real* vsl) (integer* ldvsl) (real* vsr) (integer* ldvsr) (real* rconde) (real* rcondv) (real* work) (integer* lwork) (integer* iwork) (integer* liwork) (logical* bwork) (integer* info)))

(define-wrapper int sggev_ ((char* jobvl) (char* jobvr) (integer* n) (real* a) (integer* lda) (real* b) (integer* ldb) (real* alphar) (real* alphai) (real* beta) (real* vl) (integer* ldvl) (real* vr) (integer* ldvr) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sggevx_ ((char* balanc) (char* jobvl) (char* jobvr) (char* sense) (integer* n) (real* a) (integer* lda) (real* b) (integer* ldb) (real* alphar) (real* alphai) (real* beta) (real* vl) (integer* ldvl) (real* vr) (integer* ldvr) (integer* ilo) (integer* ihi) (real* lscale) (real* rscale) (real* abnrm) (real* bbnrm) (real* rconde) (real* rcondv) (real* work) (integer* lwork) (integer* iwork) (logical* bwork) (integer* info)))

(define-wrapper int sggglm_ ((integer* n) (integer* m) (integer* p) (real* a) (integer* lda) (real* b) (integer* ldb) (real* d__) (real* x) (real* y) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sgghrd_ ((char* compq) (char* compz) (integer* n) (integer* ilo) (integer* ihi) (real* a) (integer* lda) (real* b) (integer* ldb) (real* q) (integer* ldq) (real* z__) (integer* ldz) (integer* info)))

(define-wrapper int sgglse_ ((integer* m) (integer* n) (integer* p) (real* a) (integer* lda) (real* b) (integer* ldb) (real* c__) (real* d__) (real* x) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sggqrf_ ((integer* n) (integer* m) (integer* p) (real* a) (integer* lda) (real* taua) (real* b) (integer* ldb) (real* taub) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sggrqf_ ((integer* m) (integer* p) (integer* n) (real* a) (integer* lda) (real* taua) (real* b) (integer* ldb) (real* taub) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sggsvd_ ((char* jobu) (char* jobv) (char* jobq) (integer* m) (integer* n) (integer* p) (integer* k) (integer* l) (real* a) (integer* lda) (real* b) (integer* ldb) (real* alpha) (real* beta) (real* u) (integer* ldu) (real* v) (integer* ldv) (real* q) (integer* ldq) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int sggsvp_ ((char* jobu) (char* jobv) (char* jobq) (integer* m) (integer* p) (integer* n) (real* a) (integer* lda) (real* b) (integer* ldb) (real* tola) (real* tolb) (integer* k) (integer* l) (real* u) (integer* ldu) (real* v) (integer* ldv) (real* q) (integer* ldq) (integer* iwork) (real* tau) (real* work) (integer* info)))

(define-wrapper int sgtcon_ ((char* norm) (integer* n) (real* dl) (real* d__) (real* du) (real* du2) (integer* ipiv) (real* anorm) (real* rcond) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int sgtrfs_ ((char* trans) (integer* n) (integer* nrhs) (real* dl) (real* d__) (real* du) (real* dlf) (real* df) (real* duf) (real* du2) (integer* ipiv) (real* b) (integer* ldb) (real* x) (integer* ldx) (real* ferr) (real* berr) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int sgtsv_ ((integer* n) (integer* nrhs) (real* dl) (real* d__) (real* du) (real* b) (integer* ldb) (integer* info)))

(define-wrapper int sgtsvx_ ((char* fact) (char* trans) (integer* n) (integer* nrhs) (real* dl) (real* d__) (real* du) (real* dlf) (real* df) (real* duf) (real* du2) (integer* ipiv) (real* b) (integer* ldb) (real* x) (integer* ldx) (real* rcond) (real* ferr) (real* berr) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int sgttrf_ ((integer* n) (real* dl) (real* d__) (real* du) (real* du2) (integer* ipiv) (integer* info)))

(define-wrapper int sgttrs_ ((char* trans) (integer* n) (integer* nrhs) (real* dl) (real* d__) (real* du) (real* du2) (integer* ipiv) (real* b) (integer* ldb) (integer* info)))

(define-wrapper int sgtts2_ ((integer* itrans) (integer* n) (integer* nrhs) (real* dl) (real* d__) (real* du) (real* du2) (integer* ipiv) (real* b) (integer* ldb)))

(define-wrapper int shgeqz_ ((char* job) (char* compq) (char* compz) (integer* n) (integer* ilo) (integer* ihi) (real* a) (integer* lda) (real* b) (integer* ldb) (real* alphar) (real* alphai) (real* beta) (real* q) (integer* ldq) (real* z__) (integer* ldz) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int shsein_ ((char* side) (char* eigsrc) (char* initv) (logical* select) (integer* n) (real* h__) (integer* ldh) (real* wr) (real* wi) (real* vl) (integer* ldvl) (real* vr) (integer* ldvr) (integer* mm) (integer* m) (real* work) (integer* ifaill) (integer* ifailr) (integer* info)))

(define-wrapper int shseqr_ ((char* job) (char* compz) (integer* n) (integer* ilo) (integer* ihi) (real* h__) (integer* ldh) (real* wr) (real* wi) (real* z__) (integer* ldz) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int slabad_ ((real* small) (real* large)))

(define-wrapper int slabrd_ ((integer* m) (integer* n) (integer* nb) (real* a) (integer* lda) (real* d__) (real* e) (real* tauq) (real* taup) (real* x) (integer* ldx) (real* y) (integer* ldy)))

(define-wrapper int slacon_ ((integer* n) (real* v) (real* x) (integer* isgn) (real* est) (integer* kase)))

(define-wrapper int slacpy_ ((char* uplo) (integer* m) (integer* n) (real* a) (integer* lda) (real* b) (integer* ldb)))

(define-wrapper int sladiv_ ((real* a) (real* b) (real* c__) (real* d__) (real* p) (real* q)))

(define-wrapper int slae2_ ((real* a) (real* b) (real* c__) (real* rt1) (real* rt2)))

(define-wrapper int slaebz_ ((integer* ijob) (integer* nitmax) (integer* n) (integer* mmax) (integer* minp) (integer* nbmin) (real* abstol) (real* reltol) (real* pivmin) (real* d__) (real* e) (real* e2) (integer* nval) (real* ab) (real* c__) (integer* mout) (integer* nab) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int slaed0_ ((integer* icompq) (integer* qsiz) (integer* n) (real* d__) (real* e) (real* q) (integer* ldq) (real* qstore) (integer* ldqs) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int slaed1_ ((integer* n) (real* d__) (real* q) (integer* ldq) (integer* indxq) (real* rho) (integer* cutpnt) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int slaed2_ ((integer* k) (integer* n) (integer* n1) (real* d__) (real* q) (integer* ldq) (integer* indxq) (real* rho) (real* z__) (real* dlamda) (real* w) (real* q2) (integer* indx) (integer* indxc) (integer* indxp) (integer* coltyp) (integer* info)))

(define-wrapper int slaed3_ ((integer* k) (integer* n) (integer* n1) (real* d__) (real* q) (integer* ldq) (real* rho) (real* dlamda) (real* q2) (integer* indx) (integer* ctot) (real* w) (real* s) (integer* info)))

(define-wrapper int slaed4_ ((integer* n) (integer* i__) (real* d__) (real* z__) (real* delta) (real* rho) (real* dlam) (integer* info)))

(define-wrapper int slaed5_ ((integer* i__) (real* d__) (real* z__) (real* delta) (real* rho) (real* dlam)))

(define-wrapper int slaed6_ ((integer* kniter) (logical* orgati) (real* rho) (real* d__) (real* z__) (real* finit) (real* tau) (integer* info)))

(define-wrapper int slaed7_ ((integer* icompq) (integer* n) (integer* qsiz) (integer* tlvls) (integer* curlvl) (integer* curpbm) (real* d__) (real* q) (integer* ldq) (integer* indxq) (real* rho) (integer* cutpnt) (real* qstore) (integer* qptr) (integer* prmptr) (integer* perm) (integer* givptr) (integer* givcol) (real* givnum) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int slaed8_ ((integer* icompq) (integer* k) (integer* n) (integer* qsiz) (real* d__) (real* q) (integer* ldq) (integer* indxq) (real* rho) (integer* cutpnt) (real* z__) (real* dlamda) (real* q2) (integer* ldq2) (real* w) (integer* perm) (integer* givptr) (integer* givcol) (real* givnum) (integer* indxp) (integer* indx) (integer* info)))

(define-wrapper int slaed9_ ((integer* k) (integer* kstart) (integer* kstop) (integer* n) (real* d__) (real* q) (integer* ldq) (real* rho) (real* dlamda) (real* w) (real* s) (integer* lds) (integer* info)))

(define-wrapper int slaeda_ ((integer* n) (integer* tlvls) (integer* curlvl) (integer* curpbm) (integer* prmptr) (integer* perm) (integer* givptr) (integer* givcol) (real* givnum) (real* q) (integer* qptr) (real* z__) (real* ztemp) (integer* info)))

(define-wrapper int slaein_ ((logical* rightv) (logical* noinit) (integer* n) (real* h__) (integer* ldh) (real* wr) (real* wi) (real* vr) (real* vi) (real* b) (integer* ldb) (real* work) (real* eps3) (real* smlnum) (real* bignum) (integer* info)))

(define-wrapper int slaev2_ ((real* a) (real* b) (real* c__) (real* rt1) (real* rt2) (real* cs1) (real* sn1)))

(define-wrapper int slaexc_ ((logical* wantq) (integer* n) (real* t) (integer* ldt) (real* q) (integer* ldq) (integer* j1) (integer* n1) (integer* n2) (real* work) (integer* info)))

(define-wrapper int slag2_ ((real* a) (integer* lda) (real* b) (integer* ldb) (real* safmin) (real* scale1) (real* scale2) (real* wr1) (real* wr2) (real* wi)))

(define-wrapper int slags2_ ((logical* upper) (real* a1) (real* a2) (real* a3) (real* b1) (real* b2) (real* b3) (real* csu) (real* snu) (real* csv) (real* snv) (real* csq) (real* snq)))

(define-wrapper int slagtf_ ((integer* n) (real* a) (real* lambda) (real* b) (real* c__) (real* tol) (real* d__) (integer* in) (integer* info)))

(define-wrapper int slagtm_ ((char* trans) (integer* n) (integer* nrhs) (real* alpha) (real* dl) (real* d__) (real* du) (real* x) (integer* ldx) (real* beta) (real* b) (integer* ldb)))

(define-wrapper int slagts_ ((integer* job) (integer* n) (real* a) (real* b) (real* c__) (real* d__) (integer* in) (real* y) (real* tol) (integer* info)))

(define-wrapper int slagv2_ ((real* a) (integer* lda) (real* b) (integer* ldb) (real* alphar) (real* alphai) (real* beta) (real* csl) (real* snl) (real* csr) (real* snr)))

(define-wrapper int slahqr_ ((logical* wantt) (logical* wantz) (integer* n) (integer* ilo) (integer* ihi) (real* h__) (integer* ldh) (real* wr) (real* wi) (integer* iloz) (integer* ihiz) (real* z__) (integer* ldz) (integer* info)))

(define-wrapper int slahrd_ ((integer* n) (integer* k) (integer* nb) (real* a) (integer* lda) (real* tau) (real* t) (integer* ldt) (real* y) (integer* ldy)))

(define-wrapper int slaic1_ ((integer* job) (integer* j) (real* x) (real* sest) (real* w) (real* gamma) (real* sestpr) (real* s) (real* c__)))

(define-wrapper int slaln2_ ((logical* ltrans) (integer* na) (integer* nw) (real* smin) (real* ca) (real* a) (integer* lda) (real* d1) (real* d2) (real* b) (integer* ldb) (real* wr) (real* wi) (real* x) (integer* ldx) (real* scale) (real* xnorm) (integer* info)))

(define-wrapper int slals0_ ((integer* icompq) (integer* nl) (integer* nr) (integer* sqre) (integer* nrhs) (real* b) (integer* ldb) (real* bx) (integer* ldbx) (integer* perm) (integer* givptr) (integer* givcol) (integer* ldgcol) (real* givnum) (integer* ldgnum) (real* poles) (real* difl) (real* difr) (real* z__) (integer* k) (real* c__) (real* s) (real* work) (integer* info)))

(define-wrapper int slalsa_ ((integer* icompq) (integer* smlsiz) (integer* n) (integer* nrhs) (real* b) (integer* ldb) (real* bx) (integer* ldbx) (real* u) (integer* ldu) (real* vt) (integer* k) (real* difl) (real* difr) (real* z__) (real* poles) (integer* givptr) (integer* givcol) (integer* ldgcol) (integer* perm) (real* givnum) (real* c__) (real* s) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int slalsd_ ((char* uplo) (integer* smlsiz) (integer* n) (integer* nrhs) (real* d__) (real* e) (real* b) (integer* ldb) (real* rcond) (integer* rank) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int slamc1_ ((integer* beta) (integer* t) (logical* rnd) (logical* ieee1)))

(define-wrapper int slamc2_ ((integer* beta) (integer* t) (logical* rnd) (real* eps) (integer* emin) (real* rmin) (integer* emax) (real* rmax)))

(define-wrapper int slamc4_ ((integer* emin) (real* start) (integer* base)))

(define-wrapper int slamc5_ ((integer* beta) (integer* p) (integer* emin) (logical* ieee) (integer* emax) (real* rmax)))

(define-wrapper int slamrg_ ((integer* n1) (integer* n2) (real* a) (integer* strd1) (integer* strd2) (integer* index)))

(define-wrapper int slanv2_ ((real* a) (real* b) (real* c__) (real* d__) (real* rt1r) (real* rt1i) (real* rt2r) (real* rt2i) (real* cs) (real* sn)))

(define-wrapper int slapll_ ((integer* n) (real* x) (integer* incx) (real* y) (integer* incy) (real* ssmin)))

(define-wrapper int slapmt_ ((logical* forwrd) (integer* m) (integer* n) (real* x) (integer* ldx) (integer* k)))

(define-wrapper int slaqgb_ ((integer* m) (integer* n) (integer* kl) (integer* ku) (real* ab) (integer* ldab) (real* r__) (real* c__) (real* rowcnd) (real* colcnd) (real* amax) (char* equed)))

(define-wrapper int slaqge_ ((integer* m) (integer* n) (real* a) (integer* lda) (real* r__) (real* c__) (real* rowcnd) (real* colcnd) (real* amax) (char* equed)))

(define-wrapper int slaqp2_ ((integer* m) (integer* n) (integer* offset) (real* a) (integer* lda) (integer* jpvt) (real* tau) (real* vn1) (real* vn2) (real* work)))

(define-wrapper int slaqps_ ((integer* m) (integer* n) (integer* offset) (integer* nb) (integer* kb) (real* a) (integer* lda) (integer* jpvt) (real* tau) (real* vn1) (real* vn2) (real* auxv) (real* f) (integer* ldf)))

(define-wrapper int slaqsb_ ((char* uplo) (integer* n) (integer* kd) (real* ab) (integer* ldab) (real* s) (real* scond) (real* amax) (char* equed)))

(define-wrapper int slaqsp_ ((char* uplo) (integer* n) (real* ap) (real* s) (real* scond) (real* amax) (char* equed)))

(define-wrapper int slaqsy_ ((char* uplo) (integer* n) (real* a) (integer* lda) (real* s) (real* scond) (real* amax) (char* equed)))

(define-wrapper int slaqtr_ ((logical* ltran) (logical* lreal) (integer* n) (real* t) (integer* ldt) (real* b) (real* w) (real* scale) (real* x) (real* work) (integer* info)))

(define-wrapper int slar1v_ ((integer* n) (integer* b1) (integer* bn) (real* sigma) (real* d__) (real* l) (real* ld) (real* lld) (real* gersch) (real* z__) (real* ztz) (real* mingma) (integer* r__) (integer* isuppz) (real* work)))

(define-wrapper int slar2v_ ((integer* n) (real* x) (real* y) (real* z__) (integer* incx) (real* c__) (real* s) (integer* incc)))

(define-wrapper int slarf_ ((char* side) (integer* m) (integer* n) (real* v) (integer* incv) (real* tau) (real* c__) (integer* ldc) (real* work)))

(define-wrapper int slarfb_ ((char* side) (char* trans) (char* direct) (char* storev) (integer* m) (integer* n) (integer* k) (real* v) (integer* ldv) (real* t) (integer* ldt) (real* c__) (integer* ldc) (real* work) (integer* ldwork)))

(define-wrapper int slarfg_ ((integer* n) (real* alpha) (real* x) (integer* incx) (real* tau)))

(define-wrapper int slarft_ ((char* direct) (char* storev) (integer* n) (integer* k) (real* v) (integer* ldv) (real* tau) (real* t) (integer* ldt)))

(define-wrapper int slarfx_ ((char* side) (integer* m) (integer* n) (real* v) (real* tau) (real* c__) (integer* ldc) (real* work)))

(define-wrapper int slargv_ ((integer* n) (real* x) (integer* incx) (real* y) (integer* incy) (real* c__) (integer* incc)))

(define-wrapper int slarnv_ ((integer* idist) (integer* iseed) (integer* n) (real* x)))

(define-wrapper int slarrb_ ((integer* n) (real* d__) (real* l) (real* ld) (real* lld) (integer* ifirst) (integer* ilast) (real* sigma) (real* reltol) (real* w) (real* wgap) (real* werr) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int slarre_ ((integer* n) (real* d__) (real* e) (real* tol) (integer* nsplit) (integer* isplit) (integer* m) (real* w) (real* woff) (real* gersch) (real* work) (integer* info)))

(define-wrapper int slarrf_ ((integer* n) (real* d__) (real* l) (real* ld) (real* lld) (integer* ifirst) (integer* ilast) (real* w) (real* dplus) (real* lplus) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int slarrv_ ((integer* n) (real* d__) (real* l) (integer* isplit) (integer* m) (real* w) (integer* iblock) (real* gersch) (real* tol) (real* z__) (integer* ldz) (integer* isuppz) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int slartg_ ((real* f) (real* g) (real* cs) (real* sn) (real* r__)))

(define-wrapper int slartv_ ((integer* n) (real* x) (integer* incx) (real* y) (integer* incy) (real* c__) (real* s) (integer* incc)))

(define-wrapper int slaruv_ ((integer* iseed) (integer* n) (real* x)))

(define-wrapper int slarz_ ((char* side) (integer* m) (integer* n) (integer* l) (real* v) (integer* incv) (real* tau) (real* c__) (integer* ldc) (real* work)))

(define-wrapper int slarzb_ ((char* side) (char* trans) (char* direct) (char* storev) (integer* m) (integer* n) (integer* k) (integer* l) (real* v) (integer* ldv) (real* t) (integer* ldt) (real* c__) (integer* ldc) (real* work) (integer* ldwork)))

(define-wrapper int slarzt_ ((char* direct) (char* storev) (integer* n) (integer* k) (real* v) (integer* ldv) (real* tau) (real* t) (integer* ldt)))

(define-wrapper int slas2_ ((real* f) (real* g) (real* h__) (real* ssmin) (real* ssmax)))

(define-wrapper int slascl_ ((char* type__) (integer* kl) (integer* ku) (real* cfrom) (real* cto) (integer* m) (integer* n) (real* a) (integer* lda) (integer* info)))

(define-wrapper int slasd0_ ((integer* n) (integer* sqre) (real* d__) (real* e) (real* u) (integer* ldu) (real* vt) (integer* ldvt) (integer* smlsiz) (integer* iwork) (real* work) (integer* info)))

(define-wrapper int slasd1_ ((integer* nl) (integer* nr) (integer* sqre) (real* d__) (real* alpha) (real* beta) (real* u) (integer* ldu) (real* vt) (integer* ldvt) (integer* idxq) (integer* iwork) (real* work) (integer* info)))

(define-wrapper int slasd2_ ((integer* nl) (integer* nr) (integer* sqre) (integer* k) (real* d__) (real* z__) (real* alpha) (real* beta) (real* u) (integer* ldu) (real* vt) (integer* ldvt) (real* dsigma) (real* u2) (integer* ldu2) (real* vt2) (integer* ldvt2) (integer* idxp) (integer* idx) (integer* idxc) (integer* idxq) (integer* coltyp) (integer* info)))

(define-wrapper int slasd3_ ((integer* nl) (integer* nr) (integer* sqre) (integer* k) (real* d__) (real* q) (integer* ldq) (real* dsigma) (real* u) (integer* ldu) (real* u2) (integer* ldu2) (real* vt) (integer* ldvt) (real* vt2) (integer* ldvt2) (integer* idxc) (integer* ctot) (real* z__) (integer* info)))

(define-wrapper int slasd4_ ((integer* n) (integer* i__) (real* d__) (real* z__) (real* delta) (real* rho) (real* sigma) (real* work) (integer* info)))

(define-wrapper int slasd5_ ((integer* i__) (real* d__) (real* z__) (real* delta) (real* rho) (real* dsigma) (real* work)))

(define-wrapper int slasd6_ ((integer* icompq) (integer* nl) (integer* nr) (integer* sqre) (real* d__) (real* vf) (real* vl) (real* alpha) (real* beta) (integer* idxq) (integer* perm) (integer* givptr) (integer* givcol) (integer* ldgcol) (real* givnum) (integer* ldgnum) (real* poles) (real* difl) (real* difr) (real* z__) (integer* k) (real* c__) (real* s) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int slasd7_ ((integer* icompq) (integer* nl) (integer* nr) (integer* sqre) (integer* k) (real* d__) (real* z__) (real* zw) (real* vf) (real* vfw) (real* vl) (real* vlw) (real* alpha) (real* beta) (real* dsigma) (integer* idx) (integer* idxp) (integer* idxq) (integer* perm) (integer* givptr) (integer* givcol) (integer* ldgcol) (real* givnum) (integer* ldgnum) (real* c__) (real* s) (integer* info)))

(define-wrapper int slasd8_ ((integer* icompq) (integer* k) (real* d__) (real* z__) (real* vf) (real* vl) (real* difl) (real* difr) (integer* lddifr) (real* dsigma) (real* work) (integer* info)))

(define-wrapper int slasd9_ ((integer* icompq) (integer* ldu) (integer* k) (real* d__) (real* z__) (real* vf) (real* vl) (real* difl) (real* difr) (real* dsigma) (real* work) (integer* info)))

(define-wrapper int slasda_ ((integer* icompq) (integer* smlsiz) (integer* n) (integer* sqre) (real* d__) (real* e) (real* u) (integer* ldu) (real* vt) (integer* k) (real* difl) (real* difr) (real* z__) (real* poles) (integer* givptr) (integer* givcol) (integer* ldgcol) (integer* perm) (real* givnum) (real* c__) (real* s) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int slasdq_ ((char* uplo) (integer* sqre) (integer* n) (integer* ncvt) (integer* nru) (integer* ncc) (real* d__) (real* e) (real* vt) (integer* ldvt) (real* u) (integer* ldu) (real* c__) (integer* ldc) (real* work) (integer* info)))

(define-wrapper int slasdt_ ((integer* n) (integer* lvl) (integer* nd) (integer* inode) (integer* ndiml) (integer* ndimr) (integer* msub)))

(define-wrapper int slaset_ ((char* uplo) (integer* m) (integer* n) (real* alpha) (real* beta) (real* a) (integer* lda)))

(define-wrapper int slasq1_ ((integer* n) (real* d__) (real* e) (real* work) (integer* info)))

(define-wrapper int slasq2_ ((integer* n) (real* z__) (integer* info)))

(define-wrapper int slasq3_ ((integer* i0) (integer* n0) (real* z__) (integer* pp) (real* dmin__) (real* sigma) (real* desig) (real* qmax) (integer* nfail) (integer* iter) (integer* ndiv) (logical* ieee)))

(define-wrapper int slasq4_ ((integer* i0) (integer* n0) (real* z__) (integer* pp) (integer* n0in) (real* dmin__) (real* dmin1) (real* dmin2) (real* dn) (real* dn1) (real* dn2) (real* tau) (integer* ttype)))

(define-wrapper int slasq5_ ((integer* i0) (integer* n0) (real* z__) (integer* pp) (real* tau) (real* dmin__) (real* dmin1) (real* dmin2) (real* dn) (real* dnm1) (real* dnm2) (logical* ieee)))

(define-wrapper int slasq6_ ((integer* i0) (integer* n0) (real* z__) (integer* pp) (real* dmin__) (real* dmin1) (real* dmin2) (real* dn) (real* dnm1) (real* dnm2)))

(define-wrapper int slasr_ ((char* side) (char* pivot) (char* direct) (integer* m) (integer* n) (real* c__) (real* s) (real* a) (integer* lda)))

(define-wrapper int slasrt_ ((char* id) (integer* n) (real* d__) (integer* info)))

(define-wrapper int slassq_ ((integer* n) (real* x) (integer* incx) (real* scale) (real* sumsq)))

(define-wrapper int slasv2_ ((real* f) (real* g) (real* h__) (real* ssmin) (real* ssmax) (real* snr) (real* csr) (real* snl) (real* csl)))

(define-wrapper int slaswp_ ((integer* n) (real* a) (integer* lda) (integer* k1) (integer* k2) (integer* ipiv) (integer* incx)))

(define-wrapper int slasy2_ ((logical* ltranl) (logical* ltranr) (integer* isgn) (integer* n1) (integer* n2) (real* tl) (integer* ldtl) (real* tr) (integer* ldtr) (real* b) (integer* ldb) (real* scale) (real* x) (integer* ldx) (real* xnorm) (integer* info)))

(define-wrapper int slasyf_ ((char* uplo) (integer* n) (integer* nb) (integer* kb) (real* a) (integer* lda) (integer* ipiv) (real* w) (integer* ldw) (integer* info)))

(define-wrapper int slatbs_ ((char* uplo) (char* trans) (char* diag) (char* normin) (integer* n) (integer* kd) (real* ab) (integer* ldab) (real* x) (real* scale) (real* cnorm) (integer* info)))

(define-wrapper int slatdf_ ((integer* ijob) (integer* n) (real* z__) (integer* ldz) (real* rhs) (real* rdsum) (real* rdscal) (integer* ipiv) (integer* jpiv)))

(define-wrapper int slatps_ ((char* uplo) (char* trans) (char* diag) (char* normin) (integer* n) (real* ap) (real* x) (real* scale) (real* cnorm) (integer* info)))

(define-wrapper int slatrd_ ((char* uplo) (integer* n) (integer* nb) (real* a) (integer* lda) (real* e) (real* tau) (real* w) (integer* ldw)))

(define-wrapper int slatrs_ ((char* uplo) (char* trans) (char* diag) (char* normin) (integer* n) (real* a) (integer* lda) (real* x) (real* scale) (real* cnorm) (integer* info)))

(define-wrapper int slatrz_ ((integer* m) (integer* n) (integer* l) (real* a) (integer* lda) (real* tau) (real* work)))

(define-wrapper int slatzm_ ((char* side) (integer* m) (integer* n) (real* v) (integer* incv) (real* tau) (real* c1) (real* c2) (integer* ldc) (real* work)))

(define-wrapper int slauu2_ ((char* uplo) (integer* n) (real* a) (integer* lda) (integer* info)))

(define-wrapper int slauum_ ((char* uplo) (integer* n) (real* a) (integer* lda) (integer* info)))

(define-wrapper int sopgtr_ ((char* uplo) (integer* n) (real* ap) (real* tau) (real* q) (integer* ldq) (real* work) (integer* info)))

(define-wrapper int sopmtr_ ((char* side) (char* uplo) (char* trans) (integer* m) (integer* n) (real* ap) (real* tau) (real* c__) (integer* ldc) (real* work) (integer* info)))

(define-wrapper int sorg2l_ ((integer* m) (integer* n) (integer* k) (real* a) (integer* lda) (real* tau) (real* work) (integer* info)))

(define-wrapper int sorg2r_ ((integer* m) (integer* n) (integer* k) (real* a) (integer* lda) (real* tau) (real* work) (integer* info)))

(define-wrapper int sorgbr_ ((char* vect) (integer* m) (integer* n) (integer* k) (real* a) (integer* lda) (real* tau) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sorghr_ ((integer* n) (integer* ilo) (integer* ihi) (real* a) (integer* lda) (real* tau) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sorgl2_ ((integer* m) (integer* n) (integer* k) (real* a) (integer* lda) (real* tau) (real* work) (integer* info)))

(define-wrapper int sorglq_ ((integer* m) (integer* n) (integer* k) (real* a) (integer* lda) (real* tau) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sorgql_ ((integer* m) (integer* n) (integer* k) (real* a) (integer* lda) (real* tau) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sorgqr_ ((integer* m) (integer* n) (integer* k) (real* a) (integer* lda) (real* tau) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sorgr2_ ((integer* m) (integer* n) (integer* k) (real* a) (integer* lda) (real* tau) (real* work) (integer* info)))

(define-wrapper int sorgrq_ ((integer* m) (integer* n) (integer* k) (real* a) (integer* lda) (real* tau) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sorgtr_ ((char* uplo) (integer* n) (real* a) (integer* lda) (real* tau) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sorm2l_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (real* a) (integer* lda) (real* tau) (real* c__) (integer* ldc) (real* work) (integer* info)))

(define-wrapper int sorm2r_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (real* a) (integer* lda) (real* tau) (real* c__) (integer* ldc) (real* work) (integer* info)))

(define-wrapper int sormbr_ ((char* vect) (char* side) (char* trans) (integer* m) (integer* n) (integer* k) (real* a) (integer* lda) (real* tau) (real* c__) (integer* ldc) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sormhr_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* ilo) (integer* ihi) (real* a) (integer* lda) (real* tau) (real* c__) (integer* ldc) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sorml2_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (real* a) (integer* lda) (real* tau) (real* c__) (integer* ldc) (real* work) (integer* info)))

(define-wrapper int sormlq_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (real* a) (integer* lda) (real* tau) (real* c__) (integer* ldc) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sormql_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (real* a) (integer* lda) (real* tau) (real* c__) (integer* ldc) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sormqr_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (real* a) (integer* lda) (real* tau) (real* c__) (integer* ldc) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sormr2_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (real* a) (integer* lda) (real* tau) (real* c__) (integer* ldc) (real* work) (integer* info)))

(define-wrapper int sormr3_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (integer* l) (real* a) (integer* lda) (real* tau) (real* c__) (integer* ldc) (real* work) (integer* info)))

(define-wrapper int sormrq_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (real* a) (integer* lda) (real* tau) (real* c__) (integer* ldc) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sormrz_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (integer* l) (real* a) (integer* lda) (real* tau) (real* c__) (integer* ldc) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int sormtr_ ((char* side) (char* uplo) (char* trans) (integer* m) (integer* n) (real* a) (integer* lda) (real* tau) (real* c__) (integer* ldc) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int spbcon_ ((char* uplo) (integer* n) (integer* kd) (real* ab) (integer* ldab) (real* anorm) (real* rcond) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int spbequ_ ((char* uplo) (integer* n) (integer* kd) (real* ab) (integer* ldab) (real* s) (real* scond) (real* amax) (integer* info)))

(define-wrapper int spbrfs_ ((char* uplo) (integer* n) (integer* kd) (integer* nrhs) (real* ab) (integer* ldab) (real* afb) (integer* ldafb) (real* b) (integer* ldb) (real* x) (integer* ldx) (real* ferr) (real* berr) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int spbstf_ ((char* uplo) (integer* n) (integer* kd) (real* ab) (integer* ldab) (integer* info)))

(define-wrapper int spbsv_ ((char* uplo) (integer* n) (integer* kd) (integer* nrhs) (real* ab) (integer* ldab) (real* b) (integer* ldb) (integer* info)))

(define-wrapper int spbsvx_ ((char* fact) (char* uplo) (integer* n) (integer* kd) (integer* nrhs) (real* ab) (integer* ldab) (real* afb) (integer* ldafb) (char* equed) (real* s) (real* b) (integer* ldb) (real* x) (integer* ldx) (real* rcond) (real* ferr) (real* berr) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int spbtf2_ ((char* uplo) (integer* n) (integer* kd) (real* ab) (integer* ldab) (integer* info)))

(define-wrapper int spbtrf_ ((char* uplo) (integer* n) (integer* kd) (real* ab) (integer* ldab) (integer* info)))

(define-wrapper int spbtrs_ ((char* uplo) (integer* n) (integer* kd) (integer* nrhs) (real* ab) (integer* ldab) (real* b) (integer* ldb) (integer* info)))

(define-wrapper int spocon_ ((char* uplo) (integer* n) (real* a) (integer* lda) (real* anorm) (real* rcond) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int spoequ_ ((integer* n) (real* a) (integer* lda) (real* s) (real* scond) (real* amax) (integer* info)))

(define-wrapper int sporfs_ ((char* uplo) (integer* n) (integer* nrhs) (real* a) (integer* lda) (real* af) (integer* ldaf) (real* b) (integer* ldb) (real* x) (integer* ldx) (real* ferr) (real* berr) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int sposv_ ((char* uplo) (integer* n) (integer* nrhs) (real* a) (integer* lda) (real* b) (integer* ldb) (integer* info)))

(define-wrapper int sposvx_ ((char* fact) (char* uplo) (integer* n) (integer* nrhs) (real* a) (integer* lda) (real* af) (integer* ldaf) (char* equed) (real* s) (real* b) (integer* ldb) (real* x) (integer* ldx) (real* rcond) (real* ferr) (real* berr) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int spotf2_ ((char* uplo) (integer* n) (real* a) (integer* lda) (integer* info)))

(define-wrapper int spotrf_ ((char* uplo) (integer* n) (real* a) (integer* lda) (integer* info)))

(define-wrapper int spotri_ ((char* uplo) (integer* n) (real* a) (integer* lda) (integer* info)))

(define-wrapper int spotrs_ ((char* uplo) (integer* n) (integer* nrhs) (real* a) (integer* lda) (real* b) (integer* ldb) (integer* info)))

(define-wrapper int sppcon_ ((char* uplo) (integer* n) (real* ap) (real* anorm) (real* rcond) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int sppequ_ ((char* uplo) (integer* n) (real* ap) (real* s) (real* scond) (real* amax) (integer* info)))

(define-wrapper int spprfs_ ((char* uplo) (integer* n) (integer* nrhs) (real* ap) (real* afp) (real* b) (integer* ldb) (real* x) (integer* ldx) (real* ferr) (real* berr) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int sppsv_ ((char* uplo) (integer* n) (integer* nrhs) (real* ap) (real* b) (integer* ldb) (integer* info)))

(define-wrapper int sppsvx_ ((char* fact) (char* uplo) (integer* n) (integer* nrhs) (real* ap) (real* afp) (char* equed) (real* s) (real* b) (integer* ldb) (real* x) (integer* ldx) (real* rcond) (real* ferr) (real* berr) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int spptrf_ ((char* uplo) (integer* n) (real* ap) (integer* info)))

(define-wrapper int spptri_ ((char* uplo) (integer* n) (real* ap) (integer* info)))

(define-wrapper int spptrs_ ((char* uplo) (integer* n) (integer* nrhs) (real* ap) (real* b) (integer* ldb) (integer* info)))

(define-wrapper int sptcon_ ((integer* n) (real* d__) (real* e) (real* anorm) (real* rcond) (real* work) (integer* info)))

(define-wrapper int spteqr_ ((char* compz) (integer* n) (real* d__) (real* e) (real* z__) (integer* ldz) (real* work) (integer* info)))

(define-wrapper int sptrfs_ ((integer* n) (integer* nrhs) (real* d__) (real* e) (real* df) (real* ef) (real* b) (integer* ldb) (real* x) (integer* ldx) (real* ferr) (real* berr) (real* work) (integer* info)))

(define-wrapper int sptsv_ ((integer* n) (integer* nrhs) (real* d__) (real* e) (real* b) (integer* ldb) (integer* info)))

(define-wrapper int sptsvx_ ((char* fact) (integer* n) (integer* nrhs) (real* d__) (real* e) (real* df) (real* ef) (real* b) (integer* ldb) (real* x) (integer* ldx) (real* rcond) (real* ferr) (real* berr) (real* work) (integer* info)))

(define-wrapper int spttrf_ ((integer* n) (real* d__) (real* e) (integer* info)))

(define-wrapper int spttrs_ ((integer* n) (integer* nrhs) (real* d__) (real* e) (real* b) (integer* ldb) (integer* info)))

(define-wrapper int sptts2_ ((integer* n) (integer* nrhs) (real* d__) (real* e) (real* b) (integer* ldb)))

(define-wrapper int srscl_ ((integer* n) (real* sa) (real* sx) (integer* incx)))

(define-wrapper int ssbev_ ((char* jobz) (char* uplo) (integer* n) (integer* kd) (real* ab) (integer* ldab) (real* w) (real* z__) (integer* ldz) (real* work) (integer* info)))

(define-wrapper int ssbevd_ ((char* jobz) (char* uplo) (integer* n) (integer* kd) (real* ab) (integer* ldab) (real* w) (real* z__) (integer* ldz) (real* work) (integer* lwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int ssbevx_ ((char* jobz) (char* range) (char* uplo) (integer* n) (integer* kd) (real* ab) (integer* ldab) (real* q) (integer* ldq) (real* vl) (real* vu) (integer* il) (integer* iu) (real* abstol) (integer* m) (real* w) (real* z__) (integer* ldz) (real* work) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int ssbgst_ ((char* vect) (char* uplo) (integer* n) (integer* ka) (integer* kb) (real* ab) (integer* ldab) (real* bb) (integer* ldbb) (real* x) (integer* ldx) (real* work) (integer* info)))

(define-wrapper int ssbgv_ ((char* jobz) (char* uplo) (integer* n) (integer* ka) (integer* kb) (real* ab) (integer* ldab) (real* bb) (integer* ldbb) (real* w) (real* z__) (integer* ldz) (real* work) (integer* info)))

(define-wrapper int ssbgvd_ ((char* jobz) (char* uplo) (integer* n) (integer* ka) (integer* kb) (real* ab) (integer* ldab) (real* bb) (integer* ldbb) (real* w) (real* z__) (integer* ldz) (real* work) (integer* lwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int ssbgvx_ ((char* jobz) (char* range) (char* uplo) (integer* n) (integer* ka) (integer* kb) (real* ab) (integer* ldab) (real* bb) (integer* ldbb) (real* q) (integer* ldq) (real* vl) (real* vu) (integer* il) (integer* iu) (real* abstol) (integer* m) (real* w) (real* z__) (integer* ldz) (real* work) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int ssbtrd_ ((char* vect) (char* uplo) (integer* n) (integer* kd) (real* ab) (integer* ldab) (real* d__) (real* e) (real* q) (integer* ldq) (real* work) (integer* info)))

(define-wrapper int sspcon_ ((char* uplo) (integer* n) (real* ap) (integer* ipiv) (real* anorm) (real* rcond) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int sspev_ ((char* jobz) (char* uplo) (integer* n) (real* ap) (real* w) (real* z__) (integer* ldz) (real* work) (integer* info)))

(define-wrapper int sspevd_ ((char* jobz) (char* uplo) (integer* n) (real* ap) (real* w) (real* z__) (integer* ldz) (real* work) (integer* lwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int sspevx_ ((char* jobz) (char* range) (char* uplo) (integer* n) (real* ap) (real* vl) (real* vu) (integer* il) (integer* iu) (real* abstol) (integer* m) (real* w) (real* z__) (integer* ldz) (real* work) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int sspgst_ ((integer* itype) (char* uplo) (integer* n) (real* ap) (real* bp) (integer* info)))

(define-wrapper int sspgv_ ((integer* itype) (char* jobz) (char* uplo) (integer* n) (real* ap) (real* bp) (real* w) (real* z__) (integer* ldz) (real* work) (integer* info)))

(define-wrapper int sspgvd_ ((integer* itype) (char* jobz) (char* uplo) (integer* n) (real* ap) (real* bp) (real* w) (real* z__) (integer* ldz) (real* work) (integer* lwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int sspgvx_ ((integer* itype) (char* jobz) (char* range) (char* uplo) (integer* n) (real* ap) (real* bp) (real* vl) (real* vu) (integer* il) (integer* iu) (real* abstol) (integer* m) (real* w) (real* z__) (integer* ldz) (real* work) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int ssprfs_ ((char* uplo) (integer* n) (integer* nrhs) (real* ap) (real* afp) (integer* ipiv) (real* b) (integer* ldb) (real* x) (integer* ldx) (real* ferr) (real* berr) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int sspsv_ ((char* uplo) (integer* n) (integer* nrhs) (real* ap) (integer* ipiv) (real* b) (integer* ldb) (integer* info)))

(define-wrapper int sspsvx_ ((char* fact) (char* uplo) (integer* n) (integer* nrhs) (real* ap) (real* afp) (integer* ipiv) (real* b) (integer* ldb) (real* x) (integer* ldx) (real* rcond) (real* ferr) (real* berr) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int ssptrd_ ((char* uplo) (integer* n) (real* ap) (real* d__) (real* e) (real* tau) (integer* info)))

(define-wrapper int ssptrf_ ((char* uplo) (integer* n) (real* ap) (integer* ipiv) (integer* info)))

(define-wrapper int ssptri_ ((char* uplo) (integer* n) (real* ap) (integer* ipiv) (real* work) (integer* info)))

(define-wrapper int ssptrs_ ((char* uplo) (integer* n) (integer* nrhs) (real* ap) (integer* ipiv) (real* b) (integer* ldb) (integer* info)))

(define-wrapper int sstebz_ ((char* range) (char* order) (integer* n) (real* vl) (real* vu) (integer* il) (integer* iu) (real* abstol) (real* d__) (real* e) (integer* m) (integer* nsplit) (real* w) (integer* iblock) (integer* isplit) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int sstedc_ ((char* compz) (integer* n) (real* d__) (real* e) (real* z__) (integer* ldz) (real* work) (integer* lwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int sstegr_ ((char* jobz) (char* range) (integer* n) (real* d__) (real* e) (real* vl) (real* vu) (integer* il) (integer* iu) (real* abstol) (integer* m) (real* w) (real* z__) (integer* ldz) (integer* isuppz) (real* work) (integer* lwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int sstein_ ((integer* n) (real* d__) (real* e) (integer* m) (real* w) (integer* iblock) (integer* isplit) (real* z__) (integer* ldz) (real* work) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int ssteqr_ ((char* compz) (integer* n) (real* d__) (real* e) (real* z__) (integer* ldz) (real* work) (integer* info)))

(define-wrapper int ssterf_ ((integer* n) (real* d__) (real* e) (integer* info)))

(define-wrapper int sstev_ ((char* jobz) (integer* n) (real* d__) (real* e) (real* z__) (integer* ldz) (real* work) (integer* info)))

(define-wrapper int sstevd_ ((char* jobz) (integer* n) (real* d__) (real* e) (real* z__) (integer* ldz) (real* work) (integer* lwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int sstevr_ ((char* jobz) (char* range) (integer* n) (real* d__) (real* e) (real* vl) (real* vu) (integer* il) (integer* iu) (real* abstol) (integer* m) (real* w) (real* z__) (integer* ldz) (integer* isuppz) (real* work) (integer* lwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int sstevx_ ((char* jobz) (char* range) (integer* n) (real* d__) (real* e) (real* vl) (real* vu) (integer* il) (integer* iu) (real* abstol) (integer* m) (real* w) (real* z__) (integer* ldz) (real* work) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int ssycon_ ((char* uplo) (integer* n) (real* a) (integer* lda) (integer* ipiv) (real* anorm) (real* rcond) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int ssyev_ ((char* jobz) (char* uplo) (integer* n) (real* a) (integer* lda) (real* w) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int ssyevd_ ((char* jobz) (char* uplo) (integer* n) (real* a) (integer* lda) (real* w) (real* work) (integer* lwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int ssyevr_ ((char* jobz) (char* range) (char* uplo) (integer* n) (real* a) (integer* lda) (real* vl) (real* vu) (integer* il) (integer* iu) (real* abstol) (integer* m) (real* w) (real* z__) (integer* ldz) (integer* isuppz) (real* work) (integer* lwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int ssyevx_ ((char* jobz) (char* range) (char* uplo) (integer* n) (real* a) (integer* lda) (real* vl) (real* vu) (integer* il) (integer* iu) (real* abstol) (integer* m) (real* w) (real* z__) (integer* ldz) (real* work) (integer* lwork) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int ssygs2_ ((integer* itype) (char* uplo) (integer* n) (real* a) (integer* lda) (real* b) (integer* ldb) (integer* info)))

(define-wrapper int ssygst_ ((integer* itype) (char* uplo) (integer* n) (real* a) (integer* lda) (real* b) (integer* ldb) (integer* info)))

(define-wrapper int ssygv_ ((integer* itype) (char* jobz) (char* uplo) (integer* n) (real* a) (integer* lda) (real* b) (integer* ldb) (real* w) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int ssygvd_ ((integer* itype) (char* jobz) (char* uplo) (integer* n) (real* a) (integer* lda) (real* b) (integer* ldb) (real* w) (real* work) (integer* lwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int ssygvx_ ((integer* itype) (char* jobz) (char* range) (char* uplo) (integer* n) (real* a) (integer* lda) (real* b) (integer* ldb) (real* vl) (real* vu) (integer* il) (integer* iu) (real* abstol) (integer* m) (real* w) (real* z__) (integer* ldz) (real* work) (integer* lwork) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int ssyrfs_ ((char* uplo) (integer* n) (integer* nrhs) (real* a) (integer* lda) (real* af) (integer* ldaf) (integer* ipiv) (real* b) (integer* ldb) (real* x) (integer* ldx) (real* ferr) (real* berr) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int ssysv_ ((char* uplo) (integer* n) (integer* nrhs) (real* a) (integer* lda) (integer* ipiv) (real* b) (integer* ldb) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int ssysvx_ ((char* fact) (char* uplo) (integer* n) (integer* nrhs) (real* a) (integer* lda) (real* af) (integer* ldaf) (integer* ipiv) (real* b) (integer* ldb) (real* x) (integer* ldx) (real* rcond) (real* ferr) (real* berr) (real* work) (integer* lwork) (integer* iwork) (integer* info)))

(define-wrapper int ssytd2_ ((char* uplo) (integer* n) (real* a) (integer* lda) (real* d__) (real* e) (real* tau) (integer* info)))

(define-wrapper int ssytf2_ ((char* uplo) (integer* n) (real* a) (integer* lda) (integer* ipiv) (integer* info)))

(define-wrapper int ssytrd_ ((char* uplo) (integer* n) (real* a) (integer* lda) (real* d__) (real* e) (real* tau) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int ssytrf_ ((char* uplo) (integer* n) (real* a) (integer* lda) (integer* ipiv) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int ssytri_ ((char* uplo) (integer* n) (real* a) (integer* lda) (integer* ipiv) (real* work) (integer* info)))

(define-wrapper int ssytrs_ ((char* uplo) (integer* n) (integer* nrhs) (real* a) (integer* lda) (integer* ipiv) (real* b) (integer* ldb) (integer* info)))

(define-wrapper int stbcon_ ((char* norm) (char* uplo) (char* diag) (integer* n) (integer* kd) (real* ab) (integer* ldab) (real* rcond) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int stbrfs_ ((char* uplo) (char* trans) (char* diag) (integer* n) (integer* kd) (integer* nrhs) (real* ab) (integer* ldab) (real* b) (integer* ldb) (real* x) (integer* ldx) (real* ferr) (real* berr) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int stbtrs_ ((char* uplo) (char* trans) (char* diag) (integer* n) (integer* kd) (integer* nrhs) (real* ab) (integer* ldab) (real* b) (integer* ldb) (integer* info)))

(define-wrapper int stgevc_ ((char* side) (char* howmny) (logical* select) (integer* n) (real* a) (integer* lda) (real* b) (integer* ldb) (real* vl) (integer* ldvl) (real* vr) (integer* ldvr) (integer* mm) (integer* m) (real* work) (integer* info)))

(define-wrapper int stgex2_ ((logical* wantq) (logical* wantz) (integer* n) (real* a) (integer* lda) (real* b) (integer* ldb) (real* q) (integer* ldq) (real* z__) (integer* ldz) (integer* j1) (integer* n1) (integer* n2) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int stgexc_ ((logical* wantq) (logical* wantz) (integer* n) (real* a) (integer* lda) (real* b) (integer* ldb) (real* q) (integer* ldq) (real* z__) (integer* ldz) (integer* ifst) (integer* ilst) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int stgsen_ ((integer* ijob) (logical* wantq) (logical* wantz) (logical* select) (integer* n) (real* a) (integer* lda) (real* b) (integer* ldb) (real* alphar) (real* alphai) (real* beta) (real* q) (integer* ldq) (real* z__) (integer* ldz) (integer* m) (real* pl) (real* pr) (real* dif) (real* work) (integer* lwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int stgsja_ ((char* jobu) (char* jobv) (char* jobq) (integer* m) (integer* p) (integer* n) (integer* k) (integer* l) (real* a) (integer* lda) (real* b) (integer* ldb) (real* tola) (real* tolb) (real* alpha) (real* beta) (real* u) (integer* ldu) (real* v) (integer* ldv) (real* q) (integer* ldq) (real* work) (integer* ncycle) (integer* info)))

(define-wrapper int stgsna_ ((char* job) (char* howmny) (logical* select) (integer* n) (real* a) (integer* lda) (real* b) (integer* ldb) (real* vl) (integer* ldvl) (real* vr) (integer* ldvr) (real* s) (real* dif) (integer* mm) (integer* m) (real* work) (integer* lwork) (integer* iwork) (integer* info)))

(define-wrapper int stgsy2_ ((char* trans) (integer* ijob) (integer* m) (integer* n) (real* a) (integer* lda) (real* b) (integer* ldb) (real* c__) (integer* ldc) (real* d__) (integer* ldd) (real* e) (integer* lde) (real* f) (integer* ldf) (real* scale) (real* rdsum) (real* rdscal) (integer* iwork) (integer* pq) (integer* info)))

(define-wrapper int stgsyl_ ((char* trans) (integer* ijob) (integer* m) (integer* n) (real* a) (integer* lda) (real* b) (integer* ldb) (real* c__) (integer* ldc) (real* d__) (integer* ldd) (real* e) (integer* lde) (real* f) (integer* ldf) (real* scale) (real* dif) (real* work) (integer* lwork) (integer* iwork) (integer* info)))

(define-wrapper int stpcon_ ((char* norm) (char* uplo) (char* diag) (integer* n) (real* ap) (real* rcond) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int stprfs_ ((char* uplo) (char* trans) (char* diag) (integer* n) (integer* nrhs) (real* ap) (real* b) (integer* ldb) (real* x) (integer* ldx) (real* ferr) (real* berr) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int stptri_ ((char* uplo) (char* diag) (integer* n) (real* ap) (integer* info)))

(define-wrapper int stptrs_ ((char* uplo) (char* trans) (char* diag) (integer* n) (integer* nrhs) (real* ap) (real* b) (integer* ldb) (integer* info)))

(define-wrapper int strcon_ ((char* norm) (char* uplo) (char* diag) (integer* n) (real* a) (integer* lda) (real* rcond) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int strevc_ ((char* side) (char* howmny) (logical* select) (integer* n) (real* t) (integer* ldt) (real* vl) (integer* ldvl) (real* vr) (integer* ldvr) (integer* mm) (integer* m) (real* work) (integer* info)))

(define-wrapper int strexc_ ((char* compq) (integer* n) (real* t) (integer* ldt) (real* q) (integer* ldq) (integer* ifst) (integer* ilst) (real* work) (integer* info)))

(define-wrapper int strrfs_ ((char* uplo) (char* trans) (char* diag) (integer* n) (integer* nrhs) (real* a) (integer* lda) (real* b) (integer* ldb) (real* x) (integer* ldx) (real* ferr) (real* berr) (real* work) (integer* iwork) (integer* info)))

(define-wrapper int strsen_ ((char* job) (char* compq) (logical* select) (integer* n) (real* t) (integer* ldt) (real* q) (integer* ldq) (real* wr) (real* wi) (integer* m) (real* s) (real* sep) (real* work) (integer* lwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int strsna_ ((char* job) (char* howmny) (logical* select) (integer* n) (real* t) (integer* ldt) (real* vl) (integer* ldvl) (real* vr) (integer* ldvr) (real* s) (real* sep) (integer* mm) (integer* m) (real* work) (integer* ldwork) (integer* iwork) (integer* info)))

(define-wrapper int strsyl_ ((char* trana) (char* tranb) (integer* isgn) (integer* m) (integer* n) (real* a) (integer* lda) (real* b) (integer* ldb) (real* c__) (integer* ldc) (real* scale) (integer* info)))

(define-wrapper int strti2_ ((char* uplo) (char* diag) (integer* n) (real* a) (integer* lda) (integer* info)))

(define-wrapper int strtri_ ((char* uplo) (char* diag) (integer* n) (real* a) (integer* lda) (integer* info)))

(define-wrapper int strtrs_ ((char* uplo) (char* trans) (char* diag) (integer* n) (integer* nrhs) (real* a) (integer* lda) (real* b) (integer* ldb) (integer* info)))

(define-wrapper int stzrqf_ ((integer* m) (integer* n) (real* a) (integer* lda) (real* tau) (integer* info)))

(define-wrapper int stzrzf_ ((integer* m) (integer* n) (real* a) (integer* lda) (real* tau) (real* work) (integer* lwork) (integer* info)))

(define-wrapper int xerbla_ ((char* srname) (integer* info)))

(define-wrapper int zbdsqr_ ((char* uplo) (integer* n) (integer* ncvt) (integer* nru) (integer* ncc) (doublereal* d__) (doublereal* e) (doublecomplex* vt) (integer* ldvt) (doublecomplex* u) (integer* ldu) (doublecomplex* c__) (integer* ldc) (doublereal* rwork) (integer* info)))

(define-wrapper int zdrot_ ((integer* n) (doublecomplex* cx) (integer* incx) (doublecomplex* cy) (integer* incy) (doublereal* c__) (doublereal* s)))

(define-wrapper int zdrscl_ ((integer* n) (doublereal* sa) (doublecomplex* sx) (integer* incx)))

(define-wrapper int zgbbrd_ ((char* vect) (integer* m) (integer* n) (integer* ncc) (integer* kl) (integer* ku) (doublecomplex* ab) (integer* ldab) (doublereal* d__) (doublereal* e) (doublecomplex* q) (integer* ldq) (doublecomplex* pt) (integer* ldpt) (doublecomplex* c__) (integer* ldc) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zgbcon_ ((char* norm) (integer* n) (integer* kl) (integer* ku) (doublecomplex* ab) (integer* ldab) (integer* ipiv) (doublereal* anorm) (doublereal* rcond) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zgbequ_ ((integer* m) (integer* n) (integer* kl) (integer* ku) (doublecomplex* ab) (integer* ldab) (doublereal* r__) (doublereal* c__) (doublereal* rowcnd) (doublereal* colcnd) (doublereal* amax) (integer* info)))

(define-wrapper int zgbrfs_ ((char* trans) (integer* n) (integer* kl) (integer* ku) (integer* nrhs) (doublecomplex* ab) (integer* ldab) (doublecomplex* afb) (integer* ldafb) (integer* ipiv) (doublecomplex* b) (integer* ldb) (doublecomplex* x) (integer* ldx) (doublereal* ferr) (doublereal* berr) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zgbsv_ ((integer* n) (integer* kl) (integer* ku) (integer* nrhs) (doublecomplex* ab) (integer* ldab) (integer* ipiv) (doublecomplex* b) (integer* ldb) (integer* info)))

(define-wrapper int zgbsvx_ ((char* fact) (char* trans) (integer* n) (integer* kl) (integer* ku) (integer* nrhs) (doublecomplex* ab) (integer* ldab) (doublecomplex* afb) (integer* ldafb) (integer* ipiv) (char* equed) (doublereal* r__) (doublereal* c__) (doublecomplex* b) (integer* ldb) (doublecomplex* x) (integer* ldx) (doublereal* rcond) (doublereal* ferr) (doublereal* berr) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zgbtf2_ ((integer* m) (integer* n) (integer* kl) (integer* ku) (doublecomplex* ab) (integer* ldab) (integer* ipiv) (integer* info)))

(define-wrapper int zgbtrf_ ((integer* m) (integer* n) (integer* kl) (integer* ku) (doublecomplex* ab) (integer* ldab) (integer* ipiv) (integer* info)))

(define-wrapper int zgbtrs_ ((char* trans) (integer* n) (integer* kl) (integer* ku) (integer* nrhs) (doublecomplex* ab) (integer* ldab) (integer* ipiv) (doublecomplex* b) (integer* ldb) (integer* info)))

(define-wrapper int zgebak_ ((char* job) (char* side) (integer* n) (integer* ilo) (integer* ihi) (doublereal* scale) (integer* m) (doublecomplex* v) (integer* ldv) (integer* info)))

(define-wrapper int zgebal_ ((char* job) (integer* n) (doublecomplex* a) (integer* lda) (integer* ilo) (integer* ihi) (doublereal* scale) (integer* info)))

(define-wrapper int zgebd2_ ((integer* m) (integer* n) (doublecomplex* a) (integer* lda) (doublereal* d__) (doublereal* e) (doublecomplex* tauq) (doublecomplex* taup) (doublecomplex* work) (integer* info)))

(define-wrapper int zgebrd_ ((integer* m) (integer* n) (doublecomplex* a) (integer* lda) (doublereal* d__) (doublereal* e) (doublecomplex* tauq) (doublecomplex* taup) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zgecon_ ((char* norm) (integer* n) (doublecomplex* a) (integer* lda) (doublereal* anorm) (doublereal* rcond) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zgeequ_ ((integer* m) (integer* n) (doublecomplex* a) (integer* lda) (doublereal* r__) (doublereal* c__) (doublereal* rowcnd) (doublereal* colcnd) (doublereal* amax) (integer* info)))

(define-wrapper int zgees_ ((char* jobvs) (char* sort) (L_fp select) (integer* n) (doublecomplex* a) (integer* lda) (integer* sdim) (doublecomplex* w) (doublecomplex* vs) (integer* ldvs) (doublecomplex* work) (integer* lwork) (doublereal* rwork) (logical* bwork) (integer* info)))

(define-wrapper int zgeesx_ ((char* jobvs) (char* sort) (L_fp select) (char* sense) (integer* n) (doublecomplex* a) (integer* lda) (integer* sdim) (doublecomplex* w) (doublecomplex* vs) (integer* ldvs) (doublereal* rconde) (doublereal* rcondv) (doublecomplex* work) (integer* lwork) (doublereal* rwork) (logical* bwork) (integer* info)))

(define-wrapper int zgeev_ ((char* jobvl) (char* jobvr) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* w) (doublecomplex* vl) (integer* ldvl) (doublecomplex* vr) (integer* ldvr) (doublecomplex* work) (integer* lwork) (doublereal* rwork) (integer* info)))

(define-wrapper int zgeevx_ ((char* balanc) (char* jobvl) (char* jobvr) (char* sense) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* w) (doublecomplex* vl) (integer* ldvl) (doublecomplex* vr) (integer* ldvr) (integer* ilo) (integer* ihi) (doublereal* scale) (doublereal* abnrm) (doublereal* rconde) (doublereal* rcondv) (doublecomplex* work) (integer* lwork) (doublereal* rwork) (integer* info)))

(define-wrapper int zgegs_ ((char* jobvsl) (char* jobvsr) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (doublecomplex* alpha) (doublecomplex* beta) (doublecomplex* vsl) (integer* ldvsl) (doublecomplex* vsr) (integer* ldvsr) (doublecomplex* work) (integer* lwork) (doublereal* rwork) (integer* info)))

(define-wrapper int zgegv_ ((char* jobvl) (char* jobvr) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (doublecomplex* alpha) (doublecomplex* beta) (doublecomplex* vl) (integer* ldvl) (doublecomplex* vr) (integer* ldvr) (doublecomplex* work) (integer* lwork) (doublereal* rwork) (integer* info)))

(define-wrapper int zgehd2_ ((integer* n) (integer* ilo) (integer* ihi) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* work) (integer* info)))

(define-wrapper int zgehrd_ ((integer* n) (integer* ilo) (integer* ihi) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zgelq2_ ((integer* m) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* work) (integer* info)))

(define-wrapper int zgelqf_ ((integer* m) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zgels_ ((char* trans) (integer* m) (integer* n) (integer* nrhs) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zgelsx_ ((integer* m) (integer* n) (integer* nrhs) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (integer* jpvt) (doublereal* rcond) (integer* rank) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zgelsy_ ((integer* m) (integer* n) (integer* nrhs) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (integer* jpvt) (doublereal* rcond) (integer* rank) (doublecomplex* work) (integer* lwork) (doublereal* rwork) (integer* info)))

(define-wrapper int zgeql2_ ((integer* m) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* work) (integer* info)))

(define-wrapper int zgeqlf_ ((integer* m) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zgeqp3_ ((integer* m) (integer* n) (doublecomplex* a) (integer* lda) (integer* jpvt) (doublecomplex* tau) (doublecomplex* work) (integer* lwork) (doublereal* rwork) (integer* info)))

(define-wrapper int zgeqpf_ ((integer* m) (integer* n) (doublecomplex* a) (integer* lda) (integer* jpvt) (doublecomplex* tau) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zgeqr2_ ((integer* m) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* work) (integer* info)))

(define-wrapper int zgeqrf_ ((integer* m) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zgerfs_ ((char* trans) (integer* n) (integer* nrhs) (doublecomplex* a) (integer* lda) (doublecomplex* af) (integer* ldaf) (integer* ipiv) (doublecomplex* b) (integer* ldb) (doublecomplex* x) (integer* ldx) (doublereal* ferr) (doublereal* berr) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zgerq2_ ((integer* m) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* work) (integer* info)))

(define-wrapper int zgerqf_ ((integer* m) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zgesc2_ ((integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* rhs) (integer* ipiv) (integer* jpiv) (doublereal* scale)))

(define-wrapper int zgesv_ ((integer* n) (integer* nrhs) (doublecomplex* a) (integer* lda) (integer* ipiv) (doublecomplex* b) (integer* ldb) (integer* info)))

(define-wrapper int zgesvx_ ((char* fact) (char* trans) (integer* n) (integer* nrhs) (doublecomplex* a) (integer* lda) (doublecomplex* af) (integer* ldaf) (integer* ipiv) (char* equed) (doublereal* r__) (doublereal* c__) (doublecomplex* b) (integer* ldb) (doublecomplex* x) (integer* ldx) (doublereal* rcond) (doublereal* ferr) (doublereal* berr) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zgetc2_ ((integer* n) (doublecomplex* a) (integer* lda) (integer* ipiv) (integer* jpiv) (integer* info)))

(define-wrapper int zgetf2_ ((integer* m) (integer* n) (doublecomplex* a) (integer* lda) (integer* ipiv) (integer* info)))

(define-wrapper int zgetrf_ ((integer* m) (integer* n) (doublecomplex* a) (integer* lda) (integer* ipiv) (integer* info)))

(define-wrapper int zgetri_ ((integer* n) (doublecomplex* a) (integer* lda) (integer* ipiv) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zgetrs_ ((char* trans) (integer* n) (integer* nrhs) (doublecomplex* a) (integer* lda) (integer* ipiv) (doublecomplex* b) (integer* ldb) (integer* info)))

(define-wrapper int zggbak_ ((char* job) (char* side) (integer* n) (integer* ilo) (integer* ihi) (doublereal* lscale) (doublereal* rscale) (integer* m) (doublecomplex* v) (integer* ldv) (integer* info)))

(define-wrapper int zggbal_ ((char* job) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (integer* ilo) (integer* ihi) (doublereal* lscale) (doublereal* rscale) (doublereal* work) (integer* info)))

(define-wrapper int zgges_ ((char* jobvsl) (char* jobvsr) (char* sort) (L_fp delctg) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (integer* sdim) (doublecomplex* alpha) (doublecomplex* beta) (doublecomplex* vsl) (integer* ldvsl) (doublecomplex* vsr) (integer* ldvsr) (doublecomplex* work) (integer* lwork) (doublereal* rwork) (logical* bwork) (integer* info)))

(define-wrapper int zggesx_ ((char* jobvsl) (char* jobvsr) (char* sort) (L_fp delctg) (char* sense) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (integer* sdim) (doublecomplex* alpha) (doublecomplex* beta) (doublecomplex* vsl) (integer* ldvsl) (doublecomplex* vsr) (integer* ldvsr) (doublereal* rconde) (doublereal* rcondv) (doublecomplex* work) (integer* lwork) (doublereal* rwork) (integer* iwork) (integer* liwork) (logical* bwork) (integer* info)))

(define-wrapper int zggev_ ((char* jobvl) (char* jobvr) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (doublecomplex* alpha) (doublecomplex* beta) (doublecomplex* vl) (integer* ldvl) (doublecomplex* vr) (integer* ldvr) (doublecomplex* work) (integer* lwork) (doublereal* rwork) (integer* info)))

(define-wrapper int zggevx_ ((char* balanc) (char* jobvl) (char* jobvr) (char* sense) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (doublecomplex* alpha) (doublecomplex* beta) (doublecomplex* vl) (integer* ldvl) (doublecomplex* vr) (integer* ldvr) (integer* ilo) (integer* ihi) (doublereal* lscale) (doublereal* rscale) (doublereal* abnrm) (doublereal* bbnrm) (doublereal* rconde) (doublereal* rcondv) (doublecomplex* work) (integer* lwork) (doublereal* rwork) (integer* iwork) (logical* bwork) (integer* info)))

(define-wrapper int zggglm_ ((integer* n) (integer* m) (integer* p) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (doublecomplex* d__) (doublecomplex* x) (doublecomplex* y) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zgghrd_ ((char* compq) (char* compz) (integer* n) (integer* ilo) (integer* ihi) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (doublecomplex* q) (integer* ldq) (doublecomplex* z__) (integer* ldz) (integer* info)))

(define-wrapper int zgglse_ ((integer* m) (integer* n) (integer* p) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (doublecomplex* c__) (doublecomplex* d__) (doublecomplex* x) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zggqrf_ ((integer* n) (integer* m) (integer* p) (doublecomplex* a) (integer* lda) (doublecomplex* taua) (doublecomplex* b) (integer* ldb) (doublecomplex* taub) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zggrqf_ ((integer* m) (integer* p) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* taua) (doublecomplex* b) (integer* ldb) (doublecomplex* taub) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zggsvd_ ((char* jobu) (char* jobv) (char* jobq) (integer* m) (integer* n) (integer* p) (integer* k) (integer* l) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (doublereal* alpha) (doublereal* beta) (doublecomplex* u) (integer* ldu) (doublecomplex* v) (integer* ldv) (doublecomplex* q) (integer* ldq) (doublecomplex* work) (doublereal* rwork) (integer* iwork) (integer* info)))

(define-wrapper int zggsvp_ ((char* jobu) (char* jobv) (char* jobq) (integer* m) (integer* p) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (doublereal* tola) (doublereal* tolb) (integer* k) (integer* l) (doublecomplex* u) (integer* ldu) (doublecomplex* v) (integer* ldv) (doublecomplex* q) (integer* ldq) (integer* iwork) (doublereal* rwork) (doublecomplex* tau) (doublecomplex* work) (integer* info)))

(define-wrapper int zgtcon_ ((char* norm) (integer* n) (doublecomplex* dl) (doublecomplex* d__) (doublecomplex* du) (doublecomplex* du2) (integer* ipiv) (doublereal* anorm) (doublereal* rcond) (doublecomplex* work) (integer* info)))

(define-wrapper int zgtrfs_ ((char* trans) (integer* n) (integer* nrhs) (doublecomplex* dl) (doublecomplex* d__) (doublecomplex* du) (doublecomplex* dlf) (doublecomplex* df) (doublecomplex* duf) (doublecomplex* du2) (integer* ipiv) (doublecomplex* b) (integer* ldb) (doublecomplex* x) (integer* ldx) (doublereal* ferr) (doublereal* berr) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zgtsv_ ((integer* n) (integer* nrhs) (doublecomplex* dl) (doublecomplex* d__) (doublecomplex* du) (doublecomplex* b) (integer* ldb) (integer* info)))

(define-wrapper int zgtsvx_ ((char* fact) (char* trans) (integer* n) (integer* nrhs) (doublecomplex* dl) (doublecomplex* d__) (doublecomplex* du) (doublecomplex* dlf) (doublecomplex* df) (doublecomplex* duf) (doublecomplex* du2) (integer* ipiv) (doublecomplex* b) (integer* ldb) (doublecomplex* x) (integer* ldx) (doublereal* rcond) (doublereal* ferr) (doublereal* berr) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zgttrf_ ((integer* n) (doublecomplex* dl) (doublecomplex* d__) (doublecomplex* du) (doublecomplex* du2) (integer* ipiv) (integer* info)))

(define-wrapper int zgttrs_ ((char* trans) (integer* n) (integer* nrhs) (doublecomplex* dl) (doublecomplex* d__) (doublecomplex* du) (doublecomplex* du2) (integer* ipiv) (doublecomplex* b) (integer* ldb) (integer* info)))

(define-wrapper int zgtts2_ ((integer* itrans) (integer* n) (integer* nrhs) (doublecomplex* dl) (doublecomplex* d__) (doublecomplex* du) (doublecomplex* du2) (integer* ipiv) (doublecomplex* b) (integer* ldb)))

(define-wrapper int zhbev_ ((char* jobz) (char* uplo) (integer* n) (integer* kd) (doublecomplex* ab) (integer* ldab) (doublereal* w) (doublecomplex* z__) (integer* ldz) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zhbevd_ ((char* jobz) (char* uplo) (integer* n) (integer* kd) (doublecomplex* ab) (integer* ldab) (doublereal* w) (doublecomplex* z__) (integer* ldz) (doublecomplex* work) (integer* lwork) (doublereal* rwork) (integer* lrwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int zhbevx_ ((char* jobz) (char* range) (char* uplo) (integer* n) (integer* kd) (doublecomplex* ab) (integer* ldab) (doublecomplex* q) (integer* ldq) (doublereal* vl) (doublereal* vu) (integer* il) (integer* iu) (doublereal* abstol) (integer* m) (doublereal* w) (doublecomplex* z__) (integer* ldz) (doublecomplex* work) (doublereal* rwork) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int zhbgst_ ((char* vect) (char* uplo) (integer* n) (integer* ka) (integer* kb) (doublecomplex* ab) (integer* ldab) (doublecomplex* bb) (integer* ldbb) (doublecomplex* x) (integer* ldx) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zhbgv_ ((char* jobz) (char* uplo) (integer* n) (integer* ka) (integer* kb) (doublecomplex* ab) (integer* ldab) (doublecomplex* bb) (integer* ldbb) (doublereal* w) (doublecomplex* z__) (integer* ldz) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zhbgvx_ ((char* jobz) (char* range) (char* uplo) (integer* n) (integer* ka) (integer* kb) (doublecomplex* ab) (integer* ldab) (doublecomplex* bb) (integer* ldbb) (doublecomplex* q) (integer* ldq) (doublereal* vl) (doublereal* vu) (integer* il) (integer* iu) (doublereal* abstol) (integer* m) (doublereal* w) (doublecomplex* z__) (integer* ldz) (doublecomplex* work) (doublereal* rwork) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int zhbtrd_ ((char* vect) (char* uplo) (integer* n) (integer* kd) (doublecomplex* ab) (integer* ldab) (doublereal* d__) (doublereal* e) (doublecomplex* q) (integer* ldq) (doublecomplex* work) (integer* info)))

(define-wrapper int zhecon_ ((char* uplo) (integer* n) (doublecomplex* a) (integer* lda) (integer* ipiv) (doublereal* anorm) (doublereal* rcond) (doublecomplex* work) (integer* info)))

(define-wrapper int zheev_ ((char* jobz) (char* uplo) (integer* n) (doublecomplex* a) (integer* lda) (doublereal* w) (doublecomplex* work) (integer* lwork) (doublereal* rwork) (integer* info)))

(define-wrapper int zheevd_ ((char* jobz) (char* uplo) (integer* n) (doublecomplex* a) (integer* lda) (doublereal* w) (doublecomplex* work) (integer* lwork) (doublereal* rwork) (integer* lrwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int zheevr_ ((char* jobz) (char* range) (char* uplo) (integer* n) (doublecomplex* a) (integer* lda) (doublereal* vl) (doublereal* vu) (integer* il) (integer* iu) (doublereal* abstol) (integer* m) (doublereal* w) (doublecomplex* z__) (integer* ldz) (integer* isuppz) (doublecomplex* work) (integer* lwork) (doublereal* rwork) (integer* lrwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int zheevx_ ((char* jobz) (char* range) (char* uplo) (integer* n) (doublecomplex* a) (integer* lda) (doublereal* vl) (doublereal* vu) (integer* il) (integer* iu) (doublereal* abstol) (integer* m) (doublereal* w) (doublecomplex* z__) (integer* ldz) (doublecomplex* work) (integer* lwork) (doublereal* rwork) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int zhegs2_ ((integer* itype) (char* uplo) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (integer* info)))

(define-wrapper int zhegst_ ((integer* itype) (char* uplo) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (integer* info)))

(define-wrapper int zhegv_ ((integer* itype) (char* jobz) (char* uplo) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (doublereal* w) (doublecomplex* work) (integer* lwork) (doublereal* rwork) (integer* info)))

(define-wrapper int zhegvd_ ((integer* itype) (char* jobz) (char* uplo) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (doublereal* w) (doublecomplex* work) (integer* lwork) (doublereal* rwork) (integer* lrwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int zhegvx_ ((integer* itype) (char* jobz) (char* range) (char* uplo) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (doublereal* vl) (doublereal* vu) (integer* il) (integer* iu) (doublereal* abstol) (integer* m) (doublereal* w) (doublecomplex* z__) (integer* ldz) (doublecomplex* work) (integer* lwork) (doublereal* rwork) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int zherfs_ ((char* uplo) (integer* n) (integer* nrhs) (doublecomplex* a) (integer* lda) (doublecomplex* af) (integer* ldaf) (integer* ipiv) (doublecomplex* b) (integer* ldb) (doublecomplex* x) (integer* ldx) (doublereal* ferr) (doublereal* berr) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zhesv_ ((char* uplo) (integer* n) (integer* nrhs) (doublecomplex* a) (integer* lda) (integer* ipiv) (doublecomplex* b) (integer* ldb) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zhesvx_ ((char* fact) (char* uplo) (integer* n) (integer* nrhs) (doublecomplex* a) (integer* lda) (doublecomplex* af) (integer* ldaf) (integer* ipiv) (doublecomplex* b) (integer* ldb) (doublecomplex* x) (integer* ldx) (doublereal* rcond) (doublereal* ferr) (doublereal* berr) (doublecomplex* work) (integer* lwork) (doublereal* rwork) (integer* info)))

(define-wrapper int zhetf2_ ((char* uplo) (integer* n) (doublecomplex* a) (integer* lda) (integer* ipiv) (integer* info)))

(define-wrapper int zhetrd_ ((char* uplo) (integer* n) (doublecomplex* a) (integer* lda) (doublereal* d__) (doublereal* e) (doublecomplex* tau) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zhetrf_ ((char* uplo) (integer* n) (doublecomplex* a) (integer* lda) (integer* ipiv) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zhetri_ ((char* uplo) (integer* n) (doublecomplex* a) (integer* lda) (integer* ipiv) (doublecomplex* work) (integer* info)))

(define-wrapper int zhetrs_ ((char* uplo) (integer* n) (integer* nrhs) (doublecomplex* a) (integer* lda) (integer* ipiv) (doublecomplex* b) (integer* ldb) (integer* info)))

(define-wrapper int zhgeqz_ ((char* job) (char* compq) (char* compz) (integer* n) (integer* ilo) (integer* ihi) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (doublecomplex* alpha) (doublecomplex* beta) (doublecomplex* q) (integer* ldq) (doublecomplex* z__) (integer* ldz) (doublecomplex* work) (integer* lwork) (doublereal* rwork) (integer* info)))

(define-wrapper int zhpcon_ ((char* uplo) (integer* n) (doublecomplex* ap) (integer* ipiv) (doublereal* anorm) (doublereal* rcond) (doublecomplex* work) (integer* info)))

(define-wrapper int zhpev_ ((char* jobz) (char* uplo) (integer* n) (doublecomplex* ap) (doublereal* w) (doublecomplex* z__) (integer* ldz) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zhpevd_ ((char* jobz) (char* uplo) (integer* n) (doublecomplex* ap) (doublereal* w) (doublecomplex* z__) (integer* ldz) (doublecomplex* work) (integer* lwork) (doublereal* rwork) (integer* lrwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int zhpevx_ ((char* jobz) (char* range) (char* uplo) (integer* n) (doublecomplex* ap) (doublereal* vl) (doublereal* vu) (integer* il) (integer* iu) (doublereal* abstol) (integer* m) (doublereal* w) (doublecomplex* z__) (integer* ldz) (doublecomplex* work) (doublereal* rwork) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int zhpgst_ ((integer* itype) (char* uplo) (integer* n) (doublecomplex* ap) (doublecomplex* bp) (integer* info)))

(define-wrapper int zhpgv_ ((integer* itype) (char* jobz) (char* uplo) (integer* n) (doublecomplex* ap) (doublecomplex* bp) (doublereal* w) (doublecomplex* z__) (integer* ldz) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zhpgvd_ ((integer* itype) (char* jobz) (char* uplo) (integer* n) (doublecomplex* ap) (doublecomplex* bp) (doublereal* w) (doublecomplex* z__) (integer* ldz) (doublecomplex* work) (integer* lwork) (doublereal* rwork) (integer* lrwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int zhpgvx_ ((integer* itype) (char* jobz) (char* range) (char* uplo) (integer* n) (doublecomplex* ap) (doublecomplex* bp) (doublereal* vl) (doublereal* vu) (integer* il) (integer* iu) (doublereal* abstol) (integer* m) (doublereal* w) (doublecomplex* z__) (integer* ldz) (doublecomplex* work) (doublereal* rwork) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int zhprfs_ ((char* uplo) (integer* n) (integer* nrhs) (doublecomplex* ap) (doublecomplex* afp) (integer* ipiv) (doublecomplex* b) (integer* ldb) (doublecomplex* x) (integer* ldx) (doublereal* ferr) (doublereal* berr) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zhpsv_ ((char* uplo) (integer* n) (integer* nrhs) (doublecomplex* ap) (integer* ipiv) (doublecomplex* b) (integer* ldb) (integer* info)))

(define-wrapper int zhpsvx_ ((char* fact) (char* uplo) (integer* n) (integer* nrhs) (doublecomplex* ap) (doublecomplex* afp) (integer* ipiv) (doublecomplex* b) (integer* ldb) (doublecomplex* x) (integer* ldx) (doublereal* rcond) (doublereal* ferr) (doublereal* berr) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zhptrd_ ((char* uplo) (integer* n) (doublecomplex* ap) (doublereal* d__) (doublereal* e) (doublecomplex* tau) (integer* info)))

(define-wrapper int zhptrf_ ((char* uplo) (integer* n) (doublecomplex* ap) (integer* ipiv) (integer* info)))

(define-wrapper int zhptri_ ((char* uplo) (integer* n) (doublecomplex* ap) (integer* ipiv) (doublecomplex* work) (integer* info)))

(define-wrapper int zhptrs_ ((char* uplo) (integer* n) (integer* nrhs) (doublecomplex* ap) (integer* ipiv) (doublecomplex* b) (integer* ldb) (integer* info)))

(define-wrapper int zhsein_ ((char* side) (char* eigsrc) (char* initv) (logical* select) (integer* n) (doublecomplex* h__) (integer* ldh) (doublecomplex* w) (doublecomplex* vl) (integer* ldvl) (doublecomplex* vr) (integer* ldvr) (integer* mm) (integer* m) (doublecomplex* work) (doublereal* rwork) (integer* ifaill) (integer* ifailr) (integer* info)))

(define-wrapper int zhseqr_ ((char* job) (char* compz) (integer* n) (integer* ilo) (integer* ihi) (doublecomplex* h__) (integer* ldh) (doublecomplex* w) (doublecomplex* z__) (integer* ldz) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zlabrd_ ((integer* m) (integer* n) (integer* nb) (doublecomplex* a) (integer* lda) (doublereal* d__) (doublereal* e) (doublecomplex* tauq) (doublecomplex* taup) (doublecomplex* x) (integer* ldx) (doublecomplex* y) (integer* ldy)))

(define-wrapper int zlacgv_ ((integer* n) (doublecomplex* x) (integer* incx)))

(define-wrapper int zlacon_ ((integer* n) (doublecomplex* v) (doublecomplex* x) (doublereal* est) (integer* kase)))

(define-wrapper int zlacp2_ ((char* uplo) (integer* m) (integer* n) (doublereal* a) (integer* lda) (doublecomplex* b) (integer* ldb)))

(define-wrapper int zlacpy_ ((char* uplo) (integer* m) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb)))

(define-wrapper int zlacrm_ ((integer* m) (integer* n) (doublecomplex* a) (integer* lda) (doublereal* b) (integer* ldb) (doublecomplex* c__) (integer* ldc) (doublereal* rwork)))

(define-wrapper int zlacrt_ ((integer* n) (doublecomplex* cx) (integer* incx) (doublecomplex* cy) (integer* incy) (doublecomplex* c__) (doublecomplex* s)))

(define-wrapper int zlaed0_ ((integer* qsiz) (integer* n) (doublereal* d__) (doublereal* e) (doublecomplex* q) (integer* ldq) (doublecomplex* qstore) (integer* ldqs) (doublereal* rwork) (integer* iwork) (integer* info)))

(define-wrapper int zlaed7_ ((integer* n) (integer* cutpnt) (integer* qsiz) (integer* tlvls) (integer* curlvl) (integer* curpbm) (doublereal* d__) (doublecomplex* q) (integer* ldq) (doublereal* rho) (integer* indxq) (doublereal* qstore) (integer* qptr) (integer* prmptr) (integer* perm) (integer* givptr) (integer* givcol) (doublereal* givnum) (doublecomplex* work) (doublereal* rwork) (integer* iwork) (integer* info)))

(define-wrapper int zlaed8_ ((integer* k) (integer* n) (integer* qsiz) (doublecomplex* q) (integer* ldq) (doublereal* d__) (doublereal* rho) (integer* cutpnt) (doublereal* z__) (doublereal* dlamda) (doublecomplex* q2) (integer* ldq2) (doublereal* w) (integer* indxp) (integer* indx) (integer* indxq) (integer* perm) (integer* givptr) (integer* givcol) (doublereal* givnum) (integer* info)))

(define-wrapper int zlaein_ ((logical* rightv) (logical* noinit) (integer* n) (doublecomplex* h__) (integer* ldh) (doublecomplex* w) (doublecomplex* v) (doublecomplex* b) (integer* ldb) (doublereal* rwork) (doublereal* eps3) (doublereal* smlnum) (integer* info)))

(define-wrapper int zlaesy_ ((doublecomplex* a) (doublecomplex* b) (doublecomplex* c__) (doublecomplex* rt1) (doublecomplex* rt2) (doublecomplex* evscal) (doublecomplex* cs1) (doublecomplex* sn1)))

(define-wrapper int zlaev2_ ((doublecomplex* a) (doublecomplex* b) (doublecomplex* c__) (doublereal* rt1) (doublereal* rt2) (doublereal* cs1) (doublecomplex* sn1)))

(define-wrapper int zlags2_ ((logical* upper) (doublereal* a1) (doublecomplex* a2) (doublereal* a3) (doublereal* b1) (doublecomplex* b2) (doublereal* b3) (doublereal* csu) (doublecomplex* snu) (doublereal* csv) (doublecomplex* snv) (doublereal* csq) (doublecomplex* snq)))

(define-wrapper int zlagtm_ ((char* trans) (integer* n) (integer* nrhs) (doublereal* alpha) (doublecomplex* dl) (doublecomplex* d__) (doublecomplex* du) (doublecomplex* x) (integer* ldx) (doublereal* beta) (doublecomplex* b) (integer* ldb)))

(define-wrapper int zlahef_ ((char* uplo) (integer* n) (integer* nb) (integer* kb) (doublecomplex* a) (integer* lda) (integer* ipiv) (doublecomplex* w) (integer* ldw) (integer* info)))

(define-wrapper int zlahqr_ ((logical* wantt) (logical* wantz) (integer* n) (integer* ilo) (integer* ihi) (doublecomplex* h__) (integer* ldh) (doublecomplex* w) (integer* iloz) (integer* ihiz) (doublecomplex* z__) (integer* ldz) (integer* info)))

(define-wrapper int zlahrd_ ((integer* n) (integer* k) (integer* nb) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* t) (integer* ldt) (doublecomplex* y) (integer* ldy)))

(define-wrapper int zlaic1_ ((integer* job) (integer* j) (doublecomplex* x) (doublereal* sest) (doublecomplex* w) (doublecomplex* gamma) (doublereal* sestpr) (doublecomplex* s) (doublecomplex* c__)))

(define-wrapper int zlals0_ ((integer* icompq) (integer* nl) (integer* nr) (integer* sqre) (integer* nrhs) (doublecomplex* b) (integer* ldb) (doublecomplex* bx) (integer* ldbx) (integer* perm) (integer* givptr) (integer* givcol) (integer* ldgcol) (doublereal* givnum) (integer* ldgnum) (doublereal* poles) (doublereal* difl) (doublereal* difr) (doublereal* z__) (integer* k) (doublereal* c__) (doublereal* s) (doublereal* rwork) (integer* info)))

(define-wrapper int zlalsa_ ((integer* icompq) (integer* smlsiz) (integer* n) (integer* nrhs) (doublecomplex* b) (integer* ldb) (doublecomplex* bx) (integer* ldbx) (doublereal* u) (integer* ldu) (doublereal* vt) (integer* k) (doublereal* difl) (doublereal* difr) (doublereal* z__) (doublereal* poles) (integer* givptr) (integer* givcol) (integer* ldgcol) (integer* perm) (doublereal* givnum) (doublereal* c__) (doublereal* s) (doublereal* rwork) (integer* iwork) (integer* info)))

(define-wrapper int zlapll_ ((integer* n) (doublecomplex* x) (integer* incx) (doublecomplex* y) (integer* incy) (doublereal* ssmin)))

(define-wrapper int zlapmt_ ((logical* forwrd) (integer* m) (integer* n) (doublecomplex* x) (integer* ldx) (integer* k)))

(define-wrapper int zlaqgb_ ((integer* m) (integer* n) (integer* kl) (integer* ku) (doublecomplex* ab) (integer* ldab) (doublereal* r__) (doublereal* c__) (doublereal* rowcnd) (doublereal* colcnd) (doublereal* amax) (char* equed)))

(define-wrapper int zlaqge_ ((integer* m) (integer* n) (doublecomplex* a) (integer* lda) (doublereal* r__) (doublereal* c__) (doublereal* rowcnd) (doublereal* colcnd) (doublereal* amax) (char* equed)))

(define-wrapper int zlaqhb_ ((char* uplo) (integer* n) (integer* kd) (doublecomplex* ab) (integer* ldab) (doublereal* s) (doublereal* scond) (doublereal* amax) (char* equed)))

(define-wrapper int zlaqhe_ ((char* uplo) (integer* n) (doublecomplex* a) (integer* lda) (doublereal* s) (doublereal* scond) (doublereal* amax) (char* equed)))

(define-wrapper int zlaqhp_ ((char* uplo) (integer* n) (doublecomplex* ap) (doublereal* s) (doublereal* scond) (doublereal* amax) (char* equed)))

(define-wrapper int zlaqp2_ ((integer* m) (integer* n) (integer* offset) (doublecomplex* a) (integer* lda) (integer* jpvt) (doublecomplex* tau) (doublereal* vn1) (doublereal* vn2) (doublecomplex* work)))

(define-wrapper int zlaqps_ ((integer* m) (integer* n) (integer* offset) (integer* nb) (integer* kb) (doublecomplex* a) (integer* lda) (integer* jpvt) (doublecomplex* tau) (doublereal* vn1) (doublereal* vn2) (doublecomplex* auxv) (doublecomplex* f) (integer* ldf)))

(define-wrapper int zlaqsb_ ((char* uplo) (integer* n) (integer* kd) (doublecomplex* ab) (integer* ldab) (doublereal* s) (doublereal* scond) (doublereal* amax) (char* equed)))

(define-wrapper int zlaqsp_ ((char* uplo) (integer* n) (doublecomplex* ap) (doublereal* s) (doublereal* scond) (doublereal* amax) (char* equed)))

(define-wrapper int zlaqsy_ ((char* uplo) (integer* n) (doublecomplex* a) (integer* lda) (doublereal* s) (doublereal* scond) (doublereal* amax) (char* equed)))

(define-wrapper int zlar1v_ ((integer* n) (integer* b1) (integer* bn) (doublereal* sigma) (doublereal* d__) (doublereal* l) (doublereal* ld) (doublereal* lld) (doublereal* gersch) (doublecomplex* z__) (doublereal* ztz) (doublereal* mingma) (integer* r__) (integer* isuppz) (doublereal* work)))

(define-wrapper int zlar2v_ ((integer* n) (doublecomplex* x) (doublecomplex* y) (doublecomplex* z__) (integer* incx) (doublereal* c__) (doublecomplex* s) (integer* incc)))

(define-wrapper int zlarcm_ ((integer* m) (integer* n) (doublereal* a) (integer* lda) (doublecomplex* b) (integer* ldb) (doublecomplex* c__) (integer* ldc) (doublereal* rwork)))

(define-wrapper int zlarf_ ((char* side) (integer* m) (integer* n) (doublecomplex* v) (integer* incv) (doublecomplex* tau) (doublecomplex* c__) (integer* ldc) (doublecomplex* work)))

(define-wrapper int zlarfb_ ((char* side) (char* trans) (char* direct) (char* storev) (integer* m) (integer* n) (integer* k) (doublecomplex* v) (integer* ldv) (doublecomplex* t) (integer* ldt) (doublecomplex* c__) (integer* ldc) (doublecomplex* work) (integer* ldwork)))

(define-wrapper int zlarfg_ ((integer* n) (doublecomplex* alpha) (doublecomplex* x) (integer* incx) (doublecomplex* tau)))

(define-wrapper int zlarft_ ((char* direct) (char* storev) (integer* n) (integer* k) (doublecomplex* v) (integer* ldv) (doublecomplex* tau) (doublecomplex* t) (integer* ldt)))

(define-wrapper int zlarfx_ ((char* side) (integer* m) (integer* n) (doublecomplex* v) (doublecomplex* tau) (doublecomplex* c__) (integer* ldc) (doublecomplex* work)))

(define-wrapper int zlargv_ ((integer* n) (doublecomplex* x) (integer* incx) (doublecomplex* y) (integer* incy) (doublereal* c__) (integer* incc)))

(define-wrapper int zlarnv_ ((integer* idist) (integer* iseed) (integer* n) (doublecomplex* x)))

(define-wrapper int zlarrv_ ((integer* n) (doublereal* d__) (doublereal* l) (integer* isplit) (integer* m) (doublereal* w) (integer* iblock) (doublereal* gersch) (doublereal* tol) (doublecomplex* z__) (integer* ldz) (integer* isuppz) (doublereal* work) (integer* iwork) (integer* info)))

(define-wrapper int zlartg_ ((doublecomplex* f) (doublecomplex* g) (doublereal* cs) (doublecomplex* sn) (doublecomplex* r__)))

(define-wrapper int zlartv_ ((integer* n) (doublecomplex* x) (integer* incx) (doublecomplex* y) (integer* incy) (doublereal* c__) (doublecomplex* s) (integer* incc)))

(define-wrapper int zlarz_ ((char* side) (integer* m) (integer* n) (integer* l) (doublecomplex* v) (integer* incv) (doublecomplex* tau) (doublecomplex* c__) (integer* ldc) (doublecomplex* work)))

(define-wrapper int zlarzb_ ((char* side) (char* trans) (char* direct) (char* storev) (integer* m) (integer* n) (integer* k) (integer* l) (doublecomplex* v) (integer* ldv) (doublecomplex* t) (integer* ldt) (doublecomplex* c__) (integer* ldc) (doublecomplex* work) (integer* ldwork)))

(define-wrapper int zlarzt_ ((char* direct) (char* storev) (integer* n) (integer* k) (doublecomplex* v) (integer* ldv) (doublecomplex* tau) (doublecomplex* t) (integer* ldt)))

(define-wrapper int zlascl_ ((char* type__) (integer* kl) (integer* ku) (doublereal* cfrom) (doublereal* cto) (integer* m) (integer* n) (doublecomplex* a) (integer* lda) (integer* info)))

(define-wrapper int zlaset_ ((char* uplo) (integer* m) (integer* n) (doublecomplex* alpha) (doublecomplex* beta) (doublecomplex* a) (integer* lda)))

(define-wrapper int zlasr_ ((char* side) (char* pivot) (char* direct) (integer* m) (integer* n) (doublereal* c__) (doublereal* s) (doublecomplex* a) (integer* lda)))

(define-wrapper int zlassq_ ((integer* n) (doublecomplex* x) (integer* incx) (doublereal* scale) (doublereal* sumsq)))

(define-wrapper int zlaswp_ ((integer* n) (doublecomplex* a) (integer* lda) (integer* k1) (integer* k2) (integer* ipiv) (integer* incx)))

(define-wrapper int zlasyf_ ((char* uplo) (integer* n) (integer* nb) (integer* kb) (doublecomplex* a) (integer* lda) (integer* ipiv) (doublecomplex* w) (integer* ldw) (integer* info)))

(define-wrapper int zlatbs_ ((char* uplo) (char* trans) (char* diag) (char* normin) (integer* n) (integer* kd) (doublecomplex* ab) (integer* ldab) (doublecomplex* x) (doublereal* scale) (doublereal* cnorm) (integer* info)))

(define-wrapper int zlatdf_ ((integer* ijob) (integer* n) (doublecomplex* z__) (integer* ldz) (doublecomplex* rhs) (doublereal* rdsum) (doublereal* rdscal) (integer* ipiv) (integer* jpiv)))

(define-wrapper int zlatps_ ((char* uplo) (char* trans) (char* diag) (char* normin) (integer* n) (doublecomplex* ap) (doublecomplex* x) (doublereal* scale) (doublereal* cnorm) (integer* info)))

(define-wrapper int zlatrd_ ((char* uplo) (integer* n) (integer* nb) (doublecomplex* a) (integer* lda) (doublereal* e) (doublecomplex* tau) (doublecomplex* w) (integer* ldw)))

(define-wrapper int zlatrs_ ((char* uplo) (char* trans) (char* diag) (char* normin) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* x) (doublereal* scale) (doublereal* cnorm) (integer* info)))

(define-wrapper int zlatrz_ ((integer* m) (integer* n) (integer* l) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* work)))

(define-wrapper int zlatzm_ ((char* side) (integer* m) (integer* n) (doublecomplex* v) (integer* incv) (doublecomplex* tau) (doublecomplex* c1) (doublecomplex* c2) (integer* ldc) (doublecomplex* work)))

(define-wrapper int zlauu2_ ((char* uplo) (integer* n) (doublecomplex* a) (integer* lda) (integer* info)))

(define-wrapper int zlauum_ ((char* uplo) (integer* n) (doublecomplex* a) (integer* lda) (integer* info)))

(define-wrapper int zpbcon_ ((char* uplo) (integer* n) (integer* kd) (doublecomplex* ab) (integer* ldab) (doublereal* anorm) (doublereal* rcond) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zpbequ_ ((char* uplo) (integer* n) (integer* kd) (doublecomplex* ab) (integer* ldab) (doublereal* s) (doublereal* scond) (doublereal* amax) (integer* info)))

(define-wrapper int zpbrfs_ ((char* uplo) (integer* n) (integer* kd) (integer* nrhs) (doublecomplex* ab) (integer* ldab) (doublecomplex* afb) (integer* ldafb) (doublecomplex* b) (integer* ldb) (doublecomplex* x) (integer* ldx) (doublereal* ferr) (doublereal* berr) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zpbstf_ ((char* uplo) (integer* n) (integer* kd) (doublecomplex* ab) (integer* ldab) (integer* info)))

(define-wrapper int zpbsv_ ((char* uplo) (integer* n) (integer* kd) (integer* nrhs) (doublecomplex* ab) (integer* ldab) (doublecomplex* b) (integer* ldb) (integer* info)))

(define-wrapper int zpbsvx_ ((char* fact) (char* uplo) (integer* n) (integer* kd) (integer* nrhs) (doublecomplex* ab) (integer* ldab) (doublecomplex* afb) (integer* ldafb) (char* equed) (doublereal* s) (doublecomplex* b) (integer* ldb) (doublecomplex* x) (integer* ldx) (doublereal* rcond) (doublereal* ferr) (doublereal* berr) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zpbtf2_ ((char* uplo) (integer* n) (integer* kd) (doublecomplex* ab) (integer* ldab) (integer* info)))

(define-wrapper int zpbtrf_ ((char* uplo) (integer* n) (integer* kd) (doublecomplex* ab) (integer* ldab) (integer* info)))

(define-wrapper int zpbtrs_ ((char* uplo) (integer* n) (integer* kd) (integer* nrhs) (doublecomplex* ab) (integer* ldab) (doublecomplex* b) (integer* ldb) (integer* info)))

(define-wrapper int zpocon_ ((char* uplo) (integer* n) (doublecomplex* a) (integer* lda) (doublereal* anorm) (doublereal* rcond) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zpoequ_ ((integer* n) (doublecomplex* a) (integer* lda) (doublereal* s) (doublereal* scond) (doublereal* amax) (integer* info)))

(define-wrapper int zporfs_ ((char* uplo) (integer* n) (integer* nrhs) (doublecomplex* a) (integer* lda) (doublecomplex* af) (integer* ldaf) (doublecomplex* b) (integer* ldb) (doublecomplex* x) (integer* ldx) (doublereal* ferr) (doublereal* berr) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zposv_ ((char* uplo) (integer* n) (integer* nrhs) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (integer* info)))

(define-wrapper int zposvx_ ((char* fact) (char* uplo) (integer* n) (integer* nrhs) (doublecomplex* a) (integer* lda) (doublecomplex* af) (integer* ldaf) (char* equed) (doublereal* s) (doublecomplex* b) (integer* ldb) (doublecomplex* x) (integer* ldx) (doublereal* rcond) (doublereal* ferr) (doublereal* berr) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zpotf2_ ((char* uplo) (integer* n) (doublecomplex* a) (integer* lda) (integer* info)))

(define-wrapper int zpotrf_ ((char* uplo) (integer* n) (doublecomplex* a) (integer* lda) (integer* info)))

(define-wrapper int zpotri_ ((char* uplo) (integer* n) (doublecomplex* a) (integer* lda) (integer* info)))

(define-wrapper int zpotrs_ ((char* uplo) (integer* n) (integer* nrhs) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (integer* info)))

(define-wrapper int zppcon_ ((char* uplo) (integer* n) (doublecomplex* ap) (doublereal* anorm) (doublereal* rcond) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zppequ_ ((char* uplo) (integer* n) (doublecomplex* ap) (doublereal* s) (doublereal* scond) (doublereal* amax) (integer* info)))

(define-wrapper int zpprfs_ ((char* uplo) (integer* n) (integer* nrhs) (doublecomplex* ap) (doublecomplex* afp) (doublecomplex* b) (integer* ldb) (doublecomplex* x) (integer* ldx) (doublereal* ferr) (doublereal* berr) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zppsv_ ((char* uplo) (integer* n) (integer* nrhs) (doublecomplex* ap) (doublecomplex* b) (integer* ldb) (integer* info)))

(define-wrapper int zppsvx_ ((char* fact) (char* uplo) (integer* n) (integer* nrhs) (doublecomplex* ap) (doublecomplex* afp) (char* equed) (doublereal* s) (doublecomplex* b) (integer* ldb) (doublecomplex* x) (integer* ldx) (doublereal* rcond) (doublereal* ferr) (doublereal* berr) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zpptrf_ ((char* uplo) (integer* n) (doublecomplex* ap) (integer* info)))

(define-wrapper int zpptri_ ((char* uplo) (integer* n) (doublecomplex* ap) (integer* info)))

(define-wrapper int zpptrs_ ((char* uplo) (integer* n) (integer* nrhs) (doublecomplex* ap) (doublecomplex* b) (integer* ldb) (integer* info)))

(define-wrapper int zptcon_ ((integer* n) (doublereal* d__) (doublecomplex* e) (doublereal* anorm) (doublereal* rcond) (doublereal* rwork) (integer* info)))

(define-wrapper int zptrfs_ ((char* uplo) (integer* n) (integer* nrhs) (doublereal* d__) (doublecomplex* e) (doublereal* df) (doublecomplex* ef) (doublecomplex* b) (integer* ldb) (doublecomplex* x) (integer* ldx) (doublereal* ferr) (doublereal* berr) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zptsv_ ((integer* n) (integer* nrhs) (doublereal* d__) (doublecomplex* e) (doublecomplex* b) (integer* ldb) (integer* info)))

(define-wrapper int zptsvx_ ((char* fact) (integer* n) (integer* nrhs) (doublereal* d__) (doublecomplex* e) (doublereal* df) (doublecomplex* ef) (doublecomplex* b) (integer* ldb) (doublecomplex* x) (integer* ldx) (doublereal* rcond) (doublereal* ferr) (doublereal* berr) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zpttrf_ ((integer* n) (doublereal* d__) (doublecomplex* e) (integer* info)))

(define-wrapper int zpttrs_ ((char* uplo) (integer* n) (integer* nrhs) (doublereal* d__) (doublecomplex* e) (doublecomplex* b) (integer* ldb) (integer* info)))

(define-wrapper int zptts2_ ((integer* iuplo) (integer* n) (integer* nrhs) (doublereal* d__) (doublecomplex* e) (doublecomplex* b) (integer* ldb)))

(define-wrapper int zrot_ ((integer* n) (doublecomplex* cx) (integer* incx) (doublecomplex* cy) (integer* incy) (doublereal* c__) (doublecomplex* s)))

(define-wrapper int zspcon_ ((char* uplo) (integer* n) (doublecomplex* ap) (integer* ipiv) (doublereal* anorm) (doublereal* rcond) (doublecomplex* work) (integer* info)))

(define-wrapper int zspmv_ ((char* uplo) (integer* n) (doublecomplex* alpha) (doublecomplex* ap) (doublecomplex* x) (integer* incx) (doublecomplex* beta) (doublecomplex* y) (integer* incy)))

(define-wrapper int zspr_ ((char* uplo) (integer* n) (doublecomplex* alpha) (doublecomplex* x) (integer* incx) (doublecomplex* ap)))

(define-wrapper int zsprfs_ ((char* uplo) (integer* n) (integer* nrhs) (doublecomplex* ap) (doublecomplex* afp) (integer* ipiv) (doublecomplex* b) (integer* ldb) (doublecomplex* x) (integer* ldx) (doublereal* ferr) (doublereal* berr) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zspsv_ ((char* uplo) (integer* n) (integer* nrhs) (doublecomplex* ap) (integer* ipiv) (doublecomplex* b) (integer* ldb) (integer* info)))

(define-wrapper int zspsvx_ ((char* fact) (char* uplo) (integer* n) (integer* nrhs) (doublecomplex* ap) (doublecomplex* afp) (integer* ipiv) (doublecomplex* b) (integer* ldb) (doublecomplex* x) (integer* ldx) (doublereal* rcond) (doublereal* ferr) (doublereal* berr) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zsptrf_ ((char* uplo) (integer* n) (doublecomplex* ap) (integer* ipiv) (integer* info)))

(define-wrapper int zsptri_ ((char* uplo) (integer* n) (doublecomplex* ap) (integer* ipiv) (doublecomplex* work) (integer* info)))

(define-wrapper int zsptrs_ ((char* uplo) (integer* n) (integer* nrhs) (doublecomplex* ap) (integer* ipiv) (doublecomplex* b) (integer* ldb) (integer* info)))

(define-wrapper int zstedc_ ((char* compz) (integer* n) (doublereal* d__) (doublereal* e) (doublecomplex* z__) (integer* ldz) (doublecomplex* work) (integer* lwork) (doublereal* rwork) (integer* lrwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int zstein_ ((integer* n) (doublereal* d__) (doublereal* e) (integer* m) (doublereal* w) (integer* iblock) (integer* isplit) (doublecomplex* z__) (integer* ldz) (doublereal* work) (integer* iwork) (integer* ifail) (integer* info)))

(define-wrapper int zsteqr_ ((char* compz) (integer* n) (doublereal* d__) (doublereal* e) (doublecomplex* z__) (integer* ldz) (doublereal* work) (integer* info)))

(define-wrapper int zsycon_ ((char* uplo) (integer* n) (doublecomplex* a) (integer* lda) (integer* ipiv) (doublereal* anorm) (doublereal* rcond) (doublecomplex* work) (integer* info)))

(define-wrapper int zsymv_ ((char* uplo) (integer* n) (doublecomplex* alpha) (doublecomplex* a) (integer* lda) (doublecomplex* x) (integer* incx) (doublecomplex* beta) (doublecomplex* y) (integer* incy)))

(define-wrapper int zsyr_ ((char* uplo) (integer* n) (doublecomplex* alpha) (doublecomplex* x) (integer* incx) (doublecomplex* a) (integer* lda)))

(define-wrapper int zsyrfs_ ((char* uplo) (integer* n) (integer* nrhs) (doublecomplex* a) (integer* lda) (doublecomplex* af) (integer* ldaf) (integer* ipiv) (doublecomplex* b) (integer* ldb) (doublecomplex* x) (integer* ldx) (doublereal* ferr) (doublereal* berr) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int zsysv_ ((char* uplo) (integer* n) (integer* nrhs) (doublecomplex* a) (integer* lda) (integer* ipiv) (doublecomplex* b) (integer* ldb) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zsysvx_ ((char* fact) (char* uplo) (integer* n) (integer* nrhs) (doublecomplex* a) (integer* lda) (doublecomplex* af) (integer* ldaf) (integer* ipiv) (doublecomplex* b) (integer* ldb) (doublecomplex* x) (integer* ldx) (doublereal* rcond) (doublereal* ferr) (doublereal* berr) (doublecomplex* work) (integer* lwork) (doublereal* rwork) (integer* info)))

(define-wrapper int zsytf2_ ((char* uplo) (integer* n) (doublecomplex* a) (integer* lda) (integer* ipiv) (integer* info)))

(define-wrapper int zsytrf_ ((char* uplo) (integer* n) (doublecomplex* a) (integer* lda) (integer* ipiv) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zsytri_ ((char* uplo) (integer* n) (doublecomplex* a) (integer* lda) (integer* ipiv) (doublecomplex* work) (integer* info)))

(define-wrapper int zsytrs_ ((char* uplo) (integer* n) (integer* nrhs) (doublecomplex* a) (integer* lda) (integer* ipiv) (doublecomplex* b) (integer* ldb) (integer* info)))

(define-wrapper int ztbcon_ ((char* norm) (char* uplo) (char* diag) (integer* n) (integer* kd) (doublecomplex* ab) (integer* ldab) (doublereal* rcond) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int ztbrfs_ ((char* uplo) (char* trans) (char* diag) (integer* n) (integer* kd) (integer* nrhs) (doublecomplex* ab) (integer* ldab) (doublecomplex* b) (integer* ldb) (doublecomplex* x) (integer* ldx) (doublereal* ferr) (doublereal* berr) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int ztbtrs_ ((char* uplo) (char* trans) (char* diag) (integer* n) (integer* kd) (integer* nrhs) (doublecomplex* ab) (integer* ldab) (doublecomplex* b) (integer* ldb) (integer* info)))

(define-wrapper int ztgevc_ ((char* side) (char* howmny) (logical* select) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (doublecomplex* vl) (integer* ldvl) (doublecomplex* vr) (integer* ldvr) (integer* mm) (integer* m) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int ztgex2_ ((logical* wantq) (logical* wantz) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (doublecomplex* q) (integer* ldq) (doublecomplex* z__) (integer* ldz) (integer* j1) (integer* info)))

(define-wrapper int ztgexc_ ((logical* wantq) (logical* wantz) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (doublecomplex* q) (integer* ldq) (doublecomplex* z__) (integer* ldz) (integer* ifst) (integer* ilst) (integer* info)))

(define-wrapper int ztgsen_ ((integer* ijob) (logical* wantq) (logical* wantz) (logical* select) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (doublecomplex* alpha) (doublecomplex* beta) (doublecomplex* q) (integer* ldq) (doublecomplex* z__) (integer* ldz) (integer* m) (doublereal* pl) (doublereal* pr) (doublereal* dif) (doublecomplex* work) (integer* lwork) (integer* iwork) (integer* liwork) (integer* info)))

(define-wrapper int ztgsja_ ((char* jobu) (char* jobv) (char* jobq) (integer* m) (integer* p) (integer* n) (integer* k) (integer* l) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (doublereal* tola) (doublereal* tolb) (doublereal* alpha) (doublereal* beta) (doublecomplex* u) (integer* ldu) (doublecomplex* v) (integer* ldv) (doublecomplex* q) (integer* ldq) (doublecomplex* work) (integer* ncycle) (integer* info)))

(define-wrapper int ztgsna_ ((char* job) (char* howmny) (logical* select) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (doublecomplex* vl) (integer* ldvl) (doublecomplex* vr) (integer* ldvr) (doublereal* s) (doublereal* dif) (integer* mm) (integer* m) (doublecomplex* work) (integer* lwork) (integer* iwork) (integer* info)))

(define-wrapper int ztgsy2_ ((char* trans) (integer* ijob) (integer* m) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (doublecomplex* c__) (integer* ldc) (doublecomplex* d__) (integer* ldd) (doublecomplex* e) (integer* lde) (doublecomplex* f) (integer* ldf) (doublereal* scale) (doublereal* rdsum) (doublereal* rdscal) (integer* info)))

(define-wrapper int ztgsyl_ ((char* trans) (integer* ijob) (integer* m) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (doublecomplex* c__) (integer* ldc) (doublecomplex* d__) (integer* ldd) (doublecomplex* e) (integer* lde) (doublecomplex* f) (integer* ldf) (doublereal* scale) (doublereal* dif) (doublecomplex* work) (integer* lwork) (integer* iwork) (integer* info)))

(define-wrapper int ztpcon_ ((char* norm) (char* uplo) (char* diag) (integer* n) (doublecomplex* ap) (doublereal* rcond) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int ztprfs_ ((char* uplo) (char* trans) (char* diag) (integer* n) (integer* nrhs) (doublecomplex* ap) (doublecomplex* b) (integer* ldb) (doublecomplex* x) (integer* ldx) (doublereal* ferr) (doublereal* berr) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int ztptri_ ((char* uplo) (char* diag) (integer* n) (doublecomplex* ap) (integer* info)))

(define-wrapper int ztptrs_ ((char* uplo) (char* trans) (char* diag) (integer* n) (integer* nrhs) (doublecomplex* ap) (doublecomplex* b) (integer* ldb) (integer* info)))

(define-wrapper int ztrcon_ ((char* norm) (char* uplo) (char* diag) (integer* n) (doublecomplex* a) (integer* lda) (doublereal* rcond) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int ztrevc_ ((char* side) (char* howmny) (logical* select) (integer* n) (doublecomplex* t) (integer* ldt) (doublecomplex* vl) (integer* ldvl) (doublecomplex* vr) (integer* ldvr) (integer* mm) (integer* m) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int ztrexc_ ((char* compq) (integer* n) (doublecomplex* t) (integer* ldt) (doublecomplex* q) (integer* ldq) (integer* ifst) (integer* ilst) (integer* info)))

(define-wrapper int ztrrfs_ ((char* uplo) (char* trans) (char* diag) (integer* n) (integer* nrhs) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (doublecomplex* x) (integer* ldx) (doublereal* ferr) (doublereal* berr) (doublecomplex* work) (doublereal* rwork) (integer* info)))

(define-wrapper int ztrsen_ ((char* job) (char* compq) (logical* select) (integer* n) (doublecomplex* t) (integer* ldt) (doublecomplex* q) (integer* ldq) (doublecomplex* w) (integer* m) (doublereal* s) (doublereal* sep) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int ztrsna_ ((char* job) (char* howmny) (logical* select) (integer* n) (doublecomplex* t) (integer* ldt) (doublecomplex* vl) (integer* ldvl) (doublecomplex* vr) (integer* ldvr) (doublereal* s) (doublereal* sep) (integer* mm) (integer* m) (doublecomplex* work) (integer* ldwork) (doublereal* rwork) (integer* info)))

(define-wrapper int ztrsyl_ ((char* trana) (char* tranb) (integer* isgn) (integer* m) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (doublecomplex* c__) (integer* ldc) (doublereal* scale) (integer* info)))

(define-wrapper int ztrti2_ ((char* uplo) (char* diag) (integer* n) (doublecomplex* a) (integer* lda) (integer* info)))

(define-wrapper int ztrtri_ ((char* uplo) (char* diag) (integer* n) (doublecomplex* a) (integer* lda) (integer* info)))

(define-wrapper int ztrtrs_ ((char* uplo) (char* trans) (char* diag) (integer* n) (integer* nrhs) (doublecomplex* a) (integer* lda) (doublecomplex* b) (integer* ldb) (integer* info)))

(define-wrapper int ztzrqf_ ((integer* m) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (integer* info)))

(define-wrapper int ztzrzf_ ((integer* m) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zung2l_ ((integer* m) (integer* n) (integer* k) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* work) (integer* info)))

(define-wrapper int zung2r_ ((integer* m) (integer* n) (integer* k) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* work) (integer* info)))

(define-wrapper int zungbr_ ((char* vect) (integer* m) (integer* n) (integer* k) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zunghr_ ((integer* n) (integer* ilo) (integer* ihi) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zungl2_ ((integer* m) (integer* n) (integer* k) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* work) (integer* info)))

(define-wrapper int zunglq_ ((integer* m) (integer* n) (integer* k) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zungql_ ((integer* m) (integer* n) (integer* k) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zungqr_ ((integer* m) (integer* n) (integer* k) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zungr2_ ((integer* m) (integer* n) (integer* k) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* work) (integer* info)))

(define-wrapper int zungrq_ ((integer* m) (integer* n) (integer* k) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zungtr_ ((char* uplo) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zunm2l_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* c__) (integer* ldc) (doublecomplex* work) (integer* info)))

(define-wrapper int zunm2r_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* c__) (integer* ldc) (doublecomplex* work) (integer* info)))

(define-wrapper int zunmbr_ ((char* vect) (char* side) (char* trans) (integer* m) (integer* n) (integer* k) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* c__) (integer* ldc) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zunmhr_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* ilo) (integer* ihi) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* c__) (integer* ldc) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zunml2_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* c__) (integer* ldc) (doublecomplex* work) (integer* info)))

(define-wrapper int zunmlq_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* c__) (integer* ldc) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zunmql_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* c__) (integer* ldc) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zunmqr_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* c__) (integer* ldc) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zunmr2_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* c__) (integer* ldc) (doublecomplex* work) (integer* info)))

(define-wrapper int zunmr3_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (integer* l) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* c__) (integer* ldc) (doublecomplex* work) (integer* info)))

(define-wrapper int zunmrq_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* c__) (integer* ldc) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zunmrz_ ((char* side) (char* trans) (integer* m) (integer* n) (integer* k) (integer* l) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* c__) (integer* ldc) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zunmtr_ ((char* side) (char* uplo) (char* trans) (integer* m) (integer* n) (doublecomplex* a) (integer* lda) (doublecomplex* tau) (doublecomplex* c__) (integer* ldc) (doublecomplex* work) (integer* lwork) (integer* info)))

(define-wrapper int zupgtr_ ((char* uplo) (integer* n) (doublecomplex* ap) (doublecomplex* tau) (doublecomplex* q) (integer* ldq) (doublecomplex* work) (integer* info)))

(define-wrapper int zupmtr_ ((char* side) (char* uplo) (char* trans) (integer* m) (integer* n) (doublecomplex* ap) (doublecomplex* tau) (doublecomplex* c__) (integer* ldc) (doublecomplex* work) (integer* info)))


;;;; done

)

;;; end of file
