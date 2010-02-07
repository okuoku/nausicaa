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
    (begin0)
    (compensations)
    (parameters)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign memory)
    (foreign memory mempool)
    (foreign cstrings)
    (foreign math lapack platform)
    (foreign math lapack sizeof))


(define constants-pool
  (malloc (+ strideof-integer (* 6 strideof-int))))

(define pool-cursor
  constants-pool)

(define info*
  (begin0-let ((p pool-cursor))
    (pointer-set-c-integer! p 0 0)
    (pointer-incr! pool-cursor strideof-integer)))

(define-syntax define-char-pointer
  (syntax-rules ()
    ((_ ?name ?char)
     (define ?name
       (begin0-let ((p pool-cursor))
	 (pointer-incr! pool-cursor strideof-int)
	 (pointer-set-c-signed-char! p 0 (char->integer ?char)))))))

(define-char-pointer all*	#\A)
(define-char-pointer lower*	#\L)
(define-char-pointer none*	#\N)
(define-char-pointer oscar*	#\O)
(define-char-pointer sierra*	#\S)
(define-char-pointer upper*	#\U)


(define-syntax define-callout
  (lambda (stx)
    (define (name->callout-name name)
      ;;Strip the leading #\% if there is one.
      ;;
      (let ((n (symbol->string name)))
	(string->symbol (if (char=? #\% (string-ref n 0))
			    (substring n 1 (string-length n))
			  n))))
    (define (name->wrapper-name name)
      ;;Strip the trailing #\_.
      ;;
      (let ((n (symbol->string name)))
	(string->symbol (substring n 0 (- (string-length n) 1)))))
    (syntax-case stx ()
      ((_ ?name ?arg ...)
       (with-syntax ((WRAPPER-NAME (datum->syntax #'?name (name->wrapper-name (syntax->datum #'?name))))
		     (CALLOUT-NAME (datum->syntax #'?name (name->callout-name (syntax->datum #'?name)))))
	 #'(define (WRAPPER-NAME ?arg ...)
	     (pointer-set-c-integer! info* 0 0)
	     (let ((result (CALLOUT-NAME ?arg ... info*)))
	       (%process-result (quote WRAPPER-NAME)
				result (pointer-ref-c-integer info* 0)
				'(?arg ...) (list ?arg ...)))))))))

(define (%process-result who return-value info arg-names arg-values)
  (cond ((< info 0)
	 (let* ((index  (- info))
		(index1 (- index 1))) ;Fortran indexing starts at 1.
	   (error who
	     (string-append "invalid argument '"
			    (symbol->string (list-ref arg-names index1))
			    "' (position " (number->string index) ")")
	     (list-ref arg-values index1))))
	((> info 0)
	 (error who
	   (string-append "error in the course of computation at step " (number->string info))
	   info))
	(else return-value)))


(define-callout cbdsqr_ uplo n ncvt nru ncc d__ e vt ldvt u ldu c__ ldc rwork)
(define-callout cgbbrd_ vect m n ncc kl ku ab ldab d__ e q ldq pt ldpt c__ ldc work rwork)
(define-callout cgbcon_ norm n kl ku ab ldab ipiv anorm rcond work rwork)
(define-callout cgbequ_ m n kl ku ab ldab r__ c__ rowcnd colcnd amax)
(define-callout cgbrfs_ trans n kl ku nrhs ab ldab afb ldafb ipiv b ldb x ldx ferr berr work rwork)
(define-callout cgbsv_ n kl ku nrhs ab ldab ipiv b ldb)
(define-callout cgbsvx_ fact trans n kl ku nrhs ab ldab afb ldafb ipiv equed r__ c__ b ldb x ldx rcond ferr berr work rwork)
(define-callout cgbtf2_ m n kl ku ab ldab ipiv)
(define-callout cgbtrf_ m n kl ku ab ldab ipiv)
(define-callout cgbtrs_ trans n kl ku nrhs ab ldab ipiv b ldb)
(define-callout cgebak_ job side n ilo ihi scale m v ldv)
(define-callout cgebal_ job n a lda ilo ihi scale)
(define-callout cgebd2_ m n a lda d__ e tauq taup work)
(define-callout cgebrd_ m n a lda d__ e tauq taup work lwork)
(define-callout cgecon_ norm n a lda anorm rcond work rwork)
(define-callout cgeequ_ m n a lda r__ c__ rowcnd colcnd amax)
(define-callout cgees_ jobvs sort L_fp select n a lda sdim w vs ldvs work lwork rwork bwork)
(define-callout cgeesx_ jobvs sort L_fp select sense n a lda sdim w vs ldvs rconde rcondv work lwork rwork bwork)
(define-callout cgeev_ jobvl jobvr n a lda w vl ldvl vr ldvr work lwork rwork)
(define-callout cgeevx_ balanc jobvl jobvr sense n a lda w vl ldvl vr ldvr ilo ihi scale abnrm rconde rcondv work lwork rwork)
(define-callout cgegs_ jobvsl jobvsr n a lda b ldb alpha beta vsl ldvsl vsr ldvsr work lwork rwork)
(define-callout cgegv_ jobvl jobvr n a lda b ldb alpha beta vl ldvl vr ldvr work lwork rwork)
(define-callout cgehd2_ n ilo ihi a lda tau work)
(define-callout cgehrd_ n ilo ihi a lda tau work lwork)
(define-callout cgelq2_ m n a lda tau work)
(define-callout cgelqf_ m n a lda tau work lwork)
(define-callout cgels_ trans m n nrhs a lda b ldb work lwork)
(define-callout cgelsx_ m n nrhs a lda b ldb jpvt rcond rank work rwork)
(define-callout cgelsy_ m n nrhs a lda b ldb jpvt rcond rank work lwork rwork)
(define-callout cgeql2_ m n a lda tau work)
(define-callout cgeqlf_ m n a lda tau work lwork)
(define-callout cgeqp3_ m n a lda jpvt tau work lwork rwork)
(define-callout cgeqpf_ m n a lda jpvt tau work rwork)
(define-callout cgeqr2_ m n a lda tau work)
(define-callout cgeqrf_ m n a lda tau work lwork)
(define-callout cgerfs_ trans n nrhs a lda af ldaf ipiv b ldb x ldx ferr berr work rwork)
(define-callout cgerq2_ m n a lda tau work)
(define-callout cgerqf_ m n a lda tau work lwork)
(define-callout cgesc2_ n a lda rhs ipiv jpiv scale)
(define-callout cgesv_ n nrhs a lda ipiv b ldb)
(define-callout cgesvx_ fact trans n nrhs a lda af ldaf ipiv equed r__ c__ b ldb x ldx rcond ferr berr work rwork)
(define-callout cgetc2_ n a lda ipiv jpiv)
(define-callout cgetf2_ m n a lda ipiv)
(define-callout cgetrf_ m n a lda ipiv)
(define-callout cgetri_ n a lda ipiv work lwork)
(define-callout cgetrs_ trans n nrhs a lda ipiv b ldb)
(define-callout cggbak_ job side n ilo ihi lscale rscale m v ldv)
(define-callout cggbal_ job n a lda b ldb ilo ihi lscale rscale work)
(define-callout cgges_ jobvsl jobvsr sort L_fp selctg n a lda b ldb sdim alpha beta vsl ldvsl vsr ldvsr work lwork rwork bwork)
(define-callout cggesx_ jobvsl jobvsr sort L_fp selctg sense n a lda b ldb sdim alpha beta vsl ldvsl vsr ldvsr rconde rcondv work lwork rwork iwork liwork bwork)
(define-callout cggev_ jobvl jobvr n a lda b ldb alpha beta vl ldvl vr ldvr work lwork rwork)
(define-callout cggevx_ balanc jobvl jobvr sense n a lda b ldb alpha beta vl ldvl vr ldvr ilo ihi lscale rscale abnrm bbnrm rconde rcondv work lwork rwork iwork bwork)
(define-callout cggglm_ n m p a lda b ldb d__ x y work lwork)
(define-callout cgghrd_ compq compz n ilo ihi a lda b ldb q ldq z__ ldz)
(define-callout cgglse_ m n p a lda b ldb c__ d__ x work lwork)
(define-callout cggqrf_ n m p a lda taua b ldb taub work lwork)
(define-callout cggrqf_ m p n a lda taua b ldb taub work lwork)
(define-callout cggsvd_ jobu jobv jobq m n p k l a lda b ldb alpha beta u ldu v ldv q ldq work rwork iwork)
(define-callout cggsvp_ jobu jobv jobq m p n a lda b ldb tola tolb k l u ldu v ldv q ldq iwork rwork tau work)
(define-callout cgtcon_ norm n dl d__ du du2 ipiv anorm rcond work)
(define-callout cgtrfs_ trans n nrhs dl d__ du dlf df duf du2 ipiv b ldb x ldx ferr berr work rwork)
(define-callout cgtsv_ n nrhs dl d__ du b ldb)
(define-callout cgtsvx_ fact trans n nrhs dl d__ du dlf df duf du2 ipiv b ldb x ldx rcond ferr berr work rwork)
(define-callout cgttrf_ n dl d__ du du2 ipiv)
(define-callout cgttrs_ trans n nrhs dl d__ du du2 ipiv b ldb)
(define-callout cgtts2_ itrans n nrhs dl d__ du du2 ipiv b ldb)
(define-callout chbev_ jobz uplo n kd ab ldab w z__ ldz work rwork)
(define-callout chbevd_ jobz uplo n kd ab ldab w z__ ldz work lwork rwork lrwork iwork liwork)
(define-callout chbevx_ jobz range uplo n kd ab ldab q ldq vl vu il iu abstol m w z__ ldz work rwork iwork ifail)
(define-callout chbgst_ vect uplo n ka kb ab ldab bb ldbb x ldx work rwork)
(define-callout chbgv_ jobz uplo n ka kb ab ldab bb ldbb w z__ ldz work rwork)
(define-callout chbgvx_ jobz range uplo n ka kb ab ldab bb ldbb q ldq vl vu il iu abstol m w z__ ldz work rwork iwork ifail)
(define-callout chbtrd_ vect uplo n kd ab ldab d__ e q ldq work)
(define-callout checon_ uplo n a lda ipiv anorm rcond work)
(define-callout cheev_ jobz uplo n a lda w work lwork rwork)
(define-callout cheevd_ jobz uplo n a lda w work lwork rwork lrwork iwork liwork)
(define-callout cheevr_ jobz range uplo n a lda vl vu il iu abstol m w z__ ldz isuppz work lwork rwork lrwork iwork liwork)
(define-callout cheevx_ jobz range uplo n a lda vl vu il iu abstol m w z__ ldz work lwork rwork iwork ifail)
(define-callout chegs2_ itype uplo n a lda b ldb)
(define-callout chegst_ itype uplo n a lda b ldb)
(define-callout chegv_ itype jobz uplo n a lda b ldb w work lwork rwork)
(define-callout chegvd_ itype jobz uplo n a lda b ldb w work lwork rwork lrwork iwork liwork)
(define-callout chegvx_ itype jobz range uplo n a lda b ldb vl vu il iu abstol m w z__ ldz work lwork rwork iwork ifail)
(define-callout cherfs_ uplo n nrhs a lda af ldaf ipiv b ldb x ldx ferr berr work rwork)
(define-callout chesv_ uplo n nrhs a lda ipiv b ldb work lwork)
(define-callout chesvx_ fact uplo n nrhs a lda af ldaf ipiv b ldb x ldx rcond ferr berr work lwork rwork)
(define-callout chetf2_ uplo n a lda ipiv)
(define-callout chetrd_ uplo n a lda d__ e tau work lwork)
(define-callout chetrf_ uplo n a lda ipiv work lwork)
(define-callout chetri_ uplo n a lda ipiv work)
(define-callout chetrs_ uplo n nrhs a lda ipiv b ldb)
(define-callout chgeqz_ job compq compz n ilo ihi a lda b ldb alpha beta q ldq z__ ldz work lwork rwork)
(define-callout chpcon_ uplo n ap ipiv anorm rcond work)
(define-callout chpev_ jobz uplo n ap w z__ ldz work rwork)
(define-callout chpevd_ jobz uplo n ap w z__ ldz work lwork rwork lrwork iwork liwork)
(define-callout chpevx_ jobz range uplo n ap vl vu il iu abstol m w z__ ldz work rwork iwork ifail)
(define-callout chpgst_ itype uplo n ap bp)
(define-callout chpgv_ itype jobz uplo n ap bp w z__ ldz work rwork)
(define-callout chpgvd_ itype jobz uplo n ap bp w z__ ldz work lwork rwork lrwork iwork liwork)
(define-callout chpgvx_ itype jobz range uplo n ap bp vl vu il iu abstol m w z__ ldz work rwork iwork ifail)
(define-callout chprfs_ uplo n nrhs ap afp ipiv b ldb x ldx ferr berr work rwork)
(define-callout chpsv_ uplo n nrhs ap ipiv b ldb)
(define-callout chpsvx_ fact uplo n nrhs ap afp ipiv b ldb x ldx rcond ferr berr work rwork)
(define-callout chptrd_ uplo n ap d__ e tau)
(define-callout chptrf_ uplo n ap ipiv)
(define-callout chptri_ uplo n ap ipiv work)
(define-callout chptrs_ uplo n nrhs ap ipiv b ldb)
(define-callout chsein_ side eigsrc initv select n h__ ldh w vl ldvl vr ldvr mm m work rwork ifaill ifailr)
(define-callout chseqr_ job compz n ilo ihi h__ ldh w z__ ldz work lwork)
(define-callout clabrd_ m n nb a lda d__ e tauq taup x ldx y ldy)
(define-callout clacgv_ n x incx)
(define-callout clacon_ n v x est kase)
(define-callout clacp2_ uplo m n a lda b ldb)
(define-callout clacpy_ uplo m n a lda b ldb)
(define-callout clacrm_ m n a lda b ldb c__ ldc rwork)
(define-callout clacrt_ n cx incx cy incy c__ s)
(define-callout claed0_ qsiz n d__ e q ldq qstore ldqs rwork iwork)
(define-callout claed7_ n cutpnt qsiz tlvls curlvl curpbm d__ q ldq rho indxq qstore qptr prmptr perm givptr givcol givnum work rwork iwork)
(define-callout claed8_ k n qsiz q ldq d__ rho cutpnt z__ dlamda q2 ldq2 w indxp indx indxq perm givptr givcol givnum)
(define-callout claein_ rightv noinit n h__ ldh w v b ldb rwork eps3 smlnum)
(define-callout claesy_ a b c__ rt1 rt2 evscal cs1 sn1)
(define-callout claev2_ a b c__ rt1 rt2 cs1 sn1)
(define-callout clags2_ upper a1 a2 a3 b1 b2 b3 csu snu csv snv csq snq)
(define-callout clagtm_ trans n nrhs alpha dl d__ du x ldx beta b ldb)
(define-callout clahef_ uplo n nb kb a lda ipiv w ldw)
(define-callout clahqr_ wantt wantz n ilo ihi h__ ldh w iloz ihiz z__ ldz)
(define-callout clahrd_ n k nb a lda tau t ldt y ldy)
(define-callout claic1_ job j x sest w gamma sestpr s c__)
(define-callout clals0_ icompq nl nr sqre nrhs b ldb bx ldbx perm givptr givcol ldgcol givnum ldgnum poles difl difr z__ k c__ s rwork)
(define-callout clalsa_ icompq smlsiz n nrhs b ldb bx ldbx u ldu vt k difl difr z__ poles givptr givcol ldgcol perm givnum c__ s rwork iwork)
(define-callout clapll_ n x incx y incy ssmin)
(define-callout clapmt_ forwrd m n x ldx k)
(define-callout claqgb_ m n kl ku ab ldab r__ c__ rowcnd colcnd amax equed)
(define-callout claqge_ m n a lda r__ c__ rowcnd colcnd amax equed)
(define-callout claqhb_ uplo n kd ab ldab s scond amax equed)
(define-callout claqhe_ uplo n a lda s scond amax equed)
(define-callout claqhp_ uplo n ap s scond amax equed)
(define-callout claqp2_ m n offset a lda jpvt tau vn1 vn2 work)
(define-callout claqps_ m n offset nb kb a lda jpvt tau vn1 vn2 auxv f ldf)
(define-callout claqsb_ uplo n kd ab ldab s scond amax equed)
(define-callout claqsp_ uplo n ap s scond amax equed)
(define-callout claqsy_ uplo n a lda s scond amax equed)
(define-callout clar1v_ n b1 bn sigma d__ l ld lld gersch z__ ztz mingma r__ isuppz work)
(define-callout clar2v_ n x y z__ incx c__ s incc)
(define-callout clarcm_ m n a lda b ldb c__ ldc rwork)
(define-callout clarf_ side m n v incv tau c__ ldc work)
(define-callout clarfb_ side trans direct storev m n k v ldv t ldt c__ ldc work ldwork)
(define-callout clarfg_ n alpha x incx tau)
(define-callout clarft_ direct storev n k v ldv tau t ldt)
(define-callout clarfx_ side m n v tau c__ ldc work)
(define-callout clargv_ n x incx y incy c__ incc)
(define-callout clarnv_ idist iseed n x)
(define-callout clarrv_ n d__ l isplit m w iblock gersch tol z__ ldz isuppz work iwork)
(define-callout clartg_ f g cs sn r__)
(define-callout clartv_ n x incx y incy c__ s incc)
(define-callout clarz_ side m n l v incv tau c__ ldc work)
(define-callout clarzb_ side trans direct storev m n k l v ldv t ldt c__ ldc work ldwork)
(define-callout clarzt_ direct storev n k v ldv tau t ldt)
(define-callout clascl_ type__ kl ku cfrom cto m n a lda)
(define-callout claset_ uplo m n alpha beta a lda)
(define-callout clasr_ side pivot direct m n c__ s a lda)
(define-callout classq_ n x incx scale sumsq)
(define-callout claswp_ n a lda k1 k2 ipiv incx)
(define-callout clasyf_ uplo n nb kb a lda ipiv w ldw)
(define-callout clatbs_ uplo trans diag normin n kd ab ldab x scale cnorm)
(define-callout clatdf_ ijob n z__ ldz rhs rdsum rdscal ipiv jpiv)
(define-callout clatps_ uplo trans diag normin n ap x scale cnorm)
(define-callout clatrd_ uplo n nb a lda e tau w ldw)
(define-callout clatrs_ uplo trans diag normin n a lda x scale cnorm)
(define-callout clatrz_ m n l a lda tau work)
(define-callout clatzm_ side m n v incv tau c1 c2 ldc work)
(define-callout clauu2_ uplo n a lda)
(define-callout clauum_ uplo n a lda)
(define-callout cpbcon_ uplo n kd ab ldab anorm rcond work rwork)
(define-callout cpbequ_ uplo n kd ab ldab s scond amax)
(define-callout cpbrfs_ uplo n kd nrhs ab ldab afb ldafb b ldb x ldx ferr berr work rwork)
(define-callout cpbstf_ uplo n kd ab ldab)
(define-callout cpbsv_ uplo n kd nrhs ab ldab b ldb)
(define-callout cpbsvx_ fact uplo n kd nrhs ab ldab afb ldafb equed s b ldb x ldx rcond ferr berr work rwork)
(define-callout cpbtf2_ uplo n kd ab ldab)
(define-callout cpbtrf_ uplo n kd ab ldab)
(define-callout cpbtrs_ uplo n kd nrhs ab ldab b ldb)
(define-callout cpocon_ uplo n a lda anorm rcond work rwork)
(define-callout cpoequ_ n a lda s scond amax)
(define-callout cporfs_ uplo n nrhs a lda af ldaf b ldb x ldx ferr berr work rwork)
(define-callout cposv_ uplo n nrhs a lda b ldb)
(define-callout cposvx_ fact uplo n nrhs a lda af ldaf equed s b ldb x ldx rcond ferr berr work rwork)
(define-callout cpotf2_ uplo n a lda)
(define-callout cpotrf_ uplo n a lda)
(define-callout cpotri_ uplo n a lda)
(define-callout cpotrs_ uplo n nrhs a lda b ldb)
(define-callout cppcon_ uplo n ap anorm rcond work rwork)
(define-callout cppequ_ uplo n ap s scond amax)
(define-callout cpprfs_ uplo n nrhs ap afp b ldb x ldx ferr berr work rwork)
(define-callout cppsv_ uplo n nrhs ap b ldb)
(define-callout cppsvx_ fact uplo n nrhs ap afp equed s b ldb x ldx rcond ferr berr work rwork)
(define-callout cpptrf_ uplo n ap)
(define-callout cpptri_ uplo n ap)
(define-callout cpptrs_ uplo n nrhs ap b ldb)
(define-callout cptcon_ n d__ e anorm rcond rwork)
(define-callout cptrfs_ uplo n nrhs d__ e df ef b ldb x ldx ferr berr work rwork)
(define-callout cptsv_ n nrhs d__ e b ldb)
(define-callout cptsvx_ fact n nrhs d__ e df ef b ldb x ldx rcond ferr berr work rwork)
(define-callout cpttrf_ n d__ e)
(define-callout cpttrs_ uplo n nrhs d__ e b ldb)
(define-callout cptts2_ iuplo n nrhs d__ e b ldb)
(define-callout crot_ n cx incx cy incy c__ s)
(define-callout cspcon_ uplo n ap ipiv anorm rcond work)
(define-callout cspmv_ uplo n alpha ap x incx beta y incy)
(define-callout cspr_ uplo n alpha x incx ap)
(define-callout csprfs_ uplo n nrhs ap afp ipiv b ldb x ldx ferr berr work rwork)
(define-callout cspsv_ uplo n nrhs ap ipiv b ldb)
(define-callout cspsvx_ fact uplo n nrhs ap afp ipiv b ldb x ldx rcond ferr berr work rwork)
(define-callout csptrf_ uplo n ap ipiv)
(define-callout csptri_ uplo n ap ipiv work)
(define-callout csptrs_ uplo n nrhs ap ipiv b ldb)
(define-callout csrot_ n cx incx cy incy c__ s)
(define-callout csrscl_ n sa sx incx)
(define-callout cstedc_ compz n d__ e z__ ldz work lwork rwork lrwork iwork liwork)
(define-callout cstein_ n d__ e m w iblock isplit z__ ldz work iwork ifail)
(define-callout csteqr_ compz n d__ e z__ ldz work)
(define-callout csycon_ uplo n a lda ipiv anorm rcond work)
(define-callout csymv_ uplo n alpha a lda x incx beta y incy)
(define-callout csyr_ uplo n alpha x incx a lda)
(define-callout csyrfs_ uplo n nrhs a lda af ldaf ipiv b ldb x ldx ferr berr work rwork)
(define-callout csysv_ uplo n nrhs a lda ipiv b ldb work lwork)
(define-callout csysvx_ fact uplo n nrhs a lda af ldaf ipiv b ldb x ldx rcond ferr berr work lwork rwork)
(define-callout csytf2_ uplo n a lda ipiv)
(define-callout csytrf_ uplo n a lda ipiv work lwork)
(define-callout csytri_ uplo n a lda ipiv work)
(define-callout csytrs_ uplo n nrhs a lda ipiv b ldb)
(define-callout ctbcon_ norm uplo diag n kd ab ldab rcond work rwork)
(define-callout ctbrfs_ uplo trans diag n kd nrhs ab ldab b ldb x ldx ferr berr work rwork)
(define-callout ctbtrs_ uplo trans diag n kd nrhs ab ldab b ldb)
(define-callout ctgevc_ side howmny select n a lda b ldb vl ldvl vr ldvr mm m work rwork)
(define-callout ctgex2_ wantq wantz n a lda b ldb q ldq z__ ldz j1)
(define-callout ctgexc_ wantq wantz n a lda b ldb q ldq z__ ldz ifst ilst)
(define-callout ctgsen_ ijob wantq wantz select n a lda b ldb alpha beta q ldq z__ ldz m pl pr dif work lwork iwork liwork)
(define-callout ctgsja_ jobu jobv jobq m p n k l a lda b ldb tola tolb alpha beta u ldu v ldv q ldq work ncycle)
(define-callout ctgsna_ job howmny select n a lda b ldb vl ldvl vr ldvr s dif mm m work lwork iwork)
(define-callout ctgsy2_ trans ijob m n a lda b ldb c__ ldc d__ ldd e lde f ldf scale rdsum rdscal)
(define-callout ctgsyl_ trans ijob m n a lda b ldb c__ ldc d__ ldd e lde f ldf scale dif work lwork iwork)
(define-callout ctpcon_ norm uplo diag n ap rcond work rwork)
(define-callout ctprfs_ uplo trans diag n nrhs ap b ldb x ldx ferr berr work rwork)
(define-callout ctptri_ uplo diag n ap)
(define-callout ctptrs_ uplo trans diag n nrhs ap b ldb)
(define-callout ctrcon_ norm uplo diag n a lda rcond work rwork)
(define-callout ctrevc_ side howmny select n t ldt vl ldvl vr ldvr mm m work rwork)
(define-callout ctrexc_ compq n t ldt q ldq ifst ilst)
(define-callout ctrrfs_ uplo trans diag n nrhs a lda b ldb x ldx ferr berr work rwork)
(define-callout ctrsen_ job compq select n t ldt q ldq w m s sep work lwork)
(define-callout ctrsna_ job howmny select n t ldt vl ldvl vr ldvr s sep mm m work ldwork rwork)
(define-callout ctrsyl_ trana tranb isgn m n a lda b ldb c__ ldc scale)
(define-callout ctrti2_ uplo diag n a lda)
(define-callout ctrtri_ uplo diag n a lda)
(define-callout ctrtrs_ uplo trans diag n nrhs a lda b ldb)
(define-callout ctzrqf_ m n a lda tau)
(define-callout ctzrzf_ m n a lda tau work lwork)
(define-callout cung2l_ m n k a lda tau work)
(define-callout cung2r_ m n k a lda tau work)
(define-callout cungbr_ vect m n k a lda tau work lwork)
(define-callout cunghr_ n ilo ihi a lda tau work lwork)
(define-callout cungl2_ m n k a lda tau work)
(define-callout cunglq_ m n k a lda tau work lwork)
(define-callout cungql_ m n k a lda tau work lwork)
(define-callout cungqr_ m n k a lda tau work lwork)
(define-callout cungr2_ m n k a lda tau work)
(define-callout cungrq_ m n k a lda tau work lwork)
(define-callout cungtr_ uplo n a lda tau work lwork)
(define-callout cunm2l_ side trans m n k a lda tau c__ ldc work)
(define-callout cunm2r_ side trans m n k a lda tau c__ ldc work)
(define-callout cunmbr_ vect side trans m n k a lda tau c__ ldc work lwork)
(define-callout cunmhr_ side trans m n ilo ihi a lda tau c__ ldc work lwork)
(define-callout cunml2_ side trans m n k a lda tau c__ ldc work)
(define-callout cunmlq_ side trans m n k a lda tau c__ ldc work lwork)
(define-callout cunmql_ side trans m n k a lda tau c__ ldc work lwork)
(define-callout cunmqr_ side trans m n k a lda tau c__ ldc work lwork)
(define-callout cunmr2_ side trans m n k a lda tau c__ ldc work)
(define-callout cunmr3_ side trans m n k l a lda tau c__ ldc work)
(define-callout cunmrq_ side trans m n k a lda tau c__ ldc work lwork)
(define-callout cunmrz_ side trans m n k l a lda tau c__ ldc work lwork)
(define-callout cunmtr_ side uplo trans m n a lda tau c__ ldc work lwork)
(define-callout cupgtr_ uplo n ap tau q ldq work)
(define-callout cupmtr_ side uplo trans m n ap tau c__ ldc work)
(define-callout dbdsdc_ uplo compq n d__ e u ldu vt ldvt q iq work iwork)
(define-callout dbdsqr_ uplo n ncvt nru ncc d__ e vt ldvt u ldu c__ ldc work)
(define-callout ddisna_ job m n d__ sep)
(define-callout dgbbrd_ vect m n ncc kl ku ab ldab d__ e q ldq pt ldpt c__ ldc work)
(define-callout dgbcon_ norm n kl ku ab ldab ipiv anorm rcond work iwork)
(define-callout dgbequ_ m n kl ku ab ldab r__ c__ rowcnd colcnd amax)
(define-callout dgbrfs_ trans n kl ku nrhs ab ldab afb ldafb ipiv b ldb x ldx ferr berr work iwork)
(define-callout dgbsv_ n kl ku nrhs ab ldab ipiv b ldb)
(define-callout dgbsvx_ fact trans n kl ku nrhs ab ldab afb ldafb ipiv equed r__ c__ b ldb x ldx rcond ferr berr work iwork)
(define-callout dgbtf2_ m n kl ku ab ldab ipiv)
(define-callout dgbtrf_ m n kl ku ab ldab ipiv)
(define-callout dgbtrs_ trans n kl ku nrhs ab ldab ipiv b ldb)
(define-callout dgebak_ job side n ilo ihi scale m v ldv)
(define-callout dgebal_ job n a lda ilo ihi scale)
(define-callout dgebd2_ m n a lda d__ e tauq taup work)
(define-callout dgebrd_ m n a lda d__ e tauq taup work lwork)
(define-callout dgecon_ norm n a lda anorm rcond work iwork)
(define-callout dgeequ_ m n a lda r__ c__ rowcnd colcnd amax)
(define-callout dgees_ jobvs sort L_fp select n a lda sdim wr wi vs ldvs work lwork bwork)
(define-callout dgeesx_ jobvs sort L_fp select sense n a lda sdim wr wi vs ldvs rconde rcondv work lwork iwork liwork bwork)
(define-callout dgeev_ jobvl jobvr n a lda wr wi vl ldvl vr ldvr work lwork)
(define-callout dgeevx_ balanc jobvl jobvr sense n a lda wr wi vl ldvl vr ldvr ilo ihi scale abnrm rconde rcondv work lwork iwork)
(define-callout dgegs_ jobvsl jobvsr n a lda b ldb alphar alphai beta vsl ldvsl vsr ldvsr work lwork)
(define-callout dgegv_ jobvl jobvr n a lda b ldb alphar alphai beta vl ldvl vr ldvr work lwork)
(define-callout dgehd2_ n ilo ihi a lda tau work)
(define-callout dgehrd_ n ilo ihi a lda tau work lwork)
(define-callout dgelq2_ m n a lda tau work)
(define-callout dgelqf_ m n a lda tau work lwork)
(define-callout dgels_ trans m n nrhs a lda b ldb work lwork)
(define-callout dgelsd_ m n nrhs a lda b ldb s rcond rank work lwork iwork)
(define-callout dgelss_ m n nrhs a lda b ldb s rcond rank work lwork)
(define-callout dgelsx_ m n nrhs a lda b ldb jpvt rcond rank work)
(define-callout dgelsy_ m n nrhs a lda b ldb jpvt rcond rank work lwork)
(define-callout dgeql2_ m n a lda tau work)
(define-callout dgeqlf_ m n a lda tau work lwork)
(define-callout dgeqp3_ m n a lda jpvt tau work lwork)
(define-callout dgeqpf_ m n a lda jpvt tau work)
(define-callout dgeqr2_ m n a lda tau work)
(define-callout dgeqrf_ m n a lda tau work lwork)
(define-callout dgerfs_ trans n nrhs a lda af ldaf ipiv b ldb x ldx ferr berr work iwork)
(define-callout dgerq2_ m n a lda tau work)
(define-callout dgerqf_ m n a lda tau work lwork)
(define-callout dgesc2_ n a lda rhs ipiv jpiv scale)
(define-callout dgesdd_ jobz m n a lda s u ldu vt ldvt work lwork iwork)
(define-callout %dgesv_ n nrhs a lda ipiv b ldb)
(define-callout dgesvd_ jobu jobvt m n a lda s u ldu vt ldvt work lwork)
(define-callout dgesvx_ fact trans n nrhs a lda af ldaf ipiv equed r__ c__ b ldb x ldx rcond ferr berr work iwork)
(define-callout dgetc2_ n a lda ipiv jpiv)
(define-callout dgetf2_ m n a lda ipiv)
(define-callout dgetrf_ m n a lda ipiv)
(define-callout dgetri_ n a lda ipiv work lwork)
(define-callout dgetrs_ trans n nrhs a lda ipiv b ldb)
(define-callout dggbak_ job side n ilo ihi lscale rscale m v ldv)
(define-callout dggbal_ job n a lda b ldb ilo ihi lscale rscale work)
(define-callout dgges_ jobvsl jobvsr sort L_fp delctg n a lda b ldb sdim alphar alphai beta vsl ldvsl vsr ldvsr work lwork bwork)
(define-callout dggesx_ jobvsl jobvsr sort L_fp delctg sense n a lda b ldb sdim alphar alphai beta vsl ldvsl vsr ldvsr rconde rcondv work lwork iwork liwork bwork)
(define-callout dggev_ jobvl jobvr n a lda b ldb alphar alphai beta vl ldvl vr ldvr work lwork)
(define-callout dggevx_ balanc jobvl jobvr sense n a lda b ldb alphar alphai beta vl ldvl vr ldvr ilo ihi lscale rscale abnrm bbnrm rconde rcondv work lwork iwork bwork)
(define-callout dggglm_ n m p a lda b ldb d__ x y work lwork)
(define-callout dgghrd_ compq compz n ilo ihi a lda b ldb q ldq z__ ldz)
(define-callout dgglse_ m n p a lda b ldb c__ d__ x work lwork)
(define-callout dggqrf_ n m p a lda taua b ldb taub work lwork)
(define-callout dggrqf_ m p n a lda taua b ldb taub work lwork)
(define-callout dggsvd_ jobu jobv jobq m n p k l a lda b ldb alpha beta u ldu v ldv q ldq work iwork)
(define-callout dggsvp_ jobu jobv jobq m p n a lda b ldb tola tolb k l u ldu v ldv q ldq iwork tau work)
(define-callout dgtcon_ norm n dl d__ du du2 ipiv anorm rcond work iwork)
(define-callout dgtrfs_ trans n nrhs dl d__ du dlf df duf du2 ipiv b ldb x ldx ferr berr work iwork)
(define-callout dgtsv_ n nrhs dl d__ du b ldb)
(define-callout dgtsvx_ fact trans n nrhs dl d__ du dlf df duf du2 ipiv b ldb x ldx rcond ferr berr work iwork)
(define-callout dgttrf_ n dl d__ du du2 ipiv)
(define-callout dgttrs_ trans n nrhs dl d__ du du2 ipiv b ldb)
(define-callout dgtts2_ itrans n nrhs dl d__ du du2 ipiv b ldb)
(define-callout dhgeqz_ job compq compz n ilo ihi a lda b ldb alphar alphai beta q ldq z__ ldz work lwork)
(define-callout dhsein_ side eigsrc initv select n h__ ldh wr wi vl ldvl vr ldvr mm m work ifaill ifailr)
(define-callout dhseqr_ job compz n ilo ihi h__ ldh wr wi z__ ldz work lwork)
(define-callout dlabad_ small large)
(define-callout dlabrd_ m n nb a lda d__ e tauq taup x ldx y ldy)
(define-callout dlacon_ n v x isgn est kase)
(define-callout dlacpy_ uplo m n a lda b ldb)
(define-callout dladiv_ a b c__ d__ p q)
(define-callout dlae2_ a b c__ rt1 rt2)
(define-callout dlaebz_ ijob nitmax n mmax minp nbmin abstol reltol pivmin d__ e e2 nval ab c__ mout nab work iwork)
(define-callout dlaed0_ icompq qsiz n d__ e q ldq qstore ldqs work iwork)
(define-callout dlaed1_ n d__ q ldq indxq rho cutpnt work iwork)
(define-callout dlaed2_ k n n1 d__ q ldq indxq rho z__ dlamda w q2 indx indxc indxp coltyp)
(define-callout dlaed3_ k n n1 d__ q ldq rho dlamda q2 indx ctot w s)
(define-callout dlaed4_ n i__ d__ z__ delta rho dlam)
(define-callout dlaed5_ i__ d__ z__ delta rho dlam)
(define-callout dlaed6_ kniter orgati rho d__ z__ finit tau)
(define-callout dlaed7_ icompq n qsiz tlvls curlvl curpbm d__ q ldq indxq rho cutpnt qstore qptr prmptr perm givptr givcol givnum work iwork)
(define-callout dlaed8_ icompq k n qsiz d__ q ldq indxq rho cutpnt z__ dlamda q2 ldq2 w perm givptr givcol givnum indxp indx)
(define-callout dlaed9_ k kstart kstop n d__ q ldq rho dlamda w s lds)
(define-callout dlaeda_ n tlvls curlvl curpbm prmptr perm givptr givcol givnum q qptr z__ ztemp)
(define-callout dlaein_ rightv noinit n h__ ldh wr wi vr vi b ldb work eps3 smlnum bignum)
(define-callout dlaev2_ a b c__ rt1 rt2 cs1 sn1)
(define-callout dlaexc_ wantq n t ldt q ldq j1 n1 n2 work)
(define-callout dlag2_ a lda b ldb safmin scale1 scale2 wr1 wr2 wi)
(define-callout dlags2_ upper a1 a2 a3 b1 b2 b3 csu snu csv snv csq snq)
(define-callout dlagtf_ n a lambda b c__ tol d__ in)
(define-callout dlagtm_ trans n nrhs alpha dl d__ du x ldx beta b ldb)
(define-callout dlagts_ job n a b c__ d__ in y tol)
(define-callout dlagv2_ a lda b ldb alphar alphai beta csl snl csr snr)
(define-callout dlahqr_ wantt wantz n ilo ihi h__ ldh wr wi iloz ihiz z__ ldz)
(define-callout dlahrd_ n k nb a lda tau t ldt y ldy)
(define-callout dlaic1_ job j x sest w gamma sestpr s c__)
(define-callout dlaln2_ ltrans na nw smin ca a lda d1 d2 b ldb wr wi x ldx scale xnorm)
(define-callout dlals0_ icompq nl nr sqre nrhs b ldb bx ldbx perm givptr givcol ldgcol givnum ldgnum poles difl difr z__ k c__ s work)
(define-callout dlalsa_ icompq smlsiz n nrhs b ldb bx ldbx u ldu vt k difl difr z__ poles givptr givcol ldgcol perm givnum c__ s work iwork)
(define-callout dlalsd_ uplo smlsiz n nrhs d__ e b ldb rcond rank work iwork)
(define-callout dlamc1_ beta t rnd ieee1)
(define-callout dlamc2_ beta t rnd eps emin rmin emax rmax)
(define-callout dlamc4_ emin start base)
(define-callout dlamc5_ beta p emin ieee emax rmax)
(define-callout dlamrg_ n1 n2 a dtrd1 dtrd2 index)
(define-callout dlanv2_ a b c__ d__ rt1r rt1i rt2r rt2i cs sn)
(define-callout dlapll_ n x incx y incy ssmin)
(define-callout dlapmt_ forwrd m n x ldx k)
(define-callout dlaqgb_ m n kl ku ab ldab r__ c__ rowcnd colcnd amax equed)
(define-callout dlaqge_ m n a lda r__ c__ rowcnd colcnd amax equed)
(define-callout dlaqp2_ m n offset a lda jpvt tau vn1 vn2 work)
(define-callout dlaqps_ m n offset nb kb a lda jpvt tau vn1 vn2 auxv f ldf)
(define-callout dlaqsb_ uplo n kd ab ldab s scond amax equed)
(define-callout dlaqsp_ uplo n ap s scond amax equed)
(define-callout dlaqsy_ uplo n a lda s scond amax equed)
(define-callout dlaqtr_ ltran lreal n t ldt b w scale x work)
(define-callout dlar1v_ n b1 bn sigma d__ l ld lld gersch z__ ztz mingma r__ isuppz work)
(define-callout dlar2v_ n x y z__ incx c__ s incc)
(define-callout dlarf_ side m n v incv tau c__ ldc work)
(define-callout dlarfb_ side trans direct storev m n k v ldv t ldt c__ ldc work ldwork)
(define-callout dlarfg_ n alpha x incx tau)
(define-callout dlarft_ direct storev n k v ldv tau t ldt)
(define-callout dlarfx_ side m n v tau c__ ldc work)
(define-callout dlargv_ n x incx y incy c__ incc)
(define-callout dlarnv_ idist iseed n x)
(define-callout dlarrb_ n d__ l ld lld ifirst ilast sigma reltol w wgap werr work iwork)
(define-callout dlarre_ n d__ e tol nsplit isplit m w woff gersch work)
(define-callout dlarrf_ n d__ l ld lld ifirst ilast w dplus lplus work iwork)
(define-callout dlarrv_ n d__ l isplit m w iblock gersch tol z__ ldz isuppz work iwork)
(define-callout dlartg_ f g cs sn r__)
(define-callout dlartv_ n x incx y incy c__ s incc)
(define-callout dlaruv_ iseed n x)
(define-callout dlarz_ side m n l v incv tau c__ ldc work)
(define-callout dlarzb_ side trans direct storev m n k l v ldv t ldt c__ ldc work ldwork)
(define-callout dlarzt_ direct storev n k v ldv tau t ldt)
(define-callout dlas2_ f g h__ ssmin ssmax)
(define-callout dlascl_ type__ kl ku cfrom cto m n a lda)
(define-callout dlasd0_ n sqre d__ e u ldu vt ldvt smlsiz iwork work)
(define-callout dlasd1_ nl nr sqre d__ alpha beta u ldu vt ldvt idxq iwork work)
(define-callout dlasd2_ nl nr sqre k d__ z__ alpha beta u ldu vt ldvt dsigma u2 ldu2 vt2 ldvt2 idxp idx idxc idxq coltyp)
(define-callout dlasd3_ nl nr sqre k d__ q ldq dsigma u ldu u2 ldu2 vt ldvt vt2 ldvt2 idxc ctot z__)
(define-callout dlasd4_ n i__ d__ z__ delta rho sigma work)
(define-callout dlasd5_ i__ d__ z__ delta rho dsigma work)
(define-callout dlasd6_ icompq nl nr sqre d__ vf vl alpha beta idxq perm givptr givcol ldgcol givnum ldgnum poles difl difr z__ k c__ s work iwork)
(define-callout dlasd7_ icompq nl nr sqre k d__ z__ zw vf vfw vl vlw alpha beta dsigma idx idxp idxq perm givptr givcol ldgcol givnum ldgnum c__ s)
(define-callout dlasd8_ icompq k d__ z__ vf vl difl difr lddifr dsigma work)
(define-callout dlasd9_ icompq ldu k d__ z__ vf vl difl difr dsigma work)
(define-callout dlasda_ icompq smlsiz n sqre d__ e u ldu vt k difl difr z__ poles givptr givcol ldgcol perm givnum c__ s work iwork)
(define-callout dlasdq_ uplo sqre n ncvt nru ncc d__ e vt ldvt u ldu c__ ldc work)
(define-callout dlasdt_ n lvl nd inode ndiml ndimr msub)
(define-callout dlaset_ uplo m n alpha beta a lda)
(define-callout dlasq1_ n d__ e work)
(define-callout dlasq2_ n z__)
(define-callout dlasq3_ i0 n0 z__ pp dmin__ sigma desig qmax nfail iter ndiv ieee)
(define-callout dlasq4_ i0 n0 z__ pp n0in dmin__ dmin1 dmin2 dn dn1 dn2 tau ttype)
(define-callout dlasq5_ i0 n0 z__ pp tau dmin__ dmin1 dmin2 dn dnm1 dnm2 ieee)
(define-callout dlasq6_ i0 n0 z__ pp dmin__ dmin1 dmin2 dn dnm1 dnm2)
(define-callout dlasr_ side pivot direct m n c__ s a lda)
(define-callout dlasrt_ id n d__)
(define-callout dlassq_ n x incx scale sumsq)
(define-callout dlasv2_ f g h__ ssmin ssmax snr csr snl csl)
(define-callout dlaswp_ n a lda k1 k2 ipiv incx)
(define-callout dlasy2_ ltranl ltranr isgn n1 n2 tl ldtl tr ldtr b ldb scale x ldx xnorm)
(define-callout dlasyf_ uplo n nb kb a lda ipiv w ldw)
(define-callout dlatbs_ uplo trans diag normin n kd ab ldab x scale cnorm)
(define-callout dlatdf_ ijob n z__ ldz rhs rdsum rdscal ipiv jpiv)
(define-callout dlatps_ uplo trans diag normin n ap x scale cnorm)
(define-callout dlatrd_ uplo n nb a lda e tau w ldw)
(define-callout dlatrs_ uplo trans diag normin n a lda x scale cnorm)
(define-callout dlatrz_ m n l a lda tau work)
(define-callout dlatzm_ side m n v incv tau c1 c2 ldc work)
(define-callout dlauu2_ uplo n a lda)
(define-callout dlauum_ uplo n a lda)
(define-callout dopgtr_ uplo n ap tau q ldq work)
(define-callout dopmtr_ side uplo trans m n ap tau c__ ldc work)
(define-callout dorg2l_ m n k a lda tau work)
(define-callout dorg2r_ m n k a lda tau work)
(define-callout dorgbr_ vect m n k a lda tau work lwork)
(define-callout dorghr_ n ilo ihi a lda tau work lwork)
(define-callout dorgl2_ m n k a lda tau work)
(define-callout dorglq_ m n k a lda tau work lwork)
(define-callout dorgql_ m n k a lda tau work lwork)
(define-callout dorgqr_ m n k a lda tau work lwork)
(define-callout dorgr2_ m n k a lda tau work)
(define-callout dorgrq_ m n k a lda tau work lwork)
(define-callout dorgtr_ uplo n a lda tau work lwork)
(define-callout dorm2l_ side trans m n k a lda tau c__ ldc work)
(define-callout dorm2r_ side trans m n k a lda tau c__ ldc work)
(define-callout dormbr_ vect side trans m n k a lda tau c__ ldc work lwork)
(define-callout dormhr_ side trans m n ilo ihi a lda tau c__ ldc work lwork)
(define-callout dorml2_ side trans m n k a lda tau c__ ldc work)
(define-callout dormlq_ side trans m n k a lda tau c__ ldc work lwork)
(define-callout dormql_ side trans m n k a lda tau c__ ldc work lwork)
(define-callout dormqr_ side trans m n k a lda tau c__ ldc work lwork)
(define-callout dormr2_ side trans m n k a lda tau c__ ldc work)
(define-callout dormr3_ side trans m n k l a lda tau c__ ldc work)
(define-callout dormrq_ side trans m n k a lda tau c__ ldc work lwork)
(define-callout dormrz_ side trans m n k l a lda tau c__ ldc work lwork)
(define-callout dormtr_ side uplo trans m n a lda tau c__ ldc work lwork)
(define-callout dpbcon_ uplo n kd ab ldab anorm rcond work iwork)
(define-callout dpbequ_ uplo n kd ab ldab s scond amax)
(define-callout dpbrfs_ uplo n kd nrhs ab ldab afb ldafb b ldb x ldx ferr berr work iwork)
(define-callout dpbstf_ uplo n kd ab ldab)
(define-callout dpbsv_ uplo n kd nrhs ab ldab b ldb)
(define-callout dpbsvx_ fact uplo n kd nrhs ab ldab afb ldafb equed s b ldb x ldx rcond ferr berr work iwork)
(define-callout dpbtf2_ uplo n kd ab ldab)
(define-callout dpbtrf_ uplo n kd ab ldab)
(define-callout dpbtrs_ uplo n kd nrhs ab ldab b ldb)
(define-callout dpocon_ uplo n a lda anorm rcond work iwork)
(define-callout dpoequ_ n a lda s scond amax)
(define-callout dporfs_ uplo n nrhs a lda af ldaf b ldb x ldx ferr berr work iwork)
(define-callout dposv_ uplo n nrhs a lda b ldb)
(define-callout dposvx_ fact uplo n nrhs a lda af ldaf equed s b ldb x ldx rcond ferr berr work iwork)
(define-callout dpotf2_ uplo n a lda)
(define-callout dpotrf_ uplo n a lda)
(define-callout dpotri_ uplo n a lda)
(define-callout dpotrs_ uplo n nrhs a lda b ldb)
(define-callout dppcon_ uplo n ap anorm rcond work iwork)
(define-callout dppequ_ uplo n ap s scond amax)
(define-callout dpprfs_ uplo n nrhs ap afp b ldb x ldx ferr berr work iwork)
(define-callout dppsv_ uplo n nrhs ap b ldb)
(define-callout dppsvx_ fact uplo n nrhs ap afp equed s b ldb x ldx rcond ferr berr work iwork)
(define-callout dpptrf_ uplo n ap)
(define-callout dpptri_ uplo n ap)
(define-callout dpptrs_ uplo n nrhs ap b ldb)
(define-callout dptcon_ n d__ e anorm rcond work)
(define-callout dpteqr_ compz n d__ e z__ ldz work)
(define-callout dptrfs_ n nrhs d__ e df ef b ldb x ldx ferr berr work)
(define-callout dptsv_ n nrhs d__ e b ldb)
(define-callout dptsvx_ fact n nrhs d__ e df ef b ldb x ldx rcond ferr berr work)
(define-callout dpttrf_ n d__ e)
(define-callout dpttrs_ n nrhs d__ e b ldb)
(define-callout dptts2_ n nrhs d__ e b ldb)
(define-callout drscl_ n sa sx incx)
(define-callout dsbev_ jobz uplo n kd ab ldab w z__ ldz work)
(define-callout dsbevd_ jobz uplo n kd ab ldab w z__ ldz work lwork iwork liwork)
(define-callout dsbevx_ jobz range uplo n kd ab ldab q ldq vl vu il iu abstol m w z__ ldz work iwork ifail)
(define-callout dsbgst_ vect uplo n ka kb ab ldab bb ldbb x ldx work)
(define-callout dsbgv_ jobz uplo n ka kb ab ldab bb ldbb w z__ ldz work)
(define-callout dsbgvd_ jobz uplo n ka kb ab ldab bb ldbb w z__ ldz work lwork iwork liwork)
(define-callout dsbgvx_ jobz range uplo n ka kb ab ldab bb ldbb q ldq vl vu il iu abstol m w z__ ldz work iwork ifail)
(define-callout dsbtrd_ vect uplo n kd ab ldab d__ e q ldq work)
(define-callout dspcon_ uplo n ap ipiv anorm rcond work iwork)
(define-callout dspev_ jobz uplo n ap w z__ ldz work)
(define-callout dspevd_ jobz uplo n ap w z__ ldz work lwork iwork liwork)
(define-callout dspevx_ jobz range uplo n ap vl vu il iu abstol m w z__ ldz work iwork ifail)
(define-callout dspgst_ itype uplo n ap bp)
(define-callout dspgv_ itype jobz uplo n ap bp w z__ ldz work)
(define-callout dspgvd_ itype jobz uplo n ap bp w z__ ldz work lwork iwork liwork)
(define-callout dspgvx_ itype jobz range uplo n ap bp vl vu il iu abstol m w z__ ldz work iwork ifail)
(define-callout dsprfs_ uplo n nrhs ap afp ipiv b ldb x ldx ferr berr work iwork)
(define-callout dspsv_ uplo n nrhs ap ipiv b ldb)
(define-callout dspsvx_ fact uplo n nrhs ap afp ipiv b ldb x ldx rcond ferr berr work iwork)
(define-callout dsptrd_ uplo n ap d__ e tau)
(define-callout dsptrf_ uplo n ap ipiv)
(define-callout dsptri_ uplo n ap ipiv work)
(define-callout dsptrs_ uplo n nrhs ap ipiv b ldb)
(define-callout dstebz_ range order n vl vu il iu abstol d__ e m nsplit w iblock isplit work iwork)
(define-callout dstedc_ compz n d__ e z__ ldz work lwork iwork liwork)
(define-callout dstegr_ jobz range n d__ e vl vu il iu abstol m w z__ ldz isuppz work lwork iwork liwork)
(define-callout dstein_ n d__ e m w iblock isplit z__ ldz work iwork ifail)
(define-callout dsteqr_ compz n d__ e z__ ldz work)
(define-callout dsterf_ n d__ e)
(define-callout dstev_ jobz n d__ e z__ ldz work)
(define-callout dstevd_ jobz n d__ e z__ ldz work lwork iwork liwork)
(define-callout dstevr_ jobz range n d__ e vl vu il iu abstol m w z__ ldz isuppz work lwork iwork liwork)
(define-callout dstevx_ jobz range n d__ e vl vu il iu abstol m w z__ ldz work iwork ifail)
(define-callout dsycon_ uplo n a lda ipiv anorm rcond work iwork)
(define-callout dsyev_ jobz uplo n a lda w work lwork)
(define-callout dsyevd_ jobz uplo n a lda w work lwork iwork liwork)
(define-callout dsyevr_ jobz range uplo n a lda vl vu il iu abstol m w z__ ldz isuppz work lwork iwork liwork)
(define-callout dsyevx_ jobz range uplo n a lda vl vu il iu abstol m w z__ ldz work lwork iwork ifail)
(define-callout dsygs2_ itype uplo n a lda b ldb)
(define-callout dsygst_ itype uplo n a lda b ldb)
(define-callout dsygv_ itype jobz uplo n a lda b ldb w work lwork)
(define-callout dsygvd_ itype jobz uplo n a lda b ldb w work lwork iwork liwork)
(define-callout dsygvx_ itype jobz range uplo n a lda b ldb vl vu il iu abstol m w z__ ldz work lwork iwork ifail)
(define-callout dsyrfs_ uplo n nrhs a lda af ldaf ipiv b ldb x ldx ferr berr work iwork)
(define-callout dsysv_ uplo n nrhs a lda ipiv b ldb work lwork)
(define-callout dsysvx_ fact uplo n nrhs a lda af ldaf ipiv b ldb x ldx rcond ferr berr work lwork iwork)
(define-callout dsytd2_ uplo n a lda d__ e tau)
(define-callout dsytf2_ uplo n a lda ipiv)
(define-callout dsytrd_ uplo n a lda d__ e tau work lwork)
(define-callout dsytrf_ uplo n a lda ipiv work lwork)
(define-callout dsytri_ uplo n a lda ipiv work)
(define-callout dsytrs_ uplo n nrhs a lda ipiv b ldb)
(define-callout dtbcon_ norm uplo diag n kd ab ldab rcond work iwork)
(define-callout dtbrfs_ uplo trans diag n kd nrhs ab ldab b ldb x ldx ferr berr work iwork)
(define-callout dtbtrs_ uplo trans diag n kd nrhs ab ldab b ldb)
(define-callout dtgevc_ side howmny select n a lda b ldb vl ldvl vr ldvr mm m work)
(define-callout dtgex2_ wantq wantz n a lda b ldb q ldq z__ ldz j1 n1 n2 work lwork)
(define-callout dtgexc_ wantq wantz n a lda b ldb q ldq z__ ldz ifst ilst work lwork)
(define-callout dtgsen_ ijob wantq wantz select n a lda b ldb alphar alphai beta q ldq z__ ldz m pl pr dif work lwork iwork liwork)
(define-callout dtgsja_ jobu jobv jobq m p n k l a lda b ldb tola tolb alpha beta u ldu v ldv q ldq work ncycle)
(define-callout dtgsna_ job howmny select n a lda b ldb vl ldvl vr ldvr s dif mm m work lwork iwork)
(define-callout dtgsy2_ trans ijob m n a lda b ldb c__ ldc d__ ldd e lde f ldf scale rdsum rdscal iwork pq)
(define-callout dtgsyl_ trans ijob m n a lda b ldb c__ ldc d__ ldd e lde f ldf scale dif work lwork iwork)
(define-callout dtpcon_ norm uplo diag n ap rcond work iwork)
(define-callout dtprfs_ uplo trans diag n nrhs ap b ldb x ldx ferr berr work iwork)
(define-callout dtptri_ uplo diag n ap)
(define-callout dtptrs_ uplo trans diag n nrhs ap b ldb)
(define-callout dtrcon_ norm uplo diag n a lda rcond work iwork)
(define-callout dtrevc_ side howmny select n t ldt vl ldvl vr ldvr mm m work)
(define-callout dtrexc_ compq n t ldt q ldq ifst ilst work)
(define-callout dtrrfs_ uplo trans diag n nrhs a lda b ldb x ldx ferr berr work iwork)
(define-callout dtrsen_ job compq select n t ldt q ldq wr wi m s sep work lwork iwork liwork)
(define-callout dtrsna_ job howmny select n t ldt vl ldvl vr ldvr s sep mm m work ldwork iwork)
(define-callout dtrsyl_ trana tranb isgn m n a lda b ldb c__ ldc scale)
(define-callout dtrti2_ uplo diag n a lda)
(define-callout dtrtri_ uplo diag n a lda)
(define-callout dtrtrs_ uplo trans diag n nrhs a lda b ldb)
(define-callout dtzrqf_ m n a lda tau)
(define-callout dtzrzf_ m n a lda tau work lwork)
(define-callout icmax1_ n cx incx)
(define-callout ieeeck_ ispec zero one)
(define-callout ilaenv_ ispec name__ opts n1 n2 n3 n4 name_len opts_len)
(define-callout izmax1_ n cx incx)
(define-callout sbdsdc_ uplo compq n d__ e u ldu vt ldvt q iq work iwork)
(define-callout sbdsqr_ uplo n ncvt nru ncc d__ e vt ldvt u ldu c__ ldc work)
(define-callout sdisna_ job m n d__ sep)
(define-callout sgbbrd_ vect m n ncc kl ku ab ldab d__ e q ldq pt ldpt c__ ldc work)
(define-callout sgbcon_ norm n kl ku ab ldab ipiv anorm rcond work iwork)
(define-callout sgbequ_ m n kl ku ab ldab r__ c__ rowcnd colcnd amax)
(define-callout sgbrfs_ trans n kl ku nrhs ab ldab afb ldafb ipiv b ldb x ldx ferr berr work iwork)
(define-callout sgbsv_ n kl ku nrhs ab ldab ipiv b ldb)
(define-callout sgbsvx_ fact trans n kl ku nrhs ab ldab afb ldafb ipiv equed r__ c__ b ldb x ldx rcond ferr berr work iwork)
(define-callout sgbtf2_ m n kl ku ab ldab ipiv)
(define-callout sgbtrf_ m n kl ku ab ldab ipiv)
(define-callout sgbtrs_ trans n kl ku nrhs ab ldab ipiv b ldb)
(define-callout sgebak_ job side n ilo ihi scale m v ldv)
(define-callout sgebal_ job n a lda ilo ihi scale)
(define-callout sgebd2_ m n a lda d__ e tauq taup work)
(define-callout sgebrd_ m n a lda d__ e tauq taup work lwork)
(define-callout sgecon_ norm n a lda anorm rcond work iwork)
(define-callout sgeequ_ m n a lda r__ c__ rowcnd colcnd amax)
(define-callout sgees_ jobvs sort L_fp select n a lda sdim wr wi vs ldvs work lwork bwork)
(define-callout sgeesx_ jobvs sort L_fp select sense n a lda sdim wr wi vs ldvs rconde rcondv work lwork iwork liwork bwork)
(define-callout sgeev_ jobvl jobvr n a lda wr wi vl ldvl vr ldvr work lwork)
(define-callout sgeevx_ balanc jobvl jobvr sense n a lda wr wi vl ldvl vr ldvr ilo ihi scale abnrm rconde rcondv work lwork iwork)
(define-callout sgegs_ jobvsl jobvsr n a lda b ldb alphar alphai beta vsl ldvsl vsr ldvsr work lwork)
(define-callout sgegv_ jobvl jobvr n a lda b ldb alphar alphai beta vl ldvl vr ldvr work lwork)
(define-callout sgehd2_ n ilo ihi a lda tau work)
(define-callout sgehrd_ n ilo ihi a lda tau work lwork)
(define-callout sgelq2_ m n a lda tau work)
(define-callout sgelqf_ m n a lda tau work lwork)
(define-callout sgels_ trans m n nrhs a lda b ldb work lwork)
(define-callout sgelsd_ m n nrhs a lda b ldb s rcond rank work lwork iwork)
(define-callout sgelss_ m n nrhs a lda b ldb s rcond rank work lwork)
(define-callout sgelsx_ m n nrhs a lda b ldb jpvt rcond rank work)
(define-callout sgelsy_ m n nrhs a lda b ldb jpvt rcond rank work lwork)
(define-callout sgeql2_ m n a lda tau work)
(define-callout sgeqlf_ m n a lda tau work lwork)
(define-callout sgeqp3_ m n a lda jpvt tau work lwork)
(define-callout sgeqpf_ m n a lda jpvt tau work)
(define-callout sgeqr2_ m n a lda tau work)
(define-callout sgeqrf_ m n a lda tau work lwork)
(define-callout sgerfs_ trans n nrhs a lda af ldaf ipiv b ldb x ldx ferr berr work iwork)
(define-callout sgerq2_ m n a lda tau work)
(define-callout sgerqf_ m n a lda tau work lwork)
(define-callout sgesc2_ n a lda rhs ipiv jpiv scale)
(define-callout sgesdd_ jobz m n a lda s u ldu vt ldvt work lwork iwork)
(define-callout sgesv_ n nrhs a lda ipiv b ldb)
(define-callout sgesvd_ jobu jobvt m n a lda s u ldu vt ldvt work lwork)
(define-callout sgesvx_ fact trans n nrhs a lda af ldaf ipiv equed r__ c__ b ldb x ldx rcond ferr berr work iwork)
(define-callout sgetc2_ n a lda ipiv jpiv)
(define-callout sgetf2_ m n a lda ipiv)
(define-callout sgetrf_ m n a lda ipiv)
(define-callout sgetri_ n a lda ipiv work lwork)
(define-callout sgetrs_ trans n nrhs a lda ipiv b ldb)
(define-callout sggbak_ job side n ilo ihi lscale rscale m v ldv)
(define-callout sggbal_ job n a lda b ldb ilo ihi lscale rscale work)
(define-callout sgges_ jobvsl jobvsr sort L_fp selctg n a lda b ldb sdim alphar alphai beta vsl ldvsl vsr ldvsr work lwork bwork)
(define-callout sggesx_ jobvsl jobvsr sort L_fp selctg sense n a lda b ldb sdim alphar alphai beta vsl ldvsl vsr ldvsr rconde rcondv work lwork iwork liwork bwork)
(define-callout sggev_ jobvl jobvr n a lda b ldb alphar alphai beta vl ldvl vr ldvr work lwork)
(define-callout sggevx_ balanc jobvl jobvr sense n a lda b ldb alphar alphai beta vl ldvl vr ldvr ilo ihi lscale rscale abnrm bbnrm rconde rcondv work lwork iwork bwork)
(define-callout sggglm_ n m p a lda b ldb d__ x y work lwork)
(define-callout sgghrd_ compq compz n ilo ihi a lda b ldb q ldq z__ ldz)
(define-callout sgglse_ m n p a lda b ldb c__ d__ x work lwork)
(define-callout sggqrf_ n m p a lda taua b ldb taub work lwork)
(define-callout sggrqf_ m p n a lda taua b ldb taub work lwork)
(define-callout sggsvd_ jobu jobv jobq m n p k l a lda b ldb alpha beta u ldu v ldv q ldq work iwork)
(define-callout sggsvp_ jobu jobv jobq m p n a lda b ldb tola tolb k l u ldu v ldv q ldq iwork tau work)
(define-callout sgtcon_ norm n dl d__ du du2 ipiv anorm rcond work iwork)
(define-callout sgtrfs_ trans n nrhs dl d__ du dlf df duf du2 ipiv b ldb x ldx ferr berr work iwork)
(define-callout sgtsv_ n nrhs dl d__ du b ldb)
(define-callout sgtsvx_ fact trans n nrhs dl d__ du dlf df duf du2 ipiv b ldb x ldx rcond ferr berr work iwork)
(define-callout sgttrf_ n dl d__ du du2 ipiv)
(define-callout sgttrs_ trans n nrhs dl d__ du du2 ipiv b ldb)
(define-callout sgtts2_ itrans n nrhs dl d__ du du2 ipiv b ldb)
(define-callout shgeqz_ job compq compz n ilo ihi a lda b ldb alphar alphai beta q ldq z__ ldz work lwork)
(define-callout shsein_ side eigsrc initv select n h__ ldh wr wi vl ldvl vr ldvr mm m work ifaill ifailr)
(define-callout shseqr_ job compz n ilo ihi h__ ldh wr wi z__ ldz work lwork)
(define-callout slabad_ small large)
(define-callout slabrd_ m n nb a lda d__ e tauq taup x ldx y ldy)
(define-callout slacon_ n v x isgn est kase)
(define-callout slacpy_ uplo m n a lda b ldb)
(define-callout sladiv_ a b c__ d__ p q)
(define-callout slae2_ a b c__ rt1 rt2)
(define-callout slaebz_ ijob nitmax n mmax minp nbmin abstol reltol pivmin d__ e e2 nval ab c__ mout nab work iwork)
(define-callout slaed0_ icompq qsiz n d__ e q ldq qstore ldqs work iwork)
(define-callout slaed1_ n d__ q ldq indxq rho cutpnt work iwork)
(define-callout slaed2_ k n n1 d__ q ldq indxq rho z__ dlamda w q2 indx indxc indxp coltyp)
(define-callout slaed3_ k n n1 d__ q ldq rho dlamda q2 indx ctot w s)
(define-callout slaed4_ n i__ d__ z__ delta rho dlam)
(define-callout slaed5_ i__ d__ z__ delta rho dlam)
(define-callout slaed6_ kniter orgati rho d__ z__ finit tau)
(define-callout slaed7_ icompq n qsiz tlvls curlvl curpbm d__ q ldq indxq rho cutpnt qstore qptr prmptr perm givptr givcol givnum work iwork)
(define-callout slaed8_ icompq k n qsiz d__ q ldq indxq rho cutpnt z__ dlamda q2 ldq2 w perm givptr givcol givnum indxp indx)
(define-callout slaed9_ k kstart kstop n d__ q ldq rho dlamda w s lds)
(define-callout slaeda_ n tlvls curlvl curpbm prmptr perm givptr givcol givnum q qptr z__ ztemp)
(define-callout slaein_ rightv noinit n h__ ldh wr wi vr vi b ldb work eps3 smlnum bignum)
(define-callout slaev2_ a b c__ rt1 rt2 cs1 sn1)
(define-callout slaexc_ wantq n t ldt q ldq j1 n1 n2 work)
(define-callout slag2_ a lda b ldb safmin scale1 scale2 wr1 wr2 wi)
(define-callout slags2_ upper a1 a2 a3 b1 b2 b3 csu snu csv snv csq snq)
(define-callout slagtf_ n a lambda b c__ tol d__ in)
(define-callout slagtm_ trans n nrhs alpha dl d__ du x ldx beta b ldb)
(define-callout slagts_ job n a b c__ d__ in y tol)
(define-callout slagv2_ a lda b ldb alphar alphai beta csl snl csr snr)
(define-callout slahqr_ wantt wantz n ilo ihi h__ ldh wr wi iloz ihiz z__ ldz)
(define-callout slahrd_ n k nb a lda tau t ldt y ldy)
(define-callout slaic1_ job j x sest w gamma sestpr s c__)
(define-callout slaln2_ ltrans na nw smin ca a lda d1 d2 b ldb wr wi x ldx scale xnorm)
(define-callout slals0_ icompq nl nr sqre nrhs b ldb bx ldbx perm givptr givcol ldgcol givnum ldgnum poles difl difr z__ k c__ s work)
(define-callout slalsa_ icompq smlsiz n nrhs b ldb bx ldbx u ldu vt k difl difr z__ poles givptr givcol ldgcol perm givnum c__ s work iwork)
(define-callout slalsd_ uplo smlsiz n nrhs d__ e b ldb rcond rank work iwork)
(define-callout slamc1_ beta t rnd ieee1)
(define-callout slamc2_ beta t rnd eps emin rmin emax rmax)
(define-callout slamc4_ emin start base)
(define-callout slamc5_ beta p emin ieee emax rmax)
(define-callout slamrg_ n1 n2 a strd1 strd2 index)
(define-callout slanv2_ a b c__ d__ rt1r rt1i rt2r rt2i cs sn)
(define-callout slapll_ n x incx y incy ssmin)
(define-callout slapmt_ forwrd m n x ldx k)
(define-callout slaqgb_ m n kl ku ab ldab r__ c__ rowcnd colcnd amax equed)
(define-callout slaqge_ m n a lda r__ c__ rowcnd colcnd amax equed)
(define-callout slaqp2_ m n offset a lda jpvt tau vn1 vn2 work)
(define-callout slaqps_ m n offset nb kb a lda jpvt tau vn1 vn2 auxv f ldf)
(define-callout slaqsb_ uplo n kd ab ldab s scond amax equed)
(define-callout slaqsp_ uplo n ap s scond amax equed)
(define-callout slaqsy_ uplo n a lda s scond amax equed)
(define-callout slaqtr_ ltran lreal n t ldt b w scale x work)
(define-callout slar1v_ n b1 bn sigma d__ l ld lld gersch z__ ztz mingma r__ isuppz work)
(define-callout slar2v_ n x y z__ incx c__ s incc)
(define-callout slarf_ side m n v incv tau c__ ldc work)
(define-callout slarfb_ side trans direct storev m n k v ldv t ldt c__ ldc work ldwork)
(define-callout slarfg_ n alpha x incx tau)
(define-callout slarft_ direct storev n k v ldv tau t ldt)
(define-callout slarfx_ side m n v tau c__ ldc work)
(define-callout slargv_ n x incx y incy c__ incc)
(define-callout slarnv_ idist iseed n x)
(define-callout slarrb_ n d__ l ld lld ifirst ilast sigma reltol w wgap werr work iwork)
(define-callout slarre_ n d__ e tol nsplit isplit m w woff gersch work)
(define-callout slarrf_ n d__ l ld lld ifirst ilast w dplus lplus work iwork)
(define-callout slarrv_ n d__ l isplit m w iblock gersch tol z__ ldz isuppz work iwork)
(define-callout slartg_ f g cs sn r__)
(define-callout slartv_ n x incx y incy c__ s incc)
(define-callout slaruv_ iseed n x)
(define-callout slarz_ side m n l v incv tau c__ ldc work)
(define-callout slarzb_ side trans direct storev m n k l v ldv t ldt c__ ldc work ldwork)
(define-callout slarzt_ direct storev n k v ldv tau t ldt)
(define-callout slas2_ f g h__ ssmin ssmax)
(define-callout slascl_ type__ kl ku cfrom cto m n a lda)
(define-callout slasd0_ n sqre d__ e u ldu vt ldvt smlsiz iwork work)
(define-callout slasd1_ nl nr sqre d__ alpha beta u ldu vt ldvt idxq iwork work)
(define-callout slasd2_ nl nr sqre k d__ z__ alpha beta u ldu vt ldvt dsigma u2 ldu2 vt2 ldvt2 idxp idx idxc idxq coltyp)
(define-callout slasd3_ nl nr sqre k d__ q ldq dsigma u ldu u2 ldu2 vt ldvt vt2 ldvt2 idxc ctot z__)
(define-callout slasd4_ n i__ d__ z__ delta rho sigma work)
(define-callout slasd5_ i__ d__ z__ delta rho dsigma work)
(define-callout slasd6_ icompq nl nr sqre d__ vf vl alpha beta idxq perm givptr givcol ldgcol givnum ldgnum poles difl difr z__ k c__ s work iwork)
(define-callout slasd7_ icompq nl nr sqre k d__ z__ zw vf vfw vl vlw alpha beta dsigma idx idxp idxq perm givptr givcol ldgcol givnum ldgnum c__ s)
(define-callout slasd8_ icompq k d__ z__ vf vl difl difr lddifr dsigma work)
(define-callout slasd9_ icompq ldu k d__ z__ vf vl difl difr dsigma work)
(define-callout slasda_ icompq smlsiz n sqre d__ e u ldu vt k difl difr z__ poles givptr givcol ldgcol perm givnum c__ s work iwork)
(define-callout slasdq_ uplo sqre n ncvt nru ncc d__ e vt ldvt u ldu c__ ldc work)
(define-callout slasdt_ n lvl nd inode ndiml ndimr msub)
(define-callout slaset_ uplo m n alpha beta a lda)
(define-callout slasq1_ n d__ e work)
(define-callout slasq2_ n z__)
(define-callout slasq3_ i0 n0 z__ pp dmin__ sigma desig qmax nfail iter ndiv ieee)
(define-callout slasq4_ i0 n0 z__ pp n0in dmin__ dmin1 dmin2 dn dn1 dn2 tau ttype)
(define-callout slasq5_ i0 n0 z__ pp tau dmin__ dmin1 dmin2 dn dnm1 dnm2 ieee)
(define-callout slasq6_ i0 n0 z__ pp dmin__ dmin1 dmin2 dn dnm1 dnm2)
(define-callout slasr_ side pivot direct m n c__ s a lda)
(define-callout slasrt_ id n d__)
(define-callout slassq_ n x incx scale sumsq)
(define-callout slasv2_ f g h__ ssmin ssmax snr csr snl csl)
(define-callout slaswp_ n a lda k1 k2 ipiv incx)
(define-callout slasy2_ ltranl ltranr isgn n1 n2 tl ldtl tr ldtr b ldb scale x ldx xnorm)
(define-callout slasyf_ uplo n nb kb a lda ipiv w ldw)
(define-callout slatbs_ uplo trans diag normin n kd ab ldab x scale cnorm)
(define-callout slatdf_ ijob n z__ ldz rhs rdsum rdscal ipiv jpiv)
(define-callout slatps_ uplo trans diag normin n ap x scale cnorm)
(define-callout slatrd_ uplo n nb a lda e tau w ldw)
(define-callout slatrs_ uplo trans diag normin n a lda x scale cnorm)
(define-callout slatrz_ m n l a lda tau work)
(define-callout slatzm_ side m n v incv tau c1 c2 ldc work)
(define-callout slauu2_ uplo n a lda)
(define-callout slauum_ uplo n a lda)
(define-callout sopgtr_ uplo n ap tau q ldq work)
(define-callout sopmtr_ side uplo trans m n ap tau c__ ldc work)
(define-callout sorg2l_ m n k a lda tau work)
(define-callout sorg2r_ m n k a lda tau work)
(define-callout sorgbr_ vect m n k a lda tau work lwork)
(define-callout sorghr_ n ilo ihi a lda tau work lwork)
(define-callout sorgl2_ m n k a lda tau work)
(define-callout sorglq_ m n k a lda tau work lwork)
(define-callout sorgql_ m n k a lda tau work lwork)
(define-callout sorgqr_ m n k a lda tau work lwork)
(define-callout sorgr2_ m n k a lda tau work)
(define-callout sorgrq_ m n k a lda tau work lwork)
(define-callout sorgtr_ uplo n a lda tau work lwork)
(define-callout sorm2l_ side trans m n k a lda tau c__ ldc work)
(define-callout sorm2r_ side trans m n k a lda tau c__ ldc work)
(define-callout sormbr_ vect side trans m n k a lda tau c__ ldc work lwork)
(define-callout sormhr_ side trans m n ilo ihi a lda tau c__ ldc work lwork)
(define-callout sorml2_ side trans m n k a lda tau c__ ldc work)
(define-callout sormlq_ side trans m n k a lda tau c__ ldc work lwork)
(define-callout sormql_ side trans m n k a lda tau c__ ldc work lwork)
(define-callout sormqr_ side trans m n k a lda tau c__ ldc work lwork)
(define-callout sormr2_ side trans m n k a lda tau c__ ldc work)
(define-callout sormr3_ side trans m n k l a lda tau c__ ldc work)
(define-callout sormrq_ side trans m n k a lda tau c__ ldc work lwork)
(define-callout sormrz_ side trans m n k l a lda tau c__ ldc work lwork)
(define-callout sormtr_ side uplo trans m n a lda tau c__ ldc work lwork)
(define-callout spbcon_ uplo n kd ab ldab anorm rcond work iwork)
(define-callout spbequ_ uplo n kd ab ldab s scond amax)
(define-callout spbrfs_ uplo n kd nrhs ab ldab afb ldafb b ldb x ldx ferr berr work iwork)
(define-callout spbstf_ uplo n kd ab ldab)
(define-callout spbsv_ uplo n kd nrhs ab ldab b ldb)
(define-callout spbsvx_ fact uplo n kd nrhs ab ldab afb ldafb equed s b ldb x ldx rcond ferr berr work iwork)
(define-callout spbtf2_ uplo n kd ab ldab)
(define-callout spbtrf_ uplo n kd ab ldab)
(define-callout spbtrs_ uplo n kd nrhs ab ldab b ldb)
(define-callout spocon_ uplo n a lda anorm rcond work iwork)
(define-callout spoequ_ n a lda s scond amax)
(define-callout sporfs_ uplo n nrhs a lda af ldaf b ldb x ldx ferr berr work iwork)
(define-callout sposv_ uplo n nrhs a lda b ldb)
(define-callout sposvx_ fact uplo n nrhs a lda af ldaf equed s b ldb x ldx rcond ferr berr work iwork)
(define-callout spotf2_ uplo n a lda)
(define-callout spotrf_ uplo n a lda)
(define-callout spotri_ uplo n a lda)
(define-callout spotrs_ uplo n nrhs a lda b ldb)
(define-callout sppcon_ uplo n ap anorm rcond work iwork)
(define-callout sppequ_ uplo n ap s scond amax)
(define-callout spprfs_ uplo n nrhs ap afp b ldb x ldx ferr berr work iwork)
(define-callout sppsv_ uplo n nrhs ap b ldb)
(define-callout sppsvx_ fact uplo n nrhs ap afp equed s b ldb x ldx rcond ferr berr work iwork)
(define-callout spptrf_ uplo n ap)
(define-callout spptri_ uplo n ap)
(define-callout spptrs_ uplo n nrhs ap b ldb)
(define-callout sptcon_ n d__ e anorm rcond work)
(define-callout spteqr_ compz n d__ e z__ ldz work)
(define-callout sptrfs_ n nrhs d__ e df ef b ldb x ldx ferr berr work)
(define-callout sptsv_ n nrhs d__ e b ldb)
(define-callout sptsvx_ fact n nrhs d__ e df ef b ldb x ldx rcond ferr berr work)
(define-callout spttrf_ n d__ e)
(define-callout spttrs_ n nrhs d__ e b ldb)
(define-callout sptts2_ n nrhs d__ e b ldb)
(define-callout srscl_ n sa sx incx)
(define-callout ssbev_ jobz uplo n kd ab ldab w z__ ldz work)
(define-callout ssbevd_ jobz uplo n kd ab ldab w z__ ldz work lwork iwork liwork)
(define-callout ssbevx_ jobz range uplo n kd ab ldab q ldq vl vu il iu abstol m w z__ ldz work iwork ifail)
(define-callout ssbgst_ vect uplo n ka kb ab ldab bb ldbb x ldx work)
(define-callout ssbgv_ jobz uplo n ka kb ab ldab bb ldbb w z__ ldz work)
(define-callout ssbgvd_ jobz uplo n ka kb ab ldab bb ldbb w z__ ldz work lwork iwork liwork)
(define-callout ssbgvx_ jobz range uplo n ka kb ab ldab bb ldbb q ldq vl vu il iu abstol m w z__ ldz work iwork ifail)
(define-callout ssbtrd_ vect uplo n kd ab ldab d__ e q ldq work)
(define-callout sspcon_ uplo n ap ipiv anorm rcond work iwork)
(define-callout sspev_ jobz uplo n ap w z__ ldz work)
(define-callout sspevd_ jobz uplo n ap w z__ ldz work lwork iwork liwork)
(define-callout sspevx_ jobz range uplo n ap vl vu il iu abstol m w z__ ldz work iwork ifail)
(define-callout sspgst_ itype uplo n ap bp)
(define-callout sspgv_ itype jobz uplo n ap bp w z__ ldz work)
(define-callout sspgvd_ itype jobz uplo n ap bp w z__ ldz work lwork iwork liwork)
(define-callout sspgvx_ itype jobz range uplo n ap bp vl vu il iu abstol m w z__ ldz work iwork ifail)
(define-callout ssprfs_ uplo n nrhs ap afp ipiv b ldb x ldx ferr berr work iwork)
(define-callout sspsv_ uplo n nrhs ap ipiv b ldb)
(define-callout sspsvx_ fact uplo n nrhs ap afp ipiv b ldb x ldx rcond ferr berr work iwork)
(define-callout ssptrd_ uplo n ap d__ e tau)
(define-callout ssptrf_ uplo n ap ipiv)
(define-callout ssptri_ uplo n ap ipiv work)
(define-callout ssptrs_ uplo n nrhs ap ipiv b ldb)
(define-callout sstebz_ range order n vl vu il iu abstol d__ e m nsplit w iblock isplit work iwork)
(define-callout sstedc_ compz n d__ e z__ ldz work lwork iwork liwork)
(define-callout sstegr_ jobz range n d__ e vl vu il iu abstol m w z__ ldz isuppz work lwork iwork liwork)
(define-callout sstein_ n d__ e m w iblock isplit z__ ldz work iwork ifail)
(define-callout ssteqr_ compz n d__ e z__ ldz work)
(define-callout ssterf_ n d__ e)
(define-callout sstev_ jobz n d__ e z__ ldz work)
(define-callout sstevd_ jobz n d__ e z__ ldz work lwork iwork liwork)
(define-callout sstevr_ jobz range n d__ e vl vu il iu abstol m w z__ ldz isuppz work lwork iwork liwork)
(define-callout sstevx_ jobz range n d__ e vl vu il iu abstol m w z__ ldz work iwork ifail)
(define-callout ssycon_ uplo n a lda ipiv anorm rcond work iwork)
(define-callout ssyev_ jobz uplo n a lda w work lwork)
(define-callout ssyevd_ jobz uplo n a lda w work lwork iwork liwork)
(define-callout ssyevr_ jobz range uplo n a lda vl vu il iu abstol m w z__ ldz isuppz work lwork iwork liwork)
(define-callout ssyevx_ jobz range uplo n a lda vl vu il iu abstol m w z__ ldz work lwork iwork ifail)
(define-callout ssygs2_ itype uplo n a lda b ldb)
(define-callout ssygst_ itype uplo n a lda b ldb)
(define-callout ssygv_ itype jobz uplo n a lda b ldb w work lwork)
(define-callout ssygvd_ itype jobz uplo n a lda b ldb w work lwork iwork liwork)
(define-callout ssygvx_ itype jobz range uplo n a lda b ldb vl vu il iu abstol m w z__ ldz work lwork iwork ifail)
(define-callout ssyrfs_ uplo n nrhs a lda af ldaf ipiv b ldb x ldx ferr berr work iwork)
(define-callout ssysv_ uplo n nrhs a lda ipiv b ldb work lwork)
(define-callout ssysvx_ fact uplo n nrhs a lda af ldaf ipiv b ldb x ldx rcond ferr berr work lwork iwork)
(define-callout ssytd2_ uplo n a lda d__ e tau)
(define-callout ssytf2_ uplo n a lda ipiv)
(define-callout ssytrd_ uplo n a lda d__ e tau work lwork)
(define-callout ssytrf_ uplo n a lda ipiv work lwork)
(define-callout ssytri_ uplo n a lda ipiv work)
(define-callout ssytrs_ uplo n nrhs a lda ipiv b ldb)
(define-callout stbcon_ norm uplo diag n kd ab ldab rcond work iwork)
(define-callout stbrfs_ uplo trans diag n kd nrhs ab ldab b ldb x ldx ferr berr work iwork)
(define-callout stbtrs_ uplo trans diag n kd nrhs ab ldab b ldb)
(define-callout stgevc_ side howmny select n a lda b ldb vl ldvl vr ldvr mm m work)
(define-callout stgex2_ wantq wantz n a lda b ldb q ldq z__ ldz j1 n1 n2 work lwork)
(define-callout stgexc_ wantq wantz n a lda b ldb q ldq z__ ldz ifst ilst work lwork)
(define-callout stgsen_ ijob wantq wantz select n a lda b ldb alphar alphai beta q ldq z__ ldz m pl pr dif work lwork iwork liwork)
(define-callout stgsja_ jobu jobv jobq m p n k l a lda b ldb tola tolb alpha beta u ldu v ldv q ldq work ncycle)
(define-callout stgsna_ job howmny select n a lda b ldb vl ldvl vr ldvr s dif mm m work lwork iwork)
(define-callout stgsy2_ trans ijob m n a lda b ldb c__ ldc d__ ldd e lde f ldf scale rdsum rdscal iwork pq)
(define-callout stgsyl_ trans ijob m n a lda b ldb c__ ldc d__ ldd e lde f ldf scale dif work lwork iwork)
(define-callout stpcon_ norm uplo diag n ap rcond work iwork)
(define-callout stprfs_ uplo trans diag n nrhs ap b ldb x ldx ferr berr work iwork)
(define-callout stptri_ uplo diag n ap)
(define-callout stptrs_ uplo trans diag n nrhs ap b ldb)
(define-callout strcon_ norm uplo diag n a lda rcond work iwork)
(define-callout strevc_ side howmny select n t ldt vl ldvl vr ldvr mm m work)
(define-callout strexc_ compq n t ldt q ldq ifst ilst work)
(define-callout strrfs_ uplo trans diag n nrhs a lda b ldb x ldx ferr berr work iwork)
(define-callout strsen_ job compq select n t ldt q ldq wr wi m s sep work lwork iwork liwork)
(define-callout strsna_ job howmny select n t ldt vl ldvl vr ldvr s sep mm m work ldwork iwork)
(define-callout strsyl_ trana tranb isgn m n a lda b ldb c__ ldc scale)
(define-callout strti2_ uplo diag n a lda)
(define-callout strtri_ uplo diag n a lda)
(define-callout strtrs_ uplo trans diag n nrhs a lda b ldb)
(define-callout stzrqf_ m n a lda tau)
(define-callout stzrzf_ m n a lda tau work lwork)
(define-callout xerbla_ srname)
(define-callout zbdsqr_ uplo n ncvt nru ncc d__ e vt ldvt u ldu c__ ldc rwork)
(define-callout zdrot_ n cx incx cy incy c__ s)
(define-callout zdrscl_ n sa sx incx)
(define-callout zgbbrd_ vect m n ncc kl ku ab ldab d__ e q ldq pt ldpt c__ ldc work rwork)
(define-callout zgbcon_ norm n kl ku ab ldab ipiv anorm rcond work rwork)
(define-callout zgbequ_ m n kl ku ab ldab r__ c__ rowcnd colcnd amax)
(define-callout zgbrfs_ trans n kl ku nrhs ab ldab afb ldafb ipiv b ldb x ldx ferr berr work rwork)
(define-callout zgbsv_ n kl ku nrhs ab ldab ipiv b ldb)
(define-callout zgbsvx_ fact trans n kl ku nrhs ab ldab afb ldafb ipiv equed r__ c__ b ldb x ldx rcond ferr berr work rwork)
(define-callout zgbtf2_ m n kl ku ab ldab ipiv)
(define-callout zgbtrf_ m n kl ku ab ldab ipiv)
(define-callout zgbtrs_ trans n kl ku nrhs ab ldab ipiv b ldb)
(define-callout zgebak_ job side n ilo ihi scale m v ldv)
(define-callout zgebal_ job n a lda ilo ihi scale)
(define-callout zgebd2_ m n a lda d__ e tauq taup work)
(define-callout zgebrd_ m n a lda d__ e tauq taup work lwork)
(define-callout zgecon_ norm n a lda anorm rcond work rwork)
(define-callout zgeequ_ m n a lda r__ c__ rowcnd colcnd amax)
(define-callout zgees_ jobvs sort L_fp select n a lda sdim w vs ldvs work lwork rwork bwork)
(define-callout zgeesx_ jobvs sort L_fp select sense n a lda sdim w vs ldvs rconde rcondv work lwork rwork bwork)
(define-callout zgeev_ jobvl jobvr n a lda w vl ldvl vr ldvr work lwork rwork)
(define-callout zgeevx_ balanc jobvl jobvr sense n a lda w vl ldvl vr ldvr ilo ihi scale abnrm rconde rcondv work lwork rwork)
(define-callout zgegs_ jobvsl jobvsr n a lda b ldb alpha beta vsl ldvsl vsr ldvsr work lwork rwork)
(define-callout zgegv_ jobvl jobvr n a lda b ldb alpha beta vl ldvl vr ldvr work lwork rwork)
(define-callout zgehd2_ n ilo ihi a lda tau work)
(define-callout zgehrd_ n ilo ihi a lda tau work lwork)
(define-callout zgelq2_ m n a lda tau work)
(define-callout zgelqf_ m n a lda tau work lwork)
(define-callout zgels_ trans m n nrhs a lda b ldb work lwork)
(define-callout zgelsx_ m n nrhs a lda b ldb jpvt rcond rank work rwork)
(define-callout zgelsy_ m n nrhs a lda b ldb jpvt rcond rank work lwork rwork)
(define-callout zgeql2_ m n a lda tau work)
(define-callout zgeqlf_ m n a lda tau work lwork)
(define-callout zgeqp3_ m n a lda jpvt tau work lwork rwork)
(define-callout zgeqpf_ m n a lda jpvt tau work rwork)
(define-callout zgeqr2_ m n a lda tau work)
(define-callout zgeqrf_ m n a lda tau work lwork)
(define-callout zgerfs_ trans n nrhs a lda af ldaf ipiv b ldb x ldx ferr berr work rwork)
(define-callout zgerq2_ m n a lda tau work)
(define-callout zgerqf_ m n a lda tau work lwork)
(define-callout zgesc2_ n a lda rhs ipiv jpiv scale)
(define-callout %zgesv_ n nrhs a lda ipiv b ldb)
(define-callout zgesvx_ fact trans n nrhs a lda af ldaf ipiv equed r__ c__ b ldb x ldx rcond ferr berr work rwork)
(define-callout zgetc2_ n a lda ipiv jpiv)
(define-callout zgetf2_ m n a lda ipiv)
(define-callout zgetrf_ m n a lda ipiv)
(define-callout zgetri_ n a lda ipiv work lwork)
(define-callout zgetrs_ trans n nrhs a lda ipiv b ldb)
(define-callout zggbak_ job side n ilo ihi lscale rscale m v ldv)
(define-callout zggbal_ job n a lda b ldb ilo ihi lscale rscale work)
(define-callout zgges_ jobvsl jobvsr sort L_fp delctg n a lda b ldb sdim alpha beta vsl ldvsl vsr ldvsr work lwork rwork bwork)
(define-callout zggesx_ jobvsl jobvsr sort L_fp delctg sense n a lda b ldb sdim alpha beta vsl ldvsl vsr ldvsr rconde rcondv work lwork rwork iwork liwork bwork)
(define-callout zggev_ jobvl jobvr n a lda b ldb alpha beta vl ldvl vr ldvr work lwork rwork)
(define-callout zggevx_ balanc jobvl jobvr sense n a lda b ldb alpha beta vl ldvl vr ldvr ilo ihi lscale rscale abnrm bbnrm rconde rcondv work lwork rwork iwork bwork)
(define-callout zggglm_ n m p a lda b ldb d__ x y work lwork)
(define-callout zgghrd_ compq compz n ilo ihi a lda b ldb q ldq z__ ldz)
(define-callout zgglse_ m n p a lda b ldb c__ d__ x work lwork)
(define-callout zggqrf_ n m p a lda taua b ldb taub work lwork)
(define-callout zggrqf_ m p n a lda taua b ldb taub work lwork)
(define-callout zggsvd_ jobu jobv jobq m n p k l a lda b ldb alpha beta u ldu v ldv q ldq work rwork iwork)
(define-callout zggsvp_ jobu jobv jobq m p n a lda b ldb tola tolb k l u ldu v ldv q ldq iwork rwork tau work)
(define-callout zgtcon_ norm n dl d__ du du2 ipiv anorm rcond work)
(define-callout zgtrfs_ trans n nrhs dl d__ du dlf df duf du2 ipiv b ldb x ldx ferr berr work rwork)
(define-callout zgtsv_ n nrhs dl d__ du b ldb)
(define-callout zgtsvx_ fact trans n nrhs dl d__ du dlf df duf du2 ipiv b ldb x ldx rcond ferr berr work rwork)
(define-callout zgttrf_ n dl d__ du du2 ipiv)
(define-callout zgttrs_ trans n nrhs dl d__ du du2 ipiv b ldb)
(define-callout zgtts2_ itrans n nrhs dl d__ du du2 ipiv b ldb)
(define-callout zhbev_ jobz uplo n kd ab ldab w z__ ldz work rwork)
(define-callout zhbevd_ jobz uplo n kd ab ldab w z__ ldz work lwork rwork lrwork iwork liwork)
(define-callout zhbevx_ jobz range uplo n kd ab ldab q ldq vl vu il iu abstol m w z__ ldz work rwork iwork ifail)
(define-callout zhbgst_ vect uplo n ka kb ab ldab bb ldbb x ldx work rwork)
(define-callout zhbgv_ jobz uplo n ka kb ab ldab bb ldbb w z__ ldz work rwork)
(define-callout zhbgvx_ jobz range uplo n ka kb ab ldab bb ldbb q ldq vl vu il iu abstol m w z__ ldz work rwork iwork ifail)
(define-callout zhbtrd_ vect uplo n kd ab ldab d__ e q ldq work)
(define-callout zhecon_ uplo n a lda ipiv anorm rcond work)
(define-callout zheev_ jobz uplo n a lda w work lwork rwork)
(define-callout zheevd_ jobz uplo n a lda w work lwork rwork lrwork iwork liwork)
(define-callout zheevr_ jobz range uplo n a lda vl vu il iu abstol m w z__ ldz isuppz work lwork rwork lrwork iwork liwork)
(define-callout zheevx_ jobz range uplo n a lda vl vu il iu abstol m w z__ ldz work lwork rwork iwork ifail)
(define-callout zhegs2_ itype uplo n a lda b ldb)
(define-callout zhegst_ itype uplo n a lda b ldb)
(define-callout zhegv_ itype jobz uplo n a lda b ldb w work lwork rwork)
(define-callout zhegvd_ itype jobz uplo n a lda b ldb w work lwork rwork lrwork iwork liwork)
(define-callout zhegvx_ itype jobz range uplo n a lda b ldb vl vu il iu abstol m w z__ ldz work lwork rwork iwork ifail)
(define-callout zherfs_ uplo n nrhs a lda af ldaf ipiv b ldb x ldx ferr berr work rwork)
(define-callout zhesv_ uplo n nrhs a lda ipiv b ldb work lwork)
(define-callout zhesvx_ fact uplo n nrhs a lda af ldaf ipiv b ldb x ldx rcond ferr berr work lwork rwork)
(define-callout zhetf2_ uplo n a lda ipiv)
(define-callout zhetrd_ uplo n a lda d__ e tau work lwork)
(define-callout zhetrf_ uplo n a lda ipiv work lwork)
(define-callout zhetri_ uplo n a lda ipiv work)
(define-callout zhetrs_ uplo n nrhs a lda ipiv b ldb)
(define-callout zhgeqz_ job compq compz n ilo ihi a lda b ldb alpha beta q ldq z__ ldz work lwork rwork)
(define-callout zhpcon_ uplo n ap ipiv anorm rcond work)
(define-callout zhpev_ jobz uplo n ap w z__ ldz work rwork)
(define-callout zhpevd_ jobz uplo n ap w z__ ldz work lwork rwork lrwork iwork liwork)
(define-callout zhpevx_ jobz range uplo n ap vl vu il iu abstol m w z__ ldz work rwork iwork ifail)
(define-callout zhpgst_ itype uplo n ap bp)
(define-callout zhpgv_ itype jobz uplo n ap bp w z__ ldz work rwork)
(define-callout zhpgvd_ itype jobz uplo n ap bp w z__ ldz work lwork rwork lrwork iwork liwork)
(define-callout zhpgvx_ itype jobz range uplo n ap bp vl vu il iu abstol m w z__ ldz work rwork iwork ifail)
(define-callout zhprfs_ uplo n nrhs ap afp ipiv b ldb x ldx ferr berr work rwork)
(define-callout zhpsv_ uplo n nrhs ap ipiv b ldb)
(define-callout zhpsvx_ fact uplo n nrhs ap afp ipiv b ldb x ldx rcond ferr berr work rwork)
(define-callout zhptrd_ uplo n ap d__ e tau)
(define-callout zhptrf_ uplo n ap ipiv)
(define-callout zhptri_ uplo n ap ipiv work)
(define-callout zhptrs_ uplo n nrhs ap ipiv b ldb)
(define-callout zhsein_ side eigsrc initv select n h__ ldh w vl ldvl vr ldvr mm m work rwork ifaill ifailr)
(define-callout zhseqr_ job compz n ilo ihi h__ ldh w z__ ldz work lwork)
(define-callout zlabrd_ m n nb a lda d__ e tauq taup x ldx y ldy)
(define-callout zlacgv_ n x incx)
(define-callout zlacon_ n v x est kase)
(define-callout zlacp2_ uplo m n a lda b ldb)
(define-callout zlacpy_ uplo m n a lda b ldb)
(define-callout zlacrm_ m n a lda b ldb c__ ldc rwork)
(define-callout zlacrt_ n cx incx cy incy c__ s)
(define-callout zlaed0_ qsiz n d__ e q ldq qstore ldqs rwork iwork)
(define-callout zlaed7_ n cutpnt qsiz tlvls curlvl curpbm d__ q ldq rho indxq qstore qptr prmptr perm givptr givcol givnum work rwork iwork)
(define-callout zlaed8_ k n qsiz q ldq d__ rho cutpnt z__ dlamda q2 ldq2 w indxp indx indxq perm givptr givcol givnum)
(define-callout zlaein_ rightv noinit n h__ ldh w v b ldb rwork eps3 smlnum)
(define-callout zlaesy_ a b c__ rt1 rt2 evscal cs1 sn1)
(define-callout zlaev2_ a b c__ rt1 rt2 cs1 sn1)
(define-callout zlags2_ upper a1 a2 a3 b1 b2 b3 csu snu csv snv csq snq)
(define-callout zlagtm_ trans n nrhs alpha dl d__ du x ldx beta b ldb)
(define-callout zlahef_ uplo n nb kb a lda ipiv w ldw)
(define-callout zlahqr_ wantt wantz n ilo ihi h__ ldh w iloz ihiz z__ ldz)
(define-callout zlahrd_ n k nb a lda tau t ldt y ldy)
(define-callout zlaic1_ job j x sest w gamma sestpr s c__)
(define-callout zlals0_ icompq nl nr sqre nrhs b ldb bx ldbx perm givptr givcol ldgcol givnum ldgnum poles difl difr z__ k c__ s rwork)
(define-callout zlalsa_ icompq smlsiz n nrhs b ldb bx ldbx u ldu vt k difl difr z__ poles givptr givcol ldgcol perm givnum c__ s rwork iwork)
(define-callout zlapll_ n x incx y incy ssmin)
(define-callout zlapmt_ forwrd m n x ldx k)
(define-callout zlaqgb_ m n kl ku ab ldab r__ c__ rowcnd colcnd amax equed)
(define-callout zlaqge_ m n a lda r__ c__ rowcnd colcnd amax equed)
(define-callout zlaqhb_ uplo n kd ab ldab s scond amax equed)
(define-callout zlaqhe_ uplo n a lda s scond amax equed)
(define-callout zlaqhp_ uplo n ap s scond amax equed)
(define-callout zlaqp2_ m n offset a lda jpvt tau vn1 vn2 work)
(define-callout zlaqps_ m n offset nb kb a lda jpvt tau vn1 vn2 auxv f ldf)
(define-callout zlaqsb_ uplo n kd ab ldab s scond amax equed)
(define-callout zlaqsp_ uplo n ap s scond amax equed)
(define-callout zlaqsy_ uplo n a lda s scond amax equed)
(define-callout zlar1v_ n b1 bn sigma d__ l ld lld gersch z__ ztz mingma r__ isuppz work)
(define-callout zlar2v_ n x y z__ incx c__ s incc)
(define-callout zlarcm_ m n a lda b ldb c__ ldc rwork)
(define-callout zlarf_ side m n v incv tau c__ ldc work)
(define-callout zlarfb_ side trans direct storev m n k v ldv t ldt c__ ldc work ldwork)
(define-callout zlarfg_ n alpha x incx tau)
(define-callout zlarft_ direct storev n k v ldv tau t ldt)
(define-callout zlarfx_ side m n v tau c__ ldc work)
(define-callout zlargv_ n x incx y incy c__ incc)
(define-callout zlarnv_ idist iseed n x)
(define-callout zlarrv_ n d__ l isplit m w iblock gersch tol z__ ldz isuppz work iwork)
(define-callout zlartg_ f g cs sn r__)
(define-callout zlartv_ n x incx y incy c__ s incc)
(define-callout zlarz_ side m n l v incv tau c__ ldc work)
(define-callout zlarzb_ side trans direct storev m n k l v ldv t ldt c__ ldc work ldwork)
(define-callout zlarzt_ direct storev n k v ldv tau t ldt)
(define-callout zlascl_ type__ kl ku cfrom cto m n a lda)
(define-callout zlaset_ uplo m n alpha beta a lda)
(define-callout zlasr_ side pivot direct m n c__ s a lda)
(define-callout zlassq_ n x incx scale sumsq)
(define-callout zlaswp_ n a lda k1 k2 ipiv incx)
(define-callout zlasyf_ uplo n nb kb a lda ipiv w ldw)
(define-callout zlatbs_ uplo trans diag normin n kd ab ldab x scale cnorm)
(define-callout zlatdf_ ijob n z__ ldz rhs rdsum rdscal ipiv jpiv)
(define-callout zlatps_ uplo trans diag normin n ap x scale cnorm)
(define-callout zlatrd_ uplo n nb a lda e tau w ldw)
(define-callout zlatrs_ uplo trans diag normin n a lda x scale cnorm)
(define-callout zlatrz_ m n l a lda tau work)
(define-callout zlatzm_ side m n v incv tau c1 c2 ldc work)
(define-callout zlauu2_ uplo n a lda)
(define-callout zlauum_ uplo n a lda)
(define-callout zpbcon_ uplo n kd ab ldab anorm rcond work rwork)
(define-callout zpbequ_ uplo n kd ab ldab s scond amax)
(define-callout zpbrfs_ uplo n kd nrhs ab ldab afb ldafb b ldb x ldx ferr berr work rwork)
(define-callout zpbstf_ uplo n kd ab ldab)
(define-callout zpbsv_ uplo n kd nrhs ab ldab b ldb)
(define-callout zpbsvx_ fact uplo n kd nrhs ab ldab afb ldafb equed s b ldb x ldx rcond ferr berr work rwork)
(define-callout zpbtf2_ uplo n kd ab ldab)
(define-callout zpbtrf_ uplo n kd ab ldab)
(define-callout zpbtrs_ uplo n kd nrhs ab ldab b ldb)
(define-callout zpocon_ uplo n a lda anorm rcond work rwork)
(define-callout zpoequ_ n a lda s scond amax)
(define-callout zporfs_ uplo n nrhs a lda af ldaf b ldb x ldx ferr berr work rwork)
(define-callout zposv_ uplo n nrhs a lda b ldb)
(define-callout zposvx_ fact uplo n nrhs a lda af ldaf equed s b ldb x ldx rcond ferr berr work rwork)
(define-callout zpotf2_ uplo n a lda)
(define-callout zpotrf_ uplo n a lda)
(define-callout zpotri_ uplo n a lda)
(define-callout zpotrs_ uplo n nrhs a lda b ldb)
(define-callout zppcon_ uplo n ap anorm rcond work rwork)
(define-callout zppequ_ uplo n ap s scond amax)
(define-callout zpprfs_ uplo n nrhs ap afp b ldb x ldx ferr berr work rwork)
(define-callout zppsv_ uplo n nrhs ap b ldb)
(define-callout zppsvx_ fact uplo n nrhs ap afp equed s b ldb x ldx rcond ferr berr work rwork)
(define-callout zpptrf_ uplo n ap)
(define-callout zpptri_ uplo n ap)
(define-callout zpptrs_ uplo n nrhs ap b ldb)
(define-callout zptcon_ n d__ e anorm rcond rwork)
(define-callout zptrfs_ uplo n nrhs d__ e df ef b ldb x ldx ferr berr work rwork)
(define-callout zptsv_ n nrhs d__ e b ldb)
(define-callout zptsvx_ fact n nrhs d__ e df ef b ldb x ldx rcond ferr berr work rwork)
(define-callout zpttrf_ n d__ e)
(define-callout zpttrs_ uplo n nrhs d__ e b ldb)
(define-callout zptts2_ iuplo n nrhs d__ e b ldb)
(define-callout zrot_ n cx incx cy incy c__ s)
(define-callout zspcon_ uplo n ap ipiv anorm rcond work)
(define-callout zspmv_ uplo n alpha ap x incx beta y incy)
(define-callout zspr_ uplo n alpha x incx ap)
(define-callout zsprfs_ uplo n nrhs ap afp ipiv b ldb x ldx ferr berr work rwork)
(define-callout zspsv_ uplo n nrhs ap ipiv b ldb)
(define-callout zspsvx_ fact uplo n nrhs ap afp ipiv b ldb x ldx rcond ferr berr work rwork)
(define-callout zsptrf_ uplo n ap ipiv)
(define-callout zsptri_ uplo n ap ipiv work)
(define-callout zsptrs_ uplo n nrhs ap ipiv b ldb)
(define-callout zstedc_ compz n d__ e z__ ldz work lwork rwork lrwork iwork liwork)
(define-callout zstein_ n d__ e m w iblock isplit z__ ldz work iwork ifail)
(define-callout zsteqr_ compz n d__ e z__ ldz work)
(define-callout zsycon_ uplo n a lda ipiv anorm rcond work)
(define-callout zsymv_ uplo n alpha a lda x incx beta y incy)
(define-callout zsyr_ uplo n alpha x incx a lda)
(define-callout zsyrfs_ uplo n nrhs a lda af ldaf ipiv b ldb x ldx ferr berr work rwork)
(define-callout zsysv_ uplo n nrhs a lda ipiv b ldb work lwork)
(define-callout zsysvx_ fact uplo n nrhs a lda af ldaf ipiv b ldb x ldx rcond ferr berr work lwork rwork)
(define-callout zsytf2_ uplo n a lda ipiv)
(define-callout zsytrf_ uplo n a lda ipiv work lwork)
(define-callout zsytri_ uplo n a lda ipiv work)
(define-callout zsytrs_ uplo n nrhs a lda ipiv b ldb)
(define-callout ztbcon_ norm uplo diag n kd ab ldab rcond work rwork)
(define-callout ztbrfs_ uplo trans diag n kd nrhs ab ldab b ldb x ldx ferr berr work rwork)
(define-callout ztbtrs_ uplo trans diag n kd nrhs ab ldab b ldb)
(define-callout ztgevc_ side howmny select n a lda b ldb vl ldvl vr ldvr mm m work rwork)
(define-callout ztgex2_ wantq wantz n a lda b ldb q ldq z__ ldz j1)
(define-callout ztgexc_ wantq wantz n a lda b ldb q ldq z__ ldz ifst ilst)
(define-callout ztgsen_ ijob wantq wantz select n a lda b ldb alpha beta q ldq z__ ldz m pl pr dif work lwork iwork liwork)
(define-callout ztgsja_ jobu jobv jobq m p n k l a lda b ldb tola tolb alpha beta u ldu v ldv q ldq work ncycle)
(define-callout ztgsna_ job howmny select n a lda b ldb vl ldvl vr ldvr s dif mm m work lwork iwork)
(define-callout ztgsy2_ trans ijob m n a lda b ldb c__ ldc d__ ldd e lde f ldf scale rdsum rdscal)
(define-callout ztgsyl_ trans ijob m n a lda b ldb c__ ldc d__ ldd e lde f ldf scale dif work lwork iwork)
(define-callout ztpcon_ norm uplo diag n ap rcond work rwork)
(define-callout ztprfs_ uplo trans diag n nrhs ap b ldb x ldx ferr berr work rwork)
(define-callout ztptri_ uplo diag n ap)
(define-callout ztptrs_ uplo trans diag n nrhs ap b ldb)
(define-callout ztrcon_ norm uplo diag n a lda rcond work rwork)
(define-callout ztrevc_ side howmny select n t ldt vl ldvl vr ldvr mm m work rwork)
(define-callout ztrexc_ compq n t ldt q ldq ifst ilst)
(define-callout ztrrfs_ uplo trans diag n nrhs a lda b ldb x ldx ferr berr work rwork)
(define-callout ztrsen_ job compq select n t ldt q ldq w m s sep work lwork)
(define-callout ztrsna_ job howmny select n t ldt vl ldvl vr ldvr s sep mm m work ldwork rwork)
(define-callout ztrsyl_ trana tranb isgn m n a lda b ldb c__ ldc scale)
(define-callout ztrti2_ uplo diag n a lda)
(define-callout ztrtri_ uplo diag n a lda)
(define-callout ztrtrs_ uplo trans diag n nrhs a lda b ldb)
(define-callout ztzrqf_ m n a lda tau)
(define-callout ztzrzf_ m n a lda tau work lwork)
(define-callout zung2l_ m n k a lda tau work)
(define-callout zung2r_ m n k a lda tau work)
(define-callout zungbr_ vect m n k a lda tau work lwork)
(define-callout zunghr_ n ilo ihi a lda tau work lwork)
(define-callout zungl2_ m n k a lda tau work)
(define-callout zunglq_ m n k a lda tau work lwork)
(define-callout zungql_ m n k a lda tau work lwork)
(define-callout zungqr_ m n k a lda tau work lwork)
(define-callout zungr2_ m n k a lda tau work)
(define-callout zungrq_ m n k a lda tau work lwork)
(define-callout zungtr_ uplo n a lda tau work lwork)
(define-callout zunm2l_ side trans m n k a lda tau c__ ldc work)
(define-callout zunm2r_ side trans m n k a lda tau c__ ldc work)
(define-callout zunmbr_ vect side trans m n k a lda tau c__ ldc work lwork)
(define-callout zunmhr_ side trans m n ilo ihi a lda tau c__ ldc work lwork)
(define-callout zunml2_ side trans m n k a lda tau c__ ldc work)
(define-callout zunmlq_ side trans m n k a lda tau c__ ldc work lwork)
(define-callout zunmql_ side trans m n k a lda tau c__ ldc work lwork)
(define-callout zunmqr_ side trans m n k a lda tau c__ ldc work lwork)
(define-callout zunmr2_ side trans m n k a lda tau c__ ldc work)
(define-callout zunmr3_ side trans m n k l a lda tau c__ ldc work)
(define-callout zunmrq_ side trans m n k a lda tau c__ ldc work lwork)
(define-callout zunmrz_ side trans m n k l a lda tau c__ ldc work lwork)
(define-callout zunmtr_ side uplo trans m n a lda tau c__ ldc work lwork)
(define-callout zupgtr_ uplo n ap tau q ldq work)
(define-callout zupmtr_ side uplo trans m n ap tau c__ ldc work)


;;;; high-level functions helpers

(define %memory-cache
  (make-block-cache (* 10 strideof-integer) 5))

(define-syntax define-integer-pointer
  (syntax-rules ()
    ((_ ?pointer-name ?cursor ?value)
     (define ?pointer-name
       (begin0-let ((p ?cursor))
	 (pointer-incr! ?cursor strideof-integer)
	 (pointer-set-c-integer! p 0 ?value))))))

(define-syntax let-ip
  (syntax-rules ()
    ((_ ((?ptr ?val) ...) ?form0 ?form ...)
     (let ((p (%memory-cache)))
       (dynamic-wind	;we do not use continuations
	   (lambda () #f)
	   (lambda ()
	     (let ((q p))
	       (define-integer-pointer ?ptr q ?val)
	       ...
	       ?form0 ?form ...))
	   (lambda ()
	     (%memory-cache p)))))))


;;;; linear equations

(define (dgesv n nrhs a lda ipiv b ldb)
  (let-ip ((n*		n)
	   (nrhs*	nrhs)
	   (lda*	lda)
	   (ldb*	ldb)
	   (info*	0))
	  (let ((result (dgesv_ n* nrhs* a lda* ipiv b ldb* info*)))
	    (%process-result 'dgesv result
			     (pointer-ref-c-integer info* 0)
			     '(n nrhs a lda ipiv b ldb)
			     (list n nrhs a lda ipiv b ldb)))))

(define (zgesv n nrhs a lda ipiv b ldb)
  (let-ip ((n*		n)
	   (nrhs*	nrhs)
	   (lda*	lda)
	   (ldb*	ldb)
	   (info*	0))
	  (let ((result (zgesv_ n* nrhs* a lda* ipiv b ldb* info*)))
	    (%process-result 'dgesv result
			     (pointer-ref-c-integer info* 0)
			     '(n nrhs a lda ipiv b ldb)
			     (list n nrhs a lda ipiv b ldb)))))


;;;; done

)

;;; end of file
