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

    upper*		lower*

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
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign memory)
    (foreign cstrings)
    (foreign math lapack platform)
    (foreign math lapack sizeof))


(define constants-pool
  (malloc (+ strideof-int strideof-int ;; UPPER* and LOWER*
	     strideof-integer		;; INFO*
	     )))

(define upper*
  (begin0-let ((p constants-pool))
    (pointer-set-c-signed-char! p 0 (char->integer #\U))))

(define lower*
  (begin0-let ((p (pointer-add upper* strideof-int)))
    (pointer-set-c-signed-char! p 0 (char->integer #\L))))

(define info*
  (begin0-let ((p (pointer-add lower* strideof-integer)))
    (pointer-set-c-integer! p 0 0)))


(define-syntax define-func
  (lambda (stx)
    (define (name->pubname name)
      (let ((n (symbol->string name)))
	(string->symbol (substring n 0 (- (string-length n) 1)))))
    (syntax-case stx ()
      ((_ ?name ?arg ...)
       (with-syntax ((PUBNAME (datum->syntax #'?name (name->pubname (syntax->datum #'?name)))))
	 #'(define (PUBNAME ?arg ...)
	     (pointer-set-c-integer! info* 0 0)
	     (let* ((result (?name ?arg ... info*))
		    (info   (pointer-ref-c-integer info* 0)))
	       (cond ((< info 0)
		      (let ((index (- info))
			    (names '(?arg ...))
			    (args  (list ?arg ...)))
			(error (quote PUBNAME)
			  (string-append "invalid argument '"
					 (list-ref names index)"' (position "
					 (number->string index)
					 ")")
			  (list-ref args index))))
		     ((> info 0)
		      (error (quote PUBNAME)
			(string-append "error in the course of computation at step "
				       (number->string info))))
		     (else result)))))))))


(define-func cbdsqr_ uplo n ncvt nru ncc d__ e vt ldvt u ldu c__ ldc rwork)
(define-func cgbbrd_ vect m n ncc kl ku ab ldab d__ e q ldq pt ldpt c__ ldc work rwork)
(define-func cgbcon_ norm n kl ku ab ldab ipiv anorm rcond work rwork)
(define-func cgbequ_ m n kl ku ab ldab r__ c__ rowcnd colcnd amax)
(define-func cgbrfs_ trans n kl ku nrhs ab ldab afb ldafb ipiv b ldb x ldx ferr berr work rwork)
(define-func cgbsv_ n kl ku nrhs ab ldab ipiv b ldb)
(define-func cgbsvx_ fact trans n kl ku nrhs ab ldab afb ldafb ipiv equed r__ c__ b ldb x ldx rcond ferr berr work rwork)
(define-func cgbtf2_ m n kl ku ab ldab ipiv)
(define-func cgbtrf_ m n kl ku ab ldab ipiv)
(define-func cgbtrs_ trans n kl ku nrhs ab ldab ipiv b ldb)
(define-func cgebak_ job side n ilo ihi scale m v ldv)
(define-func cgebal_ job n a lda ilo ihi scale)
(define-func cgebd2_ m n a lda d__ e tauq taup work)
(define-func cgebrd_ m n a lda d__ e tauq taup work lwork)
(define-func cgecon_ norm n a lda anorm rcond work rwork)
(define-func cgeequ_ m n a lda r__ c__ rowcnd colcnd amax)
(define-func cgees_ jobvs sort L_fp select n a lda sdim w vs ldvs work lwork rwork bwork)
(define-func cgeesx_ jobvs sort L_fp select sense n a lda sdim w vs ldvs rconde rcondv work lwork rwork bwork)
(define-func cgeev_ jobvl jobvr n a lda w vl ldvl vr ldvr work lwork rwork)
(define-func cgeevx_ balanc jobvl jobvr sense n a lda w vl ldvl vr ldvr ilo ihi scale abnrm rconde rcondv work lwork rwork)
(define-func cgegs_ jobvsl jobvsr n a lda b ldb alpha beta vsl ldvsl vsr ldvsr work lwork rwork)
(define-func cgegv_ jobvl jobvr n a lda b ldb alpha beta vl ldvl vr ldvr work lwork rwork)
(define-func cgehd2_ n ilo ihi a lda tau work)
(define-func cgehrd_ n ilo ihi a lda tau work lwork)
(define-func cgelq2_ m n a lda tau work)
(define-func cgelqf_ m n a lda tau work lwork)
(define-func cgels_ trans m n nrhs a lda b ldb work lwork)
(define-func cgelsx_ m n nrhs a lda b ldb jpvt rcond rank work rwork)
(define-func cgelsy_ m n nrhs a lda b ldb jpvt rcond rank work lwork rwork)
(define-func cgeql2_ m n a lda tau work)
(define-func cgeqlf_ m n a lda tau work lwork)
(define-func cgeqp3_ m n a lda jpvt tau work lwork rwork)
(define-func cgeqpf_ m n a lda jpvt tau work rwork)
(define-func cgeqr2_ m n a lda tau work)
(define-func cgeqrf_ m n a lda tau work lwork)
(define-func cgerfs_ trans n nrhs a lda af ldaf ipiv b ldb x ldx ferr berr work rwork)
(define-func cgerq2_ m n a lda tau work)
(define-func cgerqf_ m n a lda tau work lwork)
(define-func cgesc2_ n a lda rhs ipiv jpiv scale)
(define-func cgesv_ n nrhs a lda ipiv b ldb)
(define-func cgesvx_ fact trans n nrhs a lda af ldaf ipiv equed r__ c__ b ldb x ldx rcond ferr berr work rwork)
(define-func cgetc2_ n a lda ipiv jpiv)
(define-func cgetf2_ m n a lda ipiv)
(define-func cgetrf_ m n a lda ipiv)
(define-func cgetri_ n a lda ipiv work lwork)
(define-func cgetrs_ trans n nrhs a lda ipiv b ldb)
(define-func cggbak_ job side n ilo ihi lscale rscale m v ldv)
(define-func cggbal_ job n a lda b ldb ilo ihi lscale rscale work)
(define-func cgges_ jobvsl jobvsr sort L_fp selctg n a lda b ldb sdim alpha beta vsl ldvsl vsr ldvsr work lwork rwork bwork)
(define-func cggesx_ jobvsl jobvsr sort L_fp selctg sense n a lda b ldb sdim alpha beta vsl ldvsl vsr ldvsr rconde rcondv work lwork rwork iwork liwork bwork)
(define-func cggev_ jobvl jobvr n a lda b ldb alpha beta vl ldvl vr ldvr work lwork rwork)
(define-func cggevx_ balanc jobvl jobvr sense n a lda b ldb alpha beta vl ldvl vr ldvr ilo ihi lscale rscale abnrm bbnrm rconde rcondv work lwork rwork iwork bwork)
(define-func cggglm_ n m p a lda b ldb d__ x y work lwork)
(define-func cgghrd_ compq compz n ilo ihi a lda b ldb q ldq z__ ldz)
(define-func cgglse_ m n p a lda b ldb c__ d__ x work lwork)
(define-func cggqrf_ n m p a lda taua b ldb taub work lwork)
(define-func cggrqf_ m p n a lda taua b ldb taub work lwork)
(define-func cggsvd_ jobu jobv jobq m n p k l a lda b ldb alpha beta u ldu v ldv q ldq work rwork iwork)
(define-func cggsvp_ jobu jobv jobq m p n a lda b ldb tola tolb k l u ldu v ldv q ldq iwork rwork tau work)
(define-func cgtcon_ norm n dl d__ du du2 ipiv anorm rcond work)
(define-func cgtrfs_ trans n nrhs dl d__ du dlf df duf du2 ipiv b ldb x ldx ferr berr work rwork)
(define-func cgtsv_ n nrhs dl d__ du b ldb)
(define-func cgtsvx_ fact trans n nrhs dl d__ du dlf df duf du2 ipiv b ldb x ldx rcond ferr berr work rwork)
(define-func cgttrf_ n dl d__ du du2 ipiv)
(define-func cgttrs_ trans n nrhs dl d__ du du2 ipiv b ldb)
(define-func cgtts2_ itrans n nrhs dl d__ du du2 ipiv b ldb)
(define-func chbev_ jobz uplo n kd ab ldab w z__ ldz work rwork)
(define-func chbevd_ jobz uplo n kd ab ldab w z__ ldz work lwork rwork lrwork iwork liwork)
(define-func chbevx_ jobz range uplo n kd ab ldab q ldq vl vu il iu abstol m w z__ ldz work rwork iwork ifail)
(define-func chbgst_ vect uplo n ka kb ab ldab bb ldbb x ldx work rwork)
(define-func chbgv_ jobz uplo n ka kb ab ldab bb ldbb w z__ ldz work rwork)
(define-func chbgvx_ jobz range uplo n ka kb ab ldab bb ldbb q ldq vl vu il iu abstol m w z__ ldz work rwork iwork ifail)
(define-func chbtrd_ vect uplo n kd ab ldab d__ e q ldq work)
(define-func checon_ uplo n a lda ipiv anorm rcond work)
(define-func cheev_ jobz uplo n a lda w work lwork rwork)
(define-func cheevd_ jobz uplo n a lda w work lwork rwork lrwork iwork liwork)
(define-func cheevr_ jobz range uplo n a lda vl vu il iu abstol m w z__ ldz isuppz work lwork rwork lrwork iwork liwork)
(define-func cheevx_ jobz range uplo n a lda vl vu il iu abstol m w z__ ldz work lwork rwork iwork ifail)
(define-func chegs2_ itype uplo n a lda b ldb)
(define-func chegst_ itype uplo n a lda b ldb)
(define-func chegv_ itype jobz uplo n a lda b ldb w work lwork rwork)
(define-func chegvd_ itype jobz uplo n a lda b ldb w work lwork rwork lrwork iwork liwork)
(define-func chegvx_ itype jobz range uplo n a lda b ldb vl vu il iu abstol m w z__ ldz work lwork rwork iwork ifail)
(define-func cherfs_ uplo n nrhs a lda af ldaf ipiv b ldb x ldx ferr berr work rwork)
(define-func chesv_ uplo n nrhs a lda ipiv b ldb work lwork)
(define-func chesvx_ fact uplo n nrhs a lda af ldaf ipiv b ldb x ldx rcond ferr berr work lwork rwork)
(define-func chetf2_ uplo n a lda ipiv)
(define-func chetrd_ uplo n a lda d__ e tau work lwork)
(define-func chetrf_ uplo n a lda ipiv work lwork)
(define-func chetri_ uplo n a lda ipiv work)
(define-func chetrs_ uplo n nrhs a lda ipiv b ldb)
(define-func chgeqz_ job compq compz n ilo ihi a lda b ldb alpha beta q ldq z__ ldz work lwork rwork)
(define-func chpcon_ uplo n ap ipiv anorm rcond work)
(define-func chpev_ jobz uplo n ap w z__ ldz work rwork)
(define-func chpevd_ jobz uplo n ap w z__ ldz work lwork rwork lrwork iwork liwork)
(define-func chpevx_ jobz range uplo n ap vl vu il iu abstol m w z__ ldz work rwork iwork ifail)
(define-func chpgst_ itype uplo n ap bp)
(define-func chpgv_ itype jobz uplo n ap bp w z__ ldz work rwork)
(define-func chpgvd_ itype jobz uplo n ap bp w z__ ldz work lwork rwork lrwork iwork liwork)
(define-func chpgvx_ itype jobz range uplo n ap bp vl vu il iu abstol m w z__ ldz work rwork iwork ifail)
(define-func chprfs_ uplo n nrhs ap afp ipiv b ldb x ldx ferr berr work rwork)
(define-func chpsv_ uplo n nrhs ap ipiv b ldb)
(define-func chpsvx_ fact uplo n nrhs ap afp ipiv b ldb x ldx rcond ferr berr work rwork)
(define-func chptrd_ uplo n ap d__ e tau)
(define-func chptrf_ uplo n ap ipiv)
(define-func chptri_ uplo n ap ipiv work)
(define-func chptrs_ uplo n nrhs ap ipiv b ldb)
(define-func chsein_ side eigsrc initv select n h__ ldh w vl ldvl vr ldvr mm m work rwork ifaill ifailr)
(define-func chseqr_ job compz n ilo ihi h__ ldh w z__ ldz work lwork)
(define-func clabrd_ m n nb a lda d__ e tauq taup x ldx y ldy)
(define-func clacgv_ n x incx)
(define-func clacon_ n v x est kase)
(define-func clacp2_ uplo m n a lda b ldb)
(define-func clacpy_ uplo m n a lda b ldb)
(define-func clacrm_ m n a lda b ldb c__ ldc rwork)
(define-func clacrt_ n cx incx cy incy c__ s)
(define-func claed0_ qsiz n d__ e q ldq qstore ldqs rwork iwork)
(define-func claed7_ n cutpnt qsiz tlvls curlvl curpbm d__ q ldq rho indxq qstore qptr prmptr perm givptr givcol givnum work rwork iwork)
(define-func claed8_ k n qsiz q ldq d__ rho cutpnt z__ dlamda q2 ldq2 w indxp indx indxq perm givptr givcol givnum)
(define-func claein_ rightv noinit n h__ ldh w v b ldb rwork eps3 smlnum)
(define-func claesy_ a b c__ rt1 rt2 evscal cs1 sn1)
(define-func claev2_ a b c__ rt1 rt2 cs1 sn1)
(define-func clags2_ upper a1 a2 a3 b1 b2 b3 csu snu csv snv csq snq)
(define-func clagtm_ trans n nrhs alpha dl d__ du x ldx beta b ldb)
(define-func clahef_ uplo n nb kb a lda ipiv w ldw)
(define-func clahqr_ wantt wantz n ilo ihi h__ ldh w iloz ihiz z__ ldz)
(define-func clahrd_ n k nb a lda tau t ldt y ldy)
(define-func claic1_ job j x sest w gamma sestpr s c__)
(define-func clals0_ icompq nl nr sqre nrhs b ldb bx ldbx perm givptr givcol ldgcol givnum ldgnum poles difl difr z__ k c__ s rwork)
(define-func clalsa_ icompq smlsiz n nrhs b ldb bx ldbx u ldu vt k difl difr z__ poles givptr givcol ldgcol perm givnum c__ s rwork iwork)
(define-func clapll_ n x incx y incy ssmin)
(define-func clapmt_ forwrd m n x ldx k)
(define-func claqgb_ m n kl ku ab ldab r__ c__ rowcnd colcnd amax equed)
(define-func claqge_ m n a lda r__ c__ rowcnd colcnd amax equed)
(define-func claqhb_ uplo n kd ab ldab s scond amax equed)
(define-func claqhe_ uplo n a lda s scond amax equed)
(define-func claqhp_ uplo n ap s scond amax equed)
(define-func claqp2_ m n offset a lda jpvt tau vn1 vn2 work)
(define-func claqps_ m n offset nb kb a lda jpvt tau vn1 vn2 auxv f ldf)
(define-func claqsb_ uplo n kd ab ldab s scond amax equed)
(define-func claqsp_ uplo n ap s scond amax equed)
(define-func claqsy_ uplo n a lda s scond amax equed)
(define-func clar1v_ n b1 bn sigma d__ l ld lld gersch z__ ztz mingma r__ isuppz work)
(define-func clar2v_ n x y z__ incx c__ s incc)
(define-func clarcm_ m n a lda b ldb c__ ldc rwork)
(define-func clarf_ side m n v incv tau c__ ldc work)
(define-func clarfb_ side trans direct storev m n k v ldv t ldt c__ ldc work ldwork)
(define-func clarfg_ n alpha x incx tau)
(define-func clarft_ direct storev n k v ldv tau t ldt)
(define-func clarfx_ side m n v tau c__ ldc work)
(define-func clargv_ n x incx y incy c__ incc)
(define-func clarnv_ idist iseed n x)
(define-func clarrv_ n d__ l isplit m w iblock gersch tol z__ ldz isuppz work iwork)
(define-func clartg_ f g cs sn r__)
(define-func clartv_ n x incx y incy c__ s incc)
(define-func clarz_ side m n l v incv tau c__ ldc work)
(define-func clarzb_ side trans direct storev m n k l v ldv t ldt c__ ldc work ldwork)
(define-func clarzt_ direct storev n k v ldv tau t ldt)
(define-func clascl_ type__ kl ku cfrom cto m n a lda)
(define-func claset_ uplo m n alpha beta a lda)
(define-func clasr_ side pivot direct m n c__ s a lda)
(define-func classq_ n x incx scale sumsq)
(define-func claswp_ n a lda k1 k2 ipiv incx)
(define-func clasyf_ uplo n nb kb a lda ipiv w ldw)
(define-func clatbs_ uplo trans diag normin n kd ab ldab x scale cnorm)
(define-func clatdf_ ijob n z__ ldz rhs rdsum rdscal ipiv jpiv)
(define-func clatps_ uplo trans diag normin n ap x scale cnorm)
(define-func clatrd_ uplo n nb a lda e tau w ldw)
(define-func clatrs_ uplo trans diag normin n a lda x scale cnorm)
(define-func clatrz_ m n l a lda tau work)
(define-func clatzm_ side m n v incv tau c1 c2 ldc work)
(define-func clauu2_ uplo n a lda)
(define-func clauum_ uplo n a lda)
(define-func cpbcon_ uplo n kd ab ldab anorm rcond work rwork)
(define-func cpbequ_ uplo n kd ab ldab s scond amax)
(define-func cpbrfs_ uplo n kd nrhs ab ldab afb ldafb b ldb x ldx ferr berr work rwork)
(define-func cpbstf_ uplo n kd ab ldab)
(define-func cpbsv_ uplo n kd nrhs ab ldab b ldb)
(define-func cpbsvx_ fact uplo n kd nrhs ab ldab afb ldafb equed s b ldb x ldx rcond ferr berr work rwork)
(define-func cpbtf2_ uplo n kd ab ldab)
(define-func cpbtrf_ uplo n kd ab ldab)
(define-func cpbtrs_ uplo n kd nrhs ab ldab b ldb)
(define-func cpocon_ uplo n a lda anorm rcond work rwork)
(define-func cpoequ_ n a lda s scond amax)
(define-func cporfs_ uplo n nrhs a lda af ldaf b ldb x ldx ferr berr work rwork)
(define-func cposv_ uplo n nrhs a lda b ldb)
(define-func cposvx_ fact uplo n nrhs a lda af ldaf equed s b ldb x ldx rcond ferr berr work rwork)
(define-func cpotf2_ uplo n a lda)
(define-func cpotrf_ uplo n a lda)
(define-func cpotri_ uplo n a lda)
(define-func cpotrs_ uplo n nrhs a lda b ldb)
(define-func cppcon_ uplo n ap anorm rcond work rwork)
(define-func cppequ_ uplo n ap s scond amax)
(define-func cpprfs_ uplo n nrhs ap afp b ldb x ldx ferr berr work rwork)
(define-func cppsv_ uplo n nrhs ap b ldb)
(define-func cppsvx_ fact uplo n nrhs ap afp equed s b ldb x ldx rcond ferr berr work rwork)
(define-func cpptrf_ uplo n ap)
(define-func cpptri_ uplo n ap)
(define-func cpptrs_ uplo n nrhs ap b ldb)
(define-func cptcon_ n d__ e anorm rcond rwork)
(define-func cptrfs_ uplo n nrhs d__ e df ef b ldb x ldx ferr berr work rwork)
(define-func cptsv_ n nrhs d__ e b ldb)
(define-func cptsvx_ fact n nrhs d__ e df ef b ldb x ldx rcond ferr berr work rwork)
(define-func cpttrf_ n d__ e)
(define-func cpttrs_ uplo n nrhs d__ e b ldb)
(define-func cptts2_ iuplo n nrhs d__ e b ldb)
(define-func crot_ n cx incx cy incy c__ s)
(define-func cspcon_ uplo n ap ipiv anorm rcond work)
(define-func cspmv_ uplo n alpha ap x incx beta y incy)
(define-func cspr_ uplo n alpha x incx ap)
(define-func csprfs_ uplo n nrhs ap afp ipiv b ldb x ldx ferr berr work rwork)
(define-func cspsv_ uplo n nrhs ap ipiv b ldb)
(define-func cspsvx_ fact uplo n nrhs ap afp ipiv b ldb x ldx rcond ferr berr work rwork)
(define-func csptrf_ uplo n ap ipiv)
(define-func csptri_ uplo n ap ipiv work)
(define-func csptrs_ uplo n nrhs ap ipiv b ldb)
(define-func csrot_ n cx incx cy incy c__ s)
(define-func csrscl_ n sa sx incx)
(define-func cstedc_ compz n d__ e z__ ldz work lwork rwork lrwork iwork liwork)
(define-func cstein_ n d__ e m w iblock isplit z__ ldz work iwork ifail)
(define-func csteqr_ compz n d__ e z__ ldz work)
(define-func csycon_ uplo n a lda ipiv anorm rcond work)
(define-func csymv_ uplo n alpha a lda x incx beta y incy)
(define-func csyr_ uplo n alpha x incx a lda)
(define-func csyrfs_ uplo n nrhs a lda af ldaf ipiv b ldb x ldx ferr berr work rwork)
(define-func csysv_ uplo n nrhs a lda ipiv b ldb work lwork)
(define-func csysvx_ fact uplo n nrhs a lda af ldaf ipiv b ldb x ldx rcond ferr berr work lwork rwork)
(define-func csytf2_ uplo n a lda ipiv)
(define-func csytrf_ uplo n a lda ipiv work lwork)
(define-func csytri_ uplo n a lda ipiv work)
(define-func csytrs_ uplo n nrhs a lda ipiv b ldb)
(define-func ctbcon_ norm uplo diag n kd ab ldab rcond work rwork)
(define-func ctbrfs_ uplo trans diag n kd nrhs ab ldab b ldb x ldx ferr berr work rwork)
(define-func ctbtrs_ uplo trans diag n kd nrhs ab ldab b ldb)
(define-func ctgevc_ side howmny select n a lda b ldb vl ldvl vr ldvr mm m work rwork)
(define-func ctgex2_ wantq wantz n a lda b ldb q ldq z__ ldz j1)
(define-func ctgexc_ wantq wantz n a lda b ldb q ldq z__ ldz ifst ilst)
(define-func ctgsen_ ijob wantq wantz select n a lda b ldb alpha beta q ldq z__ ldz m pl pr dif work lwork iwork liwork)
(define-func ctgsja_ jobu jobv jobq m p n k l a lda b ldb tola tolb alpha beta u ldu v ldv q ldq work ncycle)
(define-func ctgsna_ job howmny select n a lda b ldb vl ldvl vr ldvr s dif mm m work lwork iwork)
(define-func ctgsy2_ trans ijob m n a lda b ldb c__ ldc d__ ldd e lde f ldf scale rdsum rdscal)
(define-func ctgsyl_ trans ijob m n a lda b ldb c__ ldc d__ ldd e lde f ldf scale dif work lwork iwork)
(define-func ctpcon_ norm uplo diag n ap rcond work rwork)
(define-func ctprfs_ uplo trans diag n nrhs ap b ldb x ldx ferr berr work rwork)
(define-func ctptri_ uplo diag n ap)
(define-func ctptrs_ uplo trans diag n nrhs ap b ldb)
(define-func ctrcon_ norm uplo diag n a lda rcond work rwork)
(define-func ctrevc_ side howmny select n t ldt vl ldvl vr ldvr mm m work rwork)
(define-func ctrexc_ compq n t ldt q ldq ifst ilst)
(define-func ctrrfs_ uplo trans diag n nrhs a lda b ldb x ldx ferr berr work rwork)
(define-func ctrsen_ job compq select n t ldt q ldq w m s sep work lwork)
(define-func ctrsna_ job howmny select n t ldt vl ldvl vr ldvr s sep mm m work ldwork rwork)
(define-func ctrsyl_ trana tranb isgn m n a lda b ldb c__ ldc scale)
(define-func ctrti2_ uplo diag n a lda)
(define-func ctrtri_ uplo diag n a lda)
(define-func ctrtrs_ uplo trans diag n nrhs a lda b ldb)
(define-func ctzrqf_ m n a lda tau)
(define-func ctzrzf_ m n a lda tau work lwork)
(define-func cung2l_ m n k a lda tau work)
(define-func cung2r_ m n k a lda tau work)
(define-func cungbr_ vect m n k a lda tau work lwork)
(define-func cunghr_ n ilo ihi a lda tau work lwork)
(define-func cungl2_ m n k a lda tau work)
(define-func cunglq_ m n k a lda tau work lwork)
(define-func cungql_ m n k a lda tau work lwork)
(define-func cungqr_ m n k a lda tau work lwork)
(define-func cungr2_ m n k a lda tau work)
(define-func cungrq_ m n k a lda tau work lwork)
(define-func cungtr_ uplo n a lda tau work lwork)
(define-func cunm2l_ side trans m n k a lda tau c__ ldc work)
(define-func cunm2r_ side trans m n k a lda tau c__ ldc work)
(define-func cunmbr_ vect side trans m n k a lda tau c__ ldc work lwork)
(define-func cunmhr_ side trans m n ilo ihi a lda tau c__ ldc work lwork)
(define-func cunml2_ side trans m n k a lda tau c__ ldc work)
(define-func cunmlq_ side trans m n k a lda tau c__ ldc work lwork)
(define-func cunmql_ side trans m n k a lda tau c__ ldc work lwork)
(define-func cunmqr_ side trans m n k a lda tau c__ ldc work lwork)
(define-func cunmr2_ side trans m n k a lda tau c__ ldc work)
(define-func cunmr3_ side trans m n k l a lda tau c__ ldc work)
(define-func cunmrq_ side trans m n k a lda tau c__ ldc work lwork)
(define-func cunmrz_ side trans m n k l a lda tau c__ ldc work lwork)
(define-func cunmtr_ side uplo trans m n a lda tau c__ ldc work lwork)
(define-func cupgtr_ uplo n ap tau q ldq work)
(define-func cupmtr_ side uplo trans m n ap tau c__ ldc work)
(define-func dbdsdc_ uplo compq n d__ e u ldu vt ldvt q iq work iwork)
(define-func dbdsqr_ uplo n ncvt nru ncc d__ e vt ldvt u ldu c__ ldc work)
(define-func ddisna_ job m n d__ sep)
(define-func dgbbrd_ vect m n ncc kl ku ab ldab d__ e q ldq pt ldpt c__ ldc work)
(define-func dgbcon_ norm n kl ku ab ldab ipiv anorm rcond work iwork)
(define-func dgbequ_ m n kl ku ab ldab r__ c__ rowcnd colcnd amax)
(define-func dgbrfs_ trans n kl ku nrhs ab ldab afb ldafb ipiv b ldb x ldx ferr berr work iwork)
(define-func dgbsv_ n kl ku nrhs ab ldab ipiv b ldb)
(define-func dgbsvx_ fact trans n kl ku nrhs ab ldab afb ldafb ipiv equed r__ c__ b ldb x ldx rcond ferr berr work iwork)
(define-func dgbtf2_ m n kl ku ab ldab ipiv)
(define-func dgbtrf_ m n kl ku ab ldab ipiv)
(define-func dgbtrs_ trans n kl ku nrhs ab ldab ipiv b ldb)
(define-func dgebak_ job side n ilo ihi scale m v ldv)
(define-func dgebal_ job n a lda ilo ihi scale)
(define-func dgebd2_ m n a lda d__ e tauq taup work)
(define-func dgebrd_ m n a lda d__ e tauq taup work lwork)
(define-func dgecon_ norm n a lda anorm rcond work iwork)
(define-func dgeequ_ m n a lda r__ c__ rowcnd colcnd amax)
(define-func dgees_ jobvs sort L_fp select n a lda sdim wr wi vs ldvs work lwork bwork)
(define-func dgeesx_ jobvs sort L_fp select sense n a lda sdim wr wi vs ldvs rconde rcondv work lwork iwork liwork bwork)
(define-func dgeev_ jobvl jobvr n a lda wr wi vl ldvl vr ldvr work lwork)
(define-func dgeevx_ balanc jobvl jobvr sense n a lda wr wi vl ldvl vr ldvr ilo ihi scale abnrm rconde rcondv work lwork iwork)
(define-func dgegs_ jobvsl jobvsr n a lda b ldb alphar alphai beta vsl ldvsl vsr ldvsr work lwork)
(define-func dgegv_ jobvl jobvr n a lda b ldb alphar alphai beta vl ldvl vr ldvr work lwork)
(define-func dgehd2_ n ilo ihi a lda tau work)
(define-func dgehrd_ n ilo ihi a lda tau work lwork)
(define-func dgelq2_ m n a lda tau work)
(define-func dgelqf_ m n a lda tau work lwork)
(define-func dgels_ trans m n nrhs a lda b ldb work lwork)
(define-func dgelsd_ m n nrhs a lda b ldb s rcond rank work lwork iwork)
(define-func dgelss_ m n nrhs a lda b ldb s rcond rank work lwork)
(define-func dgelsx_ m n nrhs a lda b ldb jpvt rcond rank work)
(define-func dgelsy_ m n nrhs a lda b ldb jpvt rcond rank work lwork)
(define-func dgeql2_ m n a lda tau work)
(define-func dgeqlf_ m n a lda tau work lwork)
(define-func dgeqp3_ m n a lda jpvt tau work lwork)
(define-func dgeqpf_ m n a lda jpvt tau work)
(define-func dgeqr2_ m n a lda tau work)
(define-func dgeqrf_ m n a lda tau work lwork)
(define-func dgerfs_ trans n nrhs a lda af ldaf ipiv b ldb x ldx ferr berr work iwork)
(define-func dgerq2_ m n a lda tau work)
(define-func dgerqf_ m n a lda tau work lwork)
(define-func dgesc2_ n a lda rhs ipiv jpiv scale)
(define-func dgesdd_ jobz m n a lda s u ldu vt ldvt work lwork iwork)
(define-func dgesv_ n nrhs a lda ipiv b ldb)
(define-func dgesvd_ jobu jobvt m n a lda s u ldu vt ldvt work lwork)
(define-func dgesvx_ fact trans n nrhs a lda af ldaf ipiv equed r__ c__ b ldb x ldx rcond ferr berr work iwork)
(define-func dgetc2_ n a lda ipiv jpiv)
(define-func dgetf2_ m n a lda ipiv)
(define-func dgetrf_ m n a lda ipiv)
(define-func dgetri_ n a lda ipiv work lwork)
(define-func dgetrs_ trans n nrhs a lda ipiv b ldb)
(define-func dggbak_ job side n ilo ihi lscale rscale m v ldv)
(define-func dggbal_ job n a lda b ldb ilo ihi lscale rscale work)
(define-func dgges_ jobvsl jobvsr sort L_fp delctg n a lda b ldb sdim alphar alphai beta vsl ldvsl vsr ldvsr work lwork bwork)
(define-func dggesx_ jobvsl jobvsr sort L_fp delctg sense n a lda b ldb sdim alphar alphai beta vsl ldvsl vsr ldvsr rconde rcondv work lwork iwork liwork bwork)
(define-func dggev_ jobvl jobvr n a lda b ldb alphar alphai beta vl ldvl vr ldvr work lwork)
(define-func dggevx_ balanc jobvl jobvr sense n a lda b ldb alphar alphai beta vl ldvl vr ldvr ilo ihi lscale rscale abnrm bbnrm rconde rcondv work lwork iwork bwork)
(define-func dggglm_ n m p a lda b ldb d__ x y work lwork)
(define-func dgghrd_ compq compz n ilo ihi a lda b ldb q ldq z__ ldz)
(define-func dgglse_ m n p a lda b ldb c__ d__ x work lwork)
(define-func dggqrf_ n m p a lda taua b ldb taub work lwork)
(define-func dggrqf_ m p n a lda taua b ldb taub work lwork)
(define-func dggsvd_ jobu jobv jobq m n p k l a lda b ldb alpha beta u ldu v ldv q ldq work iwork)
(define-func dggsvp_ jobu jobv jobq m p n a lda b ldb tola tolb k l u ldu v ldv q ldq iwork tau work)
(define-func dgtcon_ norm n dl d__ du du2 ipiv anorm rcond work iwork)
(define-func dgtrfs_ trans n nrhs dl d__ du dlf df duf du2 ipiv b ldb x ldx ferr berr work iwork)
(define-func dgtsv_ n nrhs dl d__ du b ldb)
(define-func dgtsvx_ fact trans n nrhs dl d__ du dlf df duf du2 ipiv b ldb x ldx rcond ferr berr work iwork)
(define-func dgttrf_ n dl d__ du du2 ipiv)
(define-func dgttrs_ trans n nrhs dl d__ du du2 ipiv b ldb)
(define-func dgtts2_ itrans n nrhs dl d__ du du2 ipiv b ldb)
(define-func dhgeqz_ job compq compz n ilo ihi a lda b ldb alphar alphai beta q ldq z__ ldz work lwork)
(define-func dhsein_ side eigsrc initv select n h__ ldh wr wi vl ldvl vr ldvr mm m work ifaill ifailr)
(define-func dhseqr_ job compz n ilo ihi h__ ldh wr wi z__ ldz work lwork)
(define-func dlabad_ small large)
(define-func dlabrd_ m n nb a lda d__ e tauq taup x ldx y ldy)
(define-func dlacon_ n v x isgn est kase)
(define-func dlacpy_ uplo m n a lda b ldb)
(define-func dladiv_ a b c__ d__ p q)
(define-func dlae2_ a b c__ rt1 rt2)
(define-func dlaebz_ ijob nitmax n mmax minp nbmin abstol reltol pivmin d__ e e2 nval ab c__ mout nab work iwork)
(define-func dlaed0_ icompq qsiz n d__ e q ldq qstore ldqs work iwork)
(define-func dlaed1_ n d__ q ldq indxq rho cutpnt work iwork)
(define-func dlaed2_ k n n1 d__ q ldq indxq rho z__ dlamda w q2 indx indxc indxp coltyp)
(define-func dlaed3_ k n n1 d__ q ldq rho dlamda q2 indx ctot w s)
(define-func dlaed4_ n i__ d__ z__ delta rho dlam)
(define-func dlaed5_ i__ d__ z__ delta rho dlam)
(define-func dlaed6_ kniter orgati rho d__ z__ finit tau)
(define-func dlaed7_ icompq n qsiz tlvls curlvl curpbm d__ q ldq indxq rho cutpnt qstore qptr prmptr perm givptr givcol givnum work iwork)
(define-func dlaed8_ icompq k n qsiz d__ q ldq indxq rho cutpnt z__ dlamda q2 ldq2 w perm givptr givcol givnum indxp indx)
(define-func dlaed9_ k kstart kstop n d__ q ldq rho dlamda w s lds)
(define-func dlaeda_ n tlvls curlvl curpbm prmptr perm givptr givcol givnum q qptr z__ ztemp)
(define-func dlaein_ rightv noinit n h__ ldh wr wi vr vi b ldb work eps3 smlnum bignum)
(define-func dlaev2_ a b c__ rt1 rt2 cs1 sn1)
(define-func dlaexc_ wantq n t ldt q ldq j1 n1 n2 work)
(define-func dlag2_ a lda b ldb safmin scale1 scale2 wr1 wr2 wi)
(define-func dlags2_ upper a1 a2 a3 b1 b2 b3 csu snu csv snv csq snq)
(define-func dlagtf_ n a lambda b c__ tol d__ in)
(define-func dlagtm_ trans n nrhs alpha dl d__ du x ldx beta b ldb)
(define-func dlagts_ job n a b c__ d__ in y tol)
(define-func dlagv2_ a lda b ldb alphar alphai beta csl snl csr snr)
(define-func dlahqr_ wantt wantz n ilo ihi h__ ldh wr wi iloz ihiz z__ ldz)
(define-func dlahrd_ n k nb a lda tau t ldt y ldy)
(define-func dlaic1_ job j x sest w gamma sestpr s c__)
(define-func dlaln2_ ltrans na nw smin ca a lda d1 d2 b ldb wr wi x ldx scale xnorm)
(define-func dlals0_ icompq nl nr sqre nrhs b ldb bx ldbx perm givptr givcol ldgcol givnum ldgnum poles difl difr z__ k c__ s work)
(define-func dlalsa_ icompq smlsiz n nrhs b ldb bx ldbx u ldu vt k difl difr z__ poles givptr givcol ldgcol perm givnum c__ s work iwork)
(define-func dlalsd_ uplo smlsiz n nrhs d__ e b ldb rcond rank work iwork)
(define-func dlamc1_ beta t rnd ieee1)
(define-func dlamc2_ beta t rnd eps emin rmin emax rmax)
(define-func dlamc4_ emin start base)
(define-func dlamc5_ beta p emin ieee emax rmax)
(define-func dlamrg_ n1 n2 a dtrd1 dtrd2 index)
(define-func dlanv2_ a b c__ d__ rt1r rt1i rt2r rt2i cs sn)
(define-func dlapll_ n x incx y incy ssmin)
(define-func dlapmt_ forwrd m n x ldx k)
(define-func dlaqgb_ m n kl ku ab ldab r__ c__ rowcnd colcnd amax equed)
(define-func dlaqge_ m n a lda r__ c__ rowcnd colcnd amax equed)
(define-func dlaqp2_ m n offset a lda jpvt tau vn1 vn2 work)
(define-func dlaqps_ m n offset nb kb a lda jpvt tau vn1 vn2 auxv f ldf)
(define-func dlaqsb_ uplo n kd ab ldab s scond amax equed)
(define-func dlaqsp_ uplo n ap s scond amax equed)
(define-func dlaqsy_ uplo n a lda s scond amax equed)
(define-func dlaqtr_ ltran lreal n t ldt b w scale x work)
(define-func dlar1v_ n b1 bn sigma d__ l ld lld gersch z__ ztz mingma r__ isuppz work)
(define-func dlar2v_ n x y z__ incx c__ s incc)
(define-func dlarf_ side m n v incv tau c__ ldc work)
(define-func dlarfb_ side trans direct storev m n k v ldv t ldt c__ ldc work ldwork)
(define-func dlarfg_ n alpha x incx tau)
(define-func dlarft_ direct storev n k v ldv tau t ldt)
(define-func dlarfx_ side m n v tau c__ ldc work)
(define-func dlargv_ n x incx y incy c__ incc)
(define-func dlarnv_ idist iseed n x)
(define-func dlarrb_ n d__ l ld lld ifirst ilast sigma reltol w wgap werr work iwork)
(define-func dlarre_ n d__ e tol nsplit isplit m w woff gersch work)
(define-func dlarrf_ n d__ l ld lld ifirst ilast w dplus lplus work iwork)
(define-func dlarrv_ n d__ l isplit m w iblock gersch tol z__ ldz isuppz work iwork)
(define-func dlartg_ f g cs sn r__)
(define-func dlartv_ n x incx y incy c__ s incc)
(define-func dlaruv_ iseed n x)
(define-func dlarz_ side m n l v incv tau c__ ldc work)
(define-func dlarzb_ side trans direct storev m n k l v ldv t ldt c__ ldc work ldwork)
(define-func dlarzt_ direct storev n k v ldv tau t ldt)
(define-func dlas2_ f g h__ ssmin ssmax)
(define-func dlascl_ type__ kl ku cfrom cto m n a lda)
(define-func dlasd0_ n sqre d__ e u ldu vt ldvt smlsiz iwork work)
(define-func dlasd1_ nl nr sqre d__ alpha beta u ldu vt ldvt idxq iwork work)
(define-func dlasd2_ nl nr sqre k d__ z__ alpha beta u ldu vt ldvt dsigma u2 ldu2 vt2 ldvt2 idxp idx idxc idxq coltyp)
(define-func dlasd3_ nl nr sqre k d__ q ldq dsigma u ldu u2 ldu2 vt ldvt vt2 ldvt2 idxc ctot z__)
(define-func dlasd4_ n i__ d__ z__ delta rho sigma work)
(define-func dlasd5_ i__ d__ z__ delta rho dsigma work)
(define-func dlasd6_ icompq nl nr sqre d__ vf vl alpha beta idxq perm givptr givcol ldgcol givnum ldgnum poles difl difr z__ k c__ s work iwork)
(define-func dlasd7_ icompq nl nr sqre k d__ z__ zw vf vfw vl vlw alpha beta dsigma idx idxp idxq perm givptr givcol ldgcol givnum ldgnum c__ s)
(define-func dlasd8_ icompq k d__ z__ vf vl difl difr lddifr dsigma work)
(define-func dlasd9_ icompq ldu k d__ z__ vf vl difl difr dsigma work)
(define-func dlasda_ icompq smlsiz n sqre d__ e u ldu vt k difl difr z__ poles givptr givcol ldgcol perm givnum c__ s work iwork)
(define-func dlasdq_ uplo sqre n ncvt nru ncc d__ e vt ldvt u ldu c__ ldc work)
(define-func dlasdt_ n lvl nd inode ndiml ndimr msub)
(define-func dlaset_ uplo m n alpha beta a lda)
(define-func dlasq1_ n d__ e work)
(define-func dlasq2_ n z__)
(define-func dlasq3_ i0 n0 z__ pp dmin__ sigma desig qmax nfail iter ndiv ieee)
(define-func dlasq4_ i0 n0 z__ pp n0in dmin__ dmin1 dmin2 dn dn1 dn2 tau ttype)
(define-func dlasq5_ i0 n0 z__ pp tau dmin__ dmin1 dmin2 dn dnm1 dnm2 ieee)
(define-func dlasq6_ i0 n0 z__ pp dmin__ dmin1 dmin2 dn dnm1 dnm2)
(define-func dlasr_ side pivot direct m n c__ s a lda)
(define-func dlasrt_ id n d__)
(define-func dlassq_ n x incx scale sumsq)
(define-func dlasv2_ f g h__ ssmin ssmax snr csr snl csl)
(define-func dlaswp_ n a lda k1 k2 ipiv incx)
(define-func dlasy2_ ltranl ltranr isgn n1 n2 tl ldtl tr ldtr b ldb scale x ldx xnorm)
(define-func dlasyf_ uplo n nb kb a lda ipiv w ldw)
(define-func dlatbs_ uplo trans diag normin n kd ab ldab x scale cnorm)
(define-func dlatdf_ ijob n z__ ldz rhs rdsum rdscal ipiv jpiv)
(define-func dlatps_ uplo trans diag normin n ap x scale cnorm)
(define-func dlatrd_ uplo n nb a lda e tau w ldw)
(define-func dlatrs_ uplo trans diag normin n a lda x scale cnorm)
(define-func dlatrz_ m n l a lda tau work)
(define-func dlatzm_ side m n v incv tau c1 c2 ldc work)
(define-func dlauu2_ uplo n a lda)
(define-func dlauum_ uplo n a lda)
(define-func dopgtr_ uplo n ap tau q ldq work)
(define-func dopmtr_ side uplo trans m n ap tau c__ ldc work)
(define-func dorg2l_ m n k a lda tau work)
(define-func dorg2r_ m n k a lda tau work)
(define-func dorgbr_ vect m n k a lda tau work lwork)
(define-func dorghr_ n ilo ihi a lda tau work lwork)
(define-func dorgl2_ m n k a lda tau work)
(define-func dorglq_ m n k a lda tau work lwork)
(define-func dorgql_ m n k a lda tau work lwork)
(define-func dorgqr_ m n k a lda tau work lwork)
(define-func dorgr2_ m n k a lda tau work)
(define-func dorgrq_ m n k a lda tau work lwork)
(define-func dorgtr_ uplo n a lda tau work lwork)
(define-func dorm2l_ side trans m n k a lda tau c__ ldc work)
(define-func dorm2r_ side trans m n k a lda tau c__ ldc work)
(define-func dormbr_ vect side trans m n k a lda tau c__ ldc work lwork)
(define-func dormhr_ side trans m n ilo ihi a lda tau c__ ldc work lwork)
(define-func dorml2_ side trans m n k a lda tau c__ ldc work)
(define-func dormlq_ side trans m n k a lda tau c__ ldc work lwork)
(define-func dormql_ side trans m n k a lda tau c__ ldc work lwork)
(define-func dormqr_ side trans m n k a lda tau c__ ldc work lwork)
(define-func dormr2_ side trans m n k a lda tau c__ ldc work)
(define-func dormr3_ side trans m n k l a lda tau c__ ldc work)
(define-func dormrq_ side trans m n k a lda tau c__ ldc work lwork)
(define-func dormrz_ side trans m n k l a lda tau c__ ldc work lwork)
(define-func dormtr_ side uplo trans m n a lda tau c__ ldc work lwork)
(define-func dpbcon_ uplo n kd ab ldab anorm rcond work iwork)
(define-func dpbequ_ uplo n kd ab ldab s scond amax)
(define-func dpbrfs_ uplo n kd nrhs ab ldab afb ldafb b ldb x ldx ferr berr work iwork)
(define-func dpbstf_ uplo n kd ab ldab)
(define-func dpbsv_ uplo n kd nrhs ab ldab b ldb)
(define-func dpbsvx_ fact uplo n kd nrhs ab ldab afb ldafb equed s b ldb x ldx rcond ferr berr work iwork)
(define-func dpbtf2_ uplo n kd ab ldab)
(define-func dpbtrf_ uplo n kd ab ldab)
(define-func dpbtrs_ uplo n kd nrhs ab ldab b ldb)
(define-func dpocon_ uplo n a lda anorm rcond work iwork)
(define-func dpoequ_ n a lda s scond amax)
(define-func dporfs_ uplo n nrhs a lda af ldaf b ldb x ldx ferr berr work iwork)
(define-func dposv_ uplo n nrhs a lda b ldb)
(define-func dposvx_ fact uplo n nrhs a lda af ldaf equed s b ldb x ldx rcond ferr berr work iwork)
(define-func dpotf2_ uplo n a lda)
(define-func dpotrf_ uplo n a lda)
(define-func dpotri_ uplo n a lda)
(define-func dpotrs_ uplo n nrhs a lda b ldb)
(define-func dppcon_ uplo n ap anorm rcond work iwork)
(define-func dppequ_ uplo n ap s scond amax)
(define-func dpprfs_ uplo n nrhs ap afp b ldb x ldx ferr berr work iwork)
(define-func dppsv_ uplo n nrhs ap b ldb)
(define-func dppsvx_ fact uplo n nrhs ap afp equed s b ldb x ldx rcond ferr berr work iwork)
(define-func dpptrf_ uplo n ap)
(define-func dpptri_ uplo n ap)
(define-func dpptrs_ uplo n nrhs ap b ldb)
(define-func dptcon_ n d__ e anorm rcond work)
(define-func dpteqr_ compz n d__ e z__ ldz work)
(define-func dptrfs_ n nrhs d__ e df ef b ldb x ldx ferr berr work)
(define-func dptsv_ n nrhs d__ e b ldb)
(define-func dptsvx_ fact n nrhs d__ e df ef b ldb x ldx rcond ferr berr work)
(define-func dpttrf_ n d__ e)
(define-func dpttrs_ n nrhs d__ e b ldb)
(define-func dptts2_ n nrhs d__ e b ldb)
(define-func drscl_ n sa sx incx)
(define-func dsbev_ jobz uplo n kd ab ldab w z__ ldz work)
(define-func dsbevd_ jobz uplo n kd ab ldab w z__ ldz work lwork iwork liwork)
(define-func dsbevx_ jobz range uplo n kd ab ldab q ldq vl vu il iu abstol m w z__ ldz work iwork ifail)
(define-func dsbgst_ vect uplo n ka kb ab ldab bb ldbb x ldx work)
(define-func dsbgv_ jobz uplo n ka kb ab ldab bb ldbb w z__ ldz work)
(define-func dsbgvd_ jobz uplo n ka kb ab ldab bb ldbb w z__ ldz work lwork iwork liwork)
(define-func dsbgvx_ jobz range uplo n ka kb ab ldab bb ldbb q ldq vl vu il iu abstol m w z__ ldz work iwork ifail)
(define-func dsbtrd_ vect uplo n kd ab ldab d__ e q ldq work)
(define-func dspcon_ uplo n ap ipiv anorm rcond work iwork)
(define-func dspev_ jobz uplo n ap w z__ ldz work)
(define-func dspevd_ jobz uplo n ap w z__ ldz work lwork iwork liwork)
(define-func dspevx_ jobz range uplo n ap vl vu il iu abstol m w z__ ldz work iwork ifail)
(define-func dspgst_ itype uplo n ap bp)
(define-func dspgv_ itype jobz uplo n ap bp w z__ ldz work)
(define-func dspgvd_ itype jobz uplo n ap bp w z__ ldz work lwork iwork liwork)
(define-func dspgvx_ itype jobz range uplo n ap bp vl vu il iu abstol m w z__ ldz work iwork ifail)
(define-func dsprfs_ uplo n nrhs ap afp ipiv b ldb x ldx ferr berr work iwork)
(define-func dspsv_ uplo n nrhs ap ipiv b ldb)
(define-func dspsvx_ fact uplo n nrhs ap afp ipiv b ldb x ldx rcond ferr berr work iwork)
(define-func dsptrd_ uplo n ap d__ e tau)
(define-func dsptrf_ uplo n ap ipiv)
(define-func dsptri_ uplo n ap ipiv work)
(define-func dsptrs_ uplo n nrhs ap ipiv b ldb)
(define-func dstebz_ range order n vl vu il iu abstol d__ e m nsplit w iblock isplit work iwork)
(define-func dstedc_ compz n d__ e z__ ldz work lwork iwork liwork)
(define-func dstegr_ jobz range n d__ e vl vu il iu abstol m w z__ ldz isuppz work lwork iwork liwork)
(define-func dstein_ n d__ e m w iblock isplit z__ ldz work iwork ifail)
(define-func dsteqr_ compz n d__ e z__ ldz work)
(define-func dsterf_ n d__ e)
(define-func dstev_ jobz n d__ e z__ ldz work)
(define-func dstevd_ jobz n d__ e z__ ldz work lwork iwork liwork)
(define-func dstevr_ jobz range n d__ e vl vu il iu abstol m w z__ ldz isuppz work lwork iwork liwork)
(define-func dstevx_ jobz range n d__ e vl vu il iu abstol m w z__ ldz work iwork ifail)
(define-func dsycon_ uplo n a lda ipiv anorm rcond work iwork)
(define-func dsyev_ jobz uplo n a lda w work lwork)
(define-func dsyevd_ jobz uplo n a lda w work lwork iwork liwork)
(define-func dsyevr_ jobz range uplo n a lda vl vu il iu abstol m w z__ ldz isuppz work lwork iwork liwork)
(define-func dsyevx_ jobz range uplo n a lda vl vu il iu abstol m w z__ ldz work lwork iwork ifail)
(define-func dsygs2_ itype uplo n a lda b ldb)
(define-func dsygst_ itype uplo n a lda b ldb)
(define-func dsygv_ itype jobz uplo n a lda b ldb w work lwork)
(define-func dsygvd_ itype jobz uplo n a lda b ldb w work lwork iwork liwork)
(define-func dsygvx_ itype jobz range uplo n a lda b ldb vl vu il iu abstol m w z__ ldz work lwork iwork ifail)
(define-func dsyrfs_ uplo n nrhs a lda af ldaf ipiv b ldb x ldx ferr berr work iwork)
(define-func dsysv_ uplo n nrhs a lda ipiv b ldb work lwork)
(define-func dsysvx_ fact uplo n nrhs a lda af ldaf ipiv b ldb x ldx rcond ferr berr work lwork iwork)
(define-func dsytd2_ uplo n a lda d__ e tau)
(define-func dsytf2_ uplo n a lda ipiv)
(define-func dsytrd_ uplo n a lda d__ e tau work lwork)
(define-func dsytrf_ uplo n a lda ipiv work lwork)
(define-func dsytri_ uplo n a lda ipiv work)
(define-func dsytrs_ uplo n nrhs a lda ipiv b ldb)
(define-func dtbcon_ norm uplo diag n kd ab ldab rcond work iwork)
(define-func dtbrfs_ uplo trans diag n kd nrhs ab ldab b ldb x ldx ferr berr work iwork)
(define-func dtbtrs_ uplo trans diag n kd nrhs ab ldab b ldb)
(define-func dtgevc_ side howmny select n a lda b ldb vl ldvl vr ldvr mm m work)
(define-func dtgex2_ wantq wantz n a lda b ldb q ldq z__ ldz j1 n1 n2 work lwork)
(define-func dtgexc_ wantq wantz n a lda b ldb q ldq z__ ldz ifst ilst work lwork)
(define-func dtgsen_ ijob wantq wantz select n a lda b ldb alphar alphai beta q ldq z__ ldz m pl pr dif work lwork iwork liwork)
(define-func dtgsja_ jobu jobv jobq m p n k l a lda b ldb tola tolb alpha beta u ldu v ldv q ldq work ncycle)
(define-func dtgsna_ job howmny select n a lda b ldb vl ldvl vr ldvr s dif mm m work lwork iwork)
(define-func dtgsy2_ trans ijob m n a lda b ldb c__ ldc d__ ldd e lde f ldf scale rdsum rdscal iwork pq)
(define-func dtgsyl_ trans ijob m n a lda b ldb c__ ldc d__ ldd e lde f ldf scale dif work lwork iwork)
(define-func dtpcon_ norm uplo diag n ap rcond work iwork)
(define-func dtprfs_ uplo trans diag n nrhs ap b ldb x ldx ferr berr work iwork)
(define-func dtptri_ uplo diag n ap)
(define-func dtptrs_ uplo trans diag n nrhs ap b ldb)
(define-func dtrcon_ norm uplo diag n a lda rcond work iwork)
(define-func dtrevc_ side howmny select n t ldt vl ldvl vr ldvr mm m work)
(define-func dtrexc_ compq n t ldt q ldq ifst ilst work)
(define-func dtrrfs_ uplo trans diag n nrhs a lda b ldb x ldx ferr berr work iwork)
(define-func dtrsen_ job compq select n t ldt q ldq wr wi m s sep work lwork iwork liwork)
(define-func dtrsna_ job howmny select n t ldt vl ldvl vr ldvr s sep mm m work ldwork iwork)
(define-func dtrsyl_ trana tranb isgn m n a lda b ldb c__ ldc scale)
(define-func dtrti2_ uplo diag n a lda)
(define-func dtrtri_ uplo diag n a lda)
(define-func dtrtrs_ uplo trans diag n nrhs a lda b ldb)
(define-func dtzrqf_ m n a lda tau)
(define-func dtzrzf_ m n a lda tau work lwork)
(define-func icmax1_ n cx incx)
(define-func ieeeck_ ispec zero one)
(define-func ilaenv_ ispec name__ opts n1 n2 n3 n4 name_len opts_len)
(define-func izmax1_ n cx incx)
(define-func sbdsdc_ uplo compq n d__ e u ldu vt ldvt q iq work iwork)
(define-func sbdsqr_ uplo n ncvt nru ncc d__ e vt ldvt u ldu c__ ldc work)
(define-func sdisna_ job m n d__ sep)
(define-func sgbbrd_ vect m n ncc kl ku ab ldab d__ e q ldq pt ldpt c__ ldc work)
(define-func sgbcon_ norm n kl ku ab ldab ipiv anorm rcond work iwork)
(define-func sgbequ_ m n kl ku ab ldab r__ c__ rowcnd colcnd amax)
(define-func sgbrfs_ trans n kl ku nrhs ab ldab afb ldafb ipiv b ldb x ldx ferr berr work iwork)
(define-func sgbsv_ n kl ku nrhs ab ldab ipiv b ldb)
(define-func sgbsvx_ fact trans n kl ku nrhs ab ldab afb ldafb ipiv equed r__ c__ b ldb x ldx rcond ferr berr work iwork)
(define-func sgbtf2_ m n kl ku ab ldab ipiv)
(define-func sgbtrf_ m n kl ku ab ldab ipiv)
(define-func sgbtrs_ trans n kl ku nrhs ab ldab ipiv b ldb)
(define-func sgebak_ job side n ilo ihi scale m v ldv)
(define-func sgebal_ job n a lda ilo ihi scale)
(define-func sgebd2_ m n a lda d__ e tauq taup work)
(define-func sgebrd_ m n a lda d__ e tauq taup work lwork)
(define-func sgecon_ norm n a lda anorm rcond work iwork)
(define-func sgeequ_ m n a lda r__ c__ rowcnd colcnd amax)
(define-func sgees_ jobvs sort L_fp select n a lda sdim wr wi vs ldvs work lwork bwork)
(define-func sgeesx_ jobvs sort L_fp select sense n a lda sdim wr wi vs ldvs rconde rcondv work lwork iwork liwork bwork)
(define-func sgeev_ jobvl jobvr n a lda wr wi vl ldvl vr ldvr work lwork)
(define-func sgeevx_ balanc jobvl jobvr sense n a lda wr wi vl ldvl vr ldvr ilo ihi scale abnrm rconde rcondv work lwork iwork)
(define-func sgegs_ jobvsl jobvsr n a lda b ldb alphar alphai beta vsl ldvsl vsr ldvsr work lwork)
(define-func sgegv_ jobvl jobvr n a lda b ldb alphar alphai beta vl ldvl vr ldvr work lwork)
(define-func sgehd2_ n ilo ihi a lda tau work)
(define-func sgehrd_ n ilo ihi a lda tau work lwork)
(define-func sgelq2_ m n a lda tau work)
(define-func sgelqf_ m n a lda tau work lwork)
(define-func sgels_ trans m n nrhs a lda b ldb work lwork)
(define-func sgelsd_ m n nrhs a lda b ldb s rcond rank work lwork iwork)
(define-func sgelss_ m n nrhs a lda b ldb s rcond rank work lwork)
(define-func sgelsx_ m n nrhs a lda b ldb jpvt rcond rank work)
(define-func sgelsy_ m n nrhs a lda b ldb jpvt rcond rank work lwork)
(define-func sgeql2_ m n a lda tau work)
(define-func sgeqlf_ m n a lda tau work lwork)
(define-func sgeqp3_ m n a lda jpvt tau work lwork)
(define-func sgeqpf_ m n a lda jpvt tau work)
(define-func sgeqr2_ m n a lda tau work)
(define-func sgeqrf_ m n a lda tau work lwork)
(define-func sgerfs_ trans n nrhs a lda af ldaf ipiv b ldb x ldx ferr berr work iwork)
(define-func sgerq2_ m n a lda tau work)
(define-func sgerqf_ m n a lda tau work lwork)
(define-func sgesc2_ n a lda rhs ipiv jpiv scale)
(define-func sgesdd_ jobz m n a lda s u ldu vt ldvt work lwork iwork)
(define-func sgesv_ n nrhs a lda ipiv b ldb)
(define-func sgesvd_ jobu jobvt m n a lda s u ldu vt ldvt work lwork)
(define-func sgesvx_ fact trans n nrhs a lda af ldaf ipiv equed r__ c__ b ldb x ldx rcond ferr berr work iwork)
(define-func sgetc2_ n a lda ipiv jpiv)
(define-func sgetf2_ m n a lda ipiv)
(define-func sgetrf_ m n a lda ipiv)
(define-func sgetri_ n a lda ipiv work lwork)
(define-func sgetrs_ trans n nrhs a lda ipiv b ldb)
(define-func sggbak_ job side n ilo ihi lscale rscale m v ldv)
(define-func sggbal_ job n a lda b ldb ilo ihi lscale rscale work)
(define-func sgges_ jobvsl jobvsr sort L_fp selctg n a lda b ldb sdim alphar alphai beta vsl ldvsl vsr ldvsr work lwork bwork)
(define-func sggesx_ jobvsl jobvsr sort L_fp selctg sense n a lda b ldb sdim alphar alphai beta vsl ldvsl vsr ldvsr rconde rcondv work lwork iwork liwork bwork)
(define-func sggev_ jobvl jobvr n a lda b ldb alphar alphai beta vl ldvl vr ldvr work lwork)
(define-func sggevx_ balanc jobvl jobvr sense n a lda b ldb alphar alphai beta vl ldvl vr ldvr ilo ihi lscale rscale abnrm bbnrm rconde rcondv work lwork iwork bwork)
(define-func sggglm_ n m p a lda b ldb d__ x y work lwork)
(define-func sgghrd_ compq compz n ilo ihi a lda b ldb q ldq z__ ldz)
(define-func sgglse_ m n p a lda b ldb c__ d__ x work lwork)
(define-func sggqrf_ n m p a lda taua b ldb taub work lwork)
(define-func sggrqf_ m p n a lda taua b ldb taub work lwork)
(define-func sggsvd_ jobu jobv jobq m n p k l a lda b ldb alpha beta u ldu v ldv q ldq work iwork)
(define-func sggsvp_ jobu jobv jobq m p n a lda b ldb tola tolb k l u ldu v ldv q ldq iwork tau work)
(define-func sgtcon_ norm n dl d__ du du2 ipiv anorm rcond work iwork)
(define-func sgtrfs_ trans n nrhs dl d__ du dlf df duf du2 ipiv b ldb x ldx ferr berr work iwork)
(define-func sgtsv_ n nrhs dl d__ du b ldb)
(define-func sgtsvx_ fact trans n nrhs dl d__ du dlf df duf du2 ipiv b ldb x ldx rcond ferr berr work iwork)
(define-func sgttrf_ n dl d__ du du2 ipiv)
(define-func sgttrs_ trans n nrhs dl d__ du du2 ipiv b ldb)
(define-func sgtts2_ itrans n nrhs dl d__ du du2 ipiv b ldb)
(define-func shgeqz_ job compq compz n ilo ihi a lda b ldb alphar alphai beta q ldq z__ ldz work lwork)
(define-func shsein_ side eigsrc initv select n h__ ldh wr wi vl ldvl vr ldvr mm m work ifaill ifailr)
(define-func shseqr_ job compz n ilo ihi h__ ldh wr wi z__ ldz work lwork)
(define-func slabad_ small large)
(define-func slabrd_ m n nb a lda d__ e tauq taup x ldx y ldy)
(define-func slacon_ n v x isgn est kase)
(define-func slacpy_ uplo m n a lda b ldb)
(define-func sladiv_ a b c__ d__ p q)
(define-func slae2_ a b c__ rt1 rt2)
(define-func slaebz_ ijob nitmax n mmax minp nbmin abstol reltol pivmin d__ e e2 nval ab c__ mout nab work iwork)
(define-func slaed0_ icompq qsiz n d__ e q ldq qstore ldqs work iwork)
(define-func slaed1_ n d__ q ldq indxq rho cutpnt work iwork)
(define-func slaed2_ k n n1 d__ q ldq indxq rho z__ dlamda w q2 indx indxc indxp coltyp)
(define-func slaed3_ k n n1 d__ q ldq rho dlamda q2 indx ctot w s)
(define-func slaed4_ n i__ d__ z__ delta rho dlam)
(define-func slaed5_ i__ d__ z__ delta rho dlam)
(define-func slaed6_ kniter orgati rho d__ z__ finit tau)
(define-func slaed7_ icompq n qsiz tlvls curlvl curpbm d__ q ldq indxq rho cutpnt qstore qptr prmptr perm givptr givcol givnum work iwork)
(define-func slaed8_ icompq k n qsiz d__ q ldq indxq rho cutpnt z__ dlamda q2 ldq2 w perm givptr givcol givnum indxp indx)
(define-func slaed9_ k kstart kstop n d__ q ldq rho dlamda w s lds)
(define-func slaeda_ n tlvls curlvl curpbm prmptr perm givptr givcol givnum q qptr z__ ztemp)
(define-func slaein_ rightv noinit n h__ ldh wr wi vr vi b ldb work eps3 smlnum bignum)
(define-func slaev2_ a b c__ rt1 rt2 cs1 sn1)
(define-func slaexc_ wantq n t ldt q ldq j1 n1 n2 work)
(define-func slag2_ a lda b ldb safmin scale1 scale2 wr1 wr2 wi)
(define-func slags2_ upper a1 a2 a3 b1 b2 b3 csu snu csv snv csq snq)
(define-func slagtf_ n a lambda b c__ tol d__ in)
(define-func slagtm_ trans n nrhs alpha dl d__ du x ldx beta b ldb)
(define-func slagts_ job n a b c__ d__ in y tol)
(define-func slagv2_ a lda b ldb alphar alphai beta csl snl csr snr)
(define-func slahqr_ wantt wantz n ilo ihi h__ ldh wr wi iloz ihiz z__ ldz)
(define-func slahrd_ n k nb a lda tau t ldt y ldy)
(define-func slaic1_ job j x sest w gamma sestpr s c__)
(define-func slaln2_ ltrans na nw smin ca a lda d1 d2 b ldb wr wi x ldx scale xnorm)
(define-func slals0_ icompq nl nr sqre nrhs b ldb bx ldbx perm givptr givcol ldgcol givnum ldgnum poles difl difr z__ k c__ s work)
(define-func slalsa_ icompq smlsiz n nrhs b ldb bx ldbx u ldu vt k difl difr z__ poles givptr givcol ldgcol perm givnum c__ s work iwork)
(define-func slalsd_ uplo smlsiz n nrhs d__ e b ldb rcond rank work iwork)
(define-func slamc1_ beta t rnd ieee1)
(define-func slamc2_ beta t rnd eps emin rmin emax rmax)
(define-func slamc4_ emin start base)
(define-func slamc5_ beta p emin ieee emax rmax)
(define-func slamrg_ n1 n2 a strd1 strd2 index)
(define-func slanv2_ a b c__ d__ rt1r rt1i rt2r rt2i cs sn)
(define-func slapll_ n x incx y incy ssmin)
(define-func slapmt_ forwrd m n x ldx k)
(define-func slaqgb_ m n kl ku ab ldab r__ c__ rowcnd colcnd amax equed)
(define-func slaqge_ m n a lda r__ c__ rowcnd colcnd amax equed)
(define-func slaqp2_ m n offset a lda jpvt tau vn1 vn2 work)
(define-func slaqps_ m n offset nb kb a lda jpvt tau vn1 vn2 auxv f ldf)
(define-func slaqsb_ uplo n kd ab ldab s scond amax equed)
(define-func slaqsp_ uplo n ap s scond amax equed)
(define-func slaqsy_ uplo n a lda s scond amax equed)
(define-func slaqtr_ ltran lreal n t ldt b w scale x work)
(define-func slar1v_ n b1 bn sigma d__ l ld lld gersch z__ ztz mingma r__ isuppz work)
(define-func slar2v_ n x y z__ incx c__ s incc)
(define-func slarf_ side m n v incv tau c__ ldc work)
(define-func slarfb_ side trans direct storev m n k v ldv t ldt c__ ldc work ldwork)
(define-func slarfg_ n alpha x incx tau)
(define-func slarft_ direct storev n k v ldv tau t ldt)
(define-func slarfx_ side m n v tau c__ ldc work)
(define-func slargv_ n x incx y incy c__ incc)
(define-func slarnv_ idist iseed n x)
(define-func slarrb_ n d__ l ld lld ifirst ilast sigma reltol w wgap werr work iwork)
(define-func slarre_ n d__ e tol nsplit isplit m w woff gersch work)
(define-func slarrf_ n d__ l ld lld ifirst ilast w dplus lplus work iwork)
(define-func slarrv_ n d__ l isplit m w iblock gersch tol z__ ldz isuppz work iwork)
(define-func slartg_ f g cs sn r__)
(define-func slartv_ n x incx y incy c__ s incc)
(define-func slaruv_ iseed n x)
(define-func slarz_ side m n l v incv tau c__ ldc work)
(define-func slarzb_ side trans direct storev m n k l v ldv t ldt c__ ldc work ldwork)
(define-func slarzt_ direct storev n k v ldv tau t ldt)
(define-func slas2_ f g h__ ssmin ssmax)
(define-func slascl_ type__ kl ku cfrom cto m n a lda)
(define-func slasd0_ n sqre d__ e u ldu vt ldvt smlsiz iwork work)
(define-func slasd1_ nl nr sqre d__ alpha beta u ldu vt ldvt idxq iwork work)
(define-func slasd2_ nl nr sqre k d__ z__ alpha beta u ldu vt ldvt dsigma u2 ldu2 vt2 ldvt2 idxp idx idxc idxq coltyp)
(define-func slasd3_ nl nr sqre k d__ q ldq dsigma u ldu u2 ldu2 vt ldvt vt2 ldvt2 idxc ctot z__)
(define-func slasd4_ n i__ d__ z__ delta rho sigma work)
(define-func slasd5_ i__ d__ z__ delta rho dsigma work)
(define-func slasd6_ icompq nl nr sqre d__ vf vl alpha beta idxq perm givptr givcol ldgcol givnum ldgnum poles difl difr z__ k c__ s work iwork)
(define-func slasd7_ icompq nl nr sqre k d__ z__ zw vf vfw vl vlw alpha beta dsigma idx idxp idxq perm givptr givcol ldgcol givnum ldgnum c__ s)
(define-func slasd8_ icompq k d__ z__ vf vl difl difr lddifr dsigma work)
(define-func slasd9_ icompq ldu k d__ z__ vf vl difl difr dsigma work)
(define-func slasda_ icompq smlsiz n sqre d__ e u ldu vt k difl difr z__ poles givptr givcol ldgcol perm givnum c__ s work iwork)
(define-func slasdq_ uplo sqre n ncvt nru ncc d__ e vt ldvt u ldu c__ ldc work)
(define-func slasdt_ n lvl nd inode ndiml ndimr msub)
(define-func slaset_ uplo m n alpha beta a lda)
(define-func slasq1_ n d__ e work)
(define-func slasq2_ n z__)
(define-func slasq3_ i0 n0 z__ pp dmin__ sigma desig qmax nfail iter ndiv ieee)
(define-func slasq4_ i0 n0 z__ pp n0in dmin__ dmin1 dmin2 dn dn1 dn2 tau ttype)
(define-func slasq5_ i0 n0 z__ pp tau dmin__ dmin1 dmin2 dn dnm1 dnm2 ieee)
(define-func slasq6_ i0 n0 z__ pp dmin__ dmin1 dmin2 dn dnm1 dnm2)
(define-func slasr_ side pivot direct m n c__ s a lda)
(define-func slasrt_ id n d__)
(define-func slassq_ n x incx scale sumsq)
(define-func slasv2_ f g h__ ssmin ssmax snr csr snl csl)
(define-func slaswp_ n a lda k1 k2 ipiv incx)
(define-func slasy2_ ltranl ltranr isgn n1 n2 tl ldtl tr ldtr b ldb scale x ldx xnorm)
(define-func slasyf_ uplo n nb kb a lda ipiv w ldw)
(define-func slatbs_ uplo trans diag normin n kd ab ldab x scale cnorm)
(define-func slatdf_ ijob n z__ ldz rhs rdsum rdscal ipiv jpiv)
(define-func slatps_ uplo trans diag normin n ap x scale cnorm)
(define-func slatrd_ uplo n nb a lda e tau w ldw)
(define-func slatrs_ uplo trans diag normin n a lda x scale cnorm)
(define-func slatrz_ m n l a lda tau work)
(define-func slatzm_ side m n v incv tau c1 c2 ldc work)
(define-func slauu2_ uplo n a lda)
(define-func slauum_ uplo n a lda)
(define-func sopgtr_ uplo n ap tau q ldq work)
(define-func sopmtr_ side uplo trans m n ap tau c__ ldc work)
(define-func sorg2l_ m n k a lda tau work)
(define-func sorg2r_ m n k a lda tau work)
(define-func sorgbr_ vect m n k a lda tau work lwork)
(define-func sorghr_ n ilo ihi a lda tau work lwork)
(define-func sorgl2_ m n k a lda tau work)
(define-func sorglq_ m n k a lda tau work lwork)
(define-func sorgql_ m n k a lda tau work lwork)
(define-func sorgqr_ m n k a lda tau work lwork)
(define-func sorgr2_ m n k a lda tau work)
(define-func sorgrq_ m n k a lda tau work lwork)
(define-func sorgtr_ uplo n a lda tau work lwork)
(define-func sorm2l_ side trans m n k a lda tau c__ ldc work)
(define-func sorm2r_ side trans m n k a lda tau c__ ldc work)
(define-func sormbr_ vect side trans m n k a lda tau c__ ldc work lwork)
(define-func sormhr_ side trans m n ilo ihi a lda tau c__ ldc work lwork)
(define-func sorml2_ side trans m n k a lda tau c__ ldc work)
(define-func sormlq_ side trans m n k a lda tau c__ ldc work lwork)
(define-func sormql_ side trans m n k a lda tau c__ ldc work lwork)
(define-func sormqr_ side trans m n k a lda tau c__ ldc work lwork)
(define-func sormr2_ side trans m n k a lda tau c__ ldc work)
(define-func sormr3_ side trans m n k l a lda tau c__ ldc work)
(define-func sormrq_ side trans m n k a lda tau c__ ldc work lwork)
(define-func sormrz_ side trans m n k l a lda tau c__ ldc work lwork)
(define-func sormtr_ side uplo trans m n a lda tau c__ ldc work lwork)
(define-func spbcon_ uplo n kd ab ldab anorm rcond work iwork)
(define-func spbequ_ uplo n kd ab ldab s scond amax)
(define-func spbrfs_ uplo n kd nrhs ab ldab afb ldafb b ldb x ldx ferr berr work iwork)
(define-func spbstf_ uplo n kd ab ldab)
(define-func spbsv_ uplo n kd nrhs ab ldab b ldb)
(define-func spbsvx_ fact uplo n kd nrhs ab ldab afb ldafb equed s b ldb x ldx rcond ferr berr work iwork)
(define-func spbtf2_ uplo n kd ab ldab)
(define-func spbtrf_ uplo n kd ab ldab)
(define-func spbtrs_ uplo n kd nrhs ab ldab b ldb)
(define-func spocon_ uplo n a lda anorm rcond work iwork)
(define-func spoequ_ n a lda s scond amax)
(define-func sporfs_ uplo n nrhs a lda af ldaf b ldb x ldx ferr berr work iwork)
(define-func sposv_ uplo n nrhs a lda b ldb)
(define-func sposvx_ fact uplo n nrhs a lda af ldaf equed s b ldb x ldx rcond ferr berr work iwork)
(define-func spotf2_ uplo n a lda)
(define-func spotrf_ uplo n a lda)
(define-func spotri_ uplo n a lda)
(define-func spotrs_ uplo n nrhs a lda b ldb)
(define-func sppcon_ uplo n ap anorm rcond work iwork)
(define-func sppequ_ uplo n ap s scond amax)
(define-func spprfs_ uplo n nrhs ap afp b ldb x ldx ferr berr work iwork)
(define-func sppsv_ uplo n nrhs ap b ldb)
(define-func sppsvx_ fact uplo n nrhs ap afp equed s b ldb x ldx rcond ferr berr work iwork)
(define-func spptrf_ uplo n ap)
(define-func spptri_ uplo n ap)
(define-func spptrs_ uplo n nrhs ap b ldb)
(define-func sptcon_ n d__ e anorm rcond work)
(define-func spteqr_ compz n d__ e z__ ldz work)
(define-func sptrfs_ n nrhs d__ e df ef b ldb x ldx ferr berr work)
(define-func sptsv_ n nrhs d__ e b ldb)
(define-func sptsvx_ fact n nrhs d__ e df ef b ldb x ldx rcond ferr berr work)
(define-func spttrf_ n d__ e)
(define-func spttrs_ n nrhs d__ e b ldb)
(define-func sptts2_ n nrhs d__ e b ldb)
(define-func srscl_ n sa sx incx)
(define-func ssbev_ jobz uplo n kd ab ldab w z__ ldz work)
(define-func ssbevd_ jobz uplo n kd ab ldab w z__ ldz work lwork iwork liwork)
(define-func ssbevx_ jobz range uplo n kd ab ldab q ldq vl vu il iu abstol m w z__ ldz work iwork ifail)
(define-func ssbgst_ vect uplo n ka kb ab ldab bb ldbb x ldx work)
(define-func ssbgv_ jobz uplo n ka kb ab ldab bb ldbb w z__ ldz work)
(define-func ssbgvd_ jobz uplo n ka kb ab ldab bb ldbb w z__ ldz work lwork iwork liwork)
(define-func ssbgvx_ jobz range uplo n ka kb ab ldab bb ldbb q ldq vl vu il iu abstol m w z__ ldz work iwork ifail)
(define-func ssbtrd_ vect uplo n kd ab ldab d__ e q ldq work)
(define-func sspcon_ uplo n ap ipiv anorm rcond work iwork)
(define-func sspev_ jobz uplo n ap w z__ ldz work)
(define-func sspevd_ jobz uplo n ap w z__ ldz work lwork iwork liwork)
(define-func sspevx_ jobz range uplo n ap vl vu il iu abstol m w z__ ldz work iwork ifail)
(define-func sspgst_ itype uplo n ap bp)
(define-func sspgv_ itype jobz uplo n ap bp w z__ ldz work)
(define-func sspgvd_ itype jobz uplo n ap bp w z__ ldz work lwork iwork liwork)
(define-func sspgvx_ itype jobz range uplo n ap bp vl vu il iu abstol m w z__ ldz work iwork ifail)
(define-func ssprfs_ uplo n nrhs ap afp ipiv b ldb x ldx ferr berr work iwork)
(define-func sspsv_ uplo n nrhs ap ipiv b ldb)
(define-func sspsvx_ fact uplo n nrhs ap afp ipiv b ldb x ldx rcond ferr berr work iwork)
(define-func ssptrd_ uplo n ap d__ e tau)
(define-func ssptrf_ uplo n ap ipiv)
(define-func ssptri_ uplo n ap ipiv work)
(define-func ssptrs_ uplo n nrhs ap ipiv b ldb)
(define-func sstebz_ range order n vl vu il iu abstol d__ e m nsplit w iblock isplit work iwork)
(define-func sstedc_ compz n d__ e z__ ldz work lwork iwork liwork)
(define-func sstegr_ jobz range n d__ e vl vu il iu abstol m w z__ ldz isuppz work lwork iwork liwork)
(define-func sstein_ n d__ e m w iblock isplit z__ ldz work iwork ifail)
(define-func ssteqr_ compz n d__ e z__ ldz work)
(define-func ssterf_ n d__ e)
(define-func sstev_ jobz n d__ e z__ ldz work)
(define-func sstevd_ jobz n d__ e z__ ldz work lwork iwork liwork)
(define-func sstevr_ jobz range n d__ e vl vu il iu abstol m w z__ ldz isuppz work lwork iwork liwork)
(define-func sstevx_ jobz range n d__ e vl vu il iu abstol m w z__ ldz work iwork ifail)
(define-func ssycon_ uplo n a lda ipiv anorm rcond work iwork)
(define-func ssyev_ jobz uplo n a lda w work lwork)
(define-func ssyevd_ jobz uplo n a lda w work lwork iwork liwork)
(define-func ssyevr_ jobz range uplo n a lda vl vu il iu abstol m w z__ ldz isuppz work lwork iwork liwork)
(define-func ssyevx_ jobz range uplo n a lda vl vu il iu abstol m w z__ ldz work lwork iwork ifail)
(define-func ssygs2_ itype uplo n a lda b ldb)
(define-func ssygst_ itype uplo n a lda b ldb)
(define-func ssygv_ itype jobz uplo n a lda b ldb w work lwork)
(define-func ssygvd_ itype jobz uplo n a lda b ldb w work lwork iwork liwork)
(define-func ssygvx_ itype jobz range uplo n a lda b ldb vl vu il iu abstol m w z__ ldz work lwork iwork ifail)
(define-func ssyrfs_ uplo n nrhs a lda af ldaf ipiv b ldb x ldx ferr berr work iwork)
(define-func ssysv_ uplo n nrhs a lda ipiv b ldb work lwork)
(define-func ssysvx_ fact uplo n nrhs a lda af ldaf ipiv b ldb x ldx rcond ferr berr work lwork iwork)
(define-func ssytd2_ uplo n a lda d__ e tau)
(define-func ssytf2_ uplo n a lda ipiv)
(define-func ssytrd_ uplo n a lda d__ e tau work lwork)
(define-func ssytrf_ uplo n a lda ipiv work lwork)
(define-func ssytri_ uplo n a lda ipiv work)
(define-func ssytrs_ uplo n nrhs a lda ipiv b ldb)
(define-func stbcon_ norm uplo diag n kd ab ldab rcond work iwork)
(define-func stbrfs_ uplo trans diag n kd nrhs ab ldab b ldb x ldx ferr berr work iwork)
(define-func stbtrs_ uplo trans diag n kd nrhs ab ldab b ldb)
(define-func stgevc_ side howmny select n a lda b ldb vl ldvl vr ldvr mm m work)
(define-func stgex2_ wantq wantz n a lda b ldb q ldq z__ ldz j1 n1 n2 work lwork)
(define-func stgexc_ wantq wantz n a lda b ldb q ldq z__ ldz ifst ilst work lwork)
(define-func stgsen_ ijob wantq wantz select n a lda b ldb alphar alphai beta q ldq z__ ldz m pl pr dif work lwork iwork liwork)
(define-func stgsja_ jobu jobv jobq m p n k l a lda b ldb tola tolb alpha beta u ldu v ldv q ldq work ncycle)
(define-func stgsna_ job howmny select n a lda b ldb vl ldvl vr ldvr s dif mm m work lwork iwork)
(define-func stgsy2_ trans ijob m n a lda b ldb c__ ldc d__ ldd e lde f ldf scale rdsum rdscal iwork pq)
(define-func stgsyl_ trans ijob m n a lda b ldb c__ ldc d__ ldd e lde f ldf scale dif work lwork iwork)
(define-func stpcon_ norm uplo diag n ap rcond work iwork)
(define-func stprfs_ uplo trans diag n nrhs ap b ldb x ldx ferr berr work iwork)
(define-func stptri_ uplo diag n ap)
(define-func stptrs_ uplo trans diag n nrhs ap b ldb)
(define-func strcon_ norm uplo diag n a lda rcond work iwork)
(define-func strevc_ side howmny select n t ldt vl ldvl vr ldvr mm m work)
(define-func strexc_ compq n t ldt q ldq ifst ilst work)
(define-func strrfs_ uplo trans diag n nrhs a lda b ldb x ldx ferr berr work iwork)
(define-func strsen_ job compq select n t ldt q ldq wr wi m s sep work lwork iwork liwork)
(define-func strsna_ job howmny select n t ldt vl ldvl vr ldvr s sep mm m work ldwork iwork)
(define-func strsyl_ trana tranb isgn m n a lda b ldb c__ ldc scale)
(define-func strti2_ uplo diag n a lda)
(define-func strtri_ uplo diag n a lda)
(define-func strtrs_ uplo trans diag n nrhs a lda b ldb)
(define-func stzrqf_ m n a lda tau)
(define-func stzrzf_ m n a lda tau work lwork)
(define-func xerbla_ srname)
(define-func zbdsqr_ uplo n ncvt nru ncc d__ e vt ldvt u ldu c__ ldc rwork)
(define-func zdrot_ n cx incx cy incy c__ s)
(define-func zdrscl_ n sa sx incx)
(define-func zgbbrd_ vect m n ncc kl ku ab ldab d__ e q ldq pt ldpt c__ ldc work rwork)
(define-func zgbcon_ norm n kl ku ab ldab ipiv anorm rcond work rwork)
(define-func zgbequ_ m n kl ku ab ldab r__ c__ rowcnd colcnd amax)
(define-func zgbrfs_ trans n kl ku nrhs ab ldab afb ldafb ipiv b ldb x ldx ferr berr work rwork)
(define-func zgbsv_ n kl ku nrhs ab ldab ipiv b ldb)
(define-func zgbsvx_ fact trans n kl ku nrhs ab ldab afb ldafb ipiv equed r__ c__ b ldb x ldx rcond ferr berr work rwork)
(define-func zgbtf2_ m n kl ku ab ldab ipiv)
(define-func zgbtrf_ m n kl ku ab ldab ipiv)
(define-func zgbtrs_ trans n kl ku nrhs ab ldab ipiv b ldb)
(define-func zgebak_ job side n ilo ihi scale m v ldv)
(define-func zgebal_ job n a lda ilo ihi scale)
(define-func zgebd2_ m n a lda d__ e tauq taup work)
(define-func zgebrd_ m n a lda d__ e tauq taup work lwork)
(define-func zgecon_ norm n a lda anorm rcond work rwork)
(define-func zgeequ_ m n a lda r__ c__ rowcnd colcnd amax)
(define-func zgees_ jobvs sort L_fp select n a lda sdim w vs ldvs work lwork rwork bwork)
(define-func zgeesx_ jobvs sort L_fp select sense n a lda sdim w vs ldvs rconde rcondv work lwork rwork bwork)
(define-func zgeev_ jobvl jobvr n a lda w vl ldvl vr ldvr work lwork rwork)
(define-func zgeevx_ balanc jobvl jobvr sense n a lda w vl ldvl vr ldvr ilo ihi scale abnrm rconde rcondv work lwork rwork)
(define-func zgegs_ jobvsl jobvsr n a lda b ldb alpha beta vsl ldvsl vsr ldvsr work lwork rwork)
(define-func zgegv_ jobvl jobvr n a lda b ldb alpha beta vl ldvl vr ldvr work lwork rwork)
(define-func zgehd2_ n ilo ihi a lda tau work)
(define-func zgehrd_ n ilo ihi a lda tau work lwork)
(define-func zgelq2_ m n a lda tau work)
(define-func zgelqf_ m n a lda tau work lwork)
(define-func zgels_ trans m n nrhs a lda b ldb work lwork)
(define-func zgelsx_ m n nrhs a lda b ldb jpvt rcond rank work rwork)
(define-func zgelsy_ m n nrhs a lda b ldb jpvt rcond rank work lwork rwork)
(define-func zgeql2_ m n a lda tau work)
(define-func zgeqlf_ m n a lda tau work lwork)
(define-func zgeqp3_ m n a lda jpvt tau work lwork rwork)
(define-func zgeqpf_ m n a lda jpvt tau work rwork)
(define-func zgeqr2_ m n a lda tau work)
(define-func zgeqrf_ m n a lda tau work lwork)
(define-func zgerfs_ trans n nrhs a lda af ldaf ipiv b ldb x ldx ferr berr work rwork)
(define-func zgerq2_ m n a lda tau work)
(define-func zgerqf_ m n a lda tau work lwork)
(define-func zgesc2_ n a lda rhs ipiv jpiv scale)
(define-func zgesv_ n nrhs a lda ipiv b ldb)
(define-func zgesvx_ fact trans n nrhs a lda af ldaf ipiv equed r__ c__ b ldb x ldx rcond ferr berr work rwork)
(define-func zgetc2_ n a lda ipiv jpiv)
(define-func zgetf2_ m n a lda ipiv)
(define-func zgetrf_ m n a lda ipiv)
(define-func zgetri_ n a lda ipiv work lwork)
(define-func zgetrs_ trans n nrhs a lda ipiv b ldb)
(define-func zggbak_ job side n ilo ihi lscale rscale m v ldv)
(define-func zggbal_ job n a lda b ldb ilo ihi lscale rscale work)
(define-func zgges_ jobvsl jobvsr sort L_fp delctg n a lda b ldb sdim alpha beta vsl ldvsl vsr ldvsr work lwork rwork bwork)
(define-func zggesx_ jobvsl jobvsr sort L_fp delctg sense n a lda b ldb sdim alpha beta vsl ldvsl vsr ldvsr rconde rcondv work lwork rwork iwork liwork bwork)
(define-func zggev_ jobvl jobvr n a lda b ldb alpha beta vl ldvl vr ldvr work lwork rwork)
(define-func zggevx_ balanc jobvl jobvr sense n a lda b ldb alpha beta vl ldvl vr ldvr ilo ihi lscale rscale abnrm bbnrm rconde rcondv work lwork rwork iwork bwork)
(define-func zggglm_ n m p a lda b ldb d__ x y work lwork)
(define-func zgghrd_ compq compz n ilo ihi a lda b ldb q ldq z__ ldz)
(define-func zgglse_ m n p a lda b ldb c__ d__ x work lwork)
(define-func zggqrf_ n m p a lda taua b ldb taub work lwork)
(define-func zggrqf_ m p n a lda taua b ldb taub work lwork)
(define-func zggsvd_ jobu jobv jobq m n p k l a lda b ldb alpha beta u ldu v ldv q ldq work rwork iwork)
(define-func zggsvp_ jobu jobv jobq m p n a lda b ldb tola tolb k l u ldu v ldv q ldq iwork rwork tau work)
(define-func zgtcon_ norm n dl d__ du du2 ipiv anorm rcond work)
(define-func zgtrfs_ trans n nrhs dl d__ du dlf df duf du2 ipiv b ldb x ldx ferr berr work rwork)
(define-func zgtsv_ n nrhs dl d__ du b ldb)
(define-func zgtsvx_ fact trans n nrhs dl d__ du dlf df duf du2 ipiv b ldb x ldx rcond ferr berr work rwork)
(define-func zgttrf_ n dl d__ du du2 ipiv)
(define-func zgttrs_ trans n nrhs dl d__ du du2 ipiv b ldb)
(define-func zgtts2_ itrans n nrhs dl d__ du du2 ipiv b ldb)
(define-func zhbev_ jobz uplo n kd ab ldab w z__ ldz work rwork)
(define-func zhbevd_ jobz uplo n kd ab ldab w z__ ldz work lwork rwork lrwork iwork liwork)
(define-func zhbevx_ jobz range uplo n kd ab ldab q ldq vl vu il iu abstol m w z__ ldz work rwork iwork ifail)
(define-func zhbgst_ vect uplo n ka kb ab ldab bb ldbb x ldx work rwork)
(define-func zhbgv_ jobz uplo n ka kb ab ldab bb ldbb w z__ ldz work rwork)
(define-func zhbgvx_ jobz range uplo n ka kb ab ldab bb ldbb q ldq vl vu il iu abstol m w z__ ldz work rwork iwork ifail)
(define-func zhbtrd_ vect uplo n kd ab ldab d__ e q ldq work)
(define-func zhecon_ uplo n a lda ipiv anorm rcond work)
(define-func zheev_ jobz uplo n a lda w work lwork rwork)
(define-func zheevd_ jobz uplo n a lda w work lwork rwork lrwork iwork liwork)
(define-func zheevr_ jobz range uplo n a lda vl vu il iu abstol m w z__ ldz isuppz work lwork rwork lrwork iwork liwork)
(define-func zheevx_ jobz range uplo n a lda vl vu il iu abstol m w z__ ldz work lwork rwork iwork ifail)
(define-func zhegs2_ itype uplo n a lda b ldb)
(define-func zhegst_ itype uplo n a lda b ldb)
(define-func zhegv_ itype jobz uplo n a lda b ldb w work lwork rwork)
(define-func zhegvd_ itype jobz uplo n a lda b ldb w work lwork rwork lrwork iwork liwork)
(define-func zhegvx_ itype jobz range uplo n a lda b ldb vl vu il iu abstol m w z__ ldz work lwork rwork iwork ifail)
(define-func zherfs_ uplo n nrhs a lda af ldaf ipiv b ldb x ldx ferr berr work rwork)
(define-func zhesv_ uplo n nrhs a lda ipiv b ldb work lwork)
(define-func zhesvx_ fact uplo n nrhs a lda af ldaf ipiv b ldb x ldx rcond ferr berr work lwork rwork)
(define-func zhetf2_ uplo n a lda ipiv)
(define-func zhetrd_ uplo n a lda d__ e tau work lwork)
(define-func zhetrf_ uplo n a lda ipiv work lwork)
(define-func zhetri_ uplo n a lda ipiv work)
(define-func zhetrs_ uplo n nrhs a lda ipiv b ldb)
(define-func zhgeqz_ job compq compz n ilo ihi a lda b ldb alpha beta q ldq z__ ldz work lwork rwork)
(define-func zhpcon_ uplo n ap ipiv anorm rcond work)
(define-func zhpev_ jobz uplo n ap w z__ ldz work rwork)
(define-func zhpevd_ jobz uplo n ap w z__ ldz work lwork rwork lrwork iwork liwork)
(define-func zhpevx_ jobz range uplo n ap vl vu il iu abstol m w z__ ldz work rwork iwork ifail)
(define-func zhpgst_ itype uplo n ap bp)
(define-func zhpgv_ itype jobz uplo n ap bp w z__ ldz work rwork)
(define-func zhpgvd_ itype jobz uplo n ap bp w z__ ldz work lwork rwork lrwork iwork liwork)
(define-func zhpgvx_ itype jobz range uplo n ap bp vl vu il iu abstol m w z__ ldz work rwork iwork ifail)
(define-func zhprfs_ uplo n nrhs ap afp ipiv b ldb x ldx ferr berr work rwork)
(define-func zhpsv_ uplo n nrhs ap ipiv b ldb)
(define-func zhpsvx_ fact uplo n nrhs ap afp ipiv b ldb x ldx rcond ferr berr work rwork)
(define-func zhptrd_ uplo n ap d__ e tau)
(define-func zhptrf_ uplo n ap ipiv)
(define-func zhptri_ uplo n ap ipiv work)
(define-func zhptrs_ uplo n nrhs ap ipiv b ldb)
(define-func zhsein_ side eigsrc initv select n h__ ldh w vl ldvl vr ldvr mm m work rwork ifaill ifailr)
(define-func zhseqr_ job compz n ilo ihi h__ ldh w z__ ldz work lwork)
(define-func zlabrd_ m n nb a lda d__ e tauq taup x ldx y ldy)
(define-func zlacgv_ n x incx)
(define-func zlacon_ n v x est kase)
(define-func zlacp2_ uplo m n a lda b ldb)
(define-func zlacpy_ uplo m n a lda b ldb)
(define-func zlacrm_ m n a lda b ldb c__ ldc rwork)
(define-func zlacrt_ n cx incx cy incy c__ s)
(define-func zlaed0_ qsiz n d__ e q ldq qstore ldqs rwork iwork)
(define-func zlaed7_ n cutpnt qsiz tlvls curlvl curpbm d__ q ldq rho indxq qstore qptr prmptr perm givptr givcol givnum work rwork iwork)
(define-func zlaed8_ k n qsiz q ldq d__ rho cutpnt z__ dlamda q2 ldq2 w indxp indx indxq perm givptr givcol givnum)
(define-func zlaein_ rightv noinit n h__ ldh w v b ldb rwork eps3 smlnum)
(define-func zlaesy_ a b c__ rt1 rt2 evscal cs1 sn1)
(define-func zlaev2_ a b c__ rt1 rt2 cs1 sn1)
(define-func zlags2_ upper a1 a2 a3 b1 b2 b3 csu snu csv snv csq snq)
(define-func zlagtm_ trans n nrhs alpha dl d__ du x ldx beta b ldb)
(define-func zlahef_ uplo n nb kb a lda ipiv w ldw)
(define-func zlahqr_ wantt wantz n ilo ihi h__ ldh w iloz ihiz z__ ldz)
(define-func zlahrd_ n k nb a lda tau t ldt y ldy)
(define-func zlaic1_ job j x sest w gamma sestpr s c__)
(define-func zlals0_ icompq nl nr sqre nrhs b ldb bx ldbx perm givptr givcol ldgcol givnum ldgnum poles difl difr z__ k c__ s rwork)
(define-func zlalsa_ icompq smlsiz n nrhs b ldb bx ldbx u ldu vt k difl difr z__ poles givptr givcol ldgcol perm givnum c__ s rwork iwork)
(define-func zlapll_ n x incx y incy ssmin)
(define-func zlapmt_ forwrd m n x ldx k)
(define-func zlaqgb_ m n kl ku ab ldab r__ c__ rowcnd colcnd amax equed)
(define-func zlaqge_ m n a lda r__ c__ rowcnd colcnd amax equed)
(define-func zlaqhb_ uplo n kd ab ldab s scond amax equed)
(define-func zlaqhe_ uplo n a lda s scond amax equed)
(define-func zlaqhp_ uplo n ap s scond amax equed)
(define-func zlaqp2_ m n offset a lda jpvt tau vn1 vn2 work)
(define-func zlaqps_ m n offset nb kb a lda jpvt tau vn1 vn2 auxv f ldf)
(define-func zlaqsb_ uplo n kd ab ldab s scond amax equed)
(define-func zlaqsp_ uplo n ap s scond amax equed)
(define-func zlaqsy_ uplo n a lda s scond amax equed)
(define-func zlar1v_ n b1 bn sigma d__ l ld lld gersch z__ ztz mingma r__ isuppz work)
(define-func zlar2v_ n x y z__ incx c__ s incc)
(define-func zlarcm_ m n a lda b ldb c__ ldc rwork)
(define-func zlarf_ side m n v incv tau c__ ldc work)
(define-func zlarfb_ side trans direct storev m n k v ldv t ldt c__ ldc work ldwork)
(define-func zlarfg_ n alpha x incx tau)
(define-func zlarft_ direct storev n k v ldv tau t ldt)
(define-func zlarfx_ side m n v tau c__ ldc work)
(define-func zlargv_ n x incx y incy c__ incc)
(define-func zlarnv_ idist iseed n x)
(define-func zlarrv_ n d__ l isplit m w iblock gersch tol z__ ldz isuppz work iwork)
(define-func zlartg_ f g cs sn r__)
(define-func zlartv_ n x incx y incy c__ s incc)
(define-func zlarz_ side m n l v incv tau c__ ldc work)
(define-func zlarzb_ side trans direct storev m n k l v ldv t ldt c__ ldc work ldwork)
(define-func zlarzt_ direct storev n k v ldv tau t ldt)
(define-func zlascl_ type__ kl ku cfrom cto m n a lda)
(define-func zlaset_ uplo m n alpha beta a lda)
(define-func zlasr_ side pivot direct m n c__ s a lda)
(define-func zlassq_ n x incx scale sumsq)
(define-func zlaswp_ n a lda k1 k2 ipiv incx)
(define-func zlasyf_ uplo n nb kb a lda ipiv w ldw)
(define-func zlatbs_ uplo trans diag normin n kd ab ldab x scale cnorm)
(define-func zlatdf_ ijob n z__ ldz rhs rdsum rdscal ipiv jpiv)
(define-func zlatps_ uplo trans diag normin n ap x scale cnorm)
(define-func zlatrd_ uplo n nb a lda e tau w ldw)
(define-func zlatrs_ uplo trans diag normin n a lda x scale cnorm)
(define-func zlatrz_ m n l a lda tau work)
(define-func zlatzm_ side m n v incv tau c1 c2 ldc work)
(define-func zlauu2_ uplo n a lda)
(define-func zlauum_ uplo n a lda)
(define-func zpbcon_ uplo n kd ab ldab anorm rcond work rwork)
(define-func zpbequ_ uplo n kd ab ldab s scond amax)
(define-func zpbrfs_ uplo n kd nrhs ab ldab afb ldafb b ldb x ldx ferr berr work rwork)
(define-func zpbstf_ uplo n kd ab ldab)
(define-func zpbsv_ uplo n kd nrhs ab ldab b ldb)
(define-func zpbsvx_ fact uplo n kd nrhs ab ldab afb ldafb equed s b ldb x ldx rcond ferr berr work rwork)
(define-func zpbtf2_ uplo n kd ab ldab)
(define-func zpbtrf_ uplo n kd ab ldab)
(define-func zpbtrs_ uplo n kd nrhs ab ldab b ldb)
(define-func zpocon_ uplo n a lda anorm rcond work rwork)
(define-func zpoequ_ n a lda s scond amax)
(define-func zporfs_ uplo n nrhs a lda af ldaf b ldb x ldx ferr berr work rwork)
(define-func zposv_ uplo n nrhs a lda b ldb)
(define-func zposvx_ fact uplo n nrhs a lda af ldaf equed s b ldb x ldx rcond ferr berr work rwork)
(define-func zpotf2_ uplo n a lda)
(define-func zpotrf_ uplo n a lda)
(define-func zpotri_ uplo n a lda)
(define-func zpotrs_ uplo n nrhs a lda b ldb)
(define-func zppcon_ uplo n ap anorm rcond work rwork)
(define-func zppequ_ uplo n ap s scond amax)
(define-func zpprfs_ uplo n nrhs ap afp b ldb x ldx ferr berr work rwork)
(define-func zppsv_ uplo n nrhs ap b ldb)
(define-func zppsvx_ fact uplo n nrhs ap afp equed s b ldb x ldx rcond ferr berr work rwork)
(define-func zpptrf_ uplo n ap)
(define-func zpptri_ uplo n ap)
(define-func zpptrs_ uplo n nrhs ap b ldb)
(define-func zptcon_ n d__ e anorm rcond rwork)
(define-func zptrfs_ uplo n nrhs d__ e df ef b ldb x ldx ferr berr work rwork)
(define-func zptsv_ n nrhs d__ e b ldb)
(define-func zptsvx_ fact n nrhs d__ e df ef b ldb x ldx rcond ferr berr work rwork)
(define-func zpttrf_ n d__ e)
(define-func zpttrs_ uplo n nrhs d__ e b ldb)
(define-func zptts2_ iuplo n nrhs d__ e b ldb)
(define-func zrot_ n cx incx cy incy c__ s)
(define-func zspcon_ uplo n ap ipiv anorm rcond work)
(define-func zspmv_ uplo n alpha ap x incx beta y incy)
(define-func zspr_ uplo n alpha x incx ap)
(define-func zsprfs_ uplo n nrhs ap afp ipiv b ldb x ldx ferr berr work rwork)
(define-func zspsv_ uplo n nrhs ap ipiv b ldb)
(define-func zspsvx_ fact uplo n nrhs ap afp ipiv b ldb x ldx rcond ferr berr work rwork)
(define-func zsptrf_ uplo n ap ipiv)
(define-func zsptri_ uplo n ap ipiv work)
(define-func zsptrs_ uplo n nrhs ap ipiv b ldb)
(define-func zstedc_ compz n d__ e z__ ldz work lwork rwork lrwork iwork liwork)
(define-func zstein_ n d__ e m w iblock isplit z__ ldz work iwork ifail)
(define-func zsteqr_ compz n d__ e z__ ldz work)
(define-func zsycon_ uplo n a lda ipiv anorm rcond work)
(define-func zsymv_ uplo n alpha a lda x incx beta y incy)
(define-func zsyr_ uplo n alpha x incx a lda)
(define-func zsyrfs_ uplo n nrhs a lda af ldaf ipiv b ldb x ldx ferr berr work rwork)
(define-func zsysv_ uplo n nrhs a lda ipiv b ldb work lwork)
(define-func zsysvx_ fact uplo n nrhs a lda af ldaf ipiv b ldb x ldx rcond ferr berr work lwork rwork)
(define-func zsytf2_ uplo n a lda ipiv)
(define-func zsytrf_ uplo n a lda ipiv work lwork)
(define-func zsytri_ uplo n a lda ipiv work)
(define-func zsytrs_ uplo n nrhs a lda ipiv b ldb)
(define-func ztbcon_ norm uplo diag n kd ab ldab rcond work rwork)
(define-func ztbrfs_ uplo trans diag n kd nrhs ab ldab b ldb x ldx ferr berr work rwork)
(define-func ztbtrs_ uplo trans diag n kd nrhs ab ldab b ldb)
(define-func ztgevc_ side howmny select n a lda b ldb vl ldvl vr ldvr mm m work rwork)
(define-func ztgex2_ wantq wantz n a lda b ldb q ldq z__ ldz j1)
(define-func ztgexc_ wantq wantz n a lda b ldb q ldq z__ ldz ifst ilst)
(define-func ztgsen_ ijob wantq wantz select n a lda b ldb alpha beta q ldq z__ ldz m pl pr dif work lwork iwork liwork)
(define-func ztgsja_ jobu jobv jobq m p n k l a lda b ldb tola tolb alpha beta u ldu v ldv q ldq work ncycle)
(define-func ztgsna_ job howmny select n a lda b ldb vl ldvl vr ldvr s dif mm m work lwork iwork)
(define-func ztgsy2_ trans ijob m n a lda b ldb c__ ldc d__ ldd e lde f ldf scale rdsum rdscal)
(define-func ztgsyl_ trans ijob m n a lda b ldb c__ ldc d__ ldd e lde f ldf scale dif work lwork iwork)
(define-func ztpcon_ norm uplo diag n ap rcond work rwork)
(define-func ztprfs_ uplo trans diag n nrhs ap b ldb x ldx ferr berr work rwork)
(define-func ztptri_ uplo diag n ap)
(define-func ztptrs_ uplo trans diag n nrhs ap b ldb)
(define-func ztrcon_ norm uplo diag n a lda rcond work rwork)
(define-func ztrevc_ side howmny select n t ldt vl ldvl vr ldvr mm m work rwork)
(define-func ztrexc_ compq n t ldt q ldq ifst ilst)
(define-func ztrrfs_ uplo trans diag n nrhs a lda b ldb x ldx ferr berr work rwork)
(define-func ztrsen_ job compq select n t ldt q ldq w m s sep work lwork)
(define-func ztrsna_ job howmny select n t ldt vl ldvl vr ldvr s sep mm m work ldwork rwork)
(define-func ztrsyl_ trana tranb isgn m n a lda b ldb c__ ldc scale)
(define-func ztrti2_ uplo diag n a lda)
(define-func ztrtri_ uplo diag n a lda)
(define-func ztrtrs_ uplo trans diag n nrhs a lda b ldb)
(define-func ztzrqf_ m n a lda tau)
(define-func ztzrzf_ m n a lda tau work lwork)
(define-func zung2l_ m n k a lda tau work)
(define-func zung2r_ m n k a lda tau work)
(define-func zungbr_ vect m n k a lda tau work lwork)
(define-func zunghr_ n ilo ihi a lda tau work lwork)
(define-func zungl2_ m n k a lda tau work)
(define-func zunglq_ m n k a lda tau work lwork)
(define-func zungql_ m n k a lda tau work lwork)
(define-func zungqr_ m n k a lda tau work lwork)
(define-func zungr2_ m n k a lda tau work)
(define-func zungrq_ m n k a lda tau work lwork)
(define-func zungtr_ uplo n a lda tau work lwork)
(define-func zunm2l_ side trans m n k a lda tau c__ ldc work)
(define-func zunm2r_ side trans m n k a lda tau c__ ldc work)
(define-func zunmbr_ vect side trans m n k a lda tau c__ ldc work lwork)
(define-func zunmhr_ side trans m n ilo ihi a lda tau c__ ldc work lwork)
(define-func zunml2_ side trans m n k a lda tau c__ ldc work)
(define-func zunmlq_ side trans m n k a lda tau c__ ldc work lwork)
(define-func zunmql_ side trans m n k a lda tau c__ ldc work lwork)
(define-func zunmqr_ side trans m n k a lda tau c__ ldc work lwork)
(define-func zunmr2_ side trans m n k a lda tau c__ ldc work)
(define-func zunmr3_ side trans m n k l a lda tau c__ ldc work)
(define-func zunmrq_ side trans m n k a lda tau c__ ldc work lwork)
(define-func zunmrz_ side trans m n k l a lda tau c__ ldc work lwork)
(define-func zunmtr_ side uplo trans m n a lda tau c__ ldc work lwork)
(define-func zupgtr_ uplo n ap tau q ldq work)
(define-func zupmtr_ side uplo trans m n ap tau c__ ldc work)


;;;; done

)

;;; end of file
