;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/LAPACK
;;;Contents: bindings to foreign functions
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


(library (foreign math lapack platform)
  (export
    cbdsqr_
    cgbbrd_
    cgbcon_
    cgbequ_
    cgbrfs_
    cgbsv_
    cgbsvx_
    cgbtf2_
    cgbtrf_
    cgbtrs_
    cgebak_
    cgebal_
    cgebd2_
    cgebrd_
    cgecon_
    cgeequ_
    cgees_
    cgeesx_
    cgeev_
    cgeevx_
    cgegs_
    cgegv_
    cgehd2_
    cgehrd_
    cgelq2_
    cgelqf_
    cgels_
    cgelsx_
    cgelsy_
    cgeql2_
    cgeqlf_
    cgeqp3_
    cgeqpf_
    cgeqr2_
    cgeqrf_
    cgerfs_
    cgerq2_
    cgerqf_
    cgesc2_
    cgesv_
    cgesvx_
    cgetc2_
    cgetf2_
    cgetrf_
    cgetri_
    cgetrs_
    cggbak_
    cggbal_
    cgges_
    cggesx_
    cggev_
    cggevx_
    cggglm_
    cgghrd_
    cgglse_
    cggqrf_
    cggrqf_
    cggsvd_
    cggsvp_
    cgtcon_
    cgtrfs_
    cgtsv_
    cgtsvx_
    cgttrf_
    cgttrs_
    cgtts2_
    chbev_
    chbevd_
    chbevx_
    chbgst_
    chbgv_
    chbgvx_
    chbtrd_
    checon_
    cheev_
    cheevd_
    cheevr_
    cheevx_
    chegs2_
    chegst_
    chegv_
    chegvd_
    chegvx_
    cherfs_
    chesv_
    chesvx_
    chetf2_
    chetrd_
    chetrf_
    chetri_
    chetrs_
    chgeqz_
    chpcon_
    chpev_
    chpevd_
    chpevx_
    chpgst_
    chpgv_
    chpgvd_
    chpgvx_
    chprfs_
    chpsv_
    chpsvx_
    chptrd_
    chptrf_
    chptri_
    chptrs_
    chsein_
    chseqr_
    clabrd_
    clacgv_
    clacon_
    clacp2_
    clacpy_
    clacrm_
    clacrt_
    claed0_
    claed7_
    claed8_
    claein_
    claesy_
    claev2_
    clags2_
    clagtm_
    clahef_
    clahqr_
    clahrd_
    claic1_
    clals0_
    clalsa_
    clapll_
    clapmt_
    claqgb_
    claqge_
    claqhb_
    claqhe_
    claqhp_
    claqp2_
    claqps_
    claqsb_
    claqsp_
    claqsy_
    clar1v_
    clar2v_
    clarcm_
    clarf_
    clarfb_
    clarfg_
    clarft_
    clarfx_
    clargv_
    clarnv_
    clarrv_
    clartg_
    clartv_
    clarz_
    clarzb_
    clarzt_
    clascl_
    claset_
    clasr_
    classq_
    claswp_
    clasyf_
    clatbs_
    clatdf_
    clatps_
    clatrd_
    clatrs_
    clatrz_
    clatzm_
    clauu2_
    clauum_
    cpbcon_
    cpbequ_
    cpbrfs_
    cpbstf_
    cpbsv_
    cpbsvx_
    cpbtf2_
    cpbtrf_
    cpbtrs_
    cpocon_
    cpoequ_
    cporfs_
    cposv_
    cposvx_
    cpotf2_
    cpotrf_
    cpotri_
    cpotrs_
    cppcon_
    cppequ_
    cpprfs_
    cppsv_
    cppsvx_
    cpptrf_
    cpptri_
    cpptrs_
    cptcon_
    cptrfs_
    cptsv_
    cptsvx_
    cpttrf_
    cpttrs_
    cptts2_
    crot_
    cspcon_
    cspmv_
    cspr_
    csprfs_
    cspsv_
    cspsvx_
    csptrf_
    csptri_
    csptrs_
    csrot_
    csrscl_
    cstedc_
    cstein_
    csteqr_
    csycon_
    csymv_
    csyr_
    csyrfs_
    csysv_
    csysvx_
    csytf2_
    csytrf_
    csytri_
    csytrs_
    ctbcon_
    ctbrfs_
    ctbtrs_
    ctgevc_
    ctgex2_
    ctgexc_
    ctgsen_
    ctgsja_
    ctgsna_
    ctgsy2_
    ctgsyl_
    ctpcon_
    ctprfs_
    ctptri_
    ctptrs_
    ctrcon_
    ctrevc_
    ctrexc_
    ctrrfs_
    ctrsen_
    ctrsna_
    ctrsyl_
    ctrti2_
    ctrtri_
    ctrtrs_
    ctzrqf_
    ctzrzf_
    cung2l_
    cung2r_
    cungbr_
    cunghr_
    cungl2_
    cunglq_
    cungql_
    cungqr_
    cungr2_
    cungrq_
    cungtr_
    cunm2l_
    cunm2r_
    cunmbr_
    cunmhr_
    cunml2_
    cunmlq_
    cunmql_
    cunmqr_
    cunmr2_
    cunmr3_
    cunmrq_
    cunmrz_
    cunmtr_
    cupgtr_
    cupmtr_
    dbdsdc_
    dbdsqr_
    ddisna_
    dgbbrd_
    dgbcon_
    dgbequ_
    dgbrfs_
    dgbsv_
    dgbsvx_
    dgbtf2_
    dgbtrf_
    dgbtrs_
    dgebak_
    dgebal_
    dgebd2_
    dgebrd_
    dgecon_
    dgeequ_
    dgees_
    dgeesx_
    dgeev_
    dgeevx_
    dgegs_
    dgegv_
    dgehd2_
    dgehrd_
    dgelq2_
    dgelqf_
    dgels_
    dgelsd_
    dgelss_
    dgelsx_
    dgelsy_
    dgeql2_
    dgeqlf_
    dgeqp3_
    dgeqpf_
    dgeqr2_
    dgeqrf_
    dgerfs_
    dgerq2_
    dgerqf_
    dgesc2_
    dgesdd_
    dgesv_
    dgesvd_
    dgesvx_
    dgetc2_
    dgetf2_
    dgetrf_
    dgetri_
    dgetrs_
    dggbak_
    dggbal_
    dgges_
    dggesx_
    dggev_
    dggevx_

    dggglm_
    dgghrd_
    dgglse_
    dggqrf_
    dggrqf_
    dggsvd_
    dggsvp_
    dgtcon_
    dgtrfs_
    dgtsv_
    dgtsvx_
    dgttrf_
    dgttrs_
    dgtts2_
    dhgeqz_
    dhsein_
    dhseqr_
    dlabad_
    dlabrd_
    dlacon_
    dlacpy_
    dladiv_
    dlae2_
    dlaebz_
    dlaed0_
    dlaed1_
    dlaed2_
    dlaed3_
    dlaed4_
    dlaed5_
    dlaed6_
    dlaed7_
    dlaed8_
    dlaed9_
    dlaeda_
    dlaein_
    dlaev2_
    dlaexc_
    dlag2_
    dlags2_
    dlagtf_
    dlagtm_
    dlagts_
    dlagv2_
    dlahqr_
    dlahrd_
    dlaic1_
    dlaln2_
    dlals0_
    dlalsa_
    dlalsd_
    dlamc1_
    dlamc2_
    dlamc4_
    dlamc5_
    dlamrg_
    dlanv2_
    dlapll_
    dlapmt_
    dlaqgb_
    dlaqge_
    dlaqp2_
    dlaqps_
    dlaqsb_
    dlaqsp_
    dlaqsy_
    dlaqtr_
    dlar1v_
    dlar2v_
    dlarf_
    dlarfb_
    dlarfg_
    dlarft_
    dlarfx_
    dlargv_
    dlarnv_
    dlarrb_
    dlarre_
    dlarrf_
    dlarrv_
    dlartg_
    dlartv_
    dlaruv_
    dlarz_
    dlarzb_
    dlarzt_
    dlas2_
    dlascl_
    dlasd0_
    dlasd1_
    dlasd2_
    dlasd3_
    dlasd4_
    dlasd5_
    dlasd6_
    dlasd7_
    dlasd8_
    dlasd9_
    dlasda_
    dlasdq_
    dlasdt_
    dlaset_
    dlasq1_
    dlasq2_
    dlasq3_
    dlasq4_
    dlasq5_
    dlasq6_
    dlasr_
    dlasrt_
    dlassq_
    dlasv2_
    dlaswp_
    dlasy2_
    dlasyf_
    dlatbs_
    dlatdf_
    dlatps_
    dlatrd_
    dlatrs_
    dlatrz_
    dlatzm_
    dlauu2_
    dlauum_
    dopgtr_
    dopmtr_
    dorg2l_
    dorg2r_
    dorgbr_
    dorghr_
    dorgl2_
    dorglq_
    dorgql_
    dorgqr_
    dorgr2_
    dorgrq_
    dorgtr_
    dorm2l_
    dorm2r_
    dormbr_
    dormhr_
    dorml2_
    dormlq_
    dormql_
    dormqr_
    dormr2_
    dormr3_
    dormrq_
    dormrz_
    dormtr_
    dpbcon_
    dpbequ_
    dpbrfs_
    dpbstf_
    dpbsv_
    dpbsvx_
    dpbtf2_
    dpbtrf_
    dpbtrs_
    dpocon_
    dpoequ_
    dporfs_
    dposv_
    dposvx_
    dpotf2_
    dpotrf_
    dpotri_
    dpotrs_
    dppcon_
    dppequ_
    dpprfs_
    dppsv_
    dppsvx_
    dpptrf_
    dpptri_
    dpptrs_
    dptcon_
    dpteqr_
    dptrfs_
    dptsv_
    dptsvx_
    dpttrf_
    dpttrs_
    dptts2_
    drscl_
    dsbev_
    dsbevd_
    dsbevx_
    dsbgst_
    dsbgv_
    dsbgvd_
    dsbgvx_
    dsbtrd_
    dspcon_
    dspev_
    dspevd_
    dspevx_
    dspgst_
    dspgv_
    dspgvd_
    dspgvx_
    dsprfs_
    dspsv_
    dspsvx_
    dsptrd_
    dsptrf_
    dsptri_
    dsptrs_
    dstebz_
    dstedc_
    dstegr_
    dstein_
    dsteqr_
    dsterf_
    dstev_
    dstevd_
    dstevr_
    dstevx_
    dsycon_
    dsyev_
    dsyevd_
    dsyevr_
    dsyevx_
    dsygs2_
    dsygst_
    dsygv_
    dsygvd_
    dsygvx_
    dsyrfs_
    dsysv_
    dsysvx_
    dsytd2_
    dsytf2_
    dsytrd_
    dsytrf_
    dsytri_
    dsytrs_
    dtbcon_
    dtbrfs_
    dtbtrs_
    dtgevc_
    dtgex2_
    dtgexc_
    dtgsen_
    dtgsja_
    dtgsna_
    dtgsy2_
    dtgsyl_
    dtpcon_
    dtprfs_
    dtptri_
    dtptrs_
    dtrcon_
    dtrevc_
    dtrexc_
    dtrrfs_
    dtrsen_
    dtrsna_
    dtrsyl_
    dtrti2_
    dtrtri_
    dtrtrs_
    dtzrqf_
    dtzrzf_
    icmax1_
    ieeeck_
    ilaenv_
    izmax1_
    sbdsdc_
    sbdsqr_
    sdisna_
    sgbbrd_
    sgbcon_
    sgbequ_
    sgbrfs_
    sgbsv_
    sgbsvx_
    sgbtf2_
    sgbtrf_
    sgbtrs_
    sgebak_
    sgebal_
    sgebd2_
    sgebrd_
    sgecon_
    sgeequ_
    sgees_
    sgeesx_
    sgeev_
    sgeevx_
    sgegs_
    sgegv_
    sgehd2_
    sgehrd_
    sgelq2_
    sgelqf_
    sgels_
    sgelsd_
    sgelss_
    sgelsx_
    sgelsy_
    sgeql2_
    sgeqlf_
    sgeqp3_
    sgeqpf_
    sgeqr2_
    sgeqrf_
    sgerfs_
    sgerq2_
    sgerqf_
    sgesc2_
    sgesdd_
    sgesv_
    sgesvd_
    sgesvx_
    sgetc2_
    sgetf2_
    sgetrf_
    sgetri_
    sgetrs_
    sggbak_
    sggbal_
    sgges_
    sggesx_
    sggev_
    sggevx_
    sggglm_
    sgghrd_
    sgglse_
    sggqrf_
    sggrqf_
    sggsvd_
    sggsvp_
    sgtcon_
    sgtrfs_
    sgtsv_
    sgtsvx_
    sgttrf_
    sgttrs_
    sgtts2_
    shgeqz_
    shsein_
    shseqr_
    slabad_
    slabrd_
    slacon_
    slacpy_
    sladiv_
    slae2_
    slaebz_
    slaed0_
    slaed1_
    slaed2_
    slaed3_
    slaed4_
    slaed5_
    slaed6_
    slaed7_
    slaed8_
    slaed9_
    slaeda_
    slaein_
    slaev2_
    slaexc_
    slag2_
    slags2_
    slagtf_
    slagtm_
    slagts_
    slagv2_
    slahqr_
    slahrd_
    slaic1_
    slaln2_
    slals0_
    slalsa_
    slalsd_
    slamc1_
    slamc2_
    slamc4_
    slamc5_
    slamrg_
    slanv2_
    slapll_
    slapmt_
    slaqgb_
    slaqge_
    slaqp2_
    slaqps_
    slaqsb_
    slaqsp_
    slaqsy_
    slaqtr_
    slar1v_
    slar2v_
    slarf_
    slarfb_
    slarfg_
    slarft_
    slarfx_
    slargv_
    slarnv_
    slarrb_
    slarre_
    slarrf_
    slarrv_
    slartg_
    slartv_
    slaruv_
    slarz_
    slarzb_
    slarzt_
    slas2_
    slascl_
    slasd0_
    slasd1_
    slasd2_
    slasd3_
    slasd4_
    slasd5_
    slasd6_
    slasd7_
    slasd8_
    slasd9_
    slasda_
    slasdq_
    slasdt_
    slaset_
    slasq1_
    slasq2_
    slasq3_
    slasq4_
    slasq5_
    slasq6_
    slasr_
    slasrt_
    slassq_
    slasv2_
    slaswp_
    slasy2_
    slasyf_
    slatbs_
    slatdf_
    slatps_
    slatrd_
    slatrs_
    slatrz_
    slatzm_
    slauu2_
    slauum_
    sopgtr_
    sopmtr_
    sorg2l_
    sorg2r_
    sorgbr_
    sorghr_
    sorgl2_
    sorglq_
    sorgql_
    sorgqr_
    sorgr2_
    sorgrq_
    sorgtr_
    sorm2l_
    sorm2r_
    sormbr_
    sormhr_
    sorml2_
    sormlq_
    sormql_
    sormqr_
    sormr2_
    sormr3_
    sormrq_
    sormrz_
    sormtr_
    spbcon_
    spbequ_
    spbrfs_
    spbstf_
    spbsv_
    spbsvx_
    spbtf2_
    spbtrf_
    spbtrs_
    spocon_
    spoequ_
    sporfs_
    sposv_
    sposvx_
    spotf2_
    spotrf_
    spotri_
    spotrs_
    sppcon_
    sppequ_
    spprfs_
    sppsv_
    sppsvx_
    spptrf_
    spptri_
    spptrs_
    sptcon_
    spteqr_
    sptrfs_
    sptsv_
    sptsvx_
    spttrf_
    spttrs_
    sptts2_
    srscl_
    ssbev_
    ssbevd_
    ssbevx_
    ssbgst_
    ssbgv_
    ssbgvd_
    ssbgvx_
    ssbtrd_
    sspcon_
    sspev_
    sspevd_
    sspevx_
    sspgst_
    sspgv_
    sspgvd_
    sspgvx_
    ssprfs_
    sspsv_
    sspsvx_
    ssptrd_
    ssptrf_
    ssptri_
    ssptrs_
    sstebz_
    sstedc_
    sstegr_
    sstein_
    ssteqr_
    ssterf_
    sstev_
    sstevd_
    sstevr_
    sstevx_
    ssycon_
    ssyev_
    ssyevd_
    ssyevr_
    ssyevx_
    ssygs2_
    ssygst_
    ssygv_
    ssygvd_
    ssygvx_
    ssyrfs_
    ssysv_
    ssysvx_
    ssytd2_
    ssytf2_
    ssytrd_
    ssytrf_
    ssytri_
    ssytrs_
    stbcon_
    stbrfs_
    stbtrs_
    stgevc_
    stgex2_
    stgexc_
    stgsen_
    stgsja_
    stgsna_
    stgsy2_
    stgsyl_
    stpcon_
    stprfs_
    stptri_
    stptrs_
    strcon_
    strevc_
    strexc_
    strrfs_
    strsen_
    strsna_
    strsyl_
    strti2_
    strtri_
    strtrs_
    stzrqf_
    stzrzf_
    xerbla_
    zbdsqr_
    zdrot_
    zdrscl_
    zgbbrd_
    zgbcon_
    zgbequ_
    zgbrfs_
    zgbsv_
    zgbsvx_
    zgbtf2_
    zgbtrf_
    zgbtrs_
    zgebak_
    zgebal_
    zgebd2_
    zgebrd_
    zgecon_
    zgeequ_
    zgees_
    zgeesx_
    zgeev_
    zgeevx_
    zgegs_
    zgegv_
    zgehd2_
    zgehrd_
    zgelq2_
    zgelqf_
    zgels_
    zgelsx_
    zgelsy_
    zgeql2_
    zgeqlf_
    zgeqp3_
    zgeqpf_
    zgeqr2_
    zgeqrf_
    zgerfs_
    zgerq2_
    zgerqf_
    zgesc2_
    zgesv_
    zgesvx_
    zgetc2_
    zgetf2_
    zgetrf_
    zgetri_
    zgetrs_
    zggbak_
    zggbal_
    zgges_
    zggesx_
    zggev_
    zggevx_

    zggglm_
    zgghrd_
    zgglse_
    zggqrf_
    zggrqf_
    zggsvd_
    zggsvp_
    zgtcon_
    zgtrfs_
    zgtsv_
    zgtsvx_
    zgttrf_
    zgttrs_
    zgtts2_
    zhbev_
    zhbevd_
    zhbevx_
    zhbgst_
    zhbgv_
    zhbgvx_
    zhbtrd_
    zhecon_
    zheev_
    zheevd_
    zheevr_
    zheevx_
    zhegs2_
    zhegst_
    zhegv_
    zhegvd_
    zhegvx_
    zherfs_
    zhesv_
    zhesvx_
    zhetf2_
    zhetrd_
    zhetrf_
    zhetri_
    zhetrs_
    zhgeqz_
    zhpcon_
    zhpev_
    zhpevd_
    zhpevx_
    zhpgst_
    zhpgv_
    zhpgvd_
    zhpgvx_
    zhprfs_
    zhpsv_
    zhpsvx_
    zhptrd_
    zhptrf_
    zhptri_
    zhptrs_
    zhsein_
    zhseqr_
    zlabrd_
    zlacgv_
    zlacon_
    zlacp2_
    zlacpy_
    zlacrm_
    zlacrt_
    zlaed0_
    zlaed7_
    zlaed8_
    zlaein_
    zlaesy_
    zlaev2_
    zlags2_
    zlagtm_
    zlahef_
    zlahqr_
    zlahrd_
    zlaic1_
    zlals0_
    zlalsa_
    zlapll_
    zlapmt_
    zlaqgb_
    zlaqge_
    zlaqhb_
    zlaqhe_
    zlaqhp_
    zlaqp2_
    zlaqps_
    zlaqsb_
    zlaqsp_
    zlaqsy_
    zlar1v_
    zlar2v_
    zlarcm_
    zlarf_
    zlarfb_
    zlarfg_
    zlarft_
    zlarfx_
    zlargv_
    zlarnv_
    zlarrv_
    zlartg_
    zlartv_
    zlarz_
    zlarzb_
    zlarzt_
    zlascl_
    zlaset_
    zlasr_
    zlassq_
    zlaswp_
    zlasyf_
    zlatbs_
    zlatdf_
    zlatps_
    zlatrd_
    zlatrs_
    zlatrz_
    zlatzm_
    zlauu2_
    zlauum_
    zpbcon_
    zpbequ_
    zpbrfs_
    zpbstf_
    zpbsv_
    zpbsvx_
    zpbtf2_
    zpbtrf_
    zpbtrs_
    zpocon_
    zpoequ_
    zporfs_
    zposv_
    zposvx_
    zpotf2_
    zpotrf_
    zpotri_
    zpotrs_
    zppcon_
    zppequ_
    zpprfs_
    zppsv_
    zppsvx_
    zpptrf_
    zpptri_
    zpptrs_
    zptcon_
    zptrfs_
    zptsv_
    zptsvx_
    zpttrf_
    zpttrs_
    zptts2_
    zrot_
    zspcon_
    zspmv_
    zspr_
    zsprfs_
    zspsv_
    zspsvx_
    zsptrf_
    zsptri_
    zsptrs_
    zstedc_
    zstein_
    zsteqr_
    zsycon_
    zsymv_
    zsyr_
    zsyrfs_
    zsysv_
    zsysvx_
    zsytf2_
    zsytrf_
    zsytri_
    zsytrs_
    ztbcon_
    ztbrfs_
    ztbtrs_
    ztgevc_
    ztgex2_
    ztgexc_
    ztgsen_
    ztgsja_
    ztgsna_
    ztgsy2_
    ztgsyl_
    ztpcon_
    ztprfs_
    ztptri_
    ztptrs_
    ztrcon_
    ztrevc_
    ztrexc_
    ztrrfs_
    ztrsen_
    ztrsna_
    ztrsyl_
    ztrti2_
    ztrtri_
    ztrtrs_
    ztzrqf_
    ztzrzf_
    zung2l_
    zung2r_
    zungbr_
    zunghr_
    zungl2_
    zunglq_
    zungql_
    zungqr_
    zungr2_
    zungrq_
    zungtr_
    zunm2l_
    zunm2r_
    zunmbr_
    zunmhr_
    zunml2_
    zunmlq_
    zunmql_
    zunmqr_
    zunmr2_
    zunmr3_
    zunmrq_
    zunmrz_
    zunmtr_
    zupgtr_
    zupmtr_)
  (import (rnrs)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign math lapack shared-object)
    (foreign math lapack sizeof))


(define-c-functions clapack-shared-object
  (cbdsqr_	(int cbdsqr_ (char* integer* integer* integer*
				    integer* real* real* complex* integer*
				    complex* integer* complex* integer* real*
				    integer*)))

  (cgbbrd_	(int cgbbrd_ (char* integer* integer* integer*
				    integer* integer* complex* integer* real*
				    real* complex* integer* complex* integer*
				    complex* integer* complex* real* integer*)))

  (cgbcon_	(int cgbcon_ (char* integer* integer* integer*
				    complex* integer* integer* real* real*
				    complex* real* integer*)))

  (cgbequ_	(int cgbequ_ (integer* integer* integer* integer*
				       complex* integer* real* real* real* real* real* integer*)))

  (cgbrfs_	(int cgbrfs_ (char* integer* integer* integer*
				    integer* complex* integer* complex* integer*
				    integer* complex* integer* complex* integer*
				    real* real* complex* real* integer*)))

  (cgbsv_	(int cgbsv_ (integer* integer* integer* integer*
				      complex* integer* integer* complex* integer*
				      integer*)))

  (cgbsvx_	(int cgbsvx_ (char* char* integer* integer*
				    integer* integer* complex* integer* complex*
				    integer* integer* char* real* real*
				    complex* integer* complex* integer* real* real*
				    real* complex* real* integer*)))

  (cgbtf2_	(int cgbtf2_ (integer* integer* integer* integer*
				       complex* integer* integer* integer*)))

  (cgbtrf_	(int cgbtrf_ (integer* integer* integer* integer*
				       complex* integer* integer* integer*)))

  (cgbtrs_	(int cgbtrs_ (char* integer* integer* integer*
				    integer* complex* integer* integer* complex* integer* integer*)))

  (cgebak_	(int cgebak_ (char* char* integer* integer*
				    integer* real* integer* complex* integer*
				    integer*)))

  (cgebal_	(int cgebal_ (char* integer* complex* integer*
				    integer* integer* real* integer*)))

  (cgebd2_	(int cgebd2_ (integer* integer* complex* integer*
				       real* real* complex* complex* complex*
				       integer*)))

  (cgebrd_	(int cgebrd_ (integer* integer* complex* integer*
				       real* real* complex* complex* complex*
				       integer* integer*)))

  (cgecon_	(int cgecon_ (char* integer* complex* integer*
				    real* real* complex* real* integer*)))

  (cgeequ_	(int cgeequ_ (integer* integer* complex* integer*
				       real* real* real* real* real*
				       integer*)))

  (cgees_	(int cgees_ (char* char* L_fp integer*
				   complex* integer* integer* complex* complex*
				   integer* complex* integer* real* logical* integer*)))

  (cgeesx_	(int cgeesx_ (char* char* L_fp char*
				    integer* complex* integer* integer* complex*
				    complex* integer* real* real* complex*
				    integer* real* logical* integer*)))

  (cgeev_	(int cgeev_ (char* char* integer* complex*
				   integer* complex* complex* integer* complex*
				   integer* complex* integer* real* integer*
				   )))

  (cgeevx_	(int cgeevx_ (char* char* char* char*
				    integer* complex* integer* complex* complex*
				    integer* complex* integer* integer* integer*
				    real* real* real* real* complex*
				    integer* real* integer*)))

  (cgegs_	(int cgegs_ (char* char* integer* complex*
				   integer* complex* integer* complex* complex*
				   complex* integer* complex* integer*
				   complex* integer* real* integer*)))

  (cgegv_	(int cgegv_ (char* char* integer* complex*
				   integer* complex* integer* complex* complex*
				   complex* integer* complex* integer* complex*
				   integer* real* integer*)))

  (cgehd2_	(int cgehd2_ (integer* integer* integer* complex*
				       integer* complex* complex* integer*)))

  (cgehrd_	(int cgehrd_ (integer* integer* integer* complex*
				       integer* complex* complex* integer* integer*)))

  (cgelq2_	(int cgelq2_ (integer* integer* complex* integer*
				       complex* complex* integer*)))

  (cgelqf_	(int cgelqf_ (integer* integer* complex* integer*
				       complex* complex* integer* integer*)))

  (cgels_	(int cgels_ (char* integer* integer* integer*
				   complex* integer* complex* integer* complex*
				   integer* integer*)))

  (cgelsx_	(int cgelsx_ (integer* integer* integer* complex*
				       integer* complex* integer* integer* real*
				       integer* complex* real* integer*)))

  (cgelsy_	(int cgelsy_ (integer* integer* integer* complex*
				       integer* complex* integer* integer* real*
				       integer* complex* integer* real* integer*)))

  (cgeql2_	(int cgeql2_ (integer* integer* complex* integer*
				       complex* complex* integer*)))

  (cgeqlf_	(int cgeqlf_ (integer* integer* complex* integer*
				       complex* complex* integer* integer*)))

  (cgeqp3_	(int cgeqp3_ (integer* integer* complex* integer*
				       integer* complex* complex* integer* real*
				       integer*)))

  (cgeqpf_	(int cgeqpf_ (integer* integer* complex* integer*
				       integer* complex* complex* real* integer*)))

  (cgeqr2_	(int cgeqr2_ (integer* integer* complex* integer*
				       complex* complex* integer*)))

  (cgeqrf_	(int cgeqrf_ (integer* integer* complex* integer*
				       complex* complex* integer* integer*)))

  (cgerfs_	(int cgerfs_ (char* integer* integer* complex*
				    integer* complex* integer* integer* complex*
				    integer* complex* integer* real* real*
				    complex* real* integer*)))

  (cgerq2_	(int cgerq2_ (integer* integer* complex* integer*
				       complex* complex* integer*)))

  (cgerqf_	(int cgerqf_ (integer* integer* complex* integer*
				       complex* complex* integer* integer*)))

  (cgesc2_	(int cgesc2_ (integer* complex* integer* complex*
				       integer* integer* real*)))

  (cgesv_	(int cgesv_ (integer* integer* complex* integer*
				      integer* complex* integer* integer*)))

  (cgesvx_	(int cgesvx_ (char* char* integer* integer*
				    complex* integer* complex* integer* integer*
				    char* real* real* complex* integer*
				    complex* integer* real* real* real*
				    complex* real* integer*)))

  (cgetc2_	(int cgetc2_ (integer* complex* integer* integer*
				       integer* integer*)))

  (cgetf2_	(int cgetf2_ (integer* integer* complex* integer*
				       integer* integer*)))

  (cgetrf_	(int cgetrf_ (integer* integer* complex* integer*
				       integer* integer*)))

  (cgetri_	(int cgetri_ (integer* complex* integer* integer*
				       complex* integer* integer*)))

  (cgetrs_	(int cgetrs_ (char* integer* integer* complex*
				    integer* integer* complex* integer* integer*)))

  (cggbak_	(int cggbak_ (char* char* integer* integer*
				    integer* real* real* integer* complex*
				    integer* integer*)))

  (cggbal_	(int cggbal_ (char* integer* complex* integer*
				    complex* integer* integer* integer* real*
				    real* real* integer*)))

  (cgges_	(int cgges_ (char* char* char* L_fp integer* complex* integer* complex* integer*
				   integer* complex* complex* complex*
				   integer* complex* integer* complex* integer*
				   real* logical* integer*)))

  (cggesx_	(int cggesx_ (char* char* char* L_fp char* integer* complex* integer* complex*
				    integer* integer* complex* complex* complex*
				    integer* complex* integer* real* real* complex*
				    integer* real* integer*
				    integer* logical* integer*)))

  (cggev_	(int cggev_ (char* char* integer* complex*
				   integer* complex* integer* complex* complex*
				   complex* integer* complex* integer* complex*
				   integer* real* integer*)))

  (cggevx_	(int cggevx_ (char* char* char* char*
				    integer* complex* integer* complex* integer*
				    complex* complex* complex* integer* complex*
				    integer* integer* integer* real* real*
				    real* real* real* real* complex* integer* real* integer* logical*
				    integer*)))

  (cggglm_	(int cggglm_ (integer* integer* integer* complex*
				       integer* complex* integer* complex* complex*
				       complex* complex* integer* integer*)))

  (cgghrd_	(int cgghrd_ (char* char* integer* integer*
				    integer* complex* integer* complex* integer*
				    complex* integer* complex* integer* integer*)))

  (cgglse_	(int cgglse_ (integer* integer* integer* complex*
				       integer* complex* integer* complex* complex*
				       complex* complex* integer* integer*)))

  (cggqrf_	(int cggqrf_ (integer* integer* integer* complex*
				       integer* complex* complex* integer* complex*
				       complex* integer* integer*)))

  (cggrqf_	(int cggrqf_ (integer* integer* integer* complex*
				       integer* complex* complex* integer* complex*
				       complex* integer* integer*)))

  (cggsvd_	(int cggsvd_ (char* char* char* integer*
				    integer* integer* integer* integer* complex* integer*
				    complex* integer* real* real* complex*
				    integer* complex* integer* complex* integer*
				    complex* real* integer* integer*)))

  (cggsvp_	(int cggsvp_ (char* char* char* integer*
				    integer* integer* complex* integer* complex*
				    integer* real* real* integer* integer* complex*
				    integer* complex* integer* complex* integer*
				    integer* real* complex* complex* integer*)))

  (cgtcon_	(int cgtcon_ (char* integer* complex* complex*
				    complex* complex* integer* real* real*
				    complex* integer*)))

  (cgtrfs_	(int cgtrfs_ (char* integer* integer* complex*
				    complex* complex* complex* complex* complex*
				    complex* integer* complex* integer* complex*
				    integer* real* real* complex* real*
				    integer*)))

  (cgtsv_	(int cgtsv_ (integer* integer* complex* complex*
				      complex* complex* integer* integer*)))

  (cgtsvx_	(int cgtsvx_ (char* char* integer* integer*
				    complex* complex* complex* complex* complex*
				    complex* complex* integer* complex* integer*
				    complex* integer* real* real* real*
				    complex* real* integer*)))

  (cgttrf_	(int cgttrf_ (integer* complex* complex* complex*
				       complex* integer* integer*)))

  (cgttrs_	(int cgttrs_ (char* integer* integer* complex*
				    complex* complex* complex* integer* complex*
				    integer* integer*)))

  (cgtts2_	(int cgtts2_ (integer* integer* integer*
				       complex* complex* complex* complex* integer*
				       complex* integer*)))

  (chbev_	(int chbev_ (char* char* integer* integer*
				   complex* integer* real* complex* integer*
				   complex* real* integer*)))

  (chbevd_	(int chbevd_ (char* char* integer* integer*
				    complex* integer* real* complex* integer*
				    complex* integer* real* integer* integer*
				    integer* integer*)))

  (chbevx_	(int chbevx_ (char* char* char* integer*
				    integer* complex* integer* complex* integer*
				    real* real* integer* integer* real* integer*
				    real* complex* integer* complex* real*
				    integer* integer* integer*)))

  (chbgst_	(int chbgst_ (char* char* integer* integer*
				    integer* complex* integer* complex* integer*
				    complex* integer* complex* real* integer*)))

  (chbgv_	(int chbgv_ (char* char* integer* integer*
				   integer* complex* integer* complex* integer*
				   real* complex* integer* complex* real*
				   integer*)))

  (chbgvx_	(int chbgvx_ (char* char* char* integer*
				    integer* integer* complex* integer* complex*
				    integer* complex* integer* real* real* integer*
				    integer* real* integer* real* complex*
				    integer* complex* real* integer* integer*
				    integer*)))

  (chbtrd_	(int chbtrd_ (char* char* integer* integer*
				    complex* integer* real* real* complex* integer*
				    complex* integer*)))

  (checon_	(int checon_ (char* integer* complex* integer*
				    integer* real* real* complex* integer*)))

  (cheev_	(int cheev_ (char* char* integer* complex*
				   integer* real* complex* integer* real*
				   integer*)))

  (cheevd_	(int cheevd_ (char* char* integer* complex*
				    integer* real* complex* integer* real*
				    integer* integer* integer* integer*)))

  (cheevr_	(int cheevr_ (char* char* char* integer*
				    complex* integer* real* real* integer* integer*
				    real* integer* real* complex* integer*
				    integer* complex* integer* real* integer*
				    integer* integer* integer*)))

  (cheevx_	(int cheevx_ (char* char* char* integer*
				    complex* integer* real* real* integer* integer*
				    real* integer* real* complex* integer*
				    complex* integer* real* integer* integer*
				    integer*)))

  (chegs2_	(int chegs2_ (integer* char* integer* complex*
				       integer* complex* integer* integer*)))

  (chegst_	(int chegst_ (integer* char* integer* complex*
				       integer* complex* integer* integer*)))

  (chegv_	(int chegv_ (integer* char* char* integer*
				      complex* integer* complex* integer* real*
				      complex* integer* real* integer*)))

  (chegvd_	(int chegvd_ (integer* char* char* integer*
				       complex* integer* complex* integer* real*
				       complex* integer* real* integer* integer*
				       integer* integer*)))

  (chegvx_	(int chegvx_ (integer* char* char* char*
				       integer* complex* integer* complex* integer*
				       real* real* integer* integer* real* integer*
				       real* complex* integer* complex* integer*
				       real* integer* integer* integer*)))

  (cherfs_	(int cherfs_ (char* integer* integer* complex*
				    integer* complex* integer* integer* complex*
				    integer* complex* integer* real* real*
				    complex* real* integer*)))

  (chesv_	(int chesv_ (char* integer* integer* complex*
				   integer* integer* complex* integer* complex*
				   integer* integer*)))

  (chesvx_	(int chesvx_ (char* char* integer* integer*
				    complex* integer* complex* integer* integer*
				    complex* integer* complex* integer* real*
				    real* real* complex* integer* real*
				    integer*)))

  (chetf2_	(int chetf2_ (char* integer* complex* integer*
				    integer* integer*)))

  (chetrd_	(int chetrd_ (char* integer* complex* integer*
				    real* real* complex* complex* integer*
				    integer*)))

  (chetrf_	(int chetrf_ (char* integer* complex* integer*
				    integer* complex* integer* integer*)))

  (chetri_	(int chetri_ (char* integer* complex* integer*
				    integer* complex* integer*)))

  (chetrs_	(int chetrs_ (char* integer* integer* complex*
				    integer* integer* complex* integer* integer*)))

  (chgeqz_	(int chgeqz_ (char* char* char* integer*
				    integer* integer* complex* integer* complex*
				    integer* complex* complex* complex* integer*
				    complex* integer* complex* integer* real*
				    integer*)))

  (chpcon_	(int chpcon_ (char* integer* complex* integer*
				    real* real* complex* integer*)))

  (chpev_	(int chpev_ (char* char* integer* complex*
				   real* complex* integer* complex* real*
				   integer*)))

  (chpevd_	(int chpevd_ (char* char* integer* complex*
				    real* complex* integer* complex* integer*
				    real* integer* integer* integer*
				    integer*)))

  (chpevx_	(int chpevx_ (char* char* char* integer*
				    complex* real* real* integer* integer* real*
				    integer* real* complex* integer* complex*
				    real* integer* integer* integer*)))

  (chpgst_	(int chpgst_ (integer* char* integer* complex*
				       complex* integer*)))

  (chpgv_	(int chpgv_ (integer* char* char* integer*
				      complex* complex* real* complex* integer*
				      complex* real* integer*)))

  (chpgvd_	(int chpgvd_ (integer* char* char* integer*
				       complex* complex* real* complex* integer*
				       complex* integer* real* integer* integer*
				       integer* integer*)))

  (chpgvx_	(int chpgvx_ (integer* char* char* char*
				       integer* complex* complex* real* real*
				       integer* integer* real* integer* real* complex*
				       integer* complex* real* integer*
				       integer* integer*)))

  (chprfs_	(int chprfs_ (char* integer* integer* complex*
				    complex* integer* complex* integer* complex*
				    integer* real* real* complex* real*
				    integer*)))

  (chpsv_	(int chpsv_ (char* integer* integer* complex*
				   integer* complex* integer* integer*)))

  (chpsvx_	(int chpsvx_ (char* char* integer* integer*
				    complex* complex* integer* complex* integer*
				    complex* integer* real* real* real*
				    complex* real* integer*)))

  (chptrd_	(int chptrd_ (char* integer* complex* real*
				    real* complex* integer*)))

  (chptrf_	(int chptrf_ (char* integer* complex* integer*
				    integer*)))

  (chptri_	(int chptri_ (char* integer* complex* integer*
				    complex* integer*)))

  (chptrs_	(int chptrs_ (char* integer* integer* complex*
				    integer* complex* integer* integer*)))

  (chsein_	(int chsein_ (char* char* char* logical*
				    integer* complex* integer* complex* complex*
				    integer* complex* integer* integer* integer*
				    complex* real* integer* integer*
				    integer*)))

  (chseqr_	(int chseqr_ (char* char* integer* integer*
				    integer* complex* integer* complex* complex*
				    integer* complex* integer* integer*)))

  (clabrd_	(int clabrd_ (integer* integer* integer* complex*
				       integer* real* real* complex* complex*
				       complex* integer* complex* integer*)))

  (clacgv_	(int clacgv_ (integer* complex* integer*)))

  (clacon_	(int clacon_ (integer* complex* complex* real*
				       integer*)))

  (clacp2_	(int clacp2_ (char* integer* integer* real*
				    integer* complex* integer*)))

  (clacpy_	(int clacpy_ (char* integer* integer* complex*
				    integer* complex* integer*)))

  (clacrm_	(int clacrm_ (integer* integer* complex* integer*
				       real* integer* complex* integer* real*)))

  (clacrt_	(int clacrt_ (integer* complex* integer* complex*
				       integer* complex* complex*)))

  (claed0_	(int claed0_ (integer* integer* real* real*
				       complex* integer* complex* integer* real*
				       integer* integer*)))

  (claed7_	(int claed7_ (integer* integer* integer*
				       integer* integer* integer* real* complex*
				       integer* real* integer* real* integer*
				       integer* integer* integer* integer*
				       real* complex* real* integer*
				       integer*)))

  (claed8_	(int claed8_ (integer* integer* integer* complex*
				       integer* real* real* integer* real*
				       real* complex* integer* real* integer*
				       integer* integer* integer* integer*
				       integer* real* integer*)))

  (claein_	(int claein_ (logical* logical* integer*
				       complex* integer* complex* complex* complex*
				       integer* real* real* real* integer*)))

  (claesy_	(int claesy_ (complex* complex* complex* complex*
				       complex* complex* complex* complex*)))

  (claev2_	(int claev2_ (complex* complex* complex* real*
				       real* real* complex*)))

  (clags2_	(int clags2_ (logical* real* complex* real*
					real* complex* real* real* complex* real*
					complex* real* complex*)))

  (clagtm_	(int clagtm_ (char* integer* integer* real*
				    complex* complex* complex* complex* integer*
				    real* complex* integer*)))

  (clahef_	(int clahef_ (char* integer* integer* integer*
				    complex* integer* integer* complex* integer*
				    integer*)))

  (clahqr_	(int clahqr_ (logical* logical* integer*
					integer* integer* complex* integer* complex*
					integer* integer* complex* integer* integer*
					)))

  (clahrd_	(int clahrd_ (integer* integer* integer* complex*
				       integer* complex* complex* integer* complex*
				       integer*)))

  (claic1_	(int claic1_ (integer* integer* complex* real*
				       complex* complex* real* complex* complex*)))

  (clals0_	(int clals0_ (integer* integer* integer*
				       integer* integer* complex* integer* complex*
				       integer* integer* integer* integer*
				       integer* real* integer* real* real*
				       real* real* integer* real* real* real*
				       integer*)))

  (clalsa_	(int clalsa_ (integer* integer* integer*
				       integer* complex* integer* complex* integer*
				       real* integer* real* integer* real* real*
				       real* real* integer* integer* integer*
				       integer* real* real* real* real*
				       integer* integer*)))

  (clapll_	(int clapll_ (integer* complex* integer* complex*
				       integer* real*)))

  (clapmt_	(int clapmt_ (logical* integer* integer* complex* integer* integer*)))

  (claqgb_	(int claqgb_ (integer* integer* integer* integer*
				       complex* integer* real* real* real* real* real* char*)))

  (claqge_	(int claqge_ (integer* integer* complex* integer*
				       real* real* real* real* real* char*)))

  (claqhb_	(int claqhb_ (char* integer* integer* complex*
				    integer* real* real* real* char*)))

  (claqhe_	(int claqhe_ (char* integer* complex* integer*
				    real* real* real* char*)))

  (claqhp_	(int claqhp_ (char* integer* complex* real*
				    real* real* char*)))

  (claqp2_	(int claqp2_ (integer* integer* integer* complex* integer* integer* complex* real* real*
				       complex*)))

  (claqps_	(int claqps_ (integer* integer* integer* integer* integer* complex* integer*
				       integer* complex*
				       real* real* complex* complex* integer*)))

  (claqsb_	(int claqsb_ (char* integer* integer* complex*
				    integer* real* real* real* char*)))

  (claqsp_	(int claqsp_ (char* integer* complex* real*
				    real* real* char*)))

  (claqsy_	(int claqsy_ (char* integer* complex* integer*
				    real* real* real* char*)))

  (clar1v_	(int clar1v_ (integer* integer* integer* real*
				       real* real* real* real* real* complex* real* real*
				       integer* integer* real*)))

  (clar2v_	(int clar2v_ (integer* complex* complex* complex*
				       integer* real* complex* integer*)))

  (clarcm_	(int clarcm_ (integer* integer* real* integer*
				       complex* integer* complex* integer* real*)))

  (clarf_	(int clarf_ (char* integer* integer* complex*
				   integer* complex* complex* integer* complex*)))

  (clarfb_	(int clarfb_ (char* char* char* char*
				    integer* integer* integer* complex* integer*
				    complex* integer* complex* integer* complex*
				    integer*)))

  (clarfg_	(int clarfg_ (integer* complex* complex* integer*
				       complex*)))

  (clarft_	(int clarft_ (char* char* integer* integer*
				    complex* integer* complex* complex* integer*)))

  (clarfx_	(int clarfx_ (char* integer* integer* complex*
				    complex* complex* integer* complex*)))

  (clargv_	(int clargv_ (integer* complex* integer* complex*
				       integer* real* integer*)))

  (clarnv_	(int clarnv_ (integer* integer* integer* complex*)))

  (clarrv_	(int clarrv_ (integer* real* real* integer*
				       integer* real* integer* real* real*
				       complex* integer* integer* real* integer*
				       integer*)))

  (clartg_	(int clartg_ (complex* complex* real* complex* complex*)))

  (clartv_	(int clartv_ (integer* complex* integer* complex*
				       integer* real* complex* integer*)))

  (clarz_	(int clarz_ (char* integer* integer* integer*
				   complex* integer* complex* complex* integer*
				   complex*)))

  (clarzb_	(int clarzb_ (char* char* char* char*
				    integer* integer* integer* integer* complex*
				    integer* complex* integer* complex* integer*
				    complex* integer*)))

  (clarzt_	(int clarzt_ (char* char* integer* integer*
				    complex* integer* complex* complex* integer*)))

  (clascl_	(int clascl_ (char* integer* integer* real*
				     real* integer* integer* complex* integer*
				     integer*)))

  (claset_	(int claset_ (char* integer* integer* complex*
				    complex* complex* integer*)))

  (clasr_	(int clasr_ (char* char* char* integer*
				   integer* real* real* complex* integer*)))

  (classq_	(int classq_ (integer* complex* integer* real*
				       real*)))

  (claswp_	(int claswp_ (integer* complex* integer* integer*
				       integer* integer* integer*)))

  (clasyf_	(int clasyf_ (char* integer* integer* integer*
				    complex* integer* integer* complex* integer*
				    integer*)))

  (clatbs_	(int clatbs_ (char* char* char* char*
				    integer* integer* complex* integer* complex*
				    real* real* integer*)))

  (clatdf_	(int clatdf_ (integer* integer* complex* integer* complex* real* real* integer* integer*)))

  (clatps_	(int clatps_ (char* char* char* char*
				    integer* complex* complex* real* real*
				    integer*)))

  (clatrd_	(int clatrd_ (char* integer* integer* complex*
				    integer* real* complex* complex* integer*)))

  (clatrs_	(int clatrs_ (char* char* char* char*
				    integer* complex* integer* complex* real*
				    real* integer*)))

  (clatrz_	(int clatrz_ (integer* integer* integer* complex*
				       integer* complex* complex*)))

  (clatzm_	(int clatzm_ (char* integer* integer* complex*
				    integer* complex* complex* complex* integer*
				    complex*)))

  (clauu2_	(int clauu2_ (char* integer* complex* integer*
				    integer*)))

  (clauum_	(int clauum_ (char* integer* complex* integer*
				    integer*)))

  (cpbcon_	(int cpbcon_ (char* integer* integer* complex*
				    integer* real* real* complex* real*
				    integer*)))

  (cpbequ_	(int cpbequ_ (char* integer* integer* complex*
				    integer* real* real* real* integer*)))

  (cpbrfs_	(int cpbrfs_ (char* integer* integer* integer*
				    complex* integer* complex* integer*
				    complex* integer* complex* integer* real* real*
				    complex* real* integer*)))

  (cpbstf_	(int cpbstf_ (char* integer* integer* complex*
				    integer* integer*)))

  (cpbsv_	(int cpbsv_ (char* integer* integer* integer*
				   complex* integer* complex* integer* integer*
				   )))

  (cpbsvx_	(int cpbsvx_ (char* char* integer* integer*
				    integer* complex* integer* complex* integer*
				    char* real* complex* integer* complex*
				    integer* real* real* real* complex*
				    real* integer*)))

  (cpbtf2_	(int cpbtf2_ (char* integer* integer* complex*
				    integer* integer*)))

  (cpbtrf_	(int cpbtrf_ (char* integer* integer* complex*
				    integer* integer*)))

  (cpbtrs_	(int cpbtrs_ (char* integer* integer* integer*
				    complex* integer* complex* integer* integer*
				    )))

  (cpocon_	(int cpocon_ (char* integer* complex* integer*
				    real* real* complex* real* integer*)))

  (cpoequ_	(int cpoequ_ (integer* complex* integer* real*
				       real* real* integer*)))

  (cporfs_	(int cporfs_ (char* integer* integer* complex*
				    integer* complex* integer* complex* integer*
				    complex* integer* real* real* complex*
				    real* integer*)))

  (cposv_	(int cposv_ (char* integer* integer* complex*
				   integer* complex* integer* integer*)))

  (cposvx_	(int cposvx_ (char* char* integer* integer*
				    complex* integer* complex* integer* char*
				    real* complex* integer* complex* integer*
				    real* real* real* complex* real*
				    integer*)))

  (cpotf2_	(int cpotf2_ (char* integer* complex* integer* integer*)))

  (cpotrf_	(int cpotrf_ (char* integer* complex* integer*
				    integer*)))

  (cpotri_	(int cpotri_ (char* integer* complex* integer*
				    integer*)))

  (cpotrs_	(int cpotrs_ (char* integer* integer* complex*
				    integer* complex* integer* integer*)))

  (cppcon_	(int cppcon_ (char* integer* complex* real*
				    real* complex* real* integer*)))

  (cppequ_	(int cppequ_ (char* integer* complex* real*
				    real* real* integer*)))

  (cpprfs_	(int cpprfs_ (char* integer* integer* complex*
				    complex* complex* integer* complex* integer*
				    real* real* complex* real* integer*)))

  (cppsv_	(int cppsv_ (char* integer* integer* complex*
				   complex* integer* integer*)))

  (cppsvx_	(int cppsvx_ (char* char* integer* integer*
				    complex* complex* char* real* complex*
				    integer* complex* integer* real* real* real* complex* real* integer*)))

  (cpptrf_	(int cpptrf_ (char* integer* complex* integer*
				    )))

  (cpptri_	(int cpptri_ (char* integer* complex* integer*
				    )))

  (cpptrs_	(int cpptrs_ (char* integer* integer* complex*
				    complex* integer* integer*)))

  (cptcon_	(int cptcon_ (integer* real* complex* real*
				       real* real* integer*)))

  (cptrfs_	(int cptrfs_ (char* integer* integer* real*
				    complex* real* complex* complex* integer* complex* integer*
				    real* real* complex* real* integer*)))

  (cptsv_	(int cptsv_ (integer* integer* real* complex*
				      complex* integer* integer*)))

  (cptsvx_	(int cptsvx_ (char* integer* integer* real*
				    complex* real* complex* complex* integer* complex* integer*
				    real* real* real* complex* real* integer*)))

  (cpttrf_	(int cpttrf_ (integer* real* complex* integer*)))

  (cpttrs_	(int cpttrs_ (char* integer* integer* real*
				    complex* complex* integer* integer*)))

  (cptts2_	(int cptts2_ (integer* integer* integer* real*
				       complex* complex* integer*)))

  (crot_	(int crot_ (integer* complex* integer* complex*
				     integer* real* complex*)))

  (cspcon_	(int cspcon_ (char* integer* complex* integer*
				    real* real* complex* integer*)))

  (cspmv_	(int cspmv_ (char* integer* complex* complex*
				   complex* integer* complex* complex* integer*
				  )))

  (cspr_	(int cspr_ (char* integer* complex* complex*
				  integer* complex*)))

  (csprfs_	(int csprfs_ (char* integer* integer* complex*
				    complex* integer* complex* integer* complex*
				    integer* real* real* complex* real*
				    integer*)))

  (cspsv_	(int cspsv_ (char* integer* integer* complex*
				   integer* complex* integer* integer*)))

  (cspsvx_	(int cspsvx_ (char* char* integer* integer*
				    complex* complex* integer* complex* integer*
				    complex* integer* real* real* real*
				    complex* real* integer*)))

  (csptrf_	(int csptrf_ (char* integer* complex* integer* integer*)))

  (csptri_	(int csptri_ (char* integer* complex* integer*
				    complex* integer*)))

  (csptrs_	(int csptrs_ (char* integer* integer* complex*
				    integer* complex* integer* integer*)))

  (csrot_	(int csrot_ (integer* complex* integer* complex*
				      integer* real* real*)))

  (csrscl_	(int csrscl_ (integer* real* complex* integer*)))

  (cstedc_	(int cstedc_ (char* integer* real* real*
				    complex* integer* complex* integer* real*
				    integer* integer* integer* integer*
				    )))

  (cstein_	(int cstein_ (integer* real* real* integer* real* integer* integer* complex* integer*
				       real* integer* integer* integer*)))

  (csteqr_	(int csteqr_ (char* integer* real* real*
				    complex* integer* real* integer*)))

  (csycon_	(int csycon_ (char* integer* complex* integer*
				    integer* real* real* complex* integer*
				    )))

  (csymv_	(int csymv_ (char* integer* complex* complex*
				   integer* complex* integer* complex* complex*
				   integer*)))

  (csyr_	(int csyr_ (char* integer* complex* complex*
				  integer* complex* integer*)))

  (csyrfs_	(int csyrfs_ (char* integer* integer* complex*
				    integer* complex* integer* integer* complex*
				    integer* complex* integer* real* real*
				    complex* real* integer*)))

  (csysv_	(int csysv_ (char* integer* integer* complex*
				   integer* integer* complex* integer* complex*
				   integer* integer*)))

  (csysvx_	(int csysvx_ (char* char* integer* integer*
				    complex* integer* complex* integer* integer*
				    complex* integer* complex* integer* real*
				    real* real* complex* integer* real*
				    integer*)))

  (csytf2_	(int csytf2_ (char* integer* complex* integer*
				    integer* integer*)))

  (csytrf_	(int csytrf_ (char* integer* complex* integer*
				    integer* complex* integer* integer*)))

  (csytri_	(int csytri_ (char* integer* complex* integer*
				    integer* complex* integer*)))

  (csytrs_	(int csytrs_ (char* integer* integer* complex*
				    integer* integer* complex* integer* integer*
				    )))

  (ctbcon_	(int ctbcon_ (char* char* char* integer*
				    integer* complex* integer* real* complex*
				    real* integer*)))

  (ctbrfs_	(int ctbrfs_ (char* char* char* integer*
				    integer* integer* complex* integer* complex*
				    integer* complex* integer* real* real*
				    complex* real* integer*)))

  (ctbtrs_	(int ctbtrs_ (char* char* char* integer*
				    integer* integer* complex* integer* complex*
				    integer* integer*)))

  (ctgevc_	(int ctgevc_ (char* char* logical*
				    integer* complex* integer* complex* integer*
				    complex* integer* complex* integer* integer*
				    integer* complex* real* integer*)))

  (ctgex2_	(int ctgex2_ (logical* logical* integer*
					complex* integer* complex* integer* complex*
					integer* complex* integer* integer* integer*)))

  (ctgexc_	(int ctgexc_ (logical* logical* integer*
					complex* integer* complex* integer* complex*
					integer* complex* integer* integer* integer*
					integer*)))

  (ctgsen_	(int ctgsen_ (integer* logical* logical*
				       logical* integer* complex* integer* complex*
				       integer* complex* complex* complex* integer*
				       complex* integer* integer* real* real* real*
				       complex* integer* integer* integer*
				       integer*)))

  (ctgsja_	(int ctgsja_ (char* char* char* integer*
				    integer* integer* integer* integer* complex* integer*
				    complex* integer* real* real* real*
				    real* complex* integer* complex* integer*
				    complex* integer* complex* integer* integer*
				    )))

  (ctgsna_	(int ctgsna_ (char* char* logical*
				    integer* complex* integer* complex* integer*
				    complex* integer* complex* integer* real* real*
				    integer* integer* complex* integer* integer* integer*)))

  (ctgsy2_	(int ctgsy2_ (char* integer* integer* integer*
				    complex* integer* complex* integer* complex*
				    integer* complex* integer* complex* integer*
				    complex* integer* real* real* real*
				    integer*)))

  (ctgsyl_	(int ctgsyl_ (char* integer* integer* integer*
				    complex* integer* complex* integer* complex*
				    integer* complex* integer* complex* integer*
				    complex* integer* real* real* complex*
				    integer* integer* integer*)))

  (ctpcon_	(int ctpcon_ (char* char* char* integer*
				    complex* real* complex* real* integer*)))

  (ctprfs_	(int ctprfs_ (char* char* char* integer*
				    integer* complex* complex* integer* complex*
				    integer* real* real* complex* real*
				    integer*)))

  (ctptri_	(int ctptri_ (char* char* integer* complex*
				    integer*)))

  (ctptrs_	(int ctptrs_ (char* char* char* integer*
				    integer* complex* complex* integer* integer*)))

  (ctrcon_	(int ctrcon_ (char* char* char* integer*
				    complex* integer* real* complex* real*
				    integer*)))

  (ctrevc_	(int ctrevc_ (char* char* logical*
				    integer* complex* integer* complex* integer*
				    complex* integer* integer* integer* complex*
				    real* integer*)))

  (ctrexc_	(int ctrexc_ (char* integer* complex* integer*
				    complex* integer* integer* integer* integer*
				    )))

  (ctrrfs_	(int ctrrfs_ (char* char* char* integer*
				    integer* complex* integer* complex* integer*
				    complex* integer* real* real* complex* real* integer*)))

  (ctrsen_	(int ctrsen_ (char* char* logical* integer* complex* integer* complex* integer* complex*
				    integer* real* real* complex* integer*
				    integer*)))

  (ctrsna_	(int ctrsna_ (char* char* logical*
				    integer* complex* integer* complex* integer*
				    complex* integer* real* real* integer* integer*
				    complex* integer* real* integer*)))

  (ctrsyl_	(int ctrsyl_ (char* char* integer* integer* integer* complex* integer* complex* integer*
				    complex* integer* real* integer*)))

  (ctrti2_	(int ctrti2_ (char* char* integer* complex*
				    integer* integer*)))

  (ctrtri_	(int ctrtri_ (char* char* integer* complex*
				    integer* integer*)))

  (ctrtrs_	(int ctrtrs_ (char* char* char* integer*
				    integer* complex* integer* complex* integer*
				    integer*)))

  (ctzrqf_	(int ctzrqf_ (integer* integer* complex* integer*
				       complex* integer*)))

  (ctzrzf_	(int ctzrzf_ (integer* integer* complex* integer*
				       complex* complex* integer* integer*)))

  (cung2l_	(int cung2l_ (integer* integer* integer* complex*
				       integer* complex* complex* integer*)))

  (cung2r_	(int cung2r_ (integer* integer* integer* complex*
				       integer* complex* complex* integer*)))

  (cungbr_	(int cungbr_ (char* integer* integer* integer*
				    complex* integer* complex* complex* integer*
				    integer*)))

  (cunghr_	(int cunghr_ (integer* integer* integer* complex*
				       integer* complex* complex* integer* integer*)))

  (cungl2_	(int cungl2_ (integer* integer* integer* complex*
				       integer* complex* complex* integer*)))

  (cunglq_	(int cunglq_ (integer* integer* integer* complex*
				       integer* complex* complex* integer* integer*
				       )))

  (cungql_	(int cungql_ (integer* integer* integer* complex*
				       integer* complex* complex* integer* integer*
				       )))

  (cungqr_	(int cungqr_ (integer* integer* integer* complex*
				       integer* complex* complex* integer* integer*
				       )))

  (cungr2_	(int cungr2_ (integer* integer* integer* complex*
				       integer* complex* complex* integer*)))

  (cungrq_	(int cungrq_ (integer* integer* integer* complex*
				       integer* complex* complex* integer* integer*
				       )))

  (cungtr_	(int cungtr_ (char* integer* complex* integer*
				    complex* complex* integer* integer*)))

  (cunm2l_	(int cunm2l_ (char* char* integer* integer*
				    integer* complex* integer* complex* complex*
				    integer* complex* integer*)))

  (cunm2r_	(int cunm2r_ (char* char* integer* integer*
				    integer* complex* integer* complex* complex*
				    integer* complex* integer*)))

  (cunmbr_	(int cunmbr_ (char* char* char* integer*
				    integer* integer* complex* integer* complex*
				    complex* integer* complex* integer* integer*
				    )))

  (cunmhr_	(int cunmhr_ (char* char* integer* integer*
				    integer* integer* complex* integer* complex*
				    complex* integer* complex* integer* integer*
				    )))

  (cunml2_	(int cunml2_ (char* char* integer* integer*
				    integer* complex* integer* complex* complex*
				    integer* complex* integer*)))

  (cunmlq_	(int cunmlq_ (char* char* integer* integer*
				    integer* complex* integer* complex* complex*
				    integer* complex* integer* integer*)))

  (cunmql_	(int cunmql_ (char* char* integer* integer*
				    integer* complex* integer* complex* complex*
				    integer* complex* integer* integer*)))

  (cunmqr_	(int cunmqr_ (char* char* integer* integer*
				    integer* complex* integer* complex* complex*
				    integer* complex* integer* integer*)))

  (cunmr2_	(int cunmr2_ (char* char* integer* integer*
				    integer* complex* integer* complex* complex*
				    integer* complex* integer*)))

  (cunmr3_	(int cunmr3_ (char* char* integer* integer*
				    integer* integer* complex* integer* complex*
				    complex* integer* complex* integer*)))

  (cunmrq_	(int cunmrq_ (char* char* integer* integer*
				    integer* complex* integer* complex* complex*
				    integer* complex* integer* integer*)))

  (cunmrz_	(int cunmrz_ (char* char* integer* integer*
				    integer* integer* complex* integer* complex*
				    complex* integer* complex* integer* integer*
				    )))

  (cunmtr_	(int cunmtr_ (char* char* char* integer*
				    integer* complex* integer* complex* complex*
				    integer* complex* integer* integer*)))

  (cupgtr_	(int cupgtr_ (char* integer* complex* complex*
				    complex* integer* complex* integer*)))

  (cupmtr_	(int cupmtr_ (char* char* char* integer*
				    integer* complex* complex* complex* integer*
				    complex* integer*)))

  (dbdsdc_	(int dbdsdc_ (char* char* integer* doublereal*
				    doublereal* doublereal* integer* doublereal*
				    integer* doublereal* integer* doublereal* integer*
				    integer*)))

  (dbdsqr_	(int dbdsqr_ (char* integer* integer* integer*
				    integer* doublereal* doublereal* doublereal*
				    integer* doublereal* integer* doublereal* integer*
				    doublereal* integer*)))

  (ddisna_	(int ddisna_ (char* integer* integer* doublereal*
				    doublereal* integer*)))

  (dgbbrd_	(int dgbbrd_ (char* integer* integer* integer*
				    integer* integer* doublereal* integer* doublereal*
				    doublereal* doublereal* integer* doublereal*
				    integer* doublereal* integer* doublereal*
				    integer*)))

  (dgbcon_	(int dgbcon_ (char* integer* integer* integer*
				    doublereal* integer* integer* doublereal*
				    doublereal* doublereal* integer* integer*)))

  (dgbequ_	(int dgbequ_ (integer* integer* integer* integer*
				       doublereal* integer* doublereal* doublereal*
				       doublereal* doublereal* doublereal* integer*
				       )))

  (dgbrfs_	(int dgbrfs_ (char* integer* integer* integer*
				    integer* doublereal* integer* doublereal*
				    integer* integer* doublereal* integer*
				    doublereal* integer* doublereal* doublereal*
				    doublereal* integer* integer*)))

  (dgbsv_	(int dgbsv_ (integer* integer* integer* integer*
				      doublereal* integer* integer* doublereal*
				      integer* integer*)))

  (dgbsvx_	(int dgbsvx_ (char* char* integer* integer*
				    integer* integer* doublereal* integer*
				    doublereal* integer* integer* char*
				    doublereal* doublereal* doublereal* integer*
				    doublereal* integer* doublereal* doublereal*
				    doublereal* doublereal* integer* integer*)))

  (dgbtf2_	(int dgbtf2_ (integer* integer* integer* integer*
				       doublereal* integer* integer* integer*)))

  (dgbtrf_	(int dgbtrf_ (integer* integer* integer* integer*
				       doublereal* integer* integer* integer*)))

  (dgbtrs_	(int dgbtrs_ (char* integer* integer* integer*
				    integer* doublereal* integer* integer*
				    doublereal* integer* integer*)))

  (dgebak_	(int dgebak_ (char* char* integer* integer*
				    integer* doublereal* integer* doublereal* integer*
				    integer*)))

  (dgebal_	(int dgebal_ (char* integer* doublereal* integer*
				    integer* integer* doublereal* integer*)))

  (dgebd2_	(int dgebd2_ (integer* integer* doublereal* integer*
				       doublereal* doublereal* doublereal* doublereal*
				       doublereal* integer*)))

  (dgebrd_	(int dgebrd_ (integer* integer* doublereal* integer*
				       doublereal* doublereal* doublereal* doublereal*
				       doublereal* integer* integer*)))

  (dgecon_	(int dgecon_ (char* integer* doublereal* integer*
				    doublereal* doublereal* doublereal* integer*
				    integer*)))

  (dgeequ_	(int dgeequ_ (integer* integer* doublereal* integer*
				       doublereal* doublereal* doublereal* doublereal*
				       doublereal* integer*)))

  (dgees_	(int dgees_ (char* char* L_fp integer*
				   doublereal* integer* integer* doublereal*
				   doublereal* doublereal* integer* doublereal*
				   integer* logical* integer*)))

  (dgeesx_	(int dgeesx_ (char* char* L_fp char*
				    integer* doublereal* integer* integer*
				    doublereal* doublereal* doublereal* integer*
				    doublereal* doublereal* doublereal* integer*
				    integer* integer* logical* integer*)))

  (dgeev_	(int dgeev_ (char* char* integer* doublereal*
				   integer* doublereal* doublereal* doublereal*
				   integer* doublereal* integer* doublereal*
				   integer* integer*)))

  (dgeevx_	(int dgeevx_ (char* char* char* char*
				    integer* doublereal* integer* doublereal*
				    doublereal* doublereal* integer* doublereal*
				    integer* integer* integer* doublereal*
				    doublereal* doublereal* doublereal* doublereal* integer*
				    integer* integer*)))

  (dgegs_	(int dgegs_ (char* char* integer*
				   doublereal* integer* doublereal* integer* doublereal*
				   doublereal* doublereal* doublereal*
				   integer* doublereal* integer* doublereal*
				   integer* integer*)))

  (dgegv_	(int dgegv_ (char* char* integer* doublereal*
				   integer* doublereal* integer* doublereal*
				   doublereal* doublereal* doublereal* integer*
				   doublereal* integer* doublereal* integer*
				   integer*)))

  (dgehd2_	(int dgehd2_ (integer* integer* integer*
				       doublereal* integer* doublereal* doublereal*
				       integer*)))

  (dgehrd_	(int dgehrd_ (integer* integer* integer*
				       doublereal* integer* doublereal* doublereal*
				       integer* integer*)))

  (dgelq2_	(int dgelq2_ (integer* integer* doublereal* integer*
				       doublereal* doublereal* integer*)))

  (dgelqf_	(int dgelqf_ (integer* integer* doublereal* integer*
				       doublereal* doublereal* integer* integer*)))

  (dgels_	(int dgels_ (char* integer* integer* integer*
				   doublereal* integer* doublereal* integer*
				   doublereal* integer* integer*)))

  (dgelsd_	(int dgelsd_ (integer* integer* integer*
				       doublereal* integer* doublereal* integer* doublereal*
				       doublereal* integer* doublereal* integer*
				       integer* integer*)))

  (dgelss_	(int dgelss_ (integer* integer* integer*
				       doublereal* integer* doublereal* integer* doublereal*
				       doublereal* integer* doublereal* integer*
				       integer*)))

  (dgelsx_	(int dgelsx_ (integer* integer* integer*
				       doublereal* integer* doublereal* integer* integer*
				       doublereal* integer* doublereal* integer*
				       )))

  (dgelsy_	(int dgelsy_ (integer* integer* integer*
				       doublereal* integer* doublereal* integer* integer*
				       doublereal* integer* doublereal* integer*
				       integer*)))

  (dgeql2_	(int dgeql2_ (integer* integer* doublereal* integer*
				       doublereal* doublereal* integer*)))

  (dgeqlf_	(int dgeqlf_ (integer* integer* doublereal* integer*
				       doublereal* doublereal* integer* integer*)))

  (dgeqp3_	(int dgeqp3_ (integer* integer* doublereal* integer*
				       integer* doublereal* doublereal* integer*
				       integer*)))

  (dgeqpf_	(int dgeqpf_ (integer* integer* doublereal* integer*
				       integer* doublereal* doublereal* integer*)))

  (dgeqr2_	(int dgeqr2_ (integer* integer* doublereal* integer*
				       doublereal* doublereal* integer*)))

  (dgeqrf_	(int dgeqrf_ (integer* integer* doublereal* integer*
				       doublereal* doublereal* integer* integer*)))

  (dgerfs_	(int dgerfs_ (char* integer* integer*
				    doublereal* integer* doublereal* integer* integer*
				    doublereal* integer* doublereal* integer*
				    doublereal* doublereal* doublereal* integer*
				    integer*)))

  (dgerq2_	(int dgerq2_ (integer* integer* doublereal* integer*
				       doublereal* doublereal* integer*)))

  (dgerqf_	(int dgerqf_ (integer* integer* doublereal* integer*
				       doublereal* doublereal* integer* integer*)))

  (dgesc2_	(int dgesc2_ (integer* doublereal* integer*
				       doublereal* integer* integer* doublereal*)))

  (dgesdd_	(int dgesdd_ (char* integer* integer* doublereal*
				    integer* doublereal* doublereal* integer*
				    doublereal* integer* doublereal* integer*
				    integer* integer*)))

  (dgesv_	(int dgesv_ (integer* integer* doublereal* integer* integer* doublereal*
				      integer* integer*)))

  (dgesvd_	(int dgesvd_ (char* char* integer* integer*
				    doublereal* integer* doublereal* doublereal* integer*
				    doublereal* integer* doublereal* integer*
				    integer*)))

  (dgesvx_	(int dgesvx_ (char* char* integer* integer*
				    doublereal* integer* doublereal* integer*
				    integer* char* doublereal* doublereal*
				    doublereal* integer* doublereal* integer* doublereal*
				    doublereal* doublereal* doublereal* integer*
				    integer*)))

  (dgetc2_	(int dgetc2_ (integer* doublereal* integer* integer* integer* integer*)))

  (dgetf2_	(int dgetf2_ (integer* integer* doublereal* integer*
				       integer* integer*)))

  (dgetrf_	(int dgetrf_ (integer* integer* doublereal* integer*
				       integer* integer*)))

  (dgetri_	(int dgetri_ (integer* doublereal* integer* integer* doublereal* integer* integer*)))

  (dgetrs_	(int dgetrs_ (char* integer* integer*
				    doublereal* integer* integer* doublereal* integer*
				    integer*)))

  (dggbak_	(int dggbak_ (char* char* integer* integer*
				    integer* doublereal* doublereal* integer*
				    doublereal* integer* integer*)))

  (dggbal_	(int dggbal_ (char* integer* doublereal* integer*
				    doublereal* integer* integer* integer*
				    doublereal* doublereal* doublereal* integer*
				    )))

  (dgges_	(int dgges_ (char* char* char* L_fp
				   integer* doublereal* integer* doublereal*
				   integer* integer* doublereal* doublereal*
				   doublereal* doublereal* integer* doublereal*
				   integer* doublereal* integer* logical*
				   integer*)))

  (dggesx_	(int dggesx_ (char* char* char* L_fp
				    char* integer* doublereal* integer*
				    doublereal* integer* integer* doublereal*
				    doublereal* doublereal* doublereal* integer*
				    doublereal* integer* doublereal* doublereal*
				    doublereal* integer* integer* integer*
				    logical* integer*)))

  (dggev_	(int dggev_ (char* char* integer* doublereal*
				   integer* doublereal* integer* doublereal*
				   doublereal* doublereal* doublereal* integer*
				   doublereal* integer* doublereal* integer*
				   integer*)))

  (dggevx_	(int dggevx_ (char* char* char* char*
				    integer* doublereal* integer* doublereal*
				    integer* doublereal* doublereal* doublereal*
				    doublereal* integer* doublereal* integer*
				    integer* integer* doublereal* doublereal*
				    doublereal* doublereal* doublereal* doublereal*
				    doublereal* integer* integer* logical*
				    integer*)))

  (dggglm_	(int dggglm_ (integer* integer* integer* doublereal*
				       integer* doublereal* integer* doublereal*
				       doublereal* doublereal* doublereal* integer*
				       integer*)))

  (dgghrd_	(int dgghrd_ (char* char* integer* integer*
				    integer* doublereal* integer* doublereal*
				    integer* doublereal* integer* doublereal* integer*
				    integer*)))

  (dgglse_	(int dgglse_ (integer* integer* integer* doublereal*
				       integer* doublereal* integer* doublereal*
				       doublereal* doublereal* doublereal* integer*
				       integer*)))

  (dggqrf_	(int dggqrf_ (integer* integer* integer* doublereal*
				       integer* doublereal* doublereal* integer*
				       doublereal* doublereal* integer* integer*)))

  (dggrqf_	(int dggrqf_ (integer* integer* integer* doublereal*
				       integer* doublereal* doublereal* integer*
				       doublereal* doublereal* integer* integer*)))

  (dggsvd_	(int dggsvd_ (char* char* char* integer*
				    integer* integer* integer* integer* doublereal*
				    integer* doublereal* integer* doublereal*
				    doublereal* doublereal* integer* doublereal* integer*
				    doublereal* integer* doublereal* integer*
				    integer*)))

  (dggsvp_	(int dggsvp_ (char* char* char* integer*
				    integer* integer* doublereal* integer* doublereal*
				    integer* doublereal* doublereal* integer* integer* doublereal*
				    integer* doublereal* integer*
				    doublereal* integer* integer* doublereal*
				    doublereal* integer*)))

  (dgtcon_	(int dgtcon_ (char* integer* doublereal*
				    doublereal* doublereal* doublereal* integer*
				    doublereal* doublereal* doublereal* integer*
				    integer*)))

  (dgtrfs_	(int dgtrfs_ (char* integer* integer*
				    doublereal* doublereal* doublereal* doublereal*
				    doublereal* doublereal* doublereal* integer*
				    doublereal* integer* doublereal* integer* doublereal*
				    doublereal* doublereal* integer* integer*
				    )))

  (dgtsv_	(int dgtsv_ (integer* integer* doublereal*
				      doublereal* doublereal* doublereal* integer* integer*)))

  (dgtsvx_	(int dgtsvx_ (char* char* integer* integer*
				    doublereal* doublereal* doublereal* doublereal*
				    doublereal* doublereal* doublereal* integer*
				    doublereal* integer* doublereal* integer* doublereal*
				    doublereal* doublereal* doublereal* integer*
				    integer*)))

  (dgttrf_	(int dgttrf_ (integer* doublereal* doublereal*
				       doublereal* doublereal* integer* integer*)))

  (dgttrs_	(int dgttrs_ (char* integer* integer*
				    doublereal* doublereal* doublereal* doublereal*
				    integer* doublereal* integer* integer*)))

  (dgtts2_	(int dgtts2_ (integer* integer* integer*
				       doublereal* doublereal* doublereal* doublereal*
				       integer* doublereal* integer*)))

  (dhgeqz_	(int dhgeqz_ (char* char* char* integer*
				    integer* integer* doublereal* integer* doublereal*
				    integer* doublereal* doublereal* doublereal*
				    doublereal* integer* doublereal* integer*
				    doublereal* integer* integer*)))

  (dhsein_	(int dhsein_ (char* char* char* logical*
				    integer* doublereal* integer* doublereal*
				    doublereal* doublereal* integer* doublereal*
				    integer* integer* integer* doublereal* integer*
				    integer* integer*)))

  (dhseqr_	(int dhseqr_ (char* char* integer* integer*
				    integer* doublereal* integer* doublereal*
				    doublereal* doublereal* integer* doublereal*
				    integer* integer*)))

  (dlabad_	(int dlabad_ (doublereal* doublereal*)))

  (dlabrd_	(int dlabrd_ (integer* integer* integer* doublereal*
				       integer* doublereal* doublereal* doublereal*
				       doublereal* doublereal* integer* doublereal* integer*)))

  (dlacon_	(int dlacon_ (integer* doublereal* doublereal*
				       integer* doublereal* integer*)))

  (dlacpy_	(int dlacpy_ (char* integer* integer* doublereal*
				    integer* doublereal* integer*)))

  (dladiv_	(int dladiv_ (doublereal* doublereal* doublereal*
					  doublereal* doublereal* doublereal*)))

  (dlae2_	(int dlae2_ (doublereal* doublereal* doublereal*
					 doublereal* doublereal*)))

  (dlaebz_	(int dlaebz_ (integer* integer* integer*
				       integer* integer* integer* doublereal*
				       doublereal* doublereal* doublereal* doublereal*
				       doublereal* integer* doublereal* doublereal*
				       integer* integer* doublereal* integer*
				       integer*)))

  (dlaed0_	(int dlaed0_ (integer* integer* integer*
				       doublereal* doublereal* doublereal* integer*
				       doublereal* integer* doublereal* integer*
				       integer*)))

  (dlaed1_	(int dlaed1_ (integer* doublereal* doublereal*
				       integer* integer* doublereal* integer*
				       doublereal* integer* integer*)))

  (dlaed2_	(int dlaed2_ (integer* integer* integer* doublereal*
				       doublereal* integer* integer* doublereal*
				       doublereal* doublereal* doublereal* doublereal*
				       integer* integer* integer* integer*
				       integer*)))

  (dlaed3_	(int dlaed3_ (integer* integer* integer* doublereal*
				       doublereal* integer* doublereal* doublereal*
				       doublereal* integer* integer* doublereal*
				       doublereal* integer*)))

  (dlaed4_	(int dlaed4_ (integer* integer* doublereal*
				       doublereal* doublereal* doublereal* doublereal*
				       integer*)))

  (dlaed5_	(int dlaed5_ (integer* doublereal* doublereal*
					doublereal* doublereal* doublereal*)))

  (dlaed6_	(int dlaed6_ (integer* logical* doublereal*
				       doublereal* doublereal* doublereal* doublereal*
				       integer*)))

  (dlaed7_	(int dlaed7_ (integer* integer* integer*
				       integer* integer* integer* doublereal*
				       doublereal* integer* integer* doublereal* integer*
				       doublereal* integer* integer* integer*
				       integer* integer* doublereal*
				       doublereal* integer* integer*)))

  (dlaed8_	(int dlaed8_ (integer* integer* integer* integer* doublereal* doublereal*
				       integer* integer*
				       doublereal* integer* doublereal* doublereal*
				       doublereal* integer* doublereal* integer* integer*
				       integer* doublereal* integer* integer* integer*)))

  (dlaed9_	(int dlaed9_ (integer* integer* integer*
				       integer* doublereal* doublereal* integer* doublereal*
				       doublereal* doublereal* doublereal* integer*
				       integer*)))

  (dlaeda_	(int dlaeda_ (integer* integer* integer*
				       integer* integer* integer* integer*
				       integer* doublereal* doublereal* integer*
				       doublereal* doublereal* integer*)))

  (dlaein_	(int dlaein_ (logical* logical* integer*
					doublereal* integer* doublereal* doublereal*
					doublereal* doublereal* doublereal* integer*
					doublereal* doublereal* doublereal* doublereal*
					integer*)))

  (dlaev2_	(int dlaev2_ (doublereal* doublereal* doublereal*
					  doublereal* doublereal* doublereal* doublereal*)))

  (dlaexc_	(int dlaexc_ (logical* integer* doublereal*
					integer* doublereal* integer* integer* integer*
					integer* doublereal* integer*)))

  (dlag2_	(int dlag2_ (doublereal* integer* doublereal*
					 integer* doublereal* doublereal* doublereal*
					 doublereal* doublereal* doublereal*)))

  (dlags2_	(int dlags2_ (logical* doublereal* doublereal*
					doublereal* doublereal* doublereal* doublereal*
					doublereal* doublereal* doublereal* doublereal*
					doublereal* doublereal*)))

  (dlagtf_	(int dlagtf_ (integer* doublereal* doublereal*
				       doublereal* doublereal* doublereal* doublereal*
				       integer* integer*)))

  (dlagtm_	(int dlagtm_ (char* integer* integer*
				    doublereal* doublereal* doublereal* doublereal*
				    doublereal* integer* doublereal* doublereal* integer*)))

  (dlagts_	(int dlagts_ (integer* integer* doublereal*
				       doublereal* doublereal* doublereal* integer*
				       doublereal* doublereal* integer*)))

  (dlagv2_	(int dlagv2_ (doublereal* integer* doublereal*
					  integer* doublereal* doublereal* doublereal*
					  doublereal* doublereal* doublereal* doublereal*
					 )))

  (dlahqr_	(int dlahqr_ (logical* logical* integer*
					integer* integer* doublereal* integer* doublereal*
					doublereal* integer* integer* doublereal*
					integer* integer*)))

  (dlahrd_	(int dlahrd_ (integer* integer* integer* doublereal*
				       integer* doublereal* doublereal* integer*
				       doublereal* integer*)))

  (dlaic1_	(int dlaic1_ (integer* integer* doublereal*
				       doublereal* doublereal* doublereal* doublereal*
				       doublereal* doublereal*)))

  (dlaln2_	(int dlaln2_ (logical* integer* integer*
					doublereal* doublereal* doublereal* integer*
					doublereal* doublereal* doublereal* integer*
					doublereal* doublereal* doublereal* integer*
					doublereal* doublereal* integer*)))

  (dlals0_	(int dlals0_ (integer* integer* integer*
				       integer* integer* doublereal* integer* doublereal*
				       integer* integer* integer* integer*
				       integer* doublereal* integer* doublereal*
				       doublereal* doublereal* doublereal* integer*
				       doublereal* doublereal* doublereal* integer*)))

  (dlalsa_	(int dlalsa_ (integer* integer* integer*
				       integer* doublereal* integer* doublereal* integer*
				       doublereal* integer* doublereal* integer*
				       doublereal* doublereal* doublereal* doublereal*
				       integer* integer* integer* integer*
				       doublereal* doublereal* doublereal* doublereal*
				       integer* integer*)))

  (dlalsd_	(int dlalsd_ (char* integer* integer* integer* doublereal* doublereal* doublereal*
				    integer* doublereal* integer* doublereal* integer* integer*)))

  (dlamc1_	(int dlamc1_ (integer* integer* logical* logical*)))

  (dlamc2_	(int dlamc2_ (integer* integer* logical*
				       doublereal* integer* doublereal* integer*
				       doublereal*)))

  (dlamc4_	(int dlamc4_ (integer* doublereal* integer*)))

  (dlamc5_	(int dlamc5_ (integer* integer* integer*
				       logical* integer* doublereal*)))

  (dlamrg_	(int dlamrg_ (integer* integer* doublereal* integer* integer* integer*)))

  (dlanv2_	(int dlanv2_ (doublereal* doublereal* doublereal*
					  doublereal* doublereal* doublereal* doublereal*
					  doublereal* doublereal* doublereal*)))

  (dlapll_	(int dlapll_ (integer* doublereal* integer*
				       doublereal* integer* doublereal*)))

  (dlapmt_	(int dlapmt_ (logical* integer* integer*
					doublereal* integer* integer*)))

  (dlaqgb_	(int dlaqgb_ (integer* integer* integer* integer*
				       doublereal* integer* doublereal* doublereal*
				       doublereal* doublereal* doublereal* char*)))

  (dlaqge_	(int dlaqge_ (integer* integer* doublereal* integer*
				       doublereal* doublereal* doublereal* doublereal*
				       doublereal* char*)))

  (dlaqp2_	(int dlaqp2_ (integer* integer* integer*
				       doublereal* integer* integer* doublereal*
				       doublereal* doublereal* doublereal*)))

  (dlaqps_	(int dlaqps_ (integer* integer* integer* integer* integer* doublereal* integer* integer*
				       doublereal* doublereal* doublereal* doublereal*
				       doublereal* integer*)))

  (dlaqsb_	(int dlaqsb_ (char* integer* integer* doublereal*
				    integer* doublereal* doublereal* doublereal*
				    char*)))

  (dlaqsp_	(int dlaqsp_ (char* integer* doublereal*
				    doublereal* doublereal* doublereal* char*)))

  (dlaqsy_	(int dlaqsy_ (char* integer* doublereal* integer*
				    doublereal* doublereal* doublereal* char*)))

  (dlaqtr_	(int dlaqtr_ (logical* logical* integer*
					doublereal* integer* doublereal* doublereal* doublereal*
					doublereal* doublereal* integer*)))

  (dlar1v_	(int dlar1v_ (integer* integer* integer* doublereal* doublereal* doublereal*
				       doublereal* doublereal*
				       doublereal* doublereal* doublereal* doublereal*
				       integer* integer* doublereal*)))

  (dlar2v_	(int dlar2v_ (integer* doublereal* doublereal*
				       doublereal* integer* doublereal* doublereal*
				       integer*)))

  (dlarf_	(int dlarf_ (char* integer* integer* doublereal*
				   integer* doublereal* doublereal* integer*
				   doublereal*)))

  (dlarfb_	(int dlarfb_ (char* char* char* char*
				    integer* integer* integer* doublereal* integer*
				    doublereal* integer* doublereal* integer*
				    doublereal* integer*)))

  (dlarfg_	(int dlarfg_ (integer* doublereal* doublereal*
				       integer* doublereal*)))

  (dlarft_	(int dlarft_ (char* char* integer* integer*
				    doublereal* integer* doublereal* doublereal*
				    integer*)))

  (dlarfx_	(int dlarfx_ (char* integer* integer* doublereal*
				    doublereal* doublereal* integer* doublereal*)))

  (dlargv_	(int dlargv_ (integer* doublereal* integer*
				       doublereal* integer* doublereal* integer*)))

  (dlarnv_	(int dlarnv_ (integer* integer* integer* doublereal*)))

  (dlarrb_	(int dlarrb_ (integer* doublereal* doublereal*
				       doublereal* doublereal* integer* integer*
				       doublereal* doublereal* doublereal* doublereal*
				       doublereal* doublereal* integer* integer*
				       )))

  (dlarre_	(int dlarre_ (integer* doublereal* doublereal*
				       doublereal* integer* integer* integer*
				       doublereal* doublereal* doublereal* doublereal*
				       integer*)))

  (dlarrf_	(int dlarrf_ (integer* doublereal* doublereal*
				       doublereal* doublereal* integer* integer*
				       doublereal* doublereal* doublereal* doublereal*
				       integer* integer*)))

  (dlarrv_	(int dlarrv_ (integer* doublereal* doublereal*
				       integer* integer* doublereal* integer*
				       doublereal* doublereal* doublereal* integer*
				       integer* doublereal* integer* integer*)))

  (dlartg_	(int dlartg_ (doublereal* doublereal* doublereal*
					  doublereal* doublereal*)))

  (dlartv_	(int dlartv_ (integer* doublereal* integer*
				       doublereal* integer* doublereal* doublereal* integer*)))

  (dlaruv_	(int dlaruv_ (integer* integer* doublereal*)))

  (dlarz_	(int dlarz_ (char* integer* integer* integer*
				   doublereal* integer* doublereal* doublereal*
				   integer* doublereal*)))

  (dlarzb_	(int dlarzb_ (char* char* char* char*
				    integer* integer* integer* integer* doublereal*
				    integer* doublereal* integer* doublereal* integer*
				    doublereal* integer*)))

  (dlarzt_	(int dlarzt_ (char* char* integer* integer*
				    doublereal* integer* doublereal* doublereal*
				    integer*)))

  (dlas2_	(int dlas2_ (doublereal* doublereal* doublereal*
					 doublereal* doublereal*)))

  (dlascl_	(int dlascl_ (char* integer* integer*
				     doublereal* doublereal* integer* integer*
				     doublereal* integer* integer*)))

  (dlasd0_	(int dlasd0_ (integer* integer* doublereal*
				       doublereal* doublereal* integer* doublereal* integer*
				       integer* integer* doublereal* integer*
				       )))

  (dlasd1_	(int dlasd1_ (integer* integer* integer*
				       doublereal* doublereal* doublereal* doublereal*
				       integer* doublereal* integer* integer* integer*
				       doublereal* integer*)))

  (dlasd2_	(int dlasd2_ (integer* integer* integer* integer* doublereal* doublereal*
				       doublereal* doublereal*
				       doublereal* integer* doublereal* integer*
				       doublereal* doublereal* integer* doublereal*
				       integer* integer* integer* integer* integer*
				       integer* integer*)))

  (dlasd3_	(int dlasd3_ (integer* integer* integer* integer* doublereal* doublereal*
				       integer* doublereal*
				       doublereal* integer* doublereal* integer*
				       doublereal* integer* doublereal* integer*
				       integer* integer* doublereal* integer*)))

  (dlasd4_	(int dlasd4_ (integer* integer* doublereal*
				       doublereal* doublereal* doublereal* doublereal*
				       doublereal* integer*)))

  (dlasd5_	(int dlasd5_ (integer* doublereal* doublereal*
					doublereal* doublereal* doublereal* doublereal*)))

  (dlasd6_	(int dlasd6_ (integer* integer* integer*
				       integer* doublereal* doublereal* doublereal*
				       doublereal* doublereal* integer* integer*
				       integer* integer* integer* doublereal*
				       integer* doublereal* doublereal* doublereal*
				       doublereal* integer* doublereal* doublereal*
				       doublereal* integer* integer*)))

  (dlasd7_	(int dlasd7_ (integer* integer* integer*
				       integer* integer* doublereal* doublereal*
				       doublereal* doublereal* doublereal* doublereal*
				       doublereal* doublereal* doublereal* doublereal*
				       integer* integer* integer* integer*
				       integer* integer* integer* doublereal*
				       integer* doublereal* doublereal* integer*)))

  (dlasd8_	(int dlasd8_ (integer* integer* doublereal*
				       doublereal* doublereal* doublereal* doublereal*
				       doublereal* integer* doublereal* doublereal*
				       integer*)))

  (dlasd9_	(int dlasd9_ (integer* integer* integer*
				       doublereal* doublereal* doublereal* doublereal*
				       doublereal* doublereal* doublereal* doublereal*
				       integer*)))

  (dlasda_	(int dlasda_ (integer* integer* integer*
				       integer* doublereal* doublereal* doublereal* integer*
				       doublereal* integer* doublereal* doublereal*
				       doublereal* doublereal* integer* integer*
				       integer* integer* doublereal* doublereal*
				       doublereal* doublereal* integer* integer*)))

  (dlasdq_	(int dlasdq_ (char* integer* integer* integer*
				    integer* integer* doublereal* doublereal*
				    doublereal* integer* doublereal* integer*
				    doublereal* integer* doublereal* integer*)))

  (dlasdt_	(int dlasdt_ (integer* integer* integer* integer*
				       integer* integer* integer*)))

  (dlaset_	(int dlaset_ (char* integer* integer* doublereal*
				    doublereal* doublereal* integer*)))

  (dlasq1_	(int dlasq1_ (integer* doublereal* doublereal*
				       doublereal* integer*)))

  (dlasq2_	(int dlasq2_ (integer* doublereal* integer*)))

  (dlasq3_	(int dlasq3_ (integer* integer* doublereal*
					integer* doublereal* doublereal* doublereal*
					doublereal* integer* integer* integer*
					logical*)))

  (dlasq4_	(int dlasq4_ (integer* integer* doublereal*
					integer* integer* doublereal* doublereal*
					doublereal* doublereal* doublereal* doublereal*
					doublereal* integer*)))

  (dlasq5_	(int dlasq5_ (integer* integer* doublereal*
					integer* doublereal* doublereal* doublereal*
					doublereal* doublereal* doublereal* doublereal*
					logical*)))

  (dlasq6_	(int dlasq6_ (integer* integer* doublereal*
					integer* doublereal* doublereal* doublereal*
					doublereal* doublereal* doublereal*)))

  (dlasr_	(int dlasr_ (char* char* char* integer*
				   integer* doublereal* doublereal* doublereal* integer*)))

  (dlasrt_	(int dlasrt_ (char* integer* doublereal* integer*)))

  (dlassq_	(int dlassq_ (integer* doublereal* integer*
				       doublereal* doublereal*)))

  (dlasv2_	(int dlasv2_ (doublereal* doublereal* doublereal*
					  doublereal* doublereal* doublereal* doublereal*
					  doublereal* doublereal*)))

  (dlaswp_	(int dlaswp_ (integer* doublereal* integer* integer* integer* integer* integer*)))

  (dlasy2_	(int dlasy2_ (logical* logical* integer*
					integer* integer* doublereal* integer* doublereal*
					integer* doublereal* integer* doublereal*
					doublereal* integer* doublereal* integer*)))

  (dlasyf_	(int dlasyf_ (char* integer* integer* integer*
				    doublereal* integer* integer* doublereal* integer*
				    integer*)))

  (dlatbs_	(int dlatbs_ (char* char* char* char*
				    integer* integer* doublereal* integer*
				    doublereal* doublereal* doublereal* integer*)))

  (dlatdf_	(int dlatdf_ (integer* integer* doublereal*
				       integer* doublereal* doublereal* doublereal*
				       integer* integer*)))

  (dlatps_	(int dlatps_ (char* char* char* char*
				    integer* doublereal* doublereal* doublereal*
				    doublereal* integer*)))

  (dlatrd_	(int dlatrd_ (char* integer* integer* doublereal*
				    integer* doublereal* doublereal* doublereal*
				    integer*)))

  (dlatrs_	(int dlatrs_ (char* char* char* char*
				    integer* doublereal* integer* doublereal*
				    doublereal* doublereal* integer*)))

  (dlatrz_	(int dlatrz_ (integer* integer* integer* doublereal*
				       integer* doublereal* doublereal*)))

  (dlatzm_	(int dlatzm_ (char* integer* integer* doublereal*
				    integer* doublereal* doublereal* doublereal*
				    integer* doublereal*)))

  (dlauu2_	(int dlauu2_ (char* integer* doublereal* integer*
				    integer*)))

  (dlauum_	(int dlauum_ (char* integer* doublereal* integer*
				    integer*)))

  (dopgtr_	(int dopgtr_ (char* integer* doublereal*
				    doublereal* doublereal* integer* doublereal*
				    integer*)))

  (dopmtr_	(int dopmtr_ (char* char* char* integer*
				    integer* doublereal* doublereal* doublereal* integer*
				    doublereal* integer*)))

  (dorg2l_	(int dorg2l_ (integer* integer* integer* doublereal*
				       integer* doublereal* doublereal* integer*)))

  (dorg2r_	(int dorg2r_ (integer* integer* integer* doublereal*
				       integer* doublereal* doublereal* integer*)))

  (dorgbr_	(int dorgbr_ (char* integer* integer* integer*
				    doublereal* integer* doublereal* doublereal*
				    integer* integer*)))

  (dorghr_	(int dorghr_ (integer* integer* integer*
				       doublereal* integer* doublereal* doublereal*
				       integer* integer*)))

  (dorgl2_	(int dorgl2_ (integer* integer* integer* doublereal*
				       integer* doublereal* doublereal* integer*)))

  (dorglq_	(int dorglq_ (integer* integer* integer* doublereal*
				       integer* doublereal* doublereal* integer*
				       integer*)))

  (dorgql_	(int dorgql_ (integer* integer* integer* doublereal*
				       integer* doublereal* doublereal* integer*
				       integer*)))

  (dorgqr_	(int dorgqr_ (integer* integer* integer* doublereal*
				       integer* doublereal* doublereal* integer*
				       integer*)))

  (dorgr2_	(int dorgr2_ (integer* integer* integer* doublereal*
				       integer* doublereal* doublereal* integer*)))

  (dorgrq_	(int dorgrq_ (integer* integer* integer* doublereal*
				       integer* doublereal* doublereal* integer*
				       integer*)))

  (dorgtr_	(int dorgtr_ (char* integer* doublereal* integer*
				    doublereal* doublereal* integer* integer*)))

  (dorm2l_	(int dorm2l_ (char* char* integer* integer*
				    integer* doublereal* integer* doublereal* doublereal*
				    integer* doublereal* integer*)))

  (dorm2r_	(int dorm2r_ (char* char* integer* integer*
				    integer* doublereal* integer* doublereal* doublereal*
				    integer* doublereal* integer*)))

  (dormbr_	(int dormbr_ (char* char* char* integer*
				    integer* integer* doublereal* integer* doublereal*
				    doublereal* integer* doublereal* integer*
				    integer*)))

  (dormhr_	(int dormhr_ (char* char* integer* integer*
				    integer* integer* doublereal* integer* doublereal*
				    doublereal* integer* doublereal* integer*
				    integer*)))

  (dorml2_	(int dorml2_ (char* char* integer* integer*
				    integer* doublereal* integer* doublereal* doublereal*
				    integer* doublereal* integer*)))

  (dormlq_	(int dormlq_ (char* char* integer* integer*
				    integer* doublereal* integer* doublereal* doublereal*
				    integer* doublereal* integer* integer*)))

  (dormql_	(int dormql_ (char* char* integer* integer*
				    integer* doublereal* integer* doublereal* doublereal*
				    integer* doublereal* integer* integer*)))

  (dormqr_	(int dormqr_ (char* char* integer* integer*
				    integer* doublereal* integer* doublereal* doublereal*
				    integer* doublereal* integer* integer*)))

  (dormr2_	(int dormr2_ (char* char* integer* integer*
				    integer* doublereal* integer* doublereal* doublereal*
				    integer* doublereal* integer*)))

  (dormr3_	(int dormr3_ (char* char* integer* integer*
				    integer* integer* doublereal* integer* doublereal*
				    doublereal* integer* doublereal* integer*)))

  (dormrq_	(int dormrq_ (char* char* integer* integer*
				    integer* doublereal* integer* doublereal* doublereal*
				    integer* doublereal* integer* integer*)))

  (dormrz_	(int dormrz_ (char* char* integer* integer*
				    integer* integer* doublereal* integer* doublereal*
				    doublereal* integer* doublereal* integer*
				    integer*)))

  (dormtr_	(int dormtr_ (char* char* char* integer*
				    integer* doublereal* integer* doublereal* doublereal*
				    integer* doublereal* integer* integer*)))

  (dpbcon_	(int dpbcon_ (char* integer* integer* doublereal*
				    integer* doublereal* doublereal* doublereal*
				    integer* integer*)))

  (dpbequ_	(int dpbequ_ (char* integer* integer* doublereal*
				    integer* doublereal* doublereal* doublereal*
				    integer*)))

  (dpbrfs_	(int dpbrfs_ (char* integer* integer* integer*
				    doublereal* integer* doublereal* integer*
				    doublereal* integer* doublereal* integer* doublereal*
				    doublereal* doublereal* integer* integer*
				    )))

  (dpbstf_	(int dpbstf_ (char* integer* integer* doublereal*
				    integer* integer*)))

  (dpbsv_	(int dpbsv_ (char* integer* integer* integer*
				   doublereal* integer* doublereal* integer*
				   integer*)))

  (dpbsvx_	(int dpbsvx_ (char* char* integer* integer*
				    integer* doublereal* integer* doublereal*
				    integer* char* doublereal* doublereal* integer*
				    doublereal* integer* doublereal* doublereal*
				    doublereal* doublereal* integer* integer*)))

  (dpbtf2_	(int dpbtf2_ (char* integer* integer* doublereal*
				    integer* integer*)))

  (dpbtrf_	(int dpbtrf_ (char* integer* integer* doublereal*
				    integer* integer*)))

  (dpbtrs_	(int dpbtrs_ (char* integer* integer* integer*
				    doublereal* integer* doublereal* integer*
				    integer*)))

  (dpocon_	(int dpocon_ (char* integer* doublereal* integer*
				    doublereal* doublereal* doublereal* integer*
				    integer*)))

  (dpoequ_	(int dpoequ_ (integer* doublereal* integer*
				       doublereal* doublereal* doublereal* integer*)))

  (dporfs_	(int dporfs_ (char* integer* integer*
				    doublereal* integer* doublereal* integer*
				    doublereal* integer* doublereal* integer* doublereal*
				    doublereal* doublereal* integer* integer*
				    )))

  (dposv_	(int dposv_ (char* integer* integer* doublereal* integer* doublereal* integer* integer*)))

  (dposvx_	(int dposvx_ (char* char* integer* integer*
				    doublereal* integer* doublereal* integer*
				    char* doublereal* doublereal* integer* doublereal*
				    integer* doublereal* doublereal* doublereal*
				    doublereal* integer* integer*)))

  (dpotf2_	(int dpotf2_ (char* integer* doublereal* integer*
				    integer*)))

  (dpotrf_	(int dpotrf_ (char* integer* doublereal* integer*
				    integer*)))

  (dpotri_	(int dpotri_ (char* integer* doublereal* integer*
				    integer*)))

  (dpotrs_	(int dpotrs_ (char* integer* integer*
				    doublereal* integer* doublereal* integer* integer*
				    )))

  (dppcon_	(int dppcon_ (char* integer* doublereal*
				    doublereal* doublereal* doublereal* integer*
				    integer*)))

  (dppequ_	(int dppequ_ (char* integer* doublereal*
				    doublereal* doublereal* doublereal* integer*)))

  (dpprfs_	(int dpprfs_ (char* integer* integer*
				    doublereal* doublereal* doublereal* integer*
				    doublereal* integer* doublereal* doublereal*
				    doublereal* integer* integer*)))

  (dppsv_	(int dppsv_ (char* integer* integer* doublereal* doublereal* integer* integer*)))

  (dppsvx_	(int dppsvx_ (char* char* integer* integer*
				    doublereal* doublereal* char* doublereal*
				    doublereal* integer* doublereal* integer* doublereal*
				    doublereal* doublereal* doublereal* integer*
				    integer*)))

  (dpptrf_	(int dpptrf_ (char* integer* doublereal* integer*
				    )))

  (dpptri_	(int dpptri_ (char* integer* doublereal* integer*
				    )))

  (dpptrs_	(int dpptrs_ (char* integer* integer*
				    doublereal* doublereal* integer* integer*)))

  (dptcon_	(int dptcon_ (integer* doublereal* doublereal*
				       doublereal* doublereal* doublereal* integer*)))

  (dpteqr_	(int dpteqr_ (char* integer* doublereal*
				    doublereal* doublereal* integer* doublereal*
				    integer*)))

  (dptrfs_	(int dptrfs_ (integer* integer* doublereal*
				       doublereal* doublereal* doublereal* doublereal*
				       integer* doublereal* integer* doublereal* doublereal*
				       doublereal* integer*)))

  (dptsv_	(int dptsv_ (integer* integer* doublereal*
				      doublereal* doublereal* integer* integer*)))

  (dptsvx_	(int dptsvx_ (char* integer* integer*
				    doublereal* doublereal* doublereal* doublereal*
				    doublereal* integer* doublereal* integer* doublereal*
				    doublereal* doublereal* doublereal* integer*
				    )))

  (dpttrf_	(int dpttrf_ (integer* doublereal* doublereal*
				       integer*)))

  (dpttrs_	(int dpttrs_ (integer* integer* doublereal*
				       doublereal* doublereal* integer* integer*)))

  (dptts2_	(int dptts2_ (integer* integer* doublereal*
				       doublereal* doublereal* integer*)))

  (drscl_	(int drscl_ (integer* doublereal* doublereal*
				      integer*)))

  (dsbev_	(int dsbev_ (char* char* integer* integer*
				   doublereal* integer* doublereal* doublereal*
				   integer* doublereal* integer*)))

  (dsbevd_	(int dsbevd_ (char* char* integer* integer*
				    doublereal* integer* doublereal* doublereal*
				    integer* doublereal* integer* integer*
				    integer* integer*)))

  (dsbevx_	(int dsbevx_ (char* char* char* integer*
				    integer* doublereal* integer* doublereal* integer*
				    doublereal* doublereal* integer* integer*
				    doublereal* integer* doublereal* doublereal*
				    integer* doublereal* integer* integer*
				    integer*)))

  (dsbgst_	(int dsbgst_ (char* char* integer* integer*
				    integer* doublereal* integer* doublereal* integer*
				    doublereal* integer* doublereal* integer*)))

  (dsbgv_	(int dsbgv_ (char* char* integer* integer*
				   integer* doublereal* integer* doublereal* integer*
				   doublereal* doublereal* integer* doublereal*
				   integer*)))

  (dsbgvd_	(int dsbgvd_ (char* char* integer* integer*
				    integer* doublereal* integer* doublereal* integer*
				    doublereal* doublereal* integer* doublereal*
				    integer* integer* integer* integer*)))

  (dsbgvx_	(int dsbgvx_ (char* char* char* integer*
				    integer* integer* doublereal* integer* doublereal*
				    integer* doublereal* integer* doublereal*
				    doublereal* integer* integer* doublereal* integer*
				    doublereal* doublereal* integer* doublereal*
				    integer* integer* integer*)))

  (dsbtrd_	(int dsbtrd_ (char* char* integer* integer*
				    doublereal* integer* doublereal* doublereal*
				    doublereal* integer* doublereal* integer*)))

  (dspcon_	(int dspcon_ (char* integer* doublereal* integer*
				    doublereal* doublereal* doublereal* integer* integer*)))

  (dspev_	(int dspev_ (char* char* integer* doublereal*
				   doublereal* doublereal* integer* doublereal*
				   integer*)))

  (dspevd_	(int dspevd_ (char* char* integer* doublereal*
				    doublereal* doublereal* integer* doublereal*
				    integer* integer* integer* integer*)))

  (dspevx_	(int dspevx_ (char* char* char* integer*
				    doublereal* doublereal* doublereal* integer* integer*
				    doublereal* integer* doublereal* doublereal*
				    integer* doublereal* integer* integer*
				    integer*)))

  (dspgst_	(int dspgst_ (integer* char* integer*
				       doublereal* doublereal* integer*)))

  (dspgv_	(int dspgv_ (integer* char* char* integer*
				      doublereal* doublereal* doublereal* doublereal*
				      integer* doublereal* integer*)))

  (dspgvd_	(int dspgvd_ (integer* char* char* integer*
				       doublereal* doublereal* doublereal* doublereal*
				       integer* doublereal* integer* integer*
				       integer* integer*)))

  (dspgvx_	(int dspgvx_ (integer* char* char* char*
				       integer* doublereal* doublereal* doublereal*
				       doublereal* integer* integer* doublereal* integer* doublereal*
				       doublereal* integer* doublereal*
				       integer* integer* integer*)))

  (dsprfs_	(int dsprfs_ (char* integer* integer*
				    doublereal* doublereal* integer* doublereal*
				    integer* doublereal* integer* doublereal*
				    doublereal* doublereal* integer* integer*)))

  (dspsv_	(int dspsv_ (char* integer* integer* doublereal* integer* doublereal* integer* integer*)))

  (dspsvx_	(int dspsvx_ (char* char* integer* integer*
				    doublereal* doublereal* integer* doublereal*
				    integer* doublereal* integer* doublereal*
				    doublereal* doublereal* doublereal* integer*
				    integer*)))

  (dsptrd_	(int dsptrd_ (char* integer* doublereal*
				    doublereal* doublereal* doublereal* integer*)))

  (dsptrf_	(int dsptrf_ (char* integer* doublereal* integer*
				    integer*)))

  (dsptri_	(int dsptri_ (char* integer* doublereal* integer*
				    doublereal* integer*)))

  (dsptrs_	(int dsptrs_ (char* integer* integer*
				    doublereal* integer* doublereal* integer* integer*
				    )))

  (dstebz_	(int dstebz_ (char* char* integer* doublereal* doublereal* integer* integer* doublereal*
				    doublereal* doublereal* integer* integer*
				    doublereal* integer* integer* doublereal*
				    integer* integer*)))

  (dstedc_	(int dstedc_ (char* integer* doublereal*
				    doublereal* doublereal* integer* doublereal*
				    integer* integer* integer* integer*)))

  (dstegr_	(int dstegr_ (char* char* integer* doublereal*
				    doublereal* doublereal* doublereal* integer*
				    integer* doublereal* integer* doublereal*
				    doublereal* integer* integer* doublereal*
				    integer* integer* integer* integer*)))

  (dstein_	(int dstein_ (integer* doublereal* doublereal*
				       integer* doublereal* integer* integer*
				       doublereal* integer* doublereal* integer*
				       integer* integer*)))

  (dsteqr_	(int dsteqr_ (char* integer* doublereal*
				    doublereal* doublereal* integer* doublereal*
				    integer*)))

  (dsterf_	(int dsterf_ (integer* doublereal* doublereal*
				       integer*)))

  (dstev_	(int dstev_ (char* integer* doublereal*
				   doublereal* doublereal* integer* doublereal*
				   integer*)))

  (dstevd_	(int dstevd_ (char* integer* doublereal*
				    doublereal* doublereal* integer* doublereal*
				    integer* integer* integer* integer*)))

  (dstevr_	(int dstevr_ (char* char* integer* doublereal*
				    doublereal* doublereal* doublereal* integer*
				    integer* doublereal* integer* doublereal*
				    doublereal* integer* integer* doublereal*
				    integer* integer* integer* integer*)))

  (dstevx_	(int dstevx_ (char* char* integer* doublereal*
				    doublereal* doublereal* doublereal* integer*
				    integer* doublereal* integer* doublereal*
				    doublereal* integer* doublereal* integer*
				    integer* integer*)))

  (dsycon_	(int dsycon_ (char* integer* doublereal* integer*
				    integer* doublereal* doublereal* doublereal*
				    integer* integer*)))

  (dsyev_	(int dsyev_ (char* char* integer* doublereal*
				   integer* doublereal* doublereal* integer*
				   integer*)))

  (dsyevd_	(int dsyevd_ (char* char* integer* doublereal*
				    integer* doublereal* doublereal* integer*
				    integer* integer* integer*)))

  (dsyevr_	(int dsyevr_ (char* char* char* integer*
				    doublereal* integer* doublereal* doublereal* integer*
				    integer* doublereal* integer* doublereal*
				    doublereal* integer* integer* doublereal*
				    integer* integer* integer* integer*)))

  (dsyevx_	(int dsyevx_ (char* char* char* integer*
				    doublereal* integer* doublereal* doublereal* integer*
				    integer* doublereal* integer* doublereal*
				    doublereal* integer* doublereal* integer*
				    integer* integer* integer*)))

  (dsygs2_	(int dsygs2_ (integer* char* integer*
				       doublereal* integer* doublereal* integer* integer*
				       )))

  (dsygst_	(int dsygst_ (integer* char* integer*
				       doublereal* integer* doublereal* integer* integer*
				       )))

  (dsygv_	(int dsygv_ (integer* char* char* integer*
				      doublereal* integer* doublereal* integer*
				      doublereal* doublereal* integer* integer*)))

  (dsygvd_	(int dsygvd_ (integer* char* char* integer*
				       doublereal* integer* doublereal* integer*
				       doublereal* doublereal* integer* integer*
				       integer* integer*)))

  (dsygvx_	(int dsygvx_ (integer* char* char* char*
				       integer* doublereal* integer* doublereal* integer*
				       doublereal* doublereal* integer* integer*
				       doublereal* integer* doublereal* doublereal*
				       integer* doublereal* integer* integer*
				       integer* integer*)))

  (dsyrfs_	(int dsyrfs_ (char* integer* integer*
				    doublereal* integer* doublereal* integer* integer*
				    doublereal* integer* doublereal* integer*
				    doublereal* doublereal* doublereal* integer*
				    integer*)))

  (dsysv_	(int dsysv_ (char* integer* integer* doublereal* integer* integer* doublereal* integer*
				   doublereal* integer* integer*)))

  (dsysvx_	(int dsysvx_ (char* char* integer* integer*
				    doublereal* integer* doublereal* integer*
				    integer* doublereal* integer* doublereal* integer*
				    doublereal* doublereal* doublereal*
				    doublereal* integer* integer* integer*)))

  (dsytd2_	(int dsytd2_ (char* integer* doublereal* integer*
				    doublereal* doublereal* doublereal* integer*)))

  (dsytf2_	(int dsytf2_ (char* integer* doublereal* integer*
				    integer* integer*)))

  (dsytrd_	(int dsytrd_ (char* integer* doublereal* integer*
				    doublereal* doublereal* doublereal* doublereal*
				    integer* integer*)))

  (dsytrf_	(int dsytrf_ (char* integer* doublereal* integer*
				    integer* doublereal* integer* integer*)))

  (dsytri_	(int dsytri_ (char* integer* doublereal* integer*
				    integer* doublereal* integer*)))

  (dsytrs_	(int dsytrs_ (char* integer* integer*
				    doublereal* integer* integer* doublereal* integer*
				    integer*)))

  (dtbcon_	(int dtbcon_ (char* char* char* integer*
				    integer* doublereal* integer* doublereal*
				    doublereal* integer* integer*)))

  (dtbrfs_	(int dtbrfs_ (char* char* char* integer*
				    integer* integer* doublereal* integer* doublereal*
				    integer* doublereal* integer* doublereal*
				    doublereal* doublereal* integer* integer*)))

  (dtbtrs_	(int dtbtrs_ (char* char* char* integer*
				    integer* integer* doublereal* integer* doublereal*
				    integer* integer*)))

  (dtgevc_	(int dtgevc_ (char* char* logical*
				    integer* doublereal* integer* doublereal* integer*
				    doublereal* integer* doublereal* integer* integer*
				    integer* doublereal* integer*)))

  (dtgex2_	(int dtgex2_ (logical* logical* integer*
					doublereal* integer* doublereal* integer* doublereal*
					integer* doublereal* integer* integer* integer*
					integer* doublereal* integer* integer*)))

  (dtgexc_	(int dtgexc_ (logical* logical* integer*
					doublereal* integer* doublereal* integer* doublereal*
					integer* doublereal* integer* integer*
					integer* doublereal* integer* integer*)))

  (dtgsen_	(int dtgsen_ (integer* logical* logical*
				       logical* integer* doublereal* integer* doublereal*
				       integer* doublereal* doublereal* doublereal*
				       doublereal* integer* doublereal* integer*
				       integer* doublereal* doublereal* doublereal*
				       doublereal* integer* integer* integer*
				       integer*)))

  (dtgsja_	(int dtgsja_ (char* char* char* integer*
				    integer* integer* integer* integer* doublereal*
				    integer* doublereal* integer* doublereal*
				    doublereal* doublereal* doublereal* doublereal*
				    integer* doublereal* integer* doublereal* integer*
				    doublereal* integer* integer*)))

  (dtgsna_	(int dtgsna_ (char* char* logical*
				    integer* doublereal* integer* doublereal* integer*
				    doublereal* integer* doublereal* integer*
				    doublereal* doublereal* integer* integer* doublereal*
				    integer* integer* integer*)))

  (dtgsy2_	(int dtgsy2_ (char* integer* integer* integer*
				    doublereal* integer* doublereal* integer*
				    doublereal* integer* doublereal* integer*
				    doublereal* integer* doublereal* integer* doublereal*
				    doublereal* doublereal* integer* integer* integer*)))

  (dtgsyl_	(int dtgsyl_ (char* integer* integer* integer*
				    doublereal* integer* doublereal* integer*
				    doublereal* integer* doublereal* integer*
				    doublereal* integer* doublereal* integer* doublereal*
				    doublereal* doublereal* integer* integer*
				    integer*)))

  (dtpcon_	(int dtpcon_ (char* char* char* integer*
				    doublereal* doublereal* doublereal* integer*
				    integer*)))

  (dtprfs_	(int dtprfs_ (char* char* char* integer*
				    integer* doublereal* doublereal* integer*
				    doublereal* integer* doublereal* doublereal*
				    doublereal* integer* integer*)))

  (dtptri_	(int dtptri_ (char* char* integer* doublereal*
				    integer*)))

  (dtptrs_	(int dtptrs_ (char* char* char* integer*
				    integer* doublereal* doublereal* integer* integer*
				    )))

  (dtrcon_	(int dtrcon_ (char* char* char* integer*
				    doublereal* integer* doublereal* doublereal*
				    integer* integer*)))

  (dtrevc_	(int dtrevc_ (char* char* logical*
				    integer* doublereal* integer* doublereal* integer*
				    doublereal* integer* integer* integer*
				    doublereal* integer*)))

  (dtrexc_	(int dtrexc_ (char* integer* doublereal* integer*
				    doublereal* integer* integer* integer*
				    doublereal* integer*)))

  (dtrrfs_	(int dtrrfs_ (char* char* char* integer*
				    integer* doublereal* integer* doublereal* integer*
				    doublereal* integer* doublereal* doublereal*
				    doublereal* integer* integer*)))

  (dtrsen_	(int dtrsen_ (char* char* logical* integer* doublereal* integer* doublereal* integer*
				    doublereal* doublereal* integer* doublereal* doublereal*
				    doublereal* integer* integer* integer*
				    integer*)))

  (dtrsna_	(int dtrsna_ (char* char* logical*
				    integer* doublereal* integer* doublereal* integer*
				    doublereal* integer* doublereal* doublereal*
				    integer* integer* doublereal* integer* integer*
				    integer*)))

  (dtrsyl_	(int dtrsyl_ (char* char* integer* integer* integer* doublereal* integer*
				    doublereal* integer*
				    doublereal* integer* doublereal* integer*)))

  (dtrti2_	(int dtrti2_ (char* char* integer* doublereal*
				    integer* integer*)))

  (dtrtri_	(int dtrtri_ (char* char* integer* doublereal*
				    integer* integer*)))

  (dtrtrs_	(int dtrtrs_ (char* char* char* integer*
				    integer* doublereal* integer* doublereal* integer*
				    integer*)))

  (dtzrqf_	(int dtzrqf_ (integer* integer* doublereal* integer*
				       doublereal* integer*)))

  (dtzrzf_	(int dtzrzf_ (integer* integer* doublereal* integer*
				       doublereal* doublereal* integer* integer*)))

  (icmax1_	(integer icmax1_ (integer* complex* integer*)))

  (ieeeck_	(integer ieeeck_ (integer* real* real*)))

  (ilaenv_	(integer ilaenv_ (integer* char* char* integer*
					   integer* integer* integer*
					   ftnlen ftnlen)))

  (izmax1_	(integer izmax1_ (integer* doublecomplex* integer*)))

  (sbdsdc_	(int sbdsdc_ (char* char* integer* real*
				    real* real* integer* real* integer* real*
				    integer* real* integer* integer*)))

  (sbdsqr_	(int sbdsqr_ (char* integer* integer* integer*
				    integer* real* real* real* integer* real*
				    integer* real* integer* real* integer*)))

  (sdisna_	(int sdisna_ (char* integer* integer* real*
				    real* integer*)))

  (sgbbrd_	(int sgbbrd_ (char* integer* integer* integer*
				    integer* integer* real* integer* real* real*
				    real* integer* real* integer* real* integer* real* integer*)))

  (sgbcon_	(int sgbcon_ (char* integer* integer* integer*
				    real* integer* integer* real* real*
				    real* integer* integer*)))

  (sgbequ_	(int sgbequ_ (integer* integer* integer* integer*
				       real* integer* real* real* real* real*
				       real* integer*)))

  (sgbrfs_	(int sgbrfs_ (char* integer* integer* integer*
				    integer* real* integer* real* integer*
				    integer* real* integer* real* integer* real*
				    real* real* integer* integer*)))

  (sgbsv_	(int sgbsv_ (integer* integer* integer* integer*
				      real* integer* integer* real* integer*
				      integer*)))

  (sgbsvx_	(int sgbsvx_ (char* char* integer* integer*
				    integer* integer* real* integer* real*
				    integer* integer* char* real* real*
				    real* integer* real* integer* real* real*
				    real* real* integer* integer*)))

  (sgbtf2_	(int sgbtf2_ (integer* integer* integer* integer*
				       real* integer* integer* integer*)))

  (sgbtrf_	(int sgbtrf_ (integer* integer* integer* integer*
				       real* integer* integer* integer*)))

  (sgbtrs_	(int sgbtrs_ (char* integer* integer* integer*
				    integer* real* integer* integer* real*
				    integer* integer*)))

  (sgebak_	(int sgebak_ (char* char* integer* integer*
				    integer* real* integer* real* integer* integer*)))

  (sgebal_	(int sgebal_ (char* integer* real* integer*
				    integer* integer* real* integer*)))

  (sgebd2_	(int sgebd2_ (integer* integer* real* integer*
				       real* real* real* real* real* integer*)))

  (sgebrd_	(int sgebrd_ (integer* integer* real* integer*
				       real* real* real* real* real* integer*
				       integer*)))

  (sgecon_	(int sgecon_ (char* integer* real* integer*
				    real* real* real* integer* integer*)))

  (sgeequ_	(int sgeequ_ (integer* integer* real* integer*
				       real* real* real* real* real* integer*)))

  (sgees_	(int sgees_ (char* char* L_fp integer*
				   real* integer* integer* real* real* real*
				   integer* real* integer* logical* integer*
				   )))

  (sgeesx_	(int sgeesx_ (char* char* L_fp char*
				    integer* real* integer* integer* real*
				    real* real* integer* real* real* real*
				    integer* integer* integer* logical*
				    integer*)))

  (sgeev_	(int sgeev_ (char* char* integer* real*
				   integer* real* real* real* integer* real*
				   integer* real* integer* integer*)))

  (sgeevx_	(int sgeevx_ (char* char* char* char*
				    integer* real* integer* real* real* real*
				    integer* real* integer* integer* integer*
				    real* real* real* real* real*
				    integer* integer* integer*)))

  (sgegs_	(int sgegs_ (char* char* integer* real*
				   integer* real* integer* real* real* real* real*
				   integer* real* integer* real*
				   integer* integer*)))

  (sgegv_	(int sgegv_ (char* char* integer* real*
				   integer* real* integer* real* real* real* real* integer*
				   real* integer* real*
				   integer* integer*)))

  (sgehd2_	(int sgehd2_ (integer* integer* integer* real*
				       integer* real* real* integer*)))

  (sgehrd_	(int sgehrd_ (integer* integer* integer* real*
				       integer* real* real* integer* integer*)))

  (sgelq2_	(int sgelq2_ (integer* integer* real* integer*
				       real* real* integer*)))

  (sgelqf_	(int sgelqf_ (integer* integer* real* integer*
				       real* real* integer* integer*)))

  (sgels_	(int sgels_ (char* integer* integer* integer*
				   real* integer* real* integer* real*
				   integer* integer*)))

  (sgelsd_	(int sgelsd_ (integer* integer* integer* real*
				       integer* real* integer* real* real* integer*
				       real* integer* integer* integer*)))

  (sgelss_	(int sgelss_ (integer* integer* integer* real*
				       integer* real* integer* real* real* integer*
				       real* integer* integer*)))

  (sgelsx_	(int sgelsx_ (integer* integer* integer* real*
				       integer* real* integer* integer* real*
				       integer* real* integer*)))

  (sgelsy_	(int sgelsy_ (integer* integer* integer* real*
				       integer* real* integer* integer* real*
				       integer* real* integer* integer*)))

  (sgeql2_	(int sgeql2_ (integer* integer* real* integer*
				       real* real* integer*)))

  (sgeqlf_	(int sgeqlf_ (integer* integer* real* integer*
				       real* real* integer* integer*)))

  (sgeqp3_	(int sgeqp3_ (integer* integer* real* integer*
				       integer* real* real* integer* integer*)))

  (sgeqpf_	(int sgeqpf_ (integer* integer* real* integer*
				       integer* real* real* integer*)))

  (sgeqr2_	(int sgeqr2_ (integer* integer* real* integer*
				       real* real* integer*)))

  (sgeqrf_	(int sgeqrf_ (integer* integer* real* integer*
				       real* real* integer* integer*)))

  (sgerfs_	(int sgerfs_ (char* integer* integer* real*
				    integer* real* integer* integer* real*
				    integer* real* integer* real* real* real*
				    integer* integer*)))

  (sgerq2_	(int sgerq2_ (integer* integer* real* integer*
				       real* real* integer*)))

  (sgerqf_	(int sgerqf_ (integer* integer* real* integer*
				       real* real* integer* integer*)))

  (sgesc2_	(int sgesc2_ (integer* real* integer* real*
				       integer* integer* real*)))

  (sgesdd_	(int sgesdd_ (char* integer* integer* real*
				    integer* real* real* integer* real* integer*
				    real* integer* integer* integer*)))

  (sgesv_	(int sgesv_ (integer* integer* real* integer*
				      integer* real* integer* integer*)))

  (sgesvd_	(int sgesvd_ (char* char* integer* integer*
				    real* integer* real* real* integer* real*
				    integer* real* integer* integer*)))

  (sgesvx_	(int sgesvx_ (char* char* integer* integer*
				    real* integer* real* integer* integer*
				    char* real* real* real* integer* real*
				    integer* real* real* real* real*
				    integer* integer*)))

  (sgetc2_	(int sgetc2_ (integer* real* integer* integer*
				       integer* integer*)))

  (sgetf2_	(int sgetf2_ (integer* integer* real* integer*
				       integer* integer*)))

  (sgetrf_	(int sgetrf_ (integer* integer* real* integer*
				       integer* integer*)))

  (sgetri_	(int sgetri_ (integer* real* integer* integer*
				       real* integer* integer*)))

  (sgetrs_	(int sgetrs_ (char* integer* integer* real*
				    integer* integer* real* integer* integer*)))

  (sggbak_	(int sggbak_ (char* char* integer* integer*
				    integer* real* real* integer* real*
				    integer* integer*)))

  (sggbal_	(int sggbal_ (char* integer* real* integer*
				    real* integer* integer* integer* real* real* real* integer*)))

  (sgges_	(int sgges_ (char* char* char* L_fp
				   integer* real* integer* real* integer*
				   integer* real* real* real* real*
				   integer* real* integer* real* integer*
				   logical* integer*)))

  (sggesx_	(int sggesx_ (char* char* char* L_fp
				    char* integer* real* integer* real*
				    integer* integer* real* real* real*
				    real* integer* real* integer* real*
				    real* real* integer* integer* integer*
				    logical* integer*)))

  (sggev_	(int sggev_ (char* char* integer* real*
				   integer* real* integer* real* real* real* real* integer*
				   real* integer* real*
				   integer* integer*)))

  (sggevx_	(int sggevx_ (char* char* char* char*
				    integer* real* integer* real* integer* real* real*
				    real* real* integer* real*
				    integer* integer* integer* real* real*
				    real* real* real* real* real*
				    integer* integer* logical* integer*)))

  (sggglm_	(int sggglm_ (integer* integer* integer* real*
				       integer* real* integer* real* real* real*
				       real* integer* integer*)))

  (sgghrd_	(int sgghrd_ (char* char* integer* integer*
				    integer* real* integer* real* integer* real* integer* real*
				    integer* integer*)))

  (sgglse_	(int sgglse_ (integer* integer* integer* real*
				       integer* real* integer* real* real* real*
				       real* integer* integer*)))

  (sggqrf_	(int sggqrf_ (integer* integer* integer* real*
				       integer* real* real* integer* real* real*
				       integer* integer*)))

  (sggrqf_	(int sggrqf_ (integer* integer* integer* real*
				       integer* real* real* integer* real* real*
				       integer* integer*)))

  (sggsvd_	(int sggsvd_ (char* char* char* integer*
				    integer* integer* integer* integer* real* integer*
				    real* integer* real* real* real* integer*
				    real* integer* real* integer* real*
				    integer* integer*)))

  (sggsvp_	(int sggsvp_ (char* char* char* integer*
				    integer* integer* real* integer* real* integer*
				    real* real* integer* integer* real* integer*
				    real* integer* real* integer* integer* real*
				    real* integer*)))

  (sgtcon_	(int sgtcon_ (char* integer* real* real*
				    real* real* integer* real* real* real*
				    integer* integer*)))

  (sgtrfs_	(int sgtrfs_ (char* integer* integer* real*
				    real* real* real* real* real* real*
				    integer* real* integer* real* integer* real*
				    real* real* integer* integer*)))

  (sgtsv_	(int sgtsv_ (integer* integer* real* real*
				      real* real* integer* integer*)))

  (sgtsvx_	(int sgtsvx_ (char* char* integer* integer*
				    real* real* real* real* real* real*
				    real* integer* real* integer* real* integer*
				    real* real* real* real* integer*
				    integer*)))

  (sgttrf_	(int sgttrf_ (integer* real* real* real* real*
				       integer* integer*)))

  (sgttrs_	(int sgttrs_ (char* integer* integer* real*
				    real* real* real* integer* real* integer*
				    integer*)))

  (sgtts2_	(int sgtts2_ (integer* integer* integer* real* real* real* real* integer* real* integer*
				      )))

  (shgeqz_	(int shgeqz_ (char* char* char* integer*
				    integer* integer* real* integer* real* integer*
				    real* real* real* real* integer*
				    real* integer* real* integer* integer*)))

  (shsein_	(int shsein_ (char* char* char* logical*
				    integer* real* integer* real* real* real* integer* real*
				    integer* integer* integer*
				    real* integer* integer* integer*)))

  (shseqr_	(int shseqr_ (char* char* integer* integer*
				    integer* real* integer* real* real* real*
				    integer* real* integer* integer*)))

  (slabad_	(int slabad_ (real* real*)))

  (slabrd_	(int slabrd_ (integer* integer* integer* real*
				       integer* real* real* real* real* real*
				       integer* real* integer*)))

  (slacon_	(int slacon_ (integer* real* real* integer*
				       real* integer*)))

  (slacpy_	(int slacpy_ (char* integer* integer* real*
				    integer* real* integer*)))

  (sladiv_	(int sladiv_ (real* real* real* real* real*
				    real*)))

  (slae2_	(int slae2_ (real* real* real* real* real*)))

  (slaebz_	(int slaebz_ (integer* integer* integer*
				       integer* integer* integer* real* real*
				       real* real* real* real* integer*
				       real* real* integer* integer* real* integer* integer*)))

  (slaed0_	(int slaed0_ (integer* integer* integer* real* real* real* integer* real* integer*
				       real* integer* integer*)))

  (slaed1_	(int slaed1_ (integer* real* real* integer*
				       integer* real* integer* real* integer*
				       integer*)))

  (slaed2_	(int slaed2_ (integer* integer* integer* real*
				       real* integer* integer* real* real* real*
				       real* real* integer* integer* integer*
				       integer* integer*)))

  (slaed3_	(int slaed3_ (integer* integer* integer* real*
				       real* integer* real* real* real* integer*
				       integer* real* real* integer*)))

  (slaed4_	(int slaed4_ (integer* integer* real* real*
				       real* real* real* integer*)))

  (slaed5_	(int slaed5_ (integer* real* real* real*
					real* real*)))

  (slaed6_	(int slaed6_ (integer* logical* real*
				       real* real* real* real* integer*)))

  (slaed7_	(int slaed7_ (integer* integer* integer*
				       integer* integer* integer* real* real*
				       integer* integer* real* integer* real*
				       integer* integer* integer* integer*
				       integer* real* real* integer*
				       integer*)))

  (slaed8_	(int slaed8_ (integer* integer* integer* integer* real* real* integer* integer* real*
				       integer* real* real* real* integer*
				       real* integer* integer* integer* real*
				       integer* integer* integer*)))

  (slaed9_	(int slaed9_ (integer* integer* integer*
				       integer* real* real* integer* real* real*
				       real* real* integer* integer*)))

  (slaeda_	(int slaeda_ (integer* integer* integer*
				       integer* integer* integer* integer*
				       integer* real* real* integer* real*
				       real* integer*)))

  (slaein_	(int slaein_ (logical* logical* integer*
					real* integer* real* real* real* real* real* integer*
					real* real* real* real*
					integer*)))

  (slaev2_	(int slaev2_ (real* real* real* real* real*
				    real* real*)))

  (slaexc_	(int slaexc_ (logical* integer* real* integer*
					real* integer* integer* integer* integer*
					real* integer*)))

  (slag2_	(int slag2_ (real* integer* real* integer*
				   real* real* real* real* real* real*
				  )))

  (slags2_	(int slags2_ (logical* real* real* real*
					real* real* real* real* real* real* real*
					real* real*)))

  (slagtf_	(int slagtf_ (integer* real* real* real* real* real* real* integer* integer*)))

  (slagtm_	(int slagtm_ (char* integer* integer* real*
				    real* real* real* real* integer* real*
				    real* integer*)))

  (slagts_	(int slagts_ (integer* integer* real* real* real* real* integer* real* real* integer*)))

  (slagv2_	(int slagv2_ (real* integer* real* integer*
				    real* real* real* real* real* real*
				    real*)))

  (slahqr_	(int slahqr_ (logical* logical* integer*
					integer* integer* real* integer* real* real*
					integer* integer* real* integer* integer*)))

  (slahrd_	(int slahrd_ (integer* integer* integer* real*
				       integer* real* real* integer* real* integer*)))

  (slaic1_	(int slaic1_ (integer* integer* real* real*
				       real* real* real* real* real*)))

  (slaln2_	(int slaln2_ (logical* integer* integer* real*
					real* real* integer* real* real* real*
					integer* real* real* real* integer* real*
					real* integer*)))

  (slals0_	(int slals0_ (integer* integer* integer*
				       integer* integer* real* integer* real*
				       integer* integer* integer* integer*
				       integer* real* integer* real* real*
				       real* real* integer* real* real* real*
				       integer*)))

  (slalsa_	(int slalsa_ (integer* integer* integer*
				       integer* real* integer* real* integer* real*
				       integer* real* integer* real* real* real*
				       real* integer* integer* integer*
				       integer* real* real* real* real* integer*
				       integer*)))

  (slalsd_	(int slalsd_ (char* integer* integer* integer* real* real* real* integer* real*
				    integer* real* integer* integer*)))

  (slamc1_	(int slamc1_ (integer* integer* logical* logical*)))

  (slamc2_	(int slamc2_ (integer* integer* logical* real*
				       integer* real* integer* real*)))

  (slamc4_	(int slamc4_ (integer* real* integer*)))

  (slamc5_	(int slamc5_ (integer* integer* integer*
				       logical* integer* real*)))

  (slamrg_	(int slamrg_ (integer* integer* real* integer*
					integer* integer*)))

  (slanv2_	(int slanv2_ (real* real* real* real* real*
				    real* real* real* real* real*)))

  (slapll_	(int slapll_ (integer* real* integer* real* integer* real*)))

  (slapmt_	(int slapmt_ (logical* integer* integer* real* integer* integer*)))

  (slaqgb_	(int slaqgb_ (integer* integer* integer* integer*
				       real* integer* real* real* real* real*
				       real* char*)))

  (slaqge_	(int slaqge_ (integer* integer* real* integer*
				       real* real* real* real* real* char*)))

  (slaqp2_	(int slaqp2_ (integer* integer* integer* real*
				       integer* integer* real* real* real* real*)))

  (slaqps_	(int slaqps_ (integer* integer* integer* integer* integer* real*
				       integer* integer* real*
				       real* real* real* real* integer*)))

  (slaqsb_	(int slaqsb_ (char* integer* integer* real*
				    integer* real* real* real* char*)))

  (slaqsp_	(int slaqsp_ (char* integer* real* real* real* real* char*)))

  (slaqsy_	(int slaqsy_ (char* integer* real* integer* real* real* real* char*)))

  (slaqtr_	(int slaqtr_ (logical* logical* integer* real* integer* real* real*
					real* real* real* integer*)))

  (slar1v_	(int slar1v_ (integer* integer* integer* real*
				       real* real* real* real* real* real*
				       real* real* integer* integer* real*)))

  (slar2v_	(int slar2v_ (integer* real* real* real* integer* real* real* integer*)))

  (slarf_	(int slarf_ (char* integer* integer* real* integer* real* real* integer* real*)))

  (slarfb_	(int slarfb_ (char* char* char* char*
				    integer* integer* integer* real* integer*
				    real* integer* real* integer* real* integer*)))

  (slarfg_	(int slarfg_ (integer* real* real* integer* real*)))

  (slarft_	(int slarft_ (char* char* integer* integer* real* integer* real* real* integer*)))

  (slarfx_	(int slarfx_ (char* integer* integer* real* real* real* integer* real*)))

  (slargv_	(int slargv_ (integer* real* integer* real* integer* real* integer*)))

  (slarnv_	(int slarnv_ (integer* integer* integer* real*)))

  (slarrb_	(int slarrb_ (integer* real* real* real* real*
				       integer* integer* real* real* real* real* real* real*
				       integer* integer*)))

  (slarre_	(int slarre_ (integer* real* real* real*
				       integer* integer* integer* real* real*
				       real* real* integer*)))

  (slarrf_	(int slarrf_ (integer* real* real* real* real*
				       integer* integer* real* real* real*
				       real* integer* integer*)))

  (slarrv_	(int slarrv_ (integer* real* real* integer*
				       integer* real* integer* real* real* real*
				       integer* integer* real* integer*
				       integer*)))

  (slartg_	(int slartg_ (real* real* real* real* real*)))

  (slartv_	(int slartv_ (integer* real* integer* real*
				       integer* real* real* integer*)))

  (slaruv_	(int slaruv_ (integer* integer* real*)))

  (slarz_	(int slarz_ (char* integer* integer* integer*
				   real* integer* real* real* integer* real*
				  )))

  (slarzb_	(int slarzb_ (char* char* char* char*
				    integer* integer* integer* integer* real*
				    integer* real* integer* real* integer* real*
				    integer*)))

  (slarzt_	(int slarzt_ (char* char* integer* integer*
				    real* integer* real* real* integer*)))

  (slas2_	(int slas2_ (real* real* real* real* real*
				  )))

  (slascl_	(int slascl_ (char* integer* integer* real*
				     real* integer* integer* real* integer*
				     integer*)))

  (slasd0_	(int slasd0_ (integer* integer* real* real*
				       real* integer* real* integer* integer*
				       integer* real* integer*)))

  (slasd1_	(int slasd1_ (integer* integer* integer* real*
				       real* real* real* integer* real*
				       integer* integer* integer* real* integer*
				       )))

  (slasd2_	(int slasd2_ (integer* integer* integer* integer* real* real* real* real* real* integer*
				       real* integer* real* real* integer*
				       real* integer* integer* integer* integer*
				       integer* integer* integer*)))

  (slasd3_	(int slasd3_ (integer* integer* integer* integer* real* real* integer* real* real* integer*
				       real* integer* real* integer* real*
				       integer* integer* integer* real* integer*
				       )))

  (slasd4_	(int slasd4_ (integer* integer* real* real*
				       real* real* real* real* integer*)))

  (slasd5_	(int slasd5_ (integer* real* real* real*
					real* real* real*)))

  (slasd6_	(int slasd6_ (integer* integer* integer*
				       integer* real* real* real* real* real*
				       integer* integer* integer* integer*
				       integer* real* integer* real* real*
				       real* real* integer* real* real* real*
				       integer* integer*)))

  (slasd7_	(int slasd7_ (integer* integer* integer*
				       integer* integer* real* real* real* real*
				       real* real* real* real* real* real*
				       integer* integer* integer* integer* integer*
				       integer* integer* real* integer*
				       real* real* integer*)))

  (slasd8_	(int slasd8_ (integer* integer* real* real*
				       real* real* real* real* integer*
				       real* real* integer*)))

  (slasd9_	(int slasd9_ (integer* integer* integer* real*
				       real* real* real* real* real* real*
				       real* integer*)))

  (slasda_	(int slasda_ (integer* integer* integer*
				       integer* real* real* real* integer* real*
				       integer* real* real* real* real* integer*
				       integer* integer* integer* real*
				       real* real* real* integer* integer*)))

  (slasdq_	(int slasdq_ (char* integer* integer* integer*
				    integer* integer* real* real* real*
				    integer* real* integer* real* integer* real*
				    integer*)))

  (slasdt_	(int slasdt_ (integer* integer* integer* integer*
				       integer* integer* integer*)))

  (slaset_	(int slaset_ (char* integer* integer* real*
				    real* real* integer*)))

  (slasq1_	(int slasq1_ (integer* real* real* real*
				       integer*)))

  (slasq2_	(int slasq2_ (integer* real* integer*)))

  (slasq3_	(int slasq3_ (integer* integer* real* integer*
					real* real* real* real* integer*
					integer* integer* logical*)))

  (slasq4_	(int slasq4_ (integer* integer* real* integer*
					integer* real* real* real* real*
					real* real* real* integer*)))

  (slasq5_	(int slasq5_ (integer* integer* real* integer*
					real* real* real* real* real* real*
					real* logical*)))

  (slasq6_	(int slasq6_ (integer* integer* real* integer*
					real* real* real* real* real* real*)))

  (slasr_	(int slasr_ (char* char* char* integer*
				   integer* real* real* real* integer*)))

  (slasrt_	(int slasrt_ (char* integer* real* integer*)))

  (slassq_	(int slassq_ (integer* real* integer* real*
				       real*)))

  (slasv2_	(int slasv2_ (real* real* real* real* real*
				    real* real* real* real*)))

  (slaswp_	(int slaswp_ (integer* real* integer* integer*
				       integer* integer* integer*)))

  (slasy2_	(int slasy2_ (logical* logical* integer*
					integer* integer* real* integer* real* integer*
					real* integer* real* real* integer* real* integer*)))

  (slasyf_	(int slasyf_ (char* integer* integer* integer*
				    real* integer* integer* real* integer* integer*)))

  (slatbs_	(int slatbs_ (char* char* char* char*
				    integer* integer* real* integer* real*
				    real* real* integer*)))

  (slatdf_	(int slatdf_ (integer* integer* real* integer*
				       real* real* real* integer* integer*
				      )))

  (slatps_	(int slatps_ (char* char* char* char*
				    integer* real* real* real* real*
				    integer*)))

  (slatrd_	(int slatrd_ (char* integer* integer* real*
				    integer* real* real* real* integer*)))

  (slatrs_	(int slatrs_ (char* char* char* char*
				    integer* real* integer* real* real* real* integer*)))

  (slatrz_	(int slatrz_ (integer* integer* integer* real*
				       integer* real* real*)))

  (slatzm_	(int slatzm_ (char* integer* integer* real*
				    integer* real* real* real* integer* real*
				   )))

  (slauu2_	(int slauu2_ (char* integer* real* integer*
				    integer*)))

  (slauum_	(int slauum_ (char* integer* real* integer*
				    integer*)))

  (sopgtr_	(int sopgtr_ (char* integer* real* real*
				    real* integer* real* integer*)))

  (sopmtr_	(int sopmtr_ (char* char* char* integer*
				    integer* real* real* real* integer* real*
				    integer*)))

  (sorg2l_	(int sorg2l_ (integer* integer* integer* real*
				       integer* real* real* integer*)))

  (sorg2r_	(int sorg2r_ (integer* integer* integer* real*
				       integer* real* real* integer*)))

  (sorgbr_	(int sorgbr_ (char* integer* integer* integer*
				    real* integer* real* real* integer* integer*)))

  (sorghr_	(int sorghr_ (integer* integer* integer* real*
				       integer* real* real* integer* integer*)))

  (sorgl2_	(int sorgl2_ (integer* integer* integer* real*
				       integer* real* real* integer*)))

  (sorglq_	(int sorglq_ (integer* integer* integer* real*
				       integer* real* real* integer* integer*)))

  (sorgql_	(int sorgql_ (integer* integer* integer* real*
				       integer* real* real* integer* integer*)))

  (sorgqr_	(int sorgqr_ (integer* integer* integer* real*
				       integer* real* real* integer* integer*)))

  (sorgr2_	(int sorgr2_ (integer* integer* integer* real*
				       integer* real* real* integer*)))

  (sorgrq_	(int sorgrq_ (integer* integer* integer* real*
				       integer* real* real* integer* integer*)))

  (sorgtr_	(int sorgtr_ (char* integer* real* integer*
				    real* real* integer* integer*)))

  (sorm2l_	(int sorm2l_ (char* char* integer* integer*
				    integer* real* integer* real* real* integer*
				    real* integer*)))

  (sorm2r_	(int sorm2r_ (char* char* integer* integer*
				    integer* real* integer* real* real* integer*
				    real* integer*)))

  (sormbr_	(int sormbr_ (char* char* char* integer*
				    integer* integer* real* integer* real* real*
				    integer* real* integer* integer*)))

  (sormhr_	(int sormhr_ (char* char* integer* integer*
				    integer* integer* real* integer* real* real*
				    integer* real* integer* integer*)))

  (sorml2_	(int sorml2_ (char* char* integer* integer*
				    integer* real* integer* real* real* integer*
				    real* integer*)))

  (sormlq_	(int sormlq_ (char* char* integer* integer*
				    integer* real* integer* real* real* integer*
				    real* integer* integer*)))

  (sormql_	(int sormql_ (char* char* integer* integer*
				    integer* real* integer* real* real* integer*
				    real* integer* integer*)))

  (sormqr_	(int sormqr_ (char* char* integer* integer*
				    integer* real* integer* real* real* integer*
				    real* integer* integer*)))

  (sormr2_	(int sormr2_ (char* char* integer* integer*
				    integer* real* integer* real* real* integer*
				    real* integer*)))

  (sormr3_	(int sormr3_ (char* char* integer* integer*
				    integer* integer* real* integer* real* real*
				    integer* real* integer*)))

  (sormrq_	(int sormrq_ (char* char* integer* integer*
				    integer* real* integer* real* real* integer*
				    real* integer* integer*)))

  (sormrz_	(int sormrz_ (char* char* integer* integer*
				    integer* integer* real* integer* real* real*
				    integer* real* integer* integer*)))

  (sormtr_	(int sormtr_ (char* char* char* integer*
				    integer* real* integer* real* real* integer*
				    real* integer* integer*)))

  (spbcon_	(int spbcon_ (char* integer* integer* real*
				    integer* real* real* real* integer*
				    integer*)))

  (spbequ_	(int spbequ_ (char* integer* integer* real*
				    integer* real* real* real* integer*)))

  (spbrfs_	(int spbrfs_ (char* integer* integer* integer*
				    real* integer* real* integer* real*
				    integer* real* integer* real* real* real*
				    integer* integer*)))

  (spbstf_	(int spbstf_ (char* integer* integer* real*
				    integer* integer*)))

  (spbsv_	(int spbsv_ (char* integer* integer* integer*
				   real* integer* real* integer* integer*)))

  (spbsvx_	(int spbsvx_ (char* char* integer* integer*
				    integer* real* integer* real* integer*
				    char* real* real* integer* real* integer*
				    real* real* real* real* integer*
				    integer*)))

  (spbtf2_	(int spbtf2_ (char* integer* integer* real*
				    integer* integer*)))

  (spbtrf_	(int spbtrf_ (char* integer* integer* real*
				    integer* integer*)))

  (spbtrs_	(int spbtrs_ (char* integer* integer* integer*
				    real* integer* real* integer* integer*)))

  (spocon_	(int spocon_ (char* integer* real* integer*
				    real* real* real* integer* integer*)))

  (spoequ_	(int spoequ_ (integer* real* integer* real* real* real* integer*)))

  (sporfs_	(int sporfs_ (char* integer* integer* real*
				    integer* real* integer* real* integer* real*
				    integer* real* real* real* integer*
				    integer*)))

  (sposv_	(int sposv_ (char* integer* integer* real*
				   integer* real* integer* integer*)))

  (sposvx_	(int sposvx_ (char* char* integer* integer*
				    real* integer* real* integer* char*
				    real* real* integer* real* integer* real*
				    real* real* real* integer* integer*)))

  (spotf2_	(int spotf2_ (char* integer* real* integer*
				    integer*)))

  (spotrf_	(int spotrf_ (char* integer* real* integer*
				    integer*)))

  (spotri_	(int spotri_ (char* integer* real* integer*
				    integer*)))

  (spotrs_	(int spotrs_ (char* integer* integer* real*
				    integer* real* integer* integer*)))

  (sppcon_	(int sppcon_ (char* integer* real* real*
				    real* real* integer* integer*)))

  (sppequ_	(int sppequ_ (char* integer* real* real* real*
				    real* integer*)))

  (spprfs_	(int spprfs_ (char* integer* integer* real*
				    real* real* integer* real* integer* real*
				    real* real* integer* integer*)))

  (sppsv_	(int sppsv_ (char* integer* integer* real*
				   real* integer* integer*)))

  (sppsvx_	(int sppsvx_ (char* char* integer* integer*
				    real* real* char* real* real* integer*
				    real* integer* real* real* real* real* integer* integer*)))

  (spptrf_	(int spptrf_ (char* integer* real* integer*)))

  (spptri_	(int spptri_ (char* integer* real* integer*)))

  (spptrs_	(int spptrs_ (char* integer* integer* real*
				    real* integer* integer*)))

  (sptcon_	(int sptcon_ (integer* real* real* real*
				       real* real* integer*)))

  (spteqr_	(int spteqr_ (char* integer* real* real*
				    real* integer* real* integer*)))

  (sptrfs_	(int sptrfs_ (integer* integer* real* real*
				       real* real* real* integer* real* integer*
				       real* real* real* integer*)))

  (sptsv_	(int sptsv_ (integer* integer* real* real*
				      real* integer* integer*)))

  (sptsvx_	(int sptsvx_ (char* integer* integer* real*
				    real* real* real* real* integer* real* integer*
				    real* real* real* real* integer*)))

  (spttrf_	(int spttrf_ (integer* real* real* integer*)))

  (spttrs_	(int spttrs_ (integer* integer* real* real*
				       real* integer* integer*)))

  (sptts2_	(int sptts2_ (integer* integer* real* real*
				       real* integer*)))

  (srscl_	(int srscl_ (integer* real* real* integer*)))

  (ssbev_	(int ssbev_ (char* char* integer* integer*
				   real* integer* real* real* integer* real*
				   integer*)))

  (ssbevd_	(int ssbevd_ (char* char* integer* integer*
				    real* integer* real* real* integer* real*
				    integer* integer* integer* integer*)))

  (ssbevx_	(int ssbevx_ (char* char* char* integer*
				    integer* real* integer* real* integer* real*
				    real* integer* integer* real* integer* real*
				    real* integer* real* integer* integer*
				    integer*)))

  (ssbgst_	(int ssbgst_ (char* char* integer* integer*
				    integer* real* integer* real* integer* real*
				    integer* real* integer*)))

  (ssbgv_	(int ssbgv_ (char* char* integer* integer*
				   integer* real* integer* real* integer* real*
				   real* integer* real* integer*)))

  (ssbgvd_	(int ssbgvd_ (char* char* integer* integer*
				    integer* real* integer* real* integer* real*
				    real* integer* real* integer* integer*
				    integer* integer*)))

  (ssbgvx_	(int ssbgvx_ (char* char* char* integer*
				    integer* integer* real* integer* real* integer*
				    real* integer* real* real* integer* integer* real*
				    integer* real* real* integer* real* integer* integer* integer*)))

  (ssbtrd_	(int ssbtrd_ (char* char* integer* integer*
				    real* integer* real* real* real* integer*
				    real* integer*)))

  (sspcon_	(int sspcon_ (char* integer* real* integer*
				    real* real* real* integer* integer*)))

  (sspev_	(int sspev_ (char* char* integer* real*
				   real* real* integer* real* integer*)))

  (sspevd_	(int sspevd_ (char* char* integer* real*
				    real* real* integer* real* integer* integer* integer* integer*)))

  (sspevx_	(int sspevx_ (char* char* char* integer*
				    real* real* real* integer* integer* real*
				    integer* real* real* integer* real* integer*
				    integer* integer*)))

  (sspgst_	(int sspgst_ (integer* char* integer* real*
				       real* integer*)))

  (sspgv_	(int sspgv_ (integer* char* char* integer*
				      real* real* real* real* integer* real*
				      integer*)))

  (sspgvd_	(int sspgvd_ (integer* char* char* integer*
				       real* real* real* real* integer* real*
				       integer* integer* integer* integer*)))

  (sspgvx_	(int sspgvx_ (integer* char* char* char*
				       integer* real* real* real* real* integer*
				       integer* real* integer* real* real* integer*
				       real* integer* integer* integer*)))

  (ssprfs_	(int ssprfs_ (char* integer* integer* real*
				    real* integer* real* integer* real* integer*
				    real* real* real* integer* integer*
				    )))

  (sspsv_	(int sspsv_ (char* integer* integer* real*
				   integer* real* integer* integer*)))

  (sspsvx_	(int sspsvx_ (char* char* integer* integer*
				    real* real* integer* real* integer* real* integer*
				    real* real* real* real*
				    integer* integer*)))

  (ssptrd_	(int ssptrd_ (char* integer* real* real*
				    real* real* integer*)))

  (ssptrf_	(int ssptrf_ (char* integer* real* integer*
				    integer*)))

  (ssptri_	(int ssptri_ (char* integer* real* integer*
				    real* integer*)))

  (ssptrs_	(int ssptrs_ (char* integer* integer* real*
				    integer* real* integer* integer*)))

  (sstebz_	(int sstebz_ (char* char* integer* real*
				    real* integer* integer* real* real* real*
				    integer* integer* real* integer* integer*
				    real* integer* integer*)))

  (sstedc_	(int sstedc_ (char* integer* real* real*
				    real* integer* real* integer* integer*
				    integer* integer*)))

  (sstegr_	(int sstegr_ (char* char* integer* real*
				    real* real* real* integer* integer* real*
				    integer* real* real* integer* integer* real*
				    integer* integer* integer* integer*)))

  (sstein_	(int sstein_ (integer* real* real* integer* real* integer* integer* real* integer* real*
				       integer* integer* integer*)))

  (ssteqr_	(int ssteqr_ (char* integer* real* real*
				    real* integer* real* integer*)))

  (ssterf_	(int ssterf_ (integer* real* real* integer*)))

  (sstev_	(int sstev_ (char* integer* real* real* real*
				   integer* real* integer*)))

  (sstevd_	(int sstevd_ (char* integer* real* real* real* integer* real* integer* integer*
				    integer* integer*)))

  (sstevr_	(int sstevr_ (char* char* integer* real*
				    real* real* real* integer* integer* real*
				    integer* real* real* integer* integer* real*
				    integer* integer* integer* integer*)))

  (sstevx_	(int sstevx_ (char* char* integer* real*
				    real* real* real* integer* integer* real*
				    integer* real* real* integer* real* integer*
				    integer* integer*)))

  (ssycon_	(int ssycon_ (char* integer* real* integer*
				    integer* real* real* real* integer*
				    integer*)))

  (ssyev_	(int ssyev_ (char* char* integer* real*
				   integer* real* real* integer* integer*)))

  (ssyevd_	(int ssyevd_ (char* char* integer* real*
				    integer* real* real* integer* integer*
				    integer* integer*)))

  (ssyevr_	(int ssyevr_ (char* char* char* integer*
				    real* integer* real* real* integer* integer*
				    real* integer* real* real* integer* integer*
				    real* integer* integer* integer*
				    integer*)))

  (ssyevx_	(int ssyevx_ (char* char* char* integer*
				    real* integer* real* real* integer* integer*
				    real* integer* real* real* integer* real*
				    integer* integer* integer* integer*)))

  (ssygs2_	(int ssygs2_ (integer* char* integer* real*
				       integer* real* integer* integer*)))

  (ssygst_	(int ssygst_ (integer* char* integer* real*
				       integer* real* integer* integer*)))

  (ssygv_	(int ssygv_ (integer* char* char* integer*
				      real* integer* real* integer* real* real*
				      integer* integer*)))

  (ssygvd_	(int ssygvd_ (integer* char* char* integer*
				       real* integer* real* integer* real* real*
				       integer* integer* integer* integer*)))

  (ssygvx_	(int ssygvx_ (integer* char* char* char*
				       integer* real* integer* real* integer* real*
				       real* integer* integer* real* integer*
				       real* real* integer* real* integer* integer* integer* integer*)))

  (ssyrfs_	(int ssyrfs_ (char* integer* integer* real*
				    integer* real* integer* integer* real*
				    integer* real* integer* real* real* real*
				    integer* integer*)))

  (ssysv_	(int ssysv_ (char* integer* integer* real*
				   integer* integer* real* integer* real*
				   integer* integer*)))

  (ssysvx_	(int ssysvx_ (char* char* integer* integer*
				    real* integer* real* integer* integer*
				    real* integer* real* integer* real* real*
				    real* real* integer* integer* integer*
				    )))

  (ssytd2_	(int ssytd2_ (char* integer* real* integer*
				    real* real* real* integer*)))

  (ssytf2_	(int ssytf2_ (char* integer* real* integer*
				    integer* integer*)))

  (ssytrd_	(int ssytrd_ (char* integer* real* integer*
				    real* real* real* real* integer* integer*
				    )))

  (ssytrf_	(int ssytrf_ (char* integer* real* integer*
				    integer* real* integer* integer*)))

  (ssytri_	(int ssytri_ (char* integer* real* integer*
				    integer* real* integer*)))

  (ssytrs_	(int ssytrs_ (char* integer* integer* real*
				    integer* integer* real* integer* integer*)))

  (stbcon_	(int stbcon_ (char* char* char* integer*
				    integer* real* integer* real* real*
				    integer* integer*)))

  (stbrfs_	(int stbrfs_ (char* char* char* integer*
				    integer* integer* real* integer* real* integer* real*
				    integer* real* real* real*
				    integer* integer*)))

  (stbtrs_	(int stbtrs_ (char* char* char* integer*
				    integer* integer* real* integer* real* integer* integer*)))

  (stgevc_	(int stgevc_ (char* char* logical*
				    integer* real* integer* real* integer* real*
				    integer* real* integer* integer* integer* real* integer*)))

  (stgex2_	(int stgex2_ (logical* logical* integer* real* integer* real* integer* real*
					integer* real*
					integer* integer* integer* integer* real*
					integer* integer*)))

  (stgexc_	(int stgexc_ (logical* logical* integer* real* integer* real* integer* real*
					integer* real*
					integer* integer* integer* real* integer*
					integer*)))

  (stgsen_	(int stgsen_ (integer* logical* logical*
				       logical* integer* real* integer* real* integer*
				       real* real* real* real* integer*
				       real* integer* integer* real* real* real*
				       real* integer* integer* integer* integer*
				       )))

  (stgsja_	(int stgsja_ (char* char* char* integer*
				    integer* integer* integer* integer* real* integer*
				    real* integer* real* real* real* real*
				    real* integer* real* integer* real* integer*
				    real* integer* integer*)))

  (stgsna_	(int stgsna_ (char* char* logical*
				    integer* real* integer* real* integer* real*
				    integer* real* integer* real* real* integer*
				    integer* real* integer* integer* integer*
				    )))

  (stgsy2_	(int stgsy2_ (char* integer* integer* integer*
				    real* integer* real* integer* real* integer*
				    real* integer* real* integer* real* integer* real* real*
				    real* integer* integer* integer*)))

  (stgsyl_	(int stgsyl_ (char* integer* integer* integer*
				    real* integer* real* integer* real* integer*
				    real* integer* real* integer* real* integer* real* real*
				    real* integer* integer*
				    integer*)))

  (stpcon_	(int stpcon_ (char* char* char* integer*
				    real* real* real* integer* integer*)))

  (stprfs_	(int stprfs_ (char* char* char* integer*
				    integer* real* real* integer* real* integer*
				    real* real* real* integer* integer*)))

  (stptri_	(int stptri_ (char* char* integer* real*
				    integer*)))

  (stptrs_	(int stptrs_ (char* char* char* integer*
				    integer* real* real* integer* integer*)))

  (strcon_	(int strcon_ (char* char* char* integer*
				    real* integer* real* real* integer*
				    integer*)))

  (strevc_	(int strevc_ (char* char* logical*
				    integer* real* integer* real* integer* real*
				    integer* integer* integer* real* integer*)))

  (strexc_	(int strexc_ (char* integer* real* integer*
				    real* integer* integer* integer* real*
				    integer*)))

  (strrfs_	(int strrfs_ (char* char* char* integer*
				    integer* real* integer* real* integer* real*
				    integer* real* real* real* integer*
				    integer*)))

  (strsen_	(int strsen_ (char* char* logical* integer* real* integer* real* integer* real* real*
				    integer* real* real* real* integer* integer*
				    integer* integer*)))

  (strsna_	(int strsna_ (char* char* logical*
				    integer* real* integer* real* integer* real*
				    integer* real* real* integer* integer* real*
				    integer* integer* integer*)))

  (strsyl_	(int strsyl_ (char* char* integer* integer* integer* real* integer* real*
				    integer* real*
				    integer* real* integer*)))

  (strti2_	(int strti2_ (char* char* integer* real*
				    integer* integer*)))

  (strtri_	(int strtri_ (char* char* integer* real*
				    integer* integer*)))

  (strtrs_	(int strtrs_ (char* char* char* integer*
				    integer* real* integer* real* integer* integer*
				    )))

  (stzrqf_	(int stzrqf_ (integer* integer* real* integer*
				       real* integer*)))

  (stzrzf_	(int stzrzf_ (integer* integer* real* integer*
				       real* real* integer* integer*)))

  (xerbla_	(int xerbla_ (char* integer*)))

  (zbdsqr_	(int zbdsqr_ (char* integer* integer* integer*
				    integer* doublereal* doublereal* doublecomplex*
				    integer* doublecomplex* integer* doublecomplex*
				    integer* doublereal* integer*)))

  (zdrot_	(int zdrot_ (integer* doublecomplex* integer*
				      doublecomplex* integer* doublereal* doublereal*)))

  (zdrscl_	(int zdrscl_ (integer* doublereal* doublecomplex*
				       integer*)))

  (zgbbrd_	(int zgbbrd_ (char* integer* integer* integer*
				    integer* integer* doublecomplex* integer*
				    doublereal* doublereal* doublecomplex* integer*
				    doublecomplex* integer* doublecomplex* integer*
				    doublecomplex* doublereal* integer*)))

  (zgbcon_	(int zgbcon_ (char* integer* integer* integer*
				    doublecomplex* integer* integer* doublereal*
				    doublereal* doublecomplex* doublereal* integer*
				    )))

  (zgbequ_	(int zgbequ_ (integer* integer* integer* integer*
				       doublecomplex* integer* doublereal* doublereal*
				       doublereal* doublereal* doublereal* integer*
				       )))

  (zgbrfs_	(int zgbrfs_ (char* integer* integer* integer*
				    integer* doublecomplex* integer* doublecomplex*
				    integer* integer* doublecomplex* integer*
				    doublecomplex* integer* doublereal* doublereal*
				    doublecomplex* doublereal* integer*)))

  (zgbsv_	(int zgbsv_ (integer* integer* integer* integer*
				      doublecomplex* integer* integer* doublecomplex*
				      integer* integer*)))

  (zgbsvx_	(int zgbsvx_ (char* char* integer* integer*
				    integer* integer* doublecomplex* integer*
				    doublecomplex* integer* integer* char*
				    doublereal* doublereal* doublecomplex* integer*
				    doublecomplex* integer* doublereal* doublereal*
				    doublereal* doublecomplex* doublereal* integer*
				    )))

  (zgbtf2_	(int zgbtf2_ (integer* integer* integer* integer*
				       doublecomplex* integer* integer* integer*)))

  (zgbtrf_	(int zgbtrf_ (integer* integer* integer* integer*
				       doublecomplex* integer* integer* integer*)))

  (zgbtrs_	(int zgbtrs_ (char* integer* integer* integer*
				    integer* doublecomplex* integer* integer*
				    doublecomplex* integer* integer*)))

  (zgebak_	(int zgebak_ (char* char* integer* integer*
				    integer* doublereal* integer* doublecomplex*
				    integer* integer*)))

  (zgebal_	(int zgebal_ (char* integer* doublecomplex* integer* integer* integer*
				    doublereal* integer*)))

  (zgebd2_	(int zgebd2_ (integer* integer* doublecomplex*
				       integer* doublereal* doublereal* doublecomplex*
				       doublecomplex* doublecomplex* integer*)))

  (zgebrd_	(int zgebrd_ (integer* integer* doublecomplex*
				       integer* doublereal* doublereal* doublecomplex*
				       doublecomplex* doublecomplex* integer* integer*
				       )))

  (zgecon_	(int zgecon_ (char* integer* doublecomplex*
				    integer* doublereal* doublereal* doublecomplex*
				    doublereal* integer*)))

  (zgeequ_	(int zgeequ_ (integer* integer* doublecomplex*
				       integer* doublereal* doublereal* doublereal*
				       doublereal* doublereal* integer*)))

  (zgees_	(int zgees_ (char* char* L_fp integer*
				   doublecomplex* integer* integer* doublecomplex*
				   doublecomplex* integer* doublecomplex* integer*
				   doublereal* logical* integer*)))

  (zgeesx_	(int zgeesx_ (char* char* L_fp char*
				    integer* doublecomplex* integer* integer*
				    doublecomplex* doublecomplex* integer* doublereal*
				    doublereal* doublecomplex* integer*
				    doublereal* logical* integer*)))

  (zgeev_	(int zgeev_ (char* char* integer*
				   doublecomplex* integer* doublecomplex* doublecomplex*
				   integer* doublecomplex* integer* doublecomplex*
				   integer* doublereal* integer*)))

  (zgeevx_	(int zgeevx_ (char* char* char* char*
				    integer* doublecomplex* integer* doublecomplex*
				    doublecomplex* integer* doublecomplex* integer*
				    integer* integer* doublereal* doublereal*
				    doublereal* doublereal* doublecomplex* integer*
				    doublereal* integer*)))

  (zgegs_	(int zgegs_ (char* char* integer*
				   doublecomplex* integer* doublecomplex* integer*
				   doublecomplex* doublecomplex* doublecomplex*
				   integer* doublecomplex* integer* doublecomplex*
				   integer* doublereal* integer*)))

  (zgegv_	(int zgegv_ (char* char* integer*
				   doublecomplex* integer* doublecomplex* integer*
				   doublecomplex* doublecomplex* doublecomplex* integer*
				   doublecomplex* integer* doublecomplex* integer*
				   doublereal* integer*)))

  (zgehd2_	(int zgehd2_ (integer* integer* integer*
				       doublecomplex* integer* doublecomplex* doublecomplex*
				       integer*)))

  (zgehrd_	(int zgehrd_ (integer* integer* integer*
				       doublecomplex* integer* doublecomplex* doublecomplex*
				       integer* integer*)))

  (zgelq2_	(int zgelq2_ (integer* integer* doublecomplex*
				       integer* doublecomplex* doublecomplex* integer*)))

  (zgelqf_	(int zgelqf_ (integer* integer* doublecomplex*
				       integer* doublecomplex* doublecomplex* integer*
				       integer*)))

  (zgels_	(int zgels_ (char* integer* integer* integer*
				   doublecomplex* integer* doublecomplex* integer*
				   doublecomplex* integer* integer*)))

  (zgelsx_	(int zgelsx_ (integer* integer* integer*
				       doublecomplex* integer* doublecomplex* integer*
				       integer* doublereal* integer* doublecomplex*
				       doublereal* integer*)))

  (zgelsy_	(int zgelsy_ (integer* integer* integer*
				       doublecomplex* integer* doublecomplex* integer*
				       integer* doublereal* integer* doublecomplex*
				       integer* doublereal* integer*)))

  (zgeql2_	(int zgeql2_ (integer* integer* doublecomplex*
				       integer* doublecomplex* doublecomplex* integer*)))

  (zgeqlf_	(int zgeqlf_ (integer* integer* doublecomplex*
				       integer* doublecomplex* doublecomplex* integer*
				       integer*)))

  (zgeqp3_	(int zgeqp3_ (integer* integer* doublecomplex*
				       integer* integer* doublecomplex* doublecomplex*
				       integer* doublereal* integer*)))

  (zgeqpf_	(int zgeqpf_ (integer* integer* doublecomplex*
				       integer* integer* doublecomplex* doublecomplex*
				       doublereal* integer*)))

  (zgeqr2_	(int zgeqr2_ (integer* integer* doublecomplex*
				       integer* doublecomplex* doublecomplex* integer*)))

  (zgeqrf_	(int zgeqrf_ (integer* integer* doublecomplex*
				       integer* doublecomplex* doublecomplex* integer*
				       integer*)))

  (zgerfs_	(int zgerfs_ (char* integer* integer*
				    doublecomplex* integer* doublecomplex* integer*
				    integer* doublecomplex* integer* doublecomplex*
				    integer* doublereal* doublereal* doublecomplex*
				    doublereal* integer*)))

  (zgerq2_	(int zgerq2_ (integer* integer* doublecomplex*
				       integer* doublecomplex* doublecomplex* integer*)))

  (zgerqf_	(int zgerqf_ (integer* integer* doublecomplex*
				       integer* doublecomplex* doublecomplex* integer*
				       integer*)))

  (zgesc2_	(int zgesc2_ (integer* doublecomplex* integer*
				       doublecomplex* integer* integer* doublereal*)))

  (zgesv_	(int zgesv_ (integer* integer* doublecomplex*
				      integer* integer* doublecomplex* integer* integer*
				      )))

  (zgesvx_	(int zgesvx_ (char* char* integer* integer*
				    doublecomplex* integer* doublecomplex* integer*
				    integer* char* doublereal* doublereal*
				    doublecomplex* integer* doublecomplex* integer*
				    doublereal* doublereal* doublereal* doublecomplex*
				    doublereal* integer*)))

  (zgetc2_	(int zgetc2_ (integer* doublecomplex* integer*
				       integer* integer* integer*)))

  (zgetf2_	(int zgetf2_ (integer* integer* doublecomplex*
				       integer* integer* integer*)))

  (zgetrf_	(int zgetrf_ (integer* integer* doublecomplex*
				       integer* integer* integer*)))

  (zgetri_	(int zgetri_ (integer* doublecomplex* integer*
				       integer* doublecomplex* integer* integer*)))

  (zgetrs_	(int zgetrs_ (char* integer* integer*
				    doublecomplex* integer* integer* doublecomplex*
				    integer* integer*)))

  (zggbak_	(int zggbak_ (char* char* integer* integer*
				    integer* doublereal* doublereal* integer*
				    doublecomplex* integer* integer*)))

  (zggbal_	(int zggbal_ (char* integer* doublecomplex* integer* doublecomplex*
				    integer* integer* integer*
				    doublereal* doublereal* doublereal* integer*
				    )))

  (zgges_	(int zgges_ (char* char* char* L_fp
				   integer* doublecomplex* integer* doublecomplex*
				   integer* integer* doublecomplex* doublecomplex*
				   doublecomplex* integer* doublecomplex* integer* doublecomplex*
				   integer* doublereal*
				   logical* integer*)))

  (zggesx_	(int zggesx_ (char* char* char* L_fp
				    char* integer* doublecomplex* integer*
				    doublecomplex* integer* integer* doublecomplex*
				    doublecomplex* doublecomplex* integer*
				    doublecomplex* integer* doublereal* doublereal*
				    doublecomplex* integer* doublereal*
				    integer* integer* logical* integer*)))

  (zggev_	(int zggev_ (char* char* integer*
				   doublecomplex* integer* doublecomplex* integer*
				   doublecomplex* doublecomplex* doublecomplex* integer*
				   doublecomplex* integer* doublecomplex* integer*
				   doublereal* integer*)))

  (zggevx_	(int zggevx_ (char* char* char* char*
				    integer* doublecomplex* integer* doublecomplex*
				    integer* doublecomplex* doublecomplex*
				    doublecomplex* integer* doublecomplex* integer*
				    integer* integer* doublereal* doublereal*
				    doublereal* doublereal* doublereal* doublereal*
				    doublecomplex* integer* doublereal*
				    integer* logical* integer*)))

  (zggglm_	(int zggglm_ (integer* integer* integer*
				       doublecomplex* integer* doublecomplex* integer*
				       doublecomplex* doublecomplex* doublecomplex*
				       doublecomplex* integer* integer*)))

  (zgghrd_	(int zgghrd_ (char* char* integer* integer*
				    integer* doublecomplex* integer* doublecomplex*
				    integer* doublecomplex* integer* doublecomplex*
				    integer* integer*)))

  (zgglse_	(int zgglse_ (integer* integer* integer*
				       doublecomplex* integer* doublecomplex* integer*
				       doublecomplex* doublecomplex* doublecomplex*
				       doublecomplex* integer* integer*)))

  (zggqrf_	(int zggqrf_ (integer* integer* integer*
				       doublecomplex* integer* doublecomplex* doublecomplex*
				       integer* doublecomplex* doublecomplex* integer*
				       integer*)))

  (zggrqf_	(int zggrqf_ (integer* integer* integer*
				       doublecomplex* integer* doublecomplex* doublecomplex*
				       integer* doublecomplex* doublecomplex* integer*
				       integer*)))

  (zggsvd_	(int zggsvd_ (char* char* char* integer*
				    integer* integer* integer* integer* doublecomplex*
				    integer* doublecomplex* integer* doublereal*
				    doublereal* doublecomplex* integer* doublecomplex*
				    integer* doublecomplex* integer* doublecomplex*
				    doublereal* integer* integer*)))

  (zggsvp_	(int zggsvp_ (char* char* char* integer*
				    integer* integer* doublecomplex* integer* doublecomplex*
				    integer* doublereal* doublereal* integer*
				    integer* doublecomplex* integer* doublecomplex* integer*
				    doublecomplex* integer* integer* doublereal*
				    doublecomplex* doublecomplex* integer*)))

  (zgtcon_	(int zgtcon_ (char* integer* doublecomplex*
				    doublecomplex* doublecomplex* doublecomplex* integer*
				    doublereal* doublereal* doublecomplex*
				    integer*)))

  (zgtrfs_	(int zgtrfs_ (char* integer* integer*
				    doublecomplex* doublecomplex* doublecomplex*
				    doublecomplex* doublecomplex* doublecomplex*
				    doublecomplex* integer* doublecomplex* integer*
				    doublecomplex* integer* doublereal* doublereal*
				    doublecomplex* doublereal* integer*)))

  (zgtsv_	(int zgtsv_ (integer* integer* doublecomplex*
				      doublecomplex* doublecomplex* doublecomplex* integer*
				      integer*)))

  (zgtsvx_	(int zgtsvx_ (char* char* integer* integer*
				    doublecomplex* doublecomplex* doublecomplex*
				    doublecomplex* doublecomplex* doublecomplex*
				    doublecomplex* integer* doublecomplex* integer*
				    doublecomplex* integer* doublereal* doublereal*
				    doublereal* doublecomplex* doublereal* integer*
				    )))

  (zgttrf_	(int zgttrf_ (integer* doublecomplex* doublecomplex*
				       doublecomplex* doublecomplex* integer* integer*
				       )))

  (zgttrs_	(int zgttrs_ (char* integer* integer*
				    doublecomplex* doublecomplex* doublecomplex*
				    doublecomplex* integer* doublecomplex* integer*
				    integer*)))

  (zgtts2_	(int zgtts2_ (integer* integer* integer*
				       doublecomplex* doublecomplex* doublecomplex*
				       doublecomplex* integer* doublecomplex* integer*)))

  (zhbev_	(int zhbev_ (char* char* integer* integer*
				   doublecomplex* integer* doublereal* doublecomplex*
				   integer* doublecomplex* doublereal* integer*)))

  (zhbevd_	(int zhbevd_ (char* char* integer* integer*
				    doublecomplex* integer* doublereal* doublecomplex*
				    integer* doublecomplex* integer* doublereal*
				    integer* integer* integer* integer*)))

  (zhbevx_	(int zhbevx_ (char* char* char* integer*
				    integer* doublecomplex* integer* doublecomplex*
				    integer* doublereal* doublereal* integer* integer*
				    doublereal* integer* doublereal* doublecomplex*
				    integer* doublecomplex* doublereal* integer*
				    integer* integer*)))

  (zhbgst_	(int zhbgst_ (char* char* integer* integer*
				    integer* doublecomplex* integer* doublecomplex*
				    integer* doublecomplex* integer* doublecomplex*
				    doublereal* integer*)))

  (zhbgv_	(int zhbgv_ (char* char* integer* integer*
				   integer* doublecomplex* integer* doublecomplex*
				   integer* doublereal* doublecomplex* integer*
				   doublecomplex* doublereal* integer*)))

  (zhbgvx_	(int zhbgvx_ (char* char* char* integer*
				    integer* integer* doublecomplex* integer*
				    doublecomplex* integer* doublecomplex* integer*
				    doublereal* doublereal* integer* integer* doublereal*
				    integer* doublereal* doublecomplex* integer*
				    doublecomplex* doublereal* integer* integer*
				    integer*)))

  (zhbtrd_	(int zhbtrd_ (char* char* integer* integer*
				    doublecomplex* integer* doublereal* doublereal*
				    doublecomplex* integer* doublecomplex* integer*)))

  (zhecon_	(int zhecon_ (char* integer* doublecomplex*
				    integer* integer* doublereal* doublereal*
				    doublecomplex* integer*)))

  (zheev_	(int zheev_ (char* char* integer* doublecomplex* integer* doublereal*
				   doublecomplex* integer*
				   doublereal* integer*)))

  (zheevd_	(int zheevd_ (char* char* integer*
				    doublecomplex* integer* doublereal* doublecomplex*
				    integer* doublereal* integer* integer*
				    integer* integer*)))

  (zheevr_	(int zheevr_ (char* char* char* integer*
				    doublecomplex* integer* doublereal* doublereal*
				    integer* integer* doublereal* integer* doublereal*
				    doublecomplex* integer* integer* doublecomplex*
				    integer* doublereal* integer* integer*
				    integer* integer*)))

  (zheevx_	(int zheevx_ (char* char* char* integer*
				    doublecomplex* integer* doublereal* doublereal*
				    integer* integer* doublereal* integer* doublereal*
				    doublecomplex* integer* doublecomplex* integer*
				    doublereal* integer* integer* integer*
				    )))

  (zhegs2_	(int zhegs2_ (integer* char* integer*
				       doublecomplex* integer* doublecomplex* integer*
				       integer*)))

  (zhegst_	(int zhegst_ (integer* char* integer*
				       doublecomplex* integer* doublecomplex* integer*
				       integer*)))

  (zhegv_	(int zhegv_ (integer* char* char* integer*
				      doublecomplex* integer* doublecomplex* integer*
				      doublereal* doublecomplex* integer* doublereal*
				      integer*)))

  (zhegvd_	(int zhegvd_ (integer* char* char* integer*
				       doublecomplex* integer* doublecomplex* integer*
				       doublereal* doublecomplex* integer* doublereal*
				       integer* integer* integer* integer*)))

  (zhegvx_	(int zhegvx_ (integer* char* char* char*
				       integer* doublecomplex* integer* doublecomplex*
				       integer* doublereal* doublereal* integer* integer*
				       doublereal* integer* doublereal* doublecomplex*
				       integer* doublecomplex* integer* doublereal*
				       integer* integer* integer*)))

  (zherfs_	(int zherfs_ (char* integer* integer*
				    doublecomplex* integer* doublecomplex* integer*
				    integer* doublecomplex* integer* doublecomplex*
				    integer* doublereal* doublereal* doublecomplex*
				    doublereal* integer*)))

  (zhesv_	(int zhesv_ (char* integer* integer*
				   doublecomplex* integer* integer* doublecomplex*
				   integer* doublecomplex* integer* integer*)))

  (zhesvx_	(int zhesvx_ (char* char* integer* integer*
				    doublecomplex* integer* doublecomplex* integer*
				    integer* doublecomplex* integer* doublecomplex*
				    integer* doublereal* doublereal* doublereal*
				    doublecomplex* integer* doublereal* integer*)))

  (zhetf2_	(int zhetf2_ (char* integer* doublecomplex*
				    integer* integer* integer*)))

  (zhetrd_	(int zhetrd_ (char* integer* doublecomplex*
				    integer* doublereal* doublereal* doublecomplex*
				    doublecomplex* integer* integer*)))

  (zhetrf_	(int zhetrf_ (char* integer* doublecomplex*
				    integer* integer* doublecomplex* integer*
				    integer*)))

  (zhetri_	(int zhetri_ (char* integer* doublecomplex*
				    integer* integer* doublecomplex* integer*)))

  (zhetrs_	(int zhetrs_ (char* integer* integer*
				    doublecomplex* integer* integer* doublecomplex*
				    integer* integer*)))

  (zhgeqz_	(int zhgeqz_ (char* char* char* integer*
				    integer* integer* doublecomplex* integer*
				    doublecomplex* integer* doublecomplex* doublecomplex*
				    doublecomplex* integer* doublecomplex* integer*
				    doublecomplex* integer* doublereal* integer*
				    )))

  (zhpcon_	(int zhpcon_ (char* integer* doublecomplex*
				    integer* doublereal* doublereal* doublecomplex*
				    integer*)))

  (zhpev_	(int zhpev_ (char* char* integer* doublecomplex* doublereal* doublecomplex*
				   integer* doublecomplex*
				   doublereal* integer*)))

  (zhpevd_	(int zhpevd_ (char* char* integer*
				    doublecomplex* doublereal* doublecomplex* integer*
				    doublecomplex* integer* doublereal* integer*
				    integer* integer* integer*)))

  (zhpevx_	(int zhpevx_ (char* char* char* integer*
				    doublecomplex* doublereal* doublereal* integer*
				    integer* doublereal* integer* doublereal*
				    doublecomplex* integer* doublecomplex* doublereal*
				    integer* integer* integer*)))

  (zhpgst_	(int zhpgst_ (integer* char* integer*
				       doublecomplex* doublecomplex* integer*)))

  (zhpgv_	(int zhpgv_ (integer* char* char* integer*
				      doublecomplex* doublecomplex* doublereal* doublecomplex*
				      integer* doublecomplex* doublereal* integer*)))

  (zhpgvd_	(int zhpgvd_ (integer* char* char* integer*
				       doublecomplex* doublecomplex* doublereal* doublecomplex*
				       integer* doublecomplex* integer* doublereal*
				       integer* integer* integer* integer*)))

  (zhpgvx_	(int zhpgvx_ (integer* char* char* char*
				       integer* doublecomplex* doublecomplex* doublereal*
				       doublereal* integer* integer* doublereal*
				       integer* doublereal* doublecomplex* integer*
				       doublecomplex* doublereal* integer* integer*
				       integer*)))

  (zhprfs_	(int zhprfs_ (char* integer* integer*
				    doublecomplex* doublecomplex* integer* doublecomplex*
				    integer* doublecomplex* integer* doublereal*
				    doublereal* doublecomplex* doublereal* integer*
				    )))

  (zhpsv_	(int zhpsv_ (char* integer* integer*
				   doublecomplex* integer* doublecomplex* integer*
				   integer*)))

  (zhpsvx_	(int zhpsvx_ (char* char* integer* integer*
				    doublecomplex* doublecomplex* integer*
				    doublecomplex* integer* doublecomplex* integer*
				    doublereal* doublereal* doublereal* doublecomplex*
				    doublereal* integer*)))

  (zhptrd_	(int zhptrd_ (char* integer* doublecomplex*
				    doublereal* doublereal* doublecomplex* integer*)))

  (zhptrf_	(int zhptrf_ (char* integer* doublecomplex*
				    integer* integer*)))

  (zhptri_	(int zhptri_ (char* integer* doublecomplex*
				    integer* doublecomplex* integer*)))

  (zhptrs_	(int zhptrs_ (char* integer* integer*
				    doublecomplex* integer* doublecomplex* integer*
				    integer*)))

  (zhsein_	(int zhsein_ (char* char* char* logical*
				    integer* doublecomplex* integer* doublecomplex*
				    doublecomplex* integer* doublecomplex* integer*
				    integer* integer* doublecomplex* doublereal*
				    integer* integer* integer*)))

  (zhseqr_	(int zhseqr_ (char* char* integer* integer*
				    integer* doublecomplex* integer* doublecomplex*
				    doublecomplex* integer* doublecomplex* integer*
				    integer*)))

  (zlabrd_	(int zlabrd_ (integer* integer* integer*
				       doublecomplex* integer* doublereal* doublereal*
				       doublecomplex* doublecomplex* doublecomplex* integer*
				       doublecomplex* integer*)))

  (zlacgv_	(int zlacgv_ (integer* doublecomplex* integer*)))

  (zlacon_	(int zlacon_ (integer* doublecomplex* doublecomplex*
				       doublereal* integer*)))

  (zlacp2_	(int zlacp2_ (char* integer* integer* doublereal*
				    integer* doublecomplex* integer*)))

  (zlacpy_	(int zlacpy_ (char* integer* integer*
				    doublecomplex* integer* doublecomplex* integer*)))

  (zlacrm_	(int zlacrm_ (integer* integer* doublecomplex*
				       integer* doublereal* integer* doublecomplex*
				       integer* doublereal*)))

  (zlacrt_	(int zlacrt_ (integer* doublecomplex* integer*
				       doublecomplex* integer* doublecomplex* doublecomplex*
				      )))

  (zlaed0_	(int zlaed0_ (integer* integer* doublereal*
				       doublereal* doublecomplex* integer* doublecomplex*
				       integer* doublereal* integer* integer*)))

  (zlaed7_	(int zlaed7_ (integer* integer* integer*
				       integer* integer* integer* doublereal*
				       doublecomplex* integer* doublereal* integer*
				       doublereal* integer* integer* integer*
				       integer* integer* doublereal* doublecomplex*
				       doublereal* integer* integer*)))

  (zlaed8_	(int zlaed8_ (integer* integer* integer*
				       doublecomplex* integer* doublereal* doublereal*
				       integer* doublereal* doublereal* doublecomplex*
				       integer* doublereal* integer* integer*
				       integer* integer* integer* integer*
				       doublereal* integer*)))

  (zlaein_	(int zlaein_ (logical* logical* integer*
					doublecomplex* integer* doublecomplex* doublecomplex*
					doublecomplex* integer* doublereal* doublereal*
					doublereal* integer*)))

  (zlaesy_	(int zlaesy_ (doublecomplex* doublecomplex*
					     doublecomplex* doublecomplex* doublecomplex*
					     doublecomplex* doublecomplex* doublecomplex*)))

  (zlaev2_	(int zlaev2_ (doublecomplex* doublecomplex*
					     doublecomplex* doublereal* doublereal* doublereal*
					     doublecomplex*)))

  (zlags2_	(int zlags2_ (logical* doublereal* doublecomplex*
					doublereal* doublereal* doublecomplex* doublereal*
					doublereal* doublecomplex* doublereal* doublecomplex*
					doublereal* doublecomplex*)))

  (zlagtm_	(int zlagtm_ (char* integer* integer*
				    doublereal* doublecomplex* doublecomplex*
				    doublecomplex* doublecomplex* integer* doublereal*
				    doublecomplex* integer*)))

  (zlahef_	(int zlahef_ (char* integer* integer* integer*
				    doublecomplex* integer* integer* doublecomplex*
				    integer* integer*)))

  (zlahqr_	(int zlahqr_ (logical* logical* integer*
					integer* integer* doublecomplex* integer*
					doublecomplex* integer* integer* doublecomplex*
					integer* integer*)))

  (zlahrd_	(int zlahrd_ (integer* integer* integer*
				       doublecomplex* integer* doublecomplex* doublecomplex*
				       integer* doublecomplex* integer*)))

  (zlaic1_	(int zlaic1_ (integer* integer* doublecomplex*
				       doublereal* doublecomplex* doublecomplex* doublereal*
				       doublecomplex* doublecomplex*)))

  (zlals0_	(int zlals0_ (integer* integer* integer*
				       integer* integer* doublecomplex* integer*
				       doublecomplex* integer* integer* integer*
				       integer* integer* doublereal* integer*
				       doublereal* doublereal* doublereal* doublereal*
				       integer* doublereal* doublereal* doublereal*
				       integer*)))

  (zlalsa_	(int zlalsa_ (integer* integer* integer*
				       integer* doublecomplex* integer* doublecomplex*
				       integer* doublereal* integer* doublereal* integer*
				       doublereal* doublereal* doublereal* doublereal*
				       integer* integer* integer* integer*
				       doublereal* doublereal* doublereal* doublereal*
				       integer* integer*)))

  (zlapll_	(int zlapll_ (integer* doublecomplex* integer*
				       doublecomplex* integer* doublereal*)))

  (zlapmt_	(int zlapmt_ (logical* integer* integer*
					doublecomplex* integer* integer*)))

  (zlaqgb_	(int zlaqgb_ (integer* integer* integer* integer*
				       doublecomplex* integer* doublereal* doublereal*
				       doublereal* doublereal* doublereal* char*)))

  (zlaqge_	(int zlaqge_ (integer* integer* doublecomplex*
				       integer* doublereal* doublereal* doublereal*
				       doublereal* doublereal* char*)))

  (zlaqhb_	(int zlaqhb_ (char* integer* integer*
				    doublecomplex* integer* doublereal* doublereal*
				    doublereal* char*)))

  (zlaqhe_	(int zlaqhe_ (char* integer* doublecomplex*
				    integer* doublereal* doublereal* doublereal*
				    char*)))

  (zlaqhp_	(int zlaqhp_ (char* integer* doublecomplex*
				    doublereal* doublereal* doublereal* char*)))

  (zlaqp2_	(int zlaqp2_ (integer* integer* integer*
				       doublecomplex* integer* integer* doublecomplex*
				       doublereal* doublereal* doublecomplex*)))

  (zlaqps_	(int zlaqps_ (integer* integer* integer* integer* integer* doublecomplex*
				       integer* integer*
				       doublecomplex* doublereal* doublereal* doublecomplex*
				       doublecomplex* integer*)))

  (zlaqsb_	(int zlaqsb_ (char* integer* integer*
				    doublecomplex* integer* doublereal* doublereal*
				    doublereal* char*)))

  (zlaqsp_	(int zlaqsp_ (char* integer* doublecomplex*
				    doublereal* doublereal* doublereal* char*)))

  (zlaqsy_	(int zlaqsy_ (char* integer* doublecomplex*
				    integer* doublereal* doublereal* doublereal*
				    char*)))

  (zlar1v_	(int zlar1v_ (integer* integer* integer* doublereal* doublereal* doublereal*
				       doublereal* doublereal*
				       doublereal* doublecomplex* doublereal*
				       doublereal* integer* integer* doublereal*)))

  (zlar2v_	(int zlar2v_ (integer* doublecomplex* doublecomplex*
				       doublecomplex* integer* doublereal* doublecomplex*
				       integer*)))

  (zlarcm_	(int zlarcm_ (integer* integer* doublereal* integer*
				       doublecomplex* integer* doublecomplex* integer*
				       doublereal*)))

  (zlarf_	(int zlarf_ (char* integer* integer* doublecomplex* integer* doublecomplex*
				   doublecomplex* integer*
				   doublecomplex*)))

  (zlarfb_	(int zlarfb_ (char* char* char* char*
				    integer* integer* integer* doublecomplex* integer*
				    doublecomplex* integer* doublecomplex* integer*
				    doublecomplex* integer*)))

  (zlarfg_	(int zlarfg_ (integer* doublecomplex* doublecomplex*
				       integer* doublecomplex*)))

  (zlarft_	(int zlarft_ (char* char* integer* integer*
				    doublecomplex* integer* doublecomplex* doublecomplex*
				    integer*)))

  (zlarfx_	(int zlarfx_ (char* integer* integer*
				    doublecomplex* doublecomplex* doublecomplex* integer*
				    doublecomplex*)))

  (zlargv_	(int zlargv_ (integer* doublecomplex* integer*
				       doublecomplex* integer* doublereal* integer*)))

  (zlarnv_	(int zlarnv_ (integer* integer* integer*
				       doublecomplex*)))

  (zlarrv_	(int zlarrv_ (integer* doublereal* doublereal*
				       integer* integer* doublereal* integer*
				       doublereal* doublereal* doublecomplex* integer*
				       integer* doublereal* integer* integer*)))

  (zlartg_	(int zlartg_ (doublecomplex* doublecomplex* doublereal*
					     doublecomplex* doublecomplex*)))

  (zlartv_	(int zlartv_ (integer* doublecomplex* integer*
				       doublecomplex* integer* doublereal* doublecomplex*
				       integer*)))

  (zlarz_	(int zlarz_ (char* integer* integer* integer*
				   doublecomplex* integer* doublecomplex* doublecomplex*
				   integer* doublecomplex*)))

  (zlarzb_	(int zlarzb_ (char* char* char* char*
				    integer* integer* integer* integer* doublecomplex*
				    integer* doublecomplex* integer* doublecomplex*
				    integer* doublecomplex* integer*)))

  (zlarzt_	(int zlarzt_ (char* char* integer* integer*
				    doublecomplex* integer* doublecomplex* doublecomplex*
				    integer*)))

  (zlascl_	(int zlascl_ (char* integer* integer*
				     doublereal* doublereal* integer* integer*
				     doublecomplex* integer* integer*)))

  (zlaset_	(int zlaset_ (char* integer* integer*
				    doublecomplex* doublecomplex* doublecomplex* integer*
				   )))

  (zlasr_	(int zlasr_ (char* char* char* integer*
				   integer* doublereal* doublereal* doublecomplex*
				   integer*)))

  (zlassq_	(int zlassq_ (integer* doublecomplex* integer*
				       doublereal* doublereal*)))

  (zlaswp_	(int zlaswp_ (integer* doublecomplex* integer*
				       integer* integer* integer* integer*)))

  (zlasyf_	(int zlasyf_ (char* integer* integer* integer*
				    doublecomplex* integer* integer* doublecomplex*
				    integer* integer*)))

  (zlatbs_	(int zlatbs_ (char* char* char* char*
				    integer* integer* doublecomplex* integer*
				    doublecomplex* doublereal* doublereal* integer*)))

  (zlatdf_	(int zlatdf_ (integer* integer* doublecomplex*
				       integer* doublecomplex* doublereal* doublereal*
				       integer* integer*)))

  (zlatps_	(int zlatps_ (char* char* char* char*
				    integer* doublecomplex* doublecomplex* doublereal*
				    doublereal* integer*)))

  (zlatrd_	(int zlatrd_ (char* integer* integer*
				    doublecomplex* integer* doublereal* doublecomplex*
				    doublecomplex* integer*)))

  (zlatrs_	(int zlatrs_ (char* char* char* char*
				    integer* doublecomplex* integer* doublecomplex*
				    doublereal* doublereal* integer*)))

  (zlatrz_	(int zlatrz_ (integer* integer* integer*
				       doublecomplex* integer* doublecomplex* doublecomplex*
				      )))

  (zlatzm_	(int zlatzm_ (char* integer* integer*
				    doublecomplex* integer* doublecomplex* doublecomplex*
				    doublecomplex* integer* doublecomplex*)))

  (zlauu2_	(int zlauu2_ (char* integer* doublecomplex*
				    integer* integer*)))

  (zlauum_	(int zlauum_ (char* integer* doublecomplex*
				    integer* integer*)))

  (zpbcon_	(int zpbcon_ (char* integer* integer*
				    doublecomplex* integer* doublereal* doublereal*
				    doublecomplex* doublereal* integer*)))

  (zpbequ_	(int zpbequ_ (char* integer* integer*
				    doublecomplex* integer* doublereal* doublereal*
				    doublereal* integer*)))

  (zpbrfs_	(int zpbrfs_ (char* integer* integer* integer*
				    doublecomplex* integer* doublecomplex* integer*
				    doublecomplex* integer* doublecomplex* integer*
				    doublereal* doublereal* doublecomplex* doublereal*
				    integer*)))

  (zpbstf_	(int zpbstf_ (char* integer* integer*
				    doublecomplex* integer* integer*)))

  (zpbsv_	(int zpbsv_ (char* integer* integer* integer*
				   doublecomplex* integer* doublecomplex* integer*
				   integer*)))

  (zpbsvx_	(int zpbsvx_ (char* char* integer* integer*
				    integer* doublecomplex* integer* doublecomplex*
				    integer* char* doublereal* doublecomplex* integer*
				    doublecomplex* integer* doublereal* doublereal*
				    doublereal* doublecomplex* doublereal*
				    integer*)))

  (zpbtf2_	(int zpbtf2_ (char* integer* integer*
				    doublecomplex* integer* integer*)))

  (zpbtrf_	(int zpbtrf_ (char* integer* integer*
				    doublecomplex* integer* integer*)))

  (zpbtrs_	(int zpbtrs_ (char* integer* integer* integer*
				    doublecomplex* integer* doublecomplex* integer*
				    integer*)))

  (zpocon_	(int zpocon_ (char* integer* doublecomplex*
				    integer* doublereal* doublereal* doublecomplex*
				    doublereal* integer*)))

  (zpoequ_	(int zpoequ_ (integer* doublecomplex* integer*
				       doublereal* doublereal* doublereal* integer*)))

  (zporfs_	(int zporfs_ (char* integer* integer*
				    doublecomplex* integer* doublecomplex* integer*
				    doublecomplex* integer* doublecomplex* integer*
				    doublereal* doublereal* doublecomplex* doublereal*
				    integer*)))

  (zposv_	(int zposv_ (char* integer* integer*
				   doublecomplex* integer* doublecomplex* integer*
				   integer*)))

  (zposvx_	(int zposvx_ (char* char* integer* integer*
				    doublecomplex* integer* doublecomplex* integer*
				    char* doublereal* doublecomplex* integer*
				    doublecomplex* integer* doublereal* doublereal*
				    doublereal* doublecomplex* doublereal* integer*
				    )))

  (zpotf2_	(int zpotf2_ (char* integer* doublecomplex*
				    integer* integer*)))

  (zpotrf_	(int zpotrf_ (char* integer* doublecomplex*
				    integer* integer*)))

  (zpotri_	(int zpotri_ (char* integer* doublecomplex*
				    integer* integer*)))

  (zpotrs_	(int zpotrs_ (char* integer* integer*
				    doublecomplex* integer* doublecomplex* integer*
				    integer*)))

  (zppcon_	(int zppcon_ (char* integer* doublecomplex*
				    doublereal* doublereal* doublecomplex* doublereal* integer*)))

  (zppequ_	(int zppequ_ (char* integer* doublecomplex*
				    doublereal* doublereal* doublereal* integer*)))

  (zpprfs_	(int zpprfs_ (char* integer* integer*
				    doublecomplex* doublecomplex* doublecomplex* integer*
				    doublecomplex* integer* doublereal* doublereal*
				    doublecomplex* doublereal* integer*)))

  (zppsv_	(int zppsv_ (char* integer* integer*
				   doublecomplex* doublecomplex* integer* integer*)))

  (zppsvx_	(int zppsvx_ (char* char* integer* integer*
				    doublecomplex* doublecomplex* char* doublereal*
				    doublecomplex* integer* doublecomplex* integer*
				    doublereal* doublereal* doublereal* doublecomplex*
				    doublereal* integer*)))

  (zpptrf_	(int zpptrf_ (char* integer* doublecomplex*
				    integer*)))

  (zpptri_	(int zpptri_ (char* integer* doublecomplex*
				    integer*)))

  (zpptrs_	(int zpptrs_ (char* integer* integer*
				    doublecomplex* doublecomplex* integer* integer*)))

  (zptcon_	(int zptcon_ (integer* doublereal* doublecomplex*
				       doublereal* doublereal* doublereal* integer*
				       )))

  (zptrfs_	(int zptrfs_ (char* integer* integer*
				    doublereal* doublecomplex* doublereal* doublecomplex*
				    doublecomplex* integer* doublecomplex* integer*
				    doublereal* doublereal* doublecomplex* doublereal*
				    integer*)))

  (zptsv_	(int zptsv_ (integer* integer* doublereal*
				      doublecomplex* doublecomplex* integer* integer*)))

  (zptsvx_	(int zptsvx_ (char* integer* integer*
				    doublereal* doublecomplex* doublereal* doublecomplex*
				    doublecomplex* integer* doublecomplex* integer*
				    doublereal* doublereal* doublereal* doublecomplex*
				    doublereal* integer*)))

  (zpttrf_	(int zpttrf_ (integer* doublereal* doublecomplex*
				       integer*)))

  (zpttrs_	(int zpttrs_ (char* integer* integer*
				    doublereal* doublecomplex* doublecomplex* integer*
				    integer*)))

  (zptts2_	(int zptts2_ (integer* integer* integer*
				       doublereal* doublecomplex* doublecomplex* integer*)))

  (zrot_	(int zrot_ (integer* doublecomplex* integer*
				     doublecomplex* integer* doublereal* doublecomplex*)))

  (zspcon_	(int zspcon_ (char* integer* doublecomplex*
				    integer* doublereal* doublereal* doublecomplex*
				    integer*)))

  (zspmv_	(int zspmv_ (char* integer* doublecomplex*
				   doublecomplex* doublecomplex* integer* doublecomplex*
				   doublecomplex* integer*)))

  (zspr_	(int zspr_ (char* integer* doublecomplex*
				  doublecomplex* integer* doublecomplex*)))

  (zsprfs_	(int zsprfs_ (char* integer* integer*
				    doublecomplex* doublecomplex* integer* doublecomplex*
				    integer* doublecomplex* integer* doublereal*
				    doublereal* doublecomplex* doublereal* integer*)))

  (zspsv_	(int zspsv_ (char* integer* integer*
				   doublecomplex* integer* doublecomplex* integer*
				   integer*)))

  (zspsvx_	(int zspsvx_ (char* char* integer* integer*
				    doublecomplex* doublecomplex* integer*
				    doublecomplex* integer* doublecomplex* integer*
				    doublereal* doublereal* doublereal* doublecomplex*
				    doublereal* integer*)))

  (zsptrf_	(int zsptrf_ (char* integer* doublecomplex*
				    integer* integer*)))

  (zsptri_	(int zsptri_ (char* integer* doublecomplex*
				    integer* doublecomplex* integer*)))

  (zsptrs_	(int zsptrs_ (char* integer* integer*
				    doublecomplex* integer* doublecomplex* integer*
				    integer*)))

  (zstedc_	(int zstedc_ (char* integer* doublereal*
				    doublereal* doublecomplex* integer* doublecomplex*
				    integer* doublereal* integer* integer*
				    integer* integer*)))

  (zstein_	(int zstein_ (integer* doublereal* doublereal*
				       integer* doublereal* integer* integer*
				       doublecomplex* integer* doublereal* integer*
				       integer* integer*)))

  (zsteqr_	(int zsteqr_ (char* integer* doublereal*
				    doublereal* doublecomplex* integer* doublereal*
				    integer*)))

  (zsycon_	(int zsycon_ (char* integer* doublecomplex*
				    integer* integer* doublereal* doublereal*
				    doublecomplex* integer*)))

  (zsymv_	(int zsymv_ (char* integer* doublecomplex*
				   doublecomplex* integer* doublecomplex* integer*
				   doublecomplex* doublecomplex* integer*)))

  (zsyr_	(int zsyr_ (char* integer* doublecomplex*
				  doublecomplex* integer* doublecomplex* integer*)))

  (zsyrfs_	(int zsyrfs_ (char* integer* integer*
				    doublecomplex* integer* doublecomplex* integer*
				    integer* doublecomplex* integer* doublecomplex*
				    integer* doublereal* doublereal* doublecomplex*
				    doublereal* integer*)))

  (zsysv_	(int zsysv_ (char* integer* integer*
				   doublecomplex* integer* integer* doublecomplex*
				   integer* doublecomplex* integer* integer*)))

  (zsysvx_	(int zsysvx_ (char* char* integer* integer*
				    doublecomplex* integer* doublecomplex* integer*
				    integer* doublecomplex* integer* doublecomplex*
				    integer* doublereal* doublereal* doublereal*
				    doublecomplex* integer* doublereal* integer*)))

  (zsytf2_	(int zsytf2_ (char* integer* doublecomplex*
				    integer* integer* integer*)))

  (zsytrf_	(int zsytrf_ (char* integer* doublecomplex*
				    integer* integer* doublecomplex* integer*
				    integer*)))

  (zsytri_	(int zsytri_ (char* integer* doublecomplex*
				    integer* integer* doublecomplex* integer*)))

  (zsytrs_	(int zsytrs_ (char* integer* integer*
				    doublecomplex* integer* integer* doublecomplex*
				    integer* integer*)))

  (ztbcon_	(int ztbcon_ (char* char* char* integer*
				    integer* doublecomplex* integer* doublereal*
				    doublecomplex* doublereal* integer*)))

  (ztbrfs_	(int ztbrfs_ (char* char* char* integer*
				    integer* integer* doublecomplex* integer*
				    doublecomplex* integer* doublecomplex* integer*
				    doublereal* doublereal* doublecomplex* doublereal*
				    integer*)))

  (ztbtrs_	(int ztbtrs_ (char* char* char* integer*
				    integer* integer* doublecomplex* integer*
				    doublecomplex* integer* integer*)))

  (ztgevc_	(int ztgevc_ (char* char* logical*
				    integer* doublecomplex* integer* doublecomplex* integer*
				    doublecomplex* integer* doublecomplex* integer*
				    integer* integer* doublecomplex* doublereal*
				    integer*)))

  (ztgex2_	(int ztgex2_ (logical* logical* integer*
					doublecomplex* integer* doublecomplex* integer*
					doublecomplex* integer* doublecomplex* integer*
					integer* integer*)))

  (ztgexc_	(int ztgexc_ (logical* logical* integer*
					doublecomplex* integer* doublecomplex* integer*
					doublecomplex* integer* doublecomplex* integer*
					integer* integer* integer*)))

  (ztgsen_	(int ztgsen_ (integer* logical* logical*
				       logical* integer* doublecomplex* integer*
				       doublecomplex* integer* doublecomplex* doublecomplex*
				       doublecomplex* integer* doublecomplex* integer*
				       integer* doublereal* doublereal* doublereal*
				       doublecomplex* integer* integer* integer*
				       integer*)))

  (ztgsja_	(int ztgsja_ (char* char* char* integer*
				    integer* integer* integer* integer* doublecomplex*
				    integer* doublecomplex* integer* doublereal*
				    doublereal* doublereal* doublereal* doublecomplex*
				    integer* doublecomplex* integer* doublecomplex*
				    integer* doublecomplex* integer* integer*)))

  (ztgsna_	(int ztgsna_ (char* char* logical*
				    integer* doublecomplex* integer* doublecomplex* integer*
				    doublecomplex* integer* doublecomplex* integer*
				    doublereal* doublereal* integer* integer*
				    doublecomplex* integer* integer* integer*)))

  (ztgsy2_	(int ztgsy2_ (char* integer* integer* integer*
				    doublecomplex* integer* doublecomplex* integer*
				    doublecomplex* integer* doublecomplex* integer*
				    doublecomplex* integer* doublecomplex* integer*
				    doublereal* doublereal* doublereal* integer*
				    )))

  (ztgsyl_	(int ztgsyl_ (char* integer* integer* integer*
				    doublecomplex* integer* doublecomplex* integer*
				    doublecomplex* integer* doublecomplex* integer*
				    doublecomplex* integer* doublecomplex* integer*
				    doublereal* doublereal* doublecomplex* integer*
				    integer* integer*)))

  (ztpcon_	(int ztpcon_ (char* char* char* integer*
				    doublecomplex* doublereal* doublecomplex* doublereal* integer*)))

  (ztprfs_	(int ztprfs_ (char* char* char* integer*
				    integer* doublecomplex* doublecomplex* integer*
				    doublecomplex* integer* doublereal* doublereal*
				    doublecomplex* doublereal* integer*)))

  (ztptri_	(int ztptri_ (char* char* integer* doublecomplex* integer*)))

  (ztptrs_	(int ztptrs_ (char* char* char* integer*
				    integer* doublecomplex* doublecomplex* integer*
				    integer*)))

  (ztrcon_	(int ztrcon_ (char* char* char* integer*
				    doublecomplex* integer* doublereal* doublecomplex*
				    doublereal* integer*)))

  (ztrevc_	(int ztrevc_ (char* char* logical*
				    integer* doublecomplex* integer* doublecomplex*
				    integer* doublecomplex* integer* integer* integer*
				    doublecomplex* doublereal* integer*)))

  (ztrexc_	(int ztrexc_ (char* integer* doublecomplex*
				    integer* doublecomplex* integer* integer* integer*
				    integer*)))

  (ztrrfs_	(int ztrrfs_ (char* char* char* integer*
				    integer* doublecomplex* integer* doublecomplex*
				    integer* doublecomplex* integer* doublereal*
				    doublereal* doublecomplex* doublereal* integer*)))

  (ztrsen_	(int ztrsen_ (char* char* logical* integer* doublecomplex* integer*
				    doublecomplex* integer*
				    doublecomplex* integer* doublereal* doublereal*
				    doublecomplex* integer* integer*)))

  (ztrsna_	(int ztrsna_ (char* char* logical*
				    integer* doublecomplex* integer* doublecomplex*
				    integer* doublecomplex* integer* doublereal*
				    doublereal* integer* integer* doublecomplex*
				    integer* doublereal* integer*)))

  (ztrsyl_	(int ztrsyl_ (char* char* integer* integer* integer* doublecomplex*
				    integer* doublecomplex*
				    integer* doublecomplex* integer* doublereal*
				    integer*)))

  (ztrti2_	(int ztrti2_ (char* char* integer*
				    doublecomplex* integer* integer*)))

  (ztrtri_	(int ztrtri_ (char* char* integer*
				    doublecomplex* integer* integer*)))

  (ztrtrs_	(int ztrtrs_ (char* char* char* integer*
				    integer* doublecomplex* integer* doublecomplex*
				    integer* integer*)))

  (ztzrqf_	(int ztzrqf_ (integer* integer* doublecomplex*
				       integer* doublecomplex* integer*)))

  (ztzrzf_	(int ztzrzf_ (integer* integer* doublecomplex*
				       integer* doublecomplex* doublecomplex* integer*
				       integer*)))

  (zung2l_	(int zung2l_ (integer* integer* integer*
				       doublecomplex* integer* doublecomplex* doublecomplex*
				       integer*)))

  (zung2r_	(int zung2r_ (integer* integer* integer*
				       doublecomplex* integer* doublecomplex* doublecomplex*
				       integer*)))

  (zungbr_	(int zungbr_ (char* integer* integer* integer*
				    doublecomplex* integer* doublecomplex* doublecomplex*
				    integer* integer*)))

  (zunghr_	(int zunghr_ (integer* integer* integer*
				       doublecomplex* integer* doublecomplex* doublecomplex*
				       integer* integer*)))

  (zungl2_	(int zungl2_ (integer* integer* integer*
				       doublecomplex* integer* doublecomplex* doublecomplex*
				       integer*)))

  (zunglq_	(int zunglq_ (integer* integer* integer*
				       doublecomplex* integer* doublecomplex* doublecomplex*
				       integer* integer*)))

  (zungql_	(int zungql_ (integer* integer* integer*
				       doublecomplex* integer* doublecomplex* doublecomplex*
				       integer* integer*)))

  (zungqr_	(int zungqr_ (integer* integer* integer*
				       doublecomplex* integer* doublecomplex* doublecomplex*
				       integer* integer*)))

  (zungr2_	(int zungr2_ (integer* integer* integer*
				       doublecomplex* integer* doublecomplex* doublecomplex*
				       integer*)))

  (zungrq_	(int zungrq_ (integer* integer* integer*
				       doublecomplex* integer* doublecomplex* doublecomplex*
				       integer* integer*)))

  (zungtr_	(int zungtr_ (char* integer* doublecomplex*
				    integer* doublecomplex* doublecomplex* integer*
				    integer*)))

  (zunm2l_	(int zunm2l_ (char* char* integer* integer*
				    integer* doublecomplex* integer* doublecomplex*
				    doublecomplex* integer* doublecomplex* integer*)))

  (zunm2r_	(int zunm2r_ (char* char* integer* integer*
				    integer* doublecomplex* integer* doublecomplex*
				    doublecomplex* integer* doublecomplex* integer*)))

  (zunmbr_	(int zunmbr_ (char* char* char* integer*
				    integer* integer* doublecomplex* integer* doublecomplex*
				    doublecomplex* integer* doublecomplex* integer*
				    integer*)))

  (zunmhr_	(int zunmhr_ (char* char* integer* integer*
				    integer* integer* doublecomplex* integer*
				    doublecomplex* doublecomplex* integer* doublecomplex*
				    integer* integer*)))

  (zunml2_	(int zunml2_ (char* char* integer* integer*
				    integer* doublecomplex* integer* doublecomplex*
				    doublecomplex* integer* doublecomplex* integer*)))

  (zunmlq_	(int zunmlq_ (char* char* integer* integer*
				    integer* doublecomplex* integer* doublecomplex*
				    doublecomplex* integer* doublecomplex* integer*
				    integer*)))

  (zunmql_	(int zunmql_ (char* char* integer* integer*
				    integer* doublecomplex* integer* doublecomplex*
				    doublecomplex* integer* doublecomplex* integer*
				    integer*)))

  (zunmqr_	(int zunmqr_ (char* char* integer* integer*
				    integer* doublecomplex* integer* doublecomplex*
				    doublecomplex* integer* doublecomplex* integer*
				    integer*)))

  (zunmr2_	(int zunmr2_ (char* char* integer* integer*
				    integer* doublecomplex* integer* doublecomplex*
				    doublecomplex* integer* doublecomplex* integer*)))

  (zunmr3_	(int zunmr3_ (char* char* integer* integer*
				    integer* integer* doublecomplex* integer*
				    doublecomplex* doublecomplex* integer* doublecomplex* integer*)))

  (zunmrq_	(int zunmrq_ (char* char* integer* integer*
				    integer* doublecomplex* integer* doublecomplex*
				    doublecomplex* integer* doublecomplex* integer*
				    integer*)))

  (zunmrz_	(int zunmrz_ (char* char* integer* integer*
				    integer* integer* doublecomplex* integer* doublecomplex*
				    doublecomplex* integer* doublecomplex* integer*
				    integer*)))

  (zunmtr_	(int zunmtr_ (char* char* char* integer*
				    integer* doublecomplex* integer* doublecomplex*
				    doublecomplex* integer* doublecomplex* integer*
				    integer*)))

  (zupgtr_	(int zupgtr_ (char* integer* doublecomplex*
				    doublecomplex* doublecomplex* integer* doublecomplex*
				    integer*)))

  (zupmtr_	(int zupmtr_ (char* char* char* integer*
				    integer* doublecomplex* doublecomplex* doublecomplex*
				    integer* doublecomplex* integer*)))

)


;;;; done

)

;;; end of file
