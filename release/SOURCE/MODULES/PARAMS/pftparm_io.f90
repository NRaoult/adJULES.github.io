! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! This module contains variables used for reading in pftparm data
! and initialisations
  
! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.


MODULE pftparm_io

  USE max_dimensions, ONLY:                                           &
    npft_max

  IMPLICIT NONE

!---------------------------------------------------------------------
! Set up variables to use in IO (a fixed size version of each array
! in pftparm that we want to initialise).
!---------------------------------------------------------------------
  INTEGER ::                                                          &
    c3_io(npft_max),                                                  &
    orient_io(npft_max)

  REAL ::                                                             &
    a_wl_io(npft_max),                                                &
    a_ws_io(npft_max),                                                &
    albsnc_max_io(npft_max),                                          &
    albsnc_min_io(npft_max),                                          &
    albsnf_max_io(npft_max),                                          &
    alpha_io(npft_max),                                               &
    alnir_io(npft_max),                                               &
    alpar_io(npft_max),                                               &
    b_wl_io(npft_max),                                                &
    catch0_io(npft_max),                                              &
    dcatch_dlai_io(npft_max),                                         &
    dgl_dm_io(npft_max),                                              &
    dgl_dt_io(npft_max),                                              &
    dqcrit_io(npft_max),                                              &
    dz0v_dh_io(npft_max),                                             &
    eta_sl_io(npft_max),                                              &
    fd_io(npft_max),                                                  &
    fsmc_of_io(npft_max),                                             &
    f0_io(npft_max),                                                  &
    g_leaf_0_io(npft_max),                                            &
    glmin_io(npft_max),                                               &
    infil_f_io(npft_max),                                             &
    kext_io(npft_max),                                                &
    kpar_io(npft_max),                                                &
    neff_io(npft_max),                                                &
    nl0_io(npft_max),                                                 &
    nr_nl_io(npft_max),                                               &
    ns_nl_io(npft_max),                                               &
    omega_io(npft_max),                                               &
    omnir_io(npft_max),                                               &
    r_grow_io(npft_max),                                              &
    rootd_ft_io(npft_max),                                            &
    sigl_io(npft_max),                                                &
    tleaf_of_io(npft_max),                                            &
    tlow_io(npft_max),                                                &
    tupp_io(npft_max),                                                &
    emis_pft_io(npft_max)

!---------------------------------------------------------------------
! Set up a namelist for reading and writing these arrays
!---------------------------------------------------------------------
!Jupp  NAMELIST /jules_pftparm/ c3_io,orient_io,a_wl_io,a_ws_io,           &
!Jupp                           albsnc_max_io,albsnc_min_io,albsnf_max_io, &
!Jupp                           alpha_io,alnir_io,alpar_io,b_wl_io,        &
!Jupp                           catch0_io,dcatch_dlai_io,dgl_dm_io,        &
!Jupp                           dgl_dt_io,dqcrit_io,dz0v_dh_io,eta_sl_io,  &
!Jupp                           fd_io,fsmc_of_io,f0_io,g_leaf_0_io,        &
!Jupp                           glmin_io,infil_f_io,kext_io,kpar_io,       &
!Jupp                           neff_io,nl0_io,nr_nl_io,ns_nl_io,omega_io, &
!Jupp                           omnir_io,r_grow_io,rootd_ft_io,sigl_io,    &
!Jupp                           tleaf_of_io,tlow_io,tupp_io,emis_pft_io

END MODULE pftparm_io
