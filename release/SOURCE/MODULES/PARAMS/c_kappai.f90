! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module with UM setting of 
! 
  
! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.
    
MODULE c_kappai

IMPLICIT NONE

! C_KAPPAI start

! Thermal conductivity of sea-ice (W per m per K).
        REAL,PARAMETER:: KAPPAI=2.09

! Thermal conductivity of sea water (W per m per K).
        REAL,PARAMETER:: kappas=0.31

! Snow density (Kg per m**3)
        REAL,PARAMETER:: rhosnow=330.0

! Effective thickness of sea-ice surface layer (m).
        REAL,PARAMETER:: DE = 0.1

! C_KAPPAI end

! Define any variables not in UM include file c_kappai.h

! Effective thickness of sea surface layer (m).
  REAL,PARAMETER:: dzsea = 1.0


END MODULE c_kappai
