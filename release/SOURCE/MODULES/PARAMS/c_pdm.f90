! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module with setting of PDM params
! 
  
! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.
       
MODULE c_pdm

! Use the include file if doing a UM run, else define variables






! Soil layer thickness for PDM (m):
  REAL :: dz_pdm = 1.0
  
! Shape factor for PDM:
  REAL :: b_pdm = 1.0



END MODULE
