! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module with UM setting of 
! parameters for mineral dust code

! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.




MODULE c_dust_ndiv

IMPLICIT NONE

!C_DUST_NDIV.............................................................
! Description: Contains parameters for mineral dust code
! Current Code Owner: Stephanie Woodward
!
! History:
! Version  Date     Comment
! -------  ----     -------
!  5.5      12/02/03  Original Code.   Stephanie Woodward
!
! Declarations:
!
      INTEGER, PARAMETER :: NDIV=6 ! number of particle size divisions
                                   ! that can be lifted from the surface

      INTEGER, PARAMETER :: NDIVH=9! number of particle size divisions
                                   ! that can be blown horizontally    
                                   ! and contribute to the lifting of  
                                   ! the 1st NDIV divisions
!.....................................................................

END MODULE c_dust_ndiv
