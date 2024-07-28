! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module with UM setting of 
! mineral dust gravitational settling

! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.


MODULE c_dustgrav

IMPLICIT NONE

!C_DUSTGRAV.............................................................
! Description:
! Contains parameters for mineral dust gravitational settling
! Current Code Owner: Stephanie Woodward
!
! History:
! Version  Date     Comment
! -------  ----     -------
!  1      12/02/03  Original Code.   Stephanie Woodward
!
      REAL,PARAMETER :: ACCF=1.257 ! Cunningham correction factor term A
      REAL,PARAMETER :: BCCF=0.4 ! Cunningham correction factor term B
      REAL,PARAMETER :: CCCF=-1.1 ! Cunningham correction factor term C
!.......................................................................

END MODULE c_dustgrav
