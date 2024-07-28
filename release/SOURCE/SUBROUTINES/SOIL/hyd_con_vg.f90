
! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT 2000, Met Office, All Rights Reserved.
! Please refer to file $UMDIR/vn$VN/copyright.txt for further details
! *****************************COPYRIGHT*******************************
!    SUBROUTINE HYD_CON_VG---------------------------------------------

! Description:
!     Calculates the hydraulic conductivity using Van Genuchten curves

! Documentation : UM Documentation Paper 25

! Subroutine Interface:
SUBROUTINE hyd_con_vg(npnts,soil_pts,soil_index,b,ks,thetak,k,    &
                    dk_dthk,ltimer )


USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim
IMPLICIT NONE

! Subroutine arguments:
!   Scalar arguments with intent(IN) :
INTEGER                                                           &
 npnts                                                            &
                  ! IN points in grid
,soil_pts         ! IN Number of soil points.

!   Array arguments with intent(IN) :
INTEGER                                                           &
 soil_index(npnts)! IN Array of soil points.

REAL                                                              &
 b(npnts)                                                         &
                  ! IN Exponent in conductivity and soil water
!                       !    suction fits.
,ks(npnts)                                                        &
                  ! IN The saturated hydraulic conductivity (kg/m2
,thetak(npnts)    ! IN Fractional saturation.

LOGICAL ltimer    ! Logical switch for TIMER diags

!   Array arguments with intent(OUT) :
REAL                                                              &
 k(npnts)                                                         &
                  ! OUT The hydraulic conductivity (kg/m2/s).
,dk_dthk(npnts) &  ! OUT The rate of change of K with THETAK (kg/m2/s) !Jupp
,DK_DTHK1 !Jupp                                                           

! Local scalars:
REAL                                                              &
 bracket(npnts)                                                   &
                  ! WORK 1-S^(b+1)
,dbr_dthk(npnts)                                                  &
                  ! WORK The rate of change of BRACKET
!                       !       with THETAK.
,kred(npnts)                                                      &
                  ! WORK KSAT*S^L_WAG (kg/m2/s).
,sdum(npnts)      ! WORK Bounded THETAK value.

INTEGER                                                           &
 i,j              ! WORK Loop counter.

REAL                                                              &
 l_wag            ! Exponent in the van Mualem / Van Genuchten
!                       ! fit to the hydraulic conduactivity curve.
PARAMETER (l_wag=0.5)

REAL                                                              &
 theta_min                                                        &
                  ! Minimum value of THETAK for K calculation.
,theta_max        ! Maximum value of THETAK for K calculation.
PARAMETER (theta_min=0.05, theta_max=0.95)

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

IF (lhook) CALL dr_hook('HYD_CON_VG',zhook_in,zhook_handle)

dk_dthk(:) = 0. !Jupp

IF (ltimer) THEN
! DEPENDS ON: timer
  CALL timer('HYDCONVG',103)
END IF

!CDIR NODEP
!Jupp
!$TAF LOOP = PARALLEL
!Jupp
DO j=1,soil_pts
  i=soil_index(j)

  sdum(i)=MAX(thetak(i),theta_min)
  sdum(i)=MIN(sdum(i),theta_max)

!Jupp  dk_dthk(i)=0.0                                                
  dk_dthk1 = 0. !Jupp

  bracket(i)=1-sdum(i)**(b(i)+1)
  kred(i)=ks(i)*sdum(i)**l_wag

  k(i)=kred(i)*(1-bracket(i)**(1.0/(b(i)+1)))**2

!----------------------------------------------------------------------
! To avoid blow-up of implicit increments approximate by piecewise
! linear functions
! (a) for THETA>THETA_MAX  (ensuring that K=KS at THETA=1)
! (b) for THETA<THETA_MIN  (ensuring that K=0 at THETA=THETA_MIN)
!----------------------------------------------------------------------
  IF (THETAK(I).LT.THETA_MIN) THEN
!Jupp DK_DTHK(I)=K(I)/THETA_MIN
!Jupp K(I)=K(I)+DK_DTHK(I)*(MAX(THETAK(I),0.0)-THETA_MIN)
    DK_DTHK1=K(I)/THETA_MIN                                 !Jupp
    K(I)=K(I)+DK_DTHK1*(MAX(THETAK(I),0.0)-THETA_MIN)       !Jupp
  ELSEIF (THETAK(I).GT.THETA_MAX) THEN
!Jupp DK_DTHK(I)=(KS(I)-K(I))/(1.0-THETA_MAX)
!Jupp K(I)=K(I)+DK_DTHK(I)*(MIN(THETAK(I),1.0)-THETA_MAX)
    DK_DTHK1=(KS(I)-K(I))/(1.0-THETA_MAX)             !Jupp
    K(I)=K(I)+DK_DTHK1*(MIN(THETAK(I),1.0)-THETA_MAX) !Jupp
      ELSE
        DBR_DTHK(I)=-(B(I)+1)*SDUM(I)**B(I)
!Jupp          DK_DTHK(I)=L_WAG*K(I)/SDUM(I)
!Jupp     &            -2*KRED(I)/(B(I)+1)
!Jupp     &            *(1-BRACKET(I)**(1.0/(B(I)+1)))
!Jupp     &            *(BRACKET(I)**(-B(I)/(B(I)+1)))
!Jupp     &            *DBR_DTHK(I)
          DK_DTHK1=L_WAG*K(I)/SDUM(I) &                !Jupp
                 -2*KRED(I)/(B(I)+1) &                 !Jupp
                 *(1-BRACKET(I)**(1.0/(B(I)+1))) &     !Jupp
                 *(BRACKET(I)**(-B(I)/(B(I)+1))) &     !Jupp
                 *DBR_DTHK(I)                          !Jupp
        ENDIF

        IF ((THETAK(I).GT.1.0).OR.(THETAK(I).LT.0.0)) THEN
!Jupp          DK_DTHK(I)=0.0
          DK_DTHK1=0.0        !Jupp
          K(I)=MAX(K(I),0.0)
          K(I)=MIN(K(I),KS(I))
        ENDIF
        dk_dthk(i) = dk_dthk1 !Jupp
      ENDDO  

IF (ltimer) THEN
! DEPENDS ON: timer
  CALL timer('HYDCONVG',104)
END IF

IF (lhook) CALL dr_hook('HYD_CON_VG',zhook_out,zhook_handle)
RETURN
END

