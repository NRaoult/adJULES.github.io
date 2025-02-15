
! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT 2000, Met Office, All Rights Reserved.
! Please refer to file $UMDIR/vn$VN/copyright.txt for further details
! *****************************COPYRIGHT*******************************

!     SUBROUTINE ELEVATE ------------------------------------------

!     Purpose:
!     Calculate temperaute and humidity at a given elevation above the
!     mean gridbox surface

!     ------------------------------------------------------------------
SUBROUTINE elevate (                                              &
 row_length,rows,land_pts,ntiles,tile_pts,land_index,tile_index,  &
 tl_1,qw_1,qs1,pstar,surf_hgt,t_elev,q_elev,                      &
 ltimer                                                           &
 )

USE c_r_cp
USE c_lheat
USE c_epslon
USE c_g

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
IMPLICIT NONE

INTEGER, INTENT(IN) ::                                            &
 row_length                                                       &
                       ! IN Number of X points?
,rows                                                             &
                       ! IN Number of Y points?
,land_pts                                                         &
                       ! IN No of land points being processed.
,ntiles                                                           &
                       ! IN Number of land tiles per land point.
,land_index(land_pts)                                             &
                       ! IN Index of land points.
,tile_index(land_pts,ntiles)                                      &
                       ! IN Index of tile points.
,tile_pts(ntiles)      ! IN Number of tile points.

REAL, INTENT(IN) ::                                               &
 tl_1(row_length,rows)                                            &
                       ! IN Liquid/frozen water temperature for
!                            !    lowest atmospheric layer (K).
,qw_1(row_length,rows)                                            &
                       ! IN Total water content of lowest
!                            !    atmospheric layer (kg per kg air).
,qs1(row_length,rows)                                             &
                       ! IN Sat. specific humidity
!                            ! qsat(TL_1,PSTAR)
,pstar(row_length,rows)                                           &
                       ! IN Surface pressure (Pascals).
,surf_hgt(land_pts,ntiles)
                       ! IN Height of elevated tile above
!                            !        mean gridbox surface (m)

LOGICAL, INTENT(IN) ::                                            &
 ltimer                ! IN Logical for TIMER

REAL, INTENT(OUT) ::                                              &
 t_elev(land_pts,ntiles)                                          &
                          ! OUT Temperature at elevated height (k)
,q_elev(land_pts,ntiles)  ! OUT Specific humidity at elevated
!                               !     height (kg per kg air)


! Local variables
REAL ::                                                           &
 tdew(row_length,rows)                                            &
                        ! Dew point temperature for input
!                             ! specific humidity (K)
,tv                                                               &
                        ! Virtual temeprature for TDEW
,salr                                                             &
                        ! Saturated adiabatic lapse rate for
!                             ! input specific humidity
,z                                                                &
                        ! Height at which air becomes saturated
,dalr                   ! Dry adiabatic lapse rate

PARAMETER( dalr = g / cp )

! Scalars
INTEGER ::                                                        &
 i,j                                                              &
                     ! Horizontal field index.
,k                                                                &
                     ! Tile field index.
,l                                                                &
                     ! Land point field index.
,n                   ! Tile index loop counter

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!----------------------------------------------------------------------
! External routines called
!----------------------------------------------------------------------
EXTERNAL timer

IF (lhook) CALL dr_hook('ELEVATE',zhook_in,zhook_handle)
IF (ltimer) THEN
! DEPENDS ON: timer
  CALL timer('ELEVATE ',3)
END IF

! Initialise (for clarity).
t_elev(:,:)=0.
q_elev(:,:)=0.

! Calculate the dew point temperature
! DEPENDS ON: dewpnt
CALL dewpnt(qw_1,pstar,tl_1,row_length*rows,tdew)


DO n=1,ntiles
  DO k=1,tile_pts(n)
    l = tile_index(k,n)
    j=(land_index(l)-1)/row_length + 1
    i = land_index(l) - (j-1)*row_length

    t_elev(l,n) = tl_1(i,j) - surf_hgt(l,n)*dalr

    IF(t_elev(l,n) .lt. tdew(i,j)) THEN
! Temperature following a dry adiabat is less than dew point temperature
! Therefore need to follow a saturated adiabate from height of
! dew point temperature

      tv = tdew(i,j) * (1.0 + c_virtual*qw_1(i,j))
      salr = g *(1.0 + lc*qw_1(i,j) / (r*tv*(1.0-qw_1(i,j)))) /   &
             ( cp + lc**2.0 * qw_1(i,j) * REPSILON /              &
                      (r*tv**2.0*(1.0-qw_1(i,j))))

      z = (tl_1(i,j) - tdew(i,j)) / dalr

      t_elev(l,n) = tdew(i,j) - (surf_hgt(l,n) - z)*salr
      CALL qsat(q_elev(l,n),t_elev(l,n),pstar(i,j),1)

    ELSE
! Temperature follows a dry adiabatic lapse rate and humidity remains constant

      q_elev(l,n) = qw_1(i,j)

    END IF

  END DO
END DO

IF (ltimer) THEN
! DEPENDS ON: timer
  CALL timer('ELEVATE ',4)
END IF

IF (lhook) CALL dr_hook('ELEVATE',zhook_out,zhook_handle)
RETURN
END SUBROUTINE elevate
