
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!   SUBROUTINE FCDCH-------------------------------------------------

!  Purpose: Calculate surface transfer coefficients at one or more
!           gridpoints.


!  Documentation: UM Documentation Paper No 24, section P243.

!--------------------------------------------------------------------
SUBROUTINE fcdch (                                                &
 row_length,rows,cor_mo_iter,points,tile_pts,                     &
 tile_index,pts_index,                                            &
 db,vshr,z0m,z0h,zh,z1_uv,z1_uv_top,z1_tq,z1_tq_top,              &
 wind_profile_factor,                                             &
 ddmfx,                                                           &
 cdv,chv,cdv_std,v_s,v_s_std,recip_l_mo,u_s_std,                  &
 ltimer )

USE c_vkman
USE surf_param, ONLY :                                            &
                       beta                                       &
                      ,third                                      &
                      ,beta_cndd                                  &
                      ,cnst_cndd_0                                &
                      ,cnst_cndd_1                                &
                      ,cnst_cndd_2                                &
                      ,min_wind                                   &
                      ,min_ustar









USE blopt8a, ONLY :                                               &
                    off                                           &
                   ,on                                            &
                   ,ip_srfexwithcnv                               &
                   ,isrfexcnvgust


USE switches, ONLY :                                            &
                    i_modiscopt

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
use fomod, only : nloopcount,iloopcount,call_fcdch_sea !Jupp

IMPLICIT NONE
!real, external :: epsilon !Jupp

INTEGER                                                           &
 cor_mo_iter          ! IN Switch for MO iteration correction

INTEGER                                                           &
 row_length                                                       &
                      ! IN Number of X points?
,rows                                                             &
                      ! IN Number of Y points?
,points                                                           &
                      ! IN Number of points.
,tile_pts                                                         &
                      ! IN Number of tile points.
,tile_index(points)                                               &
                      ! IN Index of tile points.
,pts_index(points) ! IN Index of land points.

LOGICAL                                                           &
 ltimer


REAL                                                              &
 db(points)                                                       &
               ! IN Buoyancy difference between surface and lowest
!                    !    temperature and humidity level in the
!                    !    atmosphere (m/s^2).
,vshr(row_length,rows)                                            &
                       ! IN Wind speed difference between the surf
!                    !    the lowest wind level in the atmosphere (m/s).
,z0m(points)                                                      &
               ! IN Roughness length for momentum transport (m).
,z0h(points)                                                      &
               ! IN Roughness length for heat and moisture (m).
,zh(row_length,rows)                                              &
                       ! IN Depth of boundary layer (m).
,z1_uv(row_length,rows)                                           &
                       ! IN Height of lowest wind level (m).
,z1_tq(row_length,rows)                                           &
                       ! IN Height of lowest temperature and
!                    !    humidity level (m).
,wind_profile_factor(points)
!                    ! IN for adjusting the surface transfer
!                    !    coefficients to remove form drag effects.
REAL, INTENT(IN) ::                                               &
  z1_uv_top(row_length, rows)
                     ! Height of top of lowest uv-layer
REAL, INTENT(IN) ::                                               &
  z1_tq_top(row_length, rows)
                     !  Height of top of lowest Tq-layer
REAL, INTENT(IN) :: ddmfx(row_length, rows)
!                    !    Convective downdraught mass-flux
!                    !    at cloud-base.
REAL                                                              &
 cdv(points)                                                      &
               ! OUT Surface transfer coefficient for momentum
!                    !     including orographic form drag (m/s).
,chv(points)                                                      &
               ! OUT Surface transfer coefficient for
!                    !     heat, moisture & other scalars (m/s).
,cdv_std(points)                                                  &
!                    ! OUT Surface transfer coefficient for momentum
!                    !     excluding orographic form drag (m/s).
,v_s(points)                                                      &
               ! OUT Surface layer scaling velocity
!                    !     including orographic form drag (m/s).
,v_s_std(points)                                                  &
!                    ! OUT Surface layer scaling velocity
!                    !     excluding orographic form drag (m/s).
,u_s_std(points)                                                  &
               ! OUT Scaling velocity from middle of MO iteration
!                    !     - picked up in error by dust code!
,recip_l_mo(points)
!                    ! OUT Reciprocal of the Monin-Obukhov length
!                    !     (m^-1).

REAL :: wstrcnvgust(row_length, rows)
!                   ! Turbulent velocity scale for convective gustiness
REAL :: wgst_tmp

!    Workspace usage----------------------------------------------------

!     Local work arrays.

REAL                                                              &
 phi_m(points)                                                    &
                 ! Monin-Obukhov stability function for momentum
!                      ! integrated to the model's lowest wind level.
,phi_h(points) ! Monin-Obukhov stability function for scalars
!                      ! integrated to the model's lowest temperature
!                      ! and humidity level.

!*----------------------------------------------------------------------

!EXTERNAL phi_m_h, phi_m_h_vol
EXTERNAL timer

!  Define local variables

INTEGER i,j,k,l ! Loop counter; horizontal field index.
INTEGER it    ! Iteration loop counter.
INTEGER n_its ! Number of iterations for Monin-Obukhov length
!                   ! and stability functions.

REAL                                                              &
 b_flux                                                           &
              ! Surface buoyancy flux over air density.
,u_s                                                              &
              ! Iteration surface friction velocity
              ! (effective value)
,u_s2                                                             &
              ! Iteration surface friction velocity squared
              ! (effective value)
,u_s_std2                                                         &
              ! Non-effective version of U_S2
,w_s          ! Surface turbulent convective scaling velocity.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

IF (lhook) CALL dr_hook('FCDCH',zhook_in,zhook_handle)

IF (ltimer) THEN
! DEPENDS ON: timer
  CALL timer('FCDCH   ',3)
END IF

!-----------------------------------------------------------------------
!! 0. Initialise values
!-----------------------------------------------------------------------
cdv(:)        = 0.0
chv(:)        = 0.0
cdv_std(:)    = 0.0
v_s(:)        = 0.0
v_s_std(:)    = 0.0
u_s_std(:)    = 0.0
recip_l_mo(:) = 0.0
phi_m(:)      = 0.0
phi_h(:)      = 0.0

!-----------------------------------------------------------------------
!! 1. Set initial values for the iteration.
!-----------------------------------------------------------------------
IF (cor_mo_iter == off) THEN
  n_its=5   ! original iteration count
!                 ! Found typically 2% from converged value
ELSE
  n_its=8   ! Found typically 0.2% from converged value
END IF

IF (isrfexcnvgust == ip_srfexwithcnv) THEN
!CDIR NODEP
!Jupp
!$TAF LOOP = PARALLEL
!Jupp
  DO k=1,tile_pts
    l = tile_index(k)
    j=(pts_index(l)-1)/row_length + 1
    i = pts_index(l) - (j-1)*row_length

!         Compute convective gustiness, which is unchanged during
!         the iteration. Redelsperger's adjustment is to U_10; here it
!         is scaled to apply to the friction velocity.
    wgst_tmp = MIN( 0.6, MAX(0.0, ddmfx(i, j) ) )
    wstrcnvgust(i,j) = 0.067* LOG ( cnst_cndd_0 +                 &
                                     cnst_cndd_1 * wgst_tmp +      &
                                     cnst_cndd_2 * wgst_tmp ** 2 )
  END DO
END IF

!CDIR NODEP
!Jupp
!$TAF LOOP = PARALLEL
!Jupp
DO k=1,tile_pts
  l = tile_index(k)
  j=(pts_index(l)-1)/row_length + 1
  i = pts_index(l) - (j-1)*row_length
  IF (db(l)  <   0.0 .AND. vshr(i,j)  <   2.0) THEN
!-----------------------------------------------------------------------
!         Start the iteration from the convective limit.
!-----------------------------------------------------------------------
    recip_l_mo(l) = -vkman/(beta*beta*beta*zh(i,j))
  ELSE
!-----------------------------------------------------------------------
!         Start the iteration from neutral values.
!-----------------------------------------------------------------------
    recip_l_mo(l) = 0.0
  END IF
END DO
SELECT CASE (i_modiscopt)
  CASE(off)
! DEPENDS ON: phi_m_h
    CALL phi_m_h ( row_length,rows,points,tile_pts,               &
                   tile_index,pts_index,                          &
                   recip_l_mo,z1_uv,z1_tq,z0m,z0h,                &
                   phi_m,phi_h,                                   &
                   ltimer )
  CASE(on)
! DEPENDS ON: phi_m_h_vol
    CALL phi_m_h_vol ( row_length,rows,points,tile_pts,           &
                   tile_index,pts_index,                          &
                   recip_l_mo,z1_uv_top,z1_tq_top,z0m,z0h,        &
                   phi_m,phi_h,                                   &
                   ltimer )
END SELECT

!CDIR NODEP
!Jupp
!$TAF LOOP = PARALLEL
!Jupp
DO k=1,tile_pts
  l = tile_index(k)
  j=(pts_index(l)-1)/row_length + 1
  i = pts_index(l) - (j-1)*row_length
  IF (db(l)  <   0.0 .AND. vshr(i,j)  <   2.0) THEN
!-----------------------------------------------------------------------
!         Start the iteration from the convective limit.
!-----------------------------------------------------------------------
    v_s_std(l) = beta *                                           &
        SQRT( beta * ( vkman / phi_h(l) ) * zh(i,j) * (-db(l)) )
    v_s(l) = v_s_std(l)
  ELSE
!-----------------------------------------------------------------------
!         Start the iteration from neutral values.
!-----------------------------------------------------------------------
    v_s(l) = ( vkman / phi_m(l) ) * vshr(i,j)
    v_s_std(l) = v_s(l) * wind_profile_factor(l)
  END IF
  chv(l) = ( vkman / phi_h(l) ) * v_s_std(l)
  cdv(l) = ( vkman / phi_m(l) ) * v_s(l)
  cdv_std(l) = cdv(l) * ( v_s_std(l) / v_s(l) ) *                 &
                        wind_profile_factor(l)
END DO
!-----------------------------------------------------------------------
!! 2. Iterate to obtain sucessively better approximations for CD & CH.
!-----------------------------------------------------------------------
!Luke
!!$TAF LOOP = iteration cdv,cdv_std,chv,phi_h,phi_m,v_s,v_s_std=tape_fcdch
!Luke
DO it = 1,n_its
  IF (cor_mo_iter == off) THEN
!-----------------------------------------------------------------------
!  Original version with incorrect iteration of gustiness
!-----------------------------------------------------------------------
!CDIR NODEP
!Jupp
!$TAF LOOP = PARALLEL
!Jupp
    DO k=1,tile_pts
      l = tile_index(k)
      j=(pts_index(l)-1)/row_length + 1
      i = pts_index(l) - (j-1)*row_length
      b_flux = -chv(l) * db(l)

      IF( vshr(i,j) > min_wind)THEN
        u_s = SQRT( cdv(l) * vshr(i,j) )
      ELSE
        u_s = min_ustar
      END IF

      u_s_std(l) = SQRT( cdv_std(l) * vshr(i,j) )

      IF (db(l)  <   -EPSILON(0.0)) THEN
        w_s = MAX( (zh(i,j) * b_flux)**third, EPSILON(0.0) )

        SELECT CASE(isrfexcnvgust)
          CASE(off)
            v_s(l) = SQRT(u_s*u_s + beta*beta*w_s*w_s)
            v_s_std(l) =                                            &
            SQRT( u_s_std(l)*u_s_std(l) + beta*beta*w_s*w_s )
          CASE(ip_srfexwithcnv)
            v_s(l) = SQRT(u_s*u_s + beta_cndd*beta_cndd*w_s*w_s +   &
                     wstrcnvgust(i,j)**2 )
            v_s_std(l) = SQRT( u_s_std(l)*u_s_std(l) +              &
            beta_cndd*beta_cndd*w_s*w_s + wstrcnvgust(i,j)**2 )
        END SELECT

      ELSE
        v_s(l) = u_s
        v_s_std(l) = u_s_std(l)
      END IF

      IF( v_s(l) > min_wind)THEN
        recip_l_mo(l) = -vkman * b_flux                           &
                      / (v_s(l)*v_s(l)*v_s(l))
      ELSE
        recip_l_mo(l) = SQRT(EPSILON(0.0))
      END IF
    END DO
  ELSE       ! cor_mo_iter
!-----------------------------------------------------------------------
!  Corrected version
!-----------------------------------------------------------------------
!CDIR NODEP
!Jupp
!$TAF LOOP = PARALLEL
!Jupp
    DO k=1,tile_pts
      l = tile_index(k)
      j=(pts_index(l)-1)/row_length + 1
      i = pts_index(l) - (j-1)*row_length
      b_flux = -chv(l) * db(l)

      IF( vshr(i,j) > min_wind)THEN
        u_s2 = cdv(l) * vshr(i,j)
      ELSE
        u_s2 = min_ustar
      END IF
      u_s_std2 = cdv_std(l) * vshr(i,j)
      u_s_std(l) = SQRT( u_s_std2 )

      IF (db(l)  <   -EPSILON(0.0)) THEN
        w_s = MAX( (zh(i,j) * b_flux)**third, EPSILON(0.0) )
!           ! Note that, during this iteration, CDV already includes
!           ! this gust enhancement and so U_S2 is not simply the
!           ! friction velocity arising from the mean wind, hence:
        SELECT CASE(isrfexcnvgust)
          CASE(off)
            v_s(l) = SQRT( 0.5*( beta*beta*w_s*w_s                  &
                   + SQRT( (beta*w_s)**4 + 4.0*u_s2*u_s2 )) )
            v_s_std(l) = SQRT( 0.5*( beta*beta*w_s*w_s              &
                       + SQRT( (beta*w_s)**4                        &
                       + 4.0*u_s_std2*u_s_std2 )) )
          CASE(ip_srfexwithcnv)
            v_s(l) = SQRT( 0.5*( beta*beta*w_s*w_s                  &
                   + wstrcnvgust(i,j)**2                            &
                   + SQRT( ((beta_cndd*w_s)**2                      &
                   + wstrcnvgust(i,j)**2)**2                        &
                       + 4.0*u_s2*u_s2 )) )
            v_s_std(l) = SQRT( 0.5*( beta*beta*w_s*w_s              &
                   + wstrcnvgust(i,j)**2                            &
                   + SQRT( ((beta_cndd*w_s)**2                      &
                   + wstrcnvgust(i,j)**2)**2                        &
                   + 4.0*u_s_std2*u_s_std2 )) )
        END SELECT
      ELSE
        v_s(l) = SQRT( u_s2 )
        v_s_std(l) = SQRT( u_s_std2 )
      END IF

      IF( v_s(l) > min_wind)THEN
        recip_l_mo(l) = -vkman * b_flux                           &
                      / (v_s(l)*v_s(l)*v_s(l))
      ELSE
        recip_l_mo(l) = SQRT(EPSILON(0.0))
      END IF
    END DO

  END IF  ! test on COR_MO_ITER

  SELECT CASE (i_modiscopt)
    CASE (off)
! DEPENDS ON: phi_m_h
      CALL phi_m_h ( row_length,rows,points,tile_pts,             &
                     tile_index,pts_index,                        &
                     recip_l_mo,z1_uv,z1_tq,z0m,z0h,              &
                     phi_m,phi_h,                                 &
                     ltimer )
    CASE (on)
! DEPENDS ON: phi_m_h_vol
      CALL phi_m_h_vol ( row_length,rows,points,tile_pts,         &
                     tile_index,pts_index,                        &
                     recip_l_mo,z1_uv_top,z1_tq_top,z0m,z0h,      &
                     phi_m,phi_h,                                 &
                     ltimer )
  END SELECT


!CDIR NODEP
!Jupp
!$TAF LOOP = PARALLEL
!JUpp
  DO k=1,tile_pts
    l = tile_index(k)
    j=(pts_index(l)-1)/row_length + 1
    i = pts_index(l) - (j-1)*row_length
    chv(l) = ( vkman / phi_h(l) ) * v_s_std(l)
    cdv(l) = ( vkman / phi_m(l) ) * v_s(l)
    cdv_std(l) = cdv(l) * ( v_s_std(l) / v_s(l) ) *               &
                          wind_profile_factor(l)
  END DO
END DO ! Iteration loop

!-----------------------------------------------------------------------
!! Set CD's and CH's to be dimensionless paremters
!-----------------------------------------------------------------------
!CDIR NODEP
!Jupp
!$TAF LOOP = PARALLEL
!jupp
DO k=1,tile_pts
  l = tile_index(k)
  j=(pts_index(l)-1)/row_length + 1
  i = pts_index(l) - (j-1)*row_length
  cdv(l) = cdv(l) / vshr(i,j)
  cdv_std(l) = cdv_std(l) / vshr(i,j)
  chv(l) = chv(l) / vshr(i,j)
END DO


IF (ltimer) THEN
! DEPENDS ON: timer
  CALL timer('FCDCH   ',4)
END IF

IF (lhook) CALL dr_hook('FCDCH',zhook_out,zhook_handle)
RETURN
END SUBROUTINE fcdch
