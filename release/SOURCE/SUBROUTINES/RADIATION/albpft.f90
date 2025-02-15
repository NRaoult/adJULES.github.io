
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Routine to calculate the spectral albedo of the land surface using
! the two stream approach of Sellers, 1995.

!**********************************************************************
SUBROUTINE albpft (p_field,land_field,                            &
                       land_index,tile_index,tile_pts,ilayers,    &
                       getprofile,                                &
                       albsoil,cosz,lai,alb_type,                 &
                       fapar_dir,fapar_dif,fapar_dir2dif,         &
                       fapar_dif2dif,fapar_dir2dir,fsun)

USE nstypes
USE pftparm

USE switches, ONLY :                                               &
!      imported scalars with intent(in)
   can_rad_mod

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

use ancil_info, only : land_pts
use fomod, only : iloopcount
IMPLICIT NONE

! Subroutine arguments
!   Scalar arguments with intent(in):
INTEGER, INTENT(IN) ::                                            &
 p_field                    &! Total number of grid points.
,land_field                 &! Number of land points.
,ilayers                     ! Number of layers over which
                             ! the PAR absorption profile is to
                             ! be calculated.

LOGICAL, INTENT(IN) :: getprofile
                             ! TRUE means profiles through the
!                                    canopy are calculated
!                                  ! FALSE means no profiles calculated

!   Array arguments with intent(in):
INTEGER, INTENT(IN) ::                                            &
 land_index(land_field)                                           &
                             ! Index of land points.
,tile_pts(ntype)                                                  &
                             ! Number of land points which
!                                  ! include the surface type.
,tile_index(land_field,ntype)
                             ! Indices of land points which
!                                  ! include the surface type.
REAL, INTENT(IN) ::                                               &
 albsoil(land_field)                                              &
                             ! Soil albedo.
,cosz(p_field)                                                    &
                             ! Cosine of the zenith angle.
,lai(land_field,npft)        ! Leaf area index.

!   Array arguments with intent(out):
REAL, INTENT(OUT) ::                                              &
 alb_type(land_field,ntype,4)                                     &
!                                  ! Albedos for surface types.
!                                  !   (*,*,1) - Direct beam visible
!                                  !   (*,*,2) - Diffuse visible
!                                  !   (*,*,3) - Direct beam near-IR
!                                  !   (*,*,4) - Diffuse near-IR
,fapar_dir(land_field,npft,ilayers)                               &
!                                  ! Profile of absorbed PAR
!                                  !     - Direct (fraction/LAI)
,fapar_dif(land_field,npft,ilayers)                               &
!                                  ! Profile of absorbed PAR
!                                  !     - Diffuse (fraction/LAI)
,fapar_dir2dif(land_field,npft,ilayers)                           &
!                                  ! Profile of scattered PAR           &
!                                  !   -direct to diffuse
!                                  !      (fraction/LAI)
,fapar_dif2dif(land_field,npft,ilayers)                           &
!                                  ! Profile of absorbed PAR
!                                  !     - Diffuse (fraction/LAI)
,fapar_dir2dir(land_field,npft,ilayers)                           &
!                                  ! Profile of absorbed only direct PAR
!                                  !     -  (fraction/LAI)
,fsun(land_field,npft,ilayers)
                             ! fraction of sunlit leaves

! Local arrays:
REAL                                                              &
 albudif(land_field,2)                                            &
                            ! Diffuse albedo of the underlying
!                                 ! surface.
,albudir(land_field,2)                                            &
                            ! Direct albedo of the underlying
!                                 ! surface.
,alpl(2)                                                          &
                            ! Leaf reflection coefficient.
,om(2)                                                            &
                            ! Leaf scattering coefficient.
,rnet_dir(0:ilayers)                                              &
!                                 ! Net downward PAR at layer
!                                 ! boundaries - incident direct
,rnet_dif(0:ilayers)                                              &
!                                 ! Net downward PAR at layer
!                                 ! boundaries - incident diffuse
,taul(2)                    ! Leaf transmission coefficient.

! Local scalars:
REAL                                                              &
 betadir                                                          &
                            ! Upscatter parameter for direct beam.
,betadif                                                          &
                            ! Upscatter parameter for diffuse beam
,coszm                                                            &
                            ! Mean value of COSZ.
,k                                                                &
                            ! Optical depth per unit leaf area.
,g                                                                &
                            ! Relative projected leaf area in
                            ! direction cosz.
,salb                                                             &
                            ! Single scattering albedo.
,sqcost                                                           &
                            ! Cosine squared of the mean leaf
                            ! angle to the horizontal.
,b,c,ca,d,f,h,u1                                                  &
                            ! Miscellaneous variables from
,p1,p2,p3,p4,d1                                                   &
                            ! Sellers (1985).
,h1,h2,h3,h7,h8                                                   &
                            !
,s1,s2,sig                  !

!-----------------------------------------------------------------------
! Additional work variables to calculate profiles of absorbed PAR
!-----------------------------------------------------------------------
REAL                                                              &
 dlai                                                             &
                                ! LAI increment.
,la                                                               &
                                ! Cumulative LAI through canopy.
,drdird_dlai,drdiru_dlai                                          &
,drdifd_dlai,drdifu_dlai                                          &
,u2,u3,d2,h4,h5,h6,h9,h10                                         &
                                ! Rate of change of fluxes with LAI
                                ! (W/m2/LAI).
,rdiru,rdird,rdifu,rdifd       &! work variable
,dabeer_dla(0:ilayers)          ! Attenuation of direct solar
                                ! beam per unit LAI

INTEGER                                                           &
 band,i,j,l,n               ! Loop counters.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle


IF (lhook) CALL dr_hook('ALBPFT',zhook_in,zhook_handle)
DO l=1,land_field
  albudif(l,1) = albsoil(l)
  albudif(l,2) = albsoil(l)
  albudir(l,1) = albsoil(l)
  albudir(l,2) = albsoil(l)
END DO

! Initialise to zero at top.
fapar_dir2dir(:,:,:) = 0.0
fapar_dir2dif(:,:,:) = 0.0
fapar_dif2dif(:,:,:) = 0.0
fsun(:,:,:)          = 0.0

!Jupp
!$TAF LOOP = PARALLEL
!Jupp
DO n=1,npft

  om(1) = omega(n)
  om(2) = omnir(n)
  alpl(1) = alpar(n)
  alpl(2) = alnir(n)
!Jupp
!$TAF LOOP = PARALLEL
!Jupp
  DO band=1,2  ! Visible and near-IR bands
    taul(band) = om(band) - alpl(band)
!Jupp
!$TAF LOOP = PARALLEL
!Jupp
    DO j=1,tile_pts(n)
      l = tile_index(j,n)
      i = land_index(l)
      IF (orient(n) == 0) THEN
        sqcost = 1./3.
        g = 0.5
        coszm = 1.0
        k = g / 0.01
        salb = 0.5*om(band)
        IF (cosz(i) > 0.01) THEN
          k = g / cosz(i)
          salb = 0.5*om(band) *                                   &
                 ( 1. - cosz(i)*LOG((cosz(i)+1.)/cosz(i)) )
        END IF
      ELSE IF (orient(n) == 1) THEN
        sqcost = 1.
        coszm = 1.
!             K = G / COSZ(I) = 1.0 because G = COSZ(I) for horizontal leaves
        k = 1.0
        salb = om(band)/4.0
      END IF
      betadir = (1. + coszm*k)/(om(band)*coszm*k)*salb
      c = 0.5*( alpl(band) + taul(band) +                         &
               (alpl(band) - taul(band))*sqcost )
      betadif = c / om(band)
      b = 1. - (1. - betadif)*om(band)
      d = om(band)*coszm*k*betadir
      f = om(band)*coszm*k*(1. - betadir)
      h = SQRT(b*b - c*c) / coszm
      sig = (coszm*k)**2 + c*c - b*b
      IF ( ABS(sig) < 1.0e-4 ) sig = SIGN( 1.0e-4,sig )
      u1 = b - c/albudif(l,band)
      ca = c*albudir(l,band)/albudif(l,band)
      s1 = EXP(-h*lai(l,n))
      s2 = EXP(-k*lai(l,n))
      p1 = b + coszm*h
      p2 = b - coszm*h
      p3 = b + coszm*k
      p4 = b - coszm*k
      d1 = p1*(u1 - coszm*h)/s1 - p2*(u1 + coszm*h)*s1
      h1 = -d*p4 - c*f
      h2 = ( (d - p3*h1/sig) * (u1 - coszm*h) / s1 -              &
             (d - ca - (u1 + coszm*k)*h1/sig)*p2*s2 ) / d1
      h3 = - ( (d - p3*h1/sig) * (u1 + coszm*h)*s1 -              &
               (d - ca - (u1 + coszm*k)*h1/sig)*p1*s2 ) / d1
      h7 = (c/d1)*(u1 - coszm*h) / s1
      h8 = - (c/d1)*(u1 + coszm*h) * s1
      alb_type(l,n,2*band-1) = h1/sig + h2 + h3   ! Direct beam
      alb_type(l,n,2*band) = h7 + h8              ! Diffuse
!-----------------------------------------------------------------------
! If required calculate the profile of absorbed PAR through the canopy
! of direct and diffuse beams (BEWARE: assumes PAR is band 1)
!-----------------------------------------------------------------------
      IF ( getprofile .AND. band==1 ) THEN

        u2 = b-c*albudif(l,band)
        u3 = f+c*albudif(l,band)
        d2 = (u2+coszm*h)/s1-(u2-coszm*h)*s1
        h4 = -f*p3-c*d
        h5 = -1./d2*(h4/sig*(u2+coszm*h)/s1                       &
           +(u3-h4/sig*(u2-coszm*k))*s2)
        h6 = 1./d2*(h4/sig*(u2-coszm*h)*s1                        &
          +(u3-h4/sig*(u2-coszm*k))*s2)
        h9 = 1./d2*(u2+coszm*h)/s1
        h10 = -1./d2*(u2-coszm*h)*s1
!-----------------------------------------------------------------------
! Two-stream equations for direct and diffuse upward and downward beams:
!             RDIRU(I)=H1/SIG*EXP(-K*LA)+H2*EXP(-H*LA)+H3*EXP(H*LA)
!             RDIRD(I)=(H4/SIG+1)*EXP(-K*LA)+H5*EXP(-H*LA)+H6*EXP(H*LA)
!             RDIFU(I)=H7*EXP(-H*LA)+H8*EXP(H*LA)
!             RDIFD(I)=H9*EXP(-H*LA)+H10*EXP(H*LA)
!-----------------------------------------------------------------------
        dlai=lai(l,n)/REAL(ilayers)

        IF ( can_rad_mod /= 5 ) THEN
!-----------------------------------------------------------------------
! Differentiate these equations to calculate PAR absorption per unit
! LAI down through the canopy. Centre derivatives in the centre of each
! LAI layer.
!-----------------------------------------------------------------------
          la=0.5*dlai
          DO i=1,ilayers
!$TAF STORE la = tape_albp, REC = i + ilayers*(j-1) + land_pts*ilayers*(band-1) + &
!$TAF&                                2*land_pts*ilayers*(n-1) + &
!$TAF&                                npft*2*land_pts*ilayers*(iloopcount-1)
            drdiru_dlai=-k*h1/sig*EXP(-k*la)-h*h2*EXP(-h*la)        &
                      +h*h3*EXP(h*la)
            drdird_dlai=-k*(h4/sig+1)*EXP(-k*la)-h*h5*EXP(-h*la)    &
                      +h*h6*EXP(h*la)
            drdifu_dlai=-h*h7*EXP(-h*la)+h*h8*EXP(h*la)
            drdifd_dlai=-h*h9*EXP(-h*la)+h*h10*EXP(h*la)

            fapar_dir(l,n,i)=-drdird_dlai+drdiru_dlai
            fapar_dif(l,n,i)=-drdifd_dlai+drdifu_dlai

            la=la+dlai
          END DO  !layers

        ELSE

!-----------------------------------------------------------------------
!               can_rad_mod = 5
!               Don't use derivatives.
!-----------------------------------------------------------------------

          la=dlai
          rnet_dir(0)=(h4-h1)/sig+(h5-h2)+(h6-h3)
          rnet_dif(0)=(h9-h7)+(h10-h8)

          DO i=1,ilayers
!$TAF STORE la = tape_albp, REC = i + ilayers*(j-1) + land_pts*ilayers*(band-1) + &
!$TAF&                                2*land_pts*ilayers*(n-1) + &
!$TAF&                                npft*2*land_pts*ilayers*(iloopcount-1)
            rdiru=h1/sig*EXP(-k*la)+h2*EXP(-h*la)+h3*EXP(h*la)
            rdird=h4/sig*EXP(-k*la)+h5*EXP(-h*la)+h6*EXP(h*la)
            rdifu=h7*EXP(-h*la)+h8*EXP(h*la)
            rdifd=h9*EXP(-h*la)+h10*EXP(h*la)

            rnet_dir(i)=rdird-rdiru
            rnet_dif(i)=rdifd-rdifu

            fapar_dir(l,n,i)=(rnet_dir(i-1)-rnet_dir(i))/dlai
            fapar_dif(l,n,i)=(rnet_dif(i-1)-rnet_dif(i))/dlai

            dabeer_dla(i)=(EXP(-k*(la-dlai))-EXP(-k*la))/dlai
            fapar_dir2dir(l,n,i)=(1.- om(1))*dabeer_dla(i)
            fapar_dir2dif(l,n,i)= om(1)*dabeer_dla(i)+fapar_dir(l,n,i)
            fapar_dif2dif(l,n,i)=fapar_dif(l,n,i)
            fsun(l,n,i)=EXP(-k*la)*(EXP(k*dlai)-1.)/(k*dlai)

            la=la+dlai
          END DO  !layers

        END IF  !  can_rad_mod

      END IF  !  abs. par (band=1+getProfile)

    END DO  !  points
  END DO  !  bands

END DO  !  pfts

IF (lhook) CALL dr_hook('ALBPFT',zhook_out,zhook_handle)
RETURN
END SUBROUTINE albpft
