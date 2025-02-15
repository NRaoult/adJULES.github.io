
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!   SUBROUTINE SF_EXCH------------------------------------------------
!
!  Purpose: Calculate coefficients of turbulent exchange between
!           the surface and the lowest atmospheric layer, and
!           "explicit" fluxes between the surface and this layer.
!
!  Suitable for Single Column use.
!
!  Documentation: UM Documentation Paper No 24, section P243.
!                 See especially sub-section (ix).
!
!---------------------------------------------------------------------

! Arguments :-

SUBROUTINE sf_exch (                                              &
 row_length,rows,off_x,off_y,halo_i,halo_j,                       &
 land_pts,ntiles,land_index,                                      &
 tile_index,tile_pts,fland,flandg,                                &
 ssi_pts,sea_pts,sice_pts,                                        &
 ssi_index,sea_index,sice_index,fssi,sea_frac,sice_frac,          &
 nsmax,nsnow,ds,hcons_snow,                                       &
 bq_1,bt_1,canhc_tile,canopy,catch,dzsoil,flake,gc,hcons,         &
 can_model,catch_snow, lq_mix_bl,                                 &
 ho2r2_orog,ice_fract,snowdepth,snow_tile,pstar,qw_1,radnet,      &
 radnet_tile,sil_orog,smvcst,tile_frac,timestep,                  &
 surf_hgt,emis_tile,emis_soil,                                    &
 tl_1,ti,ts1,                                                     &
 tsnow,                                                           &
 tstar_tile,tstar_land,tstar_sea,tstar_sice,tstar_ssi,z_land,     &
 l_ctile,seasalinityfactor,                                       &
 tstar,l_aggregate,l_spec_z0,z0m_scm,z0h_scm,l_dust,              &
 vfrac_tile,vshr_land,vshr_ssi,zh,ddmfx,                          &
 z0_tile,z1_uv,z1_uv_top,z1_tq,z1_tq_top,land_mask,               &
 su10,sv10,sq1p5,st1p5,sfme,sz0heff,ltimer,formdrag,fd_stab_dep,  &
 orog_drag_param,z0msea,                                          &
 alpha1,alpha1_sice,ashtf_prime,ashtf_prime_tile,cd,ch,           &
 recip_l_mo_sea,cdr10m,                                           &
 chr1p5m,chr1p5m_sice,e_sea,fme,fqw_1,fqw_tile,epot_tile,         &
 fqw_ice,                                                         &
 ftl_1,ftl_tile,ftl_ice,fraca,h_blend_orog,h_sea,charnock,        &
 rhostar,resfs,resft,rib,rib_tile,                                &
 fb_surf,u_s,q1_sd,t1_sd,z0hssi,z0h_tile,z0h_eff,                 &
 z0m_gb,z0mssi,z0m_tile,z0m_eff,rho_aresist,aresist,resist_b,     &
 rho_aresist_tile,aresist_tile,resist_b_tile,                     &
 r_b_dust,cd_std_dust,u_s_std_tile,                               &
 rho_cd_modv1,rhokh_1,rhokh_1_sice,rhokm_1,rhokm_land,rhokm_ssi,  &
 dtstar_tile,dtstar,rhokh_gb,rhokh_mix,anthrop_heat )


USE c_z0h_z0m, ONLY : z0h_z0m
USE c_vkman
USE c_rough
USE c_densty
USE blend_h
USE c_perma
USE c_r_cp
USE c_g
USE c_0_dg_c
USE c_dust_ndiv
USE c_kappai, ONLY : kappai,kappas,dzsea,de
USE c_lheat

USE blopt8a, ONLY :                                               &
                    surfdivz0t                                    &
                   ,fixed_z0t                                     &
                   ,effective_z0                                  &
                   ,use_correct_ustar                             &
                   ,max_stress_grad                               &
                   ,on

USE nstypes, ONLY :                                               &
!      imported scalars with intent(in)
   npft, urban_canyon, urban_roof

USE snow_param, ONLY :                                            &
!      imported arrays with intent(in)
   cansnowtile

USE surf_param, ONLY : h_blend_min,ls

USE urban_param, ONLY : hgt, hwr, ztm, disp, z0m_mat, z0h_z0m_c,  &
   z0h_z0m_rf

USE switches_urban, ONLY :                                        &
   l_urban2t, l_moruses_rough, l_moruses_storage

USE switches, ONLY : i_modiscopt, iseaz0t, cor_mo_iter

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
use fomod, only : call_fcdch_sea,call_fcdch_land,  &            !Jupp
                  call_sf_flux_land,call_sf_flux_land_max,iloopcount, &   !Jupp
                  softmax,softmin                                  !Luke


IMPLICIT NONE

!real, external :: epsilon !Jupp

INTEGER                                                           &
 row_length                                                       &
                       ! IN Number of X points?
,rows                                                             &
                       ! IN Number of Y points?
,off_x                                                            &
                       ! Size of small halo in i.
,off_y                                                            &
                       ! Size of small halo in j.
,halo_i                                                           &
                       ! Size of halo in i direction.
,halo_j                                                           &
                       ! Size of halo in j direction.
,land_pts                                                         &
                       ! IN No of land points being processed.
,ntiles                                                           &
                       ! IN Number of land tiles per land point.
,land_index(land_pts)                                             &
                       ! IN Index of land points.
,tile_index(land_pts,ntiles)                                      &
                       ! IN Index of tile points.
,tile_pts(ntiles)                                                 &
                       ! IN Number of tile points.
,nsmax                                                            &
                       ! IN Maximum number of snow layers
,nsnow(land_pts,ntiles)                                           &
                       ! IN Number of snow layers
,can_model                                                        &
                       ! IN Switch for thermal vegetation canopy.
,ssi_pts                                                          &
                       ! IN Number of sea and sea-ice points
,sea_pts                                                          &
                       ! IN Number of sea points
,sice_pts                                                         &
                       ! IN Number of sea-ice points
,ssi_index(row_length*rows)                                       &
                       ! IN Index of sea and sea-ice points
,sea_index(row_length*rows)                                       &
                       ! IN Index of sea points
,sice_index(row_length*rows)                                      &
                       ! IN Index of sea-ice points
,formdrag                                                         &
                       ! IN Switch for orographic form drag
,fd_stab_dep           ! IN Switch to implement stability
                       !    dependence of orog form drag


REAL                                                              &
 bq_1(row_length,rows)                                            &
                       ! IN A buoyancy parameter for lowest atm
                       !    level ("beta-q twiddle").
,bt_1(row_length,rows)                                            &
                       ! IN A buoyancy parameter for lowest atm
                       !    level ("beta-T twiddle").
,canhc_tile(land_pts,ntiles)                                      &
                       ! IN Areal heat capacity of canopy for
                       !    land tiles (J/K/m2).
,canopy(land_pts,ntiles)                                          &
                       ! IN Surface water for land tiles
                       !    (kg/m2).
,catch(land_pts,ntiles)                                           &
                       ! IN Surface capacity (max. surface water)
                       !    of land tiles (kg/m2).
,catch_snow(land_pts,ntiles)                                      &
                       ! IN Snow interception capacity of NLT
                       !    tile (kg/m2).
,ds(land_pts,ntiles,nsmax)                                        &
                       ! IN Snow layer thicknesses (m)
,dzsoil                                                           &
                       ! IN Soil or land-ice surface layer
                       !    thickness (m).
,flake(land_pts,ntiles)                                           &
                       ! IN Lake fraction.
,gc(land_pts,ntiles)                                              &
                       ! IN "Stomatal" conductance to evaporation
                       !    for land tiles (m/s).
,hcons(land_pts)                                                  &
                       ! IN Soil thermal conductivity including
                       !    effects of water and ice (W/m/K).
,hcons_snow(land_pts,ntiles)                                      &
                       ! IN Snow thermal conductivity (W/m/K)
,ho2r2_orog(land_pts)                                             &
                       ! IN Peak to trough height of unresolved
                       !    orography divided by 2SQRT(2) (m).
,orog_drag_param                                                  &
                       ! IN Drag coefficient for orographic
                       !    form drag
,fssi(row_length,rows)                                            &
                       ! IN Fraction of gridbox which is
                       !     sea or sea-ice
,sea_frac(ssi_pts)                                                &
                       ! IN Sea fraction
,sice_frac(ssi_pts)                                               &
                       ! IN Sea-ice fraction
,ice_fract(row_length,rows)                                       &
                       ! IN Fraction of gridbox which is sea-ice.
,fland(land_pts)                                                  &
                       ! IN Land fraction on land tiles.
,flandg(row_length,rows)                                          &
                       ! IN Land fraction on all tiles.
,snowdepth(land_pts,ntiles)                                       &
                       ! IN Depth of lying snow (m)
,snow_tile(land_pts,ntiles)                                       &
                       ! IN Lying snow on land tiles (kg/m2).
,pstar(row_length,rows)                                           &
                       ! IN Surface pressure (Pascals).
,qw_1(row_length,rows)                                            &
                       ! IN Total water content of lowest
                       !    atmospheric layer (kg per kg air).
,radnet(row_length,rows)                                          &
                       ! IN Sea-ice net surface radiation (W/m2)
,radnet_tile(land_pts,ntiles)                                     &
                       ! IN Land tile net surface radiation (W/m2)
,anthrop_heat(land_pts,ntiles)                                    &
                       ! IN Anthropogenic Urban heat source (W/m2)
,sil_orog(land_pts)                                               &
                       ! IN Silhouette area of unresolved
                       !    orography per unit horizontal area
,smvcst(land_pts)                                                 &
                       ! IN Volumetric saturation point
                       !    - zero at land-ice points.
,tile_frac(land_pts,ntiles)                                       &
                       ! IN Tile fractions.
,timestep                                                         &
                       ! IN Timestep in seconds for EPDT calc.
,tl_1(row_length,rows)                                            &
                       ! IN Liquid/frozen water temperature for
                       !    lowest atmospheric layer (K).
,ti(row_length,rows)                                              &
                       ! IN Temperature of sea-ice surface layer
                       !    (K)
,ts1(land_pts)                                                    &
                       ! IN Temperature of top soil or land-ice
                       !    layer (K)
,tsnow(land_pts,ntiles,nsmax)                                     &
                       !  IN Snow layer temperatures (K)
,tstar_tile(land_pts,ntiles)                                      &
                       ! IN Tile surface temperatures (K).
,tstar_land(row_length,rows)                                      &
                       ! IN Land mean surface temperature (K).
,tstar_sea(row_length,rows)                                       &
                       ! IN Open sea surface temperature (K).
,tstar_sice(row_length,rows)                                      &
                       ! IN Sea-ice surface temperature (K).
,tstar_ssi(row_length,rows)                                       &
                       ! IN Mean sea surface temperature (K).
,z_land(row_length,rows)                                          &
                       ! IN Land height (m).
,tstar(row_length,rows)                                           &
                       ! IN Gridbox Mean Surface Temperature (K)
,vfrac_tile(land_pts,ntiles)                                      &
                       ! IN Fractional canopy coverage for
                       !    land tiles.
,vshr_land(row_length,rows)                                       &
                       ! IN Magnitude of land sfc-to-lowest-level
                       !    wind shear
,vshr_ssi(row_length,rows)                                        &
                       ! IN Mag. of mean sea sfc-to-lowest-level
                       !    wind shear
,zh(row_length,rows)                                              &
                       ! IN Height above surface of top of
                       !    boundary layer (metres).
,ddmfx(row_length,rows)                                           &
                       ! IN Convective downdraught
                       !    mass-flux at cloud base
,z0_tile(land_pts,ntiles)                                         &
                       ! IN Tile roughness lengths (m).
,z1_uv(row_length,rows)                                           &
                       ! IN Height of lowest uv level (m).
,z1_tq(row_length,rows)                                           &
                       ! IN Height of lowest tq level (m).
                       !    Note, if the grid used is staggered in
                       !    the vertical, Z1_UV and Z1_TQ can be
                       !    different.
,surf_hgt(land_pts,ntiles)                                        &
                       ! IN Height of elevated tile above
                       !    mean gridbox surface (m)
,emis_tile(land_pts,ntiles)                                       &
                       ! IN Emissivity for land tiles
,emis_soil(land_pts)                                              &
                       ! IN Emissivity of underlying soil
,charnock              ! Charnock parameter for sea surface

REAL, INTENT(IN) ::                                               &
 z1_uv_top(row_length, rows)
                       ! Height of top of lowest uv-layer
REAL, INTENT(IN) ::                                               &
 z1_tq_top(row_length, rows)
                       ! Height of top of lowest Tq-layer

REAL                                                              &
 z0h_scm(row_length,rows)                                         &
                         ! IN Namelist input z0h (if >0)
                         !    (if <=0 use Z0HSEA)
                         !    Used in SCM Configurations
,z0m_scm(row_length,rows)! IN Namelist input z0m (if >0)
                         !    (if <=0 use standard Z0MSEA)
                         !    Used in SCM Configurations

LOGICAL                                                           &
 land_mask(row_length,rows)                                       &
                       ! IN .TRUE. for land; .FALSE. elsewhere.
,su10                                                             &
                       ! IN STASH flag for 10-metre W wind.
,sv10                                                             &
                       ! IN STASH flag for 10-metre S wind.
,sq1p5                                                            &
                       ! IN STASH flag for 1.5-metre sp humidity.
,st1p5                                                            &
                       ! IN STASH flag for 1.5-metre temperature.
,sfme                                                             &
                       ! IN STASH flag for wind mixing energy flux
,sz0heff                                                          &
                       ! IN STASH flag for Z0H_EFF
,lq_mix_bl                                                        &
                       ! IN TRUE if mixing ratios used in
                       !    boundary layer code
,ltimer                                                           &
                       ! IN Logical for TIMER.
,l_dust                                                           &
                       ! IN switch for mineral dust
,l_aggregate                                                      &
                       ! IN Logical to set aggregate surface schem
,l_ctile                                                          &
                       ! IN switch for coastal tiling
,l_spec_z0             ! IN T if using prescribed
                       !    sea surface roughness lengths

REAL, INTENT(IN) :: seasalinityfactor
!       Factor allowing for the effect of the salinity of
!       sea water on the evaporative flux.


!  Modified (INOUT) variables.

REAL                                                              &
 z0msea(row_length,rows)
                       ! INOUT Sea-surface roughness length for
                       !       momentum (m).  F617.

!  Output variables.

REAL                                                              &
 alpha1(land_pts,ntiles)                                          &
                       ! OUT Gradients of saturated specific
                       !     humidity with respect to temperature
                       !     between the bottom model layer and
                       !     tile surface
,alpha1_sice(row_length,rows)                                     &
                       ! OUT ALPHA1 for sea-ice.
,ashtf_prime(row_length,rows)                                     &
                       ! OUT Adjusted SEB coefficient for sea-ice
,ashtf_prime_tile(land_pts,ntiles)                                &
                       ! OUT Adjusted SEB coefficient for land
                       !     points
,cd(row_length,rows)                                              &
                       ! OUT Bulk transfer coefficient for
                       !      momentum.
,cd_ssi(row_length,rows)                                          &
                       ! OUT Bulk transfer coefficient for
                       !      momentum over sea mean.
,ch(row_length,rows)                                              &
                       ! OUT Bulk transfer coefficient for heat
                       !     and/or moisture.
,recip_l_mo_sea(row_length,rows)                                  &
                       ! OUT Reciprocal of the Monin-Obukhov
                       !     length for sea/ice points (m^-1).
,ch_ssi(row_length,rows)                                          &
                       ! OUT Bulk transfer coefficient for heat
                       !    and/or moisture over sea mean.
,cdr10m(1-off_x:row_length+off_x,1-off_y:rows+off_y)              &
                       ! OUT Reqd for calculation of 10m wind
                       !     (u & v).
                       !     NBB: This is output on the UV-grid,
                       !     but with the first and last rows set
                       !     to a "missing data indicator".
                       !     Sea-ice leads ignored.
,chr1p5m(land_pts,ntiles)                                         &
                       ! OUT Reqd for calculation of 1.5m temp for
                       !     land tiles.
,chr1p5m_sice(row_length,rows)                                    &
                       ! OUT CHR1P5M for sea and sea-ice
                       !     (leads ignored).
,e_sea(row_length,rows)                                           &
                       ! OUT Evaporation from sea times leads
                       !     fraction (kg/m2/s). Zero over land.
,fme(row_length,rows)                                             &
                       ! OUT Wind mixing energy flux (Watts/sq m).
,fqw_1(row_length,rows)                                           &
                       ! OUT "Explicit" surface flux of QW (i.e.
                       !     evaporation), on P-grid (kg/m2/s).
                       !     for whole grid-box
,fqw_tile(land_pts,ntiles)                                        &
                       ! OUT Local FQW_1 for land tiles.
,fqw_ice(row_length,rows)                                         &
                       ! OUT GBM FQW_1 for sea-ice.
,ftl_1(row_length,rows)                                           &
                       ! OUT "Explicit" surface flux of TL = H/CP.
                       !     (sensible heat / CP). grid-box mean
,ftl_tile(land_pts,ntiles)                                        &
                       ! OUT Local FTL_1 for land tiles.
,ftl_ice(row_length,rows)                                         &
                       ! OUT GBM FTL_1 for sea-ice.
,fraca(land_pts,ntiles)                                           &
                       ! OUT Fraction of surface moisture flux
                       !     with only aerodynamic resistance
                       !     for land tiles.
,h_blend_orog(row_length,rows)                                    &
                       ! OUT Blending height for orographic
                       !     roughness
,h_sea(row_length,rows)                                           &
                       ! OUT Surface sensible heat flux over sea
                       !     times leads fraction (W/m2).
                       !     Zero over land.
,rhostar(row_length,rows)                                         &
                       ! OUT Surface air density
,resfs(land_pts,ntiles)                                           &
                       ! OUT Combined soil, stomatal and
                       !     aerodynamic resistance factor for
                       !     fraction 1-FRACA of land tiles
,resft(land_pts,ntiles)                                           &
                       ! OUT Total resistance factor
                       !     FRACA+(1-FRACA)*RESFS for snow-free
                       !     tiles, 1 for snow and land-ice.
,rib(row_length,rows)                                             &
                       ! OUT Mean bulk Richardson number for
                       !     lowest layer
,rib_tile(land_pts,ntiles)                                        &
                       ! OUT RIB for land tiles.
,fb_surf(row_length,rows)                                         &
                       ! OUT Surface flux buoyancy over
                       !     density (m^2/s^3)
,u_s(row_length,rows)                                             &
                       ! OUT Surface friction velocity (m/s)
,q1_sd(row_length,rows)                                           &
                       ! OUT Standard deviation of turbulent
                       !     fluctuations of surface layer
                       !     specific humidity (kg/kg).
,t1_sd(row_length,rows)                                           &
                       ! OUT Standard deviation of turbulent
                       !     fluctuations of surface layer
                       !     temperature (K).
,z0hssi(row_length,rows)                                          &
                       ! OUT Roughness length for heat and
                       !     moisture over sea/sea ice (m)
,z0h_tile(land_pts,ntiles)                                        &
                       ! OUT Tile roughness lengths for heat
                       !     and moisture
,z0h_eff(row_length,rows)                                         &
                       ! OUT Effective roughness length for
                       !     heat, moisture (m)
,z0m_gb(row_length,rows)                                          &
                       ! OUT Gridbox mean roughness length
                       !     for momentum (m)
,z0mssi(row_length,rows)                                          &
                       ! OUT Roughness length for momentum
                       !     over sea/sea ice (m)
,z0m_tile(land_pts,ntiles)                                        &
                       ! OUT Tile roughness lengths for momentum
,z0m_eff(row_length,rows)                                         &
                       ! OUT Effective roughness length for
                       !     momentum
,rho_aresist(row_length,rows)                                     &
                       ! OUT RHOSTAR*CD_STD*VSHR  for SCYCLE
,aresist(row_length,rows)                                         &
                       ! OUT 1/(CD_STD*VSHR)      for SCYCLE
,resist_b(row_length,rows)                                        &
                       ! OUT (1/CH-1/CD_STD)/VSHR for SCYCLE
,rho_aresist_tile(land_pts,ntiles)                                &
                       ! OUT RHOSTAR*CD_STD*VSHR on land tiles
,aresist_tile(land_pts,ntiles)                                    &
                       ! OUT 1/(CD_STD*VSHR) on land tiles
,resist_b_tile(land_pts,ntiles)                                   &
                       ! OUT (1/CH-1/CD_STD)/VSHR on land tiles
,r_b_dust(row_length,rows,ndiv)                                   &
                       ! OUT surf layer res for dust
,cd_std_dust(row_length,rows)                                     &
                       ! OUT Bulk transfer coef. for
                       !     momentum, excluding orographic effects
,u_s_std_tile(land_pts,ntiles)
                       ! OUT Surface layer scaling velocity
                       !     for tiles excluding orographic
                       !     form drag (m/s).

! Surface exchange coefficients;passed to subroutine IMPL_CAL
REAL                                                              &
 rho_cd_modv1(row_length,rows)                                    &
!                            ! OUT rhostar*cD*vshr before horizontal
!                            !     interpolation output as a diagnostic.
,rhokh_1(land_pts,ntiles)                                         &
!                            ! OUT Surface exchange coefficient for land
!                            !     tiles.
,rhokh_1_sice(row_length,rows)                                    &
!                            ! OUT Surface exchange coefficient for sea
!                            !     or sea-ice.
,rhokm_1(1-off_x:row_length+off_x,1-off_y:rows+off_y)             &
!                            ! OUT For momentum. NB: This is output on
!                            !     UV-grid, but with the first and last
!                            !     rows set to "missing data indicator".
,rhokm_land(1-off_x:row_length+off_x,1-off_y:rows+off_y)          &
!                            ! OUT For land momentum. NB: This is output
!                            !     on UV-grid, but with the first and
!                            !      last rows set to "missing data".
,rhokm_ssi(1-off_x:row_length+off_x,1-off_y:rows+off_y)           &
!                            ! OUT For mean sea mom. NB: This is output
!                            !     on UV-grid, but with the first and
!                            !     last rows set to "missing data".
,rhokpm(land_pts,ntiles)                                          &
!                            ! OUT Mixing coefficient for land tiles.
,dtstar_tile(land_pts,ntiles)                                     &
!                            ! OUT Change in TSTAR over timestep for
!                            !     land tiles
,dtstar(row_length,rows)                                          &
!                            ! OUT Change is TSTAR over timestep for
!                            !     sea-ice
,rhokh_gb(row_length,rows)                                        &
!                            ! OUT Grid-box surface exchange coefficient
,rhokh_mix(row_length,rows)                                       &
!                            ! OUT Exchange coeffs for moisture.
,epot_tile(land_pts,ntiles)  ! OUT EPOT for land tiles.


!   External subprograms called.

!Jupp EXTERNAL qsat_mix,sf_orog,sf_resist,sf_rib,                       &
!Jupp  fcdch,sf_flux,stdev1,sf_orog_gb,sfl_int                          &
!Jupp ,dustresb
!Jupp EXTERNAL timer


!   Define local storage.

!   (a) Workspace.

REAL                                                              &
 qs1(row_length,rows)                                             &
                             ! Sat. specific humidity
!                                  ! qsat(TL_1,PSTAR)
,z0h_gb(row_length,rows)                                          &
                             ! Gridbox mean roughness length
!                                  !   for heat (m)
,rhokm_ssi_nohalo(row_length,rows)                                &
!                                  ! like RHOKM_SSI, but with no halo

,lh0                         ! Latent heat for snow free surface
!                                  !   =LS for sea-ice, =LC otherwise

!  Workspace for sea and sea-ice leads
REAL                                                              &
 cd_sea(row_length,rows)                                          &
                             ! Drag coefficient
,ch_sea(row_length,rows)                                          &
                             ! Transfer coefficient for heat and
!                                  ! moisture
,qstar_sea(row_length,rows)                                       &
                             ! Surface saturated sp humidity
,rib_sea(row_length,rows)                                         &
                             ! Bulk Richardson number
,z0h_sea(row_length,rows)                                         &
                             ! Roughness length for heat and
!                                  ! moisture transport
,z0m_sea(row_length,rows)                                         &
                             ! Open sea roughness length for
!                                  ! momentum transport.
,db_sea(row_length,rows)                                          &
                             ! Buoyancy difference for sea points
,v_s_sea(row_length,rows)                                         &
                             ! Surface layer scaling velocity
,alpha1_sea(row_length*rows)                                      &
                             ! ALPHA1 for sea
,ashtf_prime_sea(row_length*rows)                                 &
!                                  ! Adjusted SEB coefficient for sea
!                                  ! points
,hcons_sea(row_length*rows)                                       &
                             ! Heat conductivity into sea
,u_s_std_sea(row_length*rows)                                     &
                             ! Surface friction velocity for sea
!                                  ! (dummy variable for sea)
,v_s_std_sea(row_length*rows)                                     &
                             ! Surface layer scaling velocity
!                                  ! for sea excluding orographic
!                                  ! form drag (m/s).
!                                  ! (dummy variable for sea)
,cd_std_sea(row_length*rows) ! Local drag coefficient for calc
!                                  ! of interpolation coefficient
!                                  ! (dummy variable for sea)
!                                  ! for sea points (m/s).


!  Workspace for sea-ice and marginal ice zone
REAL                                                              &
 cd_ice(row_length,rows)                                          &
                             ! Drag coefficient
,cd_land(row_length,rows)                                         &
                             ! Bulk transfer coefficient for
!                                  !      momentum over land.
,ch_land(row_length,rows)                                         &
                             ! Bulk transfer coefficient for
!                                  !      het and moisture over land.
,cd_miz(row_length,rows)                                          &
                             ! Drag coefficient
,ch_ice(row_length,rows)                                          &
                             ! Transfer coefficient for heat and
!                                  ! moisture
,ch_miz(row_length,rows)                                          &
                             ! Transfer coefficient for heat and
!                                  ! moisture
,qstar_ice(row_length,rows)                                       &
                             ! Surface saturated sp humidity
,rib_ice(row_length,rows)                                         &
                             ! Bulk Richardson number
,rib_miz(row_length,rows)                                         &
                             ! Bulk Richardson number
,z0_ice(row_length,rows)                                          &
                             ! Roughness length.
,z0_miz(row_length,rows)                                          &
                             ! Roughness length.
,db_ice(row_length,rows)                                          &
                             ! Buoyancy difference for sea ice
,v_s_ice(row_length,rows)                                         &
                             ! Surface layer scaling velocity
!                                  ! for sea ice (m/s).
,v_s_miz(row_length,rows)                                         &
                             ! Surface layer scaling velocity
!                                  ! for marginal sea ice (m/s).
,recip_l_mo_ice(row_length,rows)                                  &
!                                  ! Reciprocal of the Monin-Obukhov
!                                  ! length for sea ice (m^-1).
,recip_l_mo_miz(row_length,rows)                                  &
!                                  ! Reciprocal of the Monin-Obukhov
!                                  ! length for marginal sea ice (m^-1).
,rho_aresist_land(row_length,rows)                                &
!                                  ! Land mean of rho_aresist_tile
,hcons_sice(row_length*rows)                                      &
                             ! Heat conductiviy into sea-ice
,u_s_std_ice(row_length*rows)                                     &
                             ! Surface friction velocity for sea-i
!                                  ! (dummy variable for sea-ice)
,u_s_std_miz(row_length*rows)                                     &
                             ! Surface friction velocity for
!                                  ! marginal sea-ice
!                                  ! (dummy variable for marginal sea-ic
,v_s_std_ice(row_length*rows)                                     &
                             ! Surface layer scaling velocity
!                                  ! for sea-ice excluding orographic
!                                  ! form drag (m/s).
!                                  ! (dummy variable for sea-ice)
,v_s_std_miz(row_length*rows)                                     &
                             ! Surface layer scaling velocity
!                                  ! for marginal sea-ice excluding
!                                  ! orographic form drag (m/s).
!                                  ! (dummy variable for marginal sea-ic
,cd_std_ice(row_length*rows)                                      &
                             ! Local drag coefficient for calc
!                                  ! of interpolation coefficient
!                                  ! (dummy variable for sea-ice)
,cd_std_miz(row_length*rows)                                      &
                             ! Local drag coefficient for calc
!                                  ! of interpolation coefficient
!                                  ! (dummy variable for marginal sea-ic
,epot_sea(row_length*rows)                                        &
                             ! Potential evaporation from
                             ! sea surface
                             ! (dummy variable for sea surface)
,epot_ice(row_length*rows)   ! Potential evaporation from sea-ice
                             ! (dummy variable for sea-ice)


REAL                                                              &
 z1_tq_sea(row_length,rows)
!                            ! Height of lowest model level
!                            ! relative to sea.
REAL, ALLOCATABLE ::                                              &
 z1_tq_top_sea(:,:)
                             ! Top of lowest Tq layer over sea

!  Workspace for land tiles
REAL                                                              &
 cd_std(land_pts,ntiles)                                          &
                             ! Local drag coefficient for calc
!                                  ! of interpolation coefficient
,cd_tile(land_pts,ntiles)                                         &
                             ! Drag coefficient
,ch_tile(land_pts,ntiles)                                         &
                             ! Transfer coefficient for heat and
!                                  ! moisture
,chn(land_pts,ntiles)                                             &
                             ! Neutral value of CH.
,ce_d_sh(land_pts)                                                &
                             ! Exposure coefficient * diffusivity
!                                  ! of water vapour * Sherwood number
!                                  ! for canopy snow (m^2/s).
,dq(land_pts)                                                     &
                             ! Sp humidity difference between
!                                  ! surface and lowest atmospheric lev
,epdt(land_pts)                                                   &
                             ! "Potential" Evaporation * Timestep
,fz0(row_length,rows)                                             &
                             ! Aggregation function for Z0.
,fz0h(row_length,rows)                                            &
                             ! Aggregation function for Z0H.
,pstar_land(land_pts)                                             &
                             ! Surface pressure for land points.
,qstar_tile(land_pts,ntiles)                                      &
                             !Surface saturated sp humidity.
,rhokh_can(land_pts,ntiles)                                       &
                             ! Exchange coefficient for canopy
!                                  ! air to surface
,rhokm_1_tile(land_pts,ntiles)                                    &
!                                  ! Momentum exchange coefficient.
,tsurf(land_pts)                                                  &
                             ! Surface layer temp (snow or soil) (
,dzsurf(land_pts)                                                 &
                             ! Surface layer thickness
!                                  ! (snow or soil) (m)
,dzssi(row_length*rows)                                           &
                             ! Surface layer thickness
!                                  ! (sea or sea-ice) (m)
,hcons_surf(land_pts)                                             &
                             ! Thermal conductivity
!                                  ! (snow or soil)  (W/m/K)
,wind_profile_factor(land_pts,ntiles)                             &
!                                  ! For transforming effective surface
!                                  ! transfer coefficients to those
!                                  ! excluding form drag.
,z0m_gb_land(land_pts)                                            &
                             ! GBM momentum land roughness length
,z0h_gb_land(land_pts)                                            &
                             ! GBM land roughness length for heat
,z0m_eff_tile(land_pts,ntiles)                                    &
!                                  ! Effective momentum roughness length
,z0_urban(land_pts,ntiles)                                        &
!                                  ! MORUSES - material r.l. for momentum
,db_tile(land_pts,ntiles)                                         &
                             ! Buoyancy difference for surface
!                                  ! tile
,v_s_tile(land_pts,ntiles)                                        &
                             ! Surface layer scaling velocity
!                                  ! for tiles (m/s).
,v_s_std(land_pts,ntiles)                                         &
!                                  ! Surface layer scaling velocity
!                                  ! for tiles excluding orographic
!                                  ! form drag (m/s).
,u_s_iter_tile(land_pts,ntiles)                                   &
!                                  ! Scaling velocity from middle of
!                                  ! MO scheme - picked up in error by
!                                  ! dust code!
,vshr(row_length,rows)                                            &
!                                  ! Level 1 to surface wind shear
,recip_l_mo_tile(land_pts,ntiles)                                 &
!                                  ! Reciprocal of the Monin-Obukhov
!                                  ! length for tiles (m^-1).
,t_elev(land_pts,ntiles)                                          &
                             ! Temperature at elevated height (k)
,q_elev(land_pts,ntiles)                                          &
                             ! Specific humidity at elevated
!                                  !     height (kg per kg air)
,qs1_elev(land_pts,ntiles)                                        &
                             ! Saturated specific humidity at elev
!                                  !     height (kg per kg air)
,scaling_urban(land_pts,ntiles)
                             ! MORUSES: ground heat flux scaling;
                             ! canyon tile only coupled to soil


! dummy arrays required for sea and se-ice to create universal
! routines for all surfaces
REAL                                                              &
 array_zero(row_length*rows)                                      &
                                ! Array of zeros
,array_one(row_length*rows)                                       &
                                ! Array of ones
,array_negone(row_length*rows)  ! Array of minus ones

INTEGER                                                           &
 array_zero_int(row_length*rows)    ! Array of zeros


!   (b) Scalars.

INTEGER                                                           &
 i,j                                                              &
             ! Loop counter (horizontal field index).
,k                                                                &
             ! Loop counter (tile field index).
,l                                                                &
             ! Loop counter (land point field index).
,n                                                                &
             ! Loop counter (tile index).
,idiv                                                             &
             ! Loop counter (dust division).
,jits        ! Counter for iteration for Z0H
REAL                                                              &
 tau                                                              &
             ! Magnitude of surface wind stress over sea.
,zetam                                                            &
             ! Temporary in calculation of CHN.
,zetah                                                            &
             ! Temporary in calculation of CHN.
,zeta1                                                            &
             ! Work space
,z0                                                               &
             ! yet more workspace
,ustr_l                                                           &
             ! Low-wind estimate of friction velocity
,ustr_n                                                           &
             ! Neutral estimate of friction velocity
,tol_ustr_n                                                       &
             ! Tolerance for USTR_N
,tol_ustr_l                                                       &
             ! Tolerance for USTR_L (see below)
,bl_stress_grad
             ! Stress gradient across boundary layer

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

IF (lhook) CALL dr_hook('SF_EXCH',zhook_in,zhook_handle)

IF (ltimer) THEN
! DEPENDS ON: timer
  CALL timer('SFEXCH  ',3)
END IF

      cd_std      = 0. !Jupp
      cd_tile      = 0. !Jupp
      ch_tile      = 0. !Jupp
      chn      = 0. !Jupp
      ce_d_sh      = 0. !Jupp
      dq      = 0. !Jupp
      epdt      = 0. !Jupp
      fz0      = 0. !Jupp
      fz0h      = 0. !Jupp
      pstar_land      = 0. !Jupp
      qstar_tile      = 0. !Jupp
      rhokh_can      = 0. !Jupp
      rhokm_1_tile      = 0. !Jupp
      tsurf      = 0. !Jupp
      dzsurf      = 0. !Jupp
      dzssi      = 0. !Jupp
      hcons_surf      = 0. !Jupp
      wind_profile_factor      = 0. !Jupp
      z0m_gb_land      = 0. !Jupp
      z0h_gb_land      = 0. !Jupp
      z0m_eff_tile      = 0. !Jupp
      z0_urban      = 0. !Jupp
      db_tile      = 0. !Jupp
      v_s_tile      = 0. !Jupp
      v_s_std      = 0. !Jupp
      u_s_iter_tile      = 0. !Jupp
      vshr      = 0. !Jupp
      recip_l_mo_tile      = 0. !Jupp
      t_elev      = 0. !Jupp
      q_elev      = 0. !Jupp
      qs1_elev      = 0. !Jupp
      scaling_urban      = 0. !Jupp



      db_tile      = 0. !Jupp
      fraca        = 0. !Jupp
      resfs        = 0. !Jupp
      resft        = 0. !Jupp
      rhokh_1_sice = 0. !Jupp
      rhokh_1      = 0. !Jupp
      rhokpm       = 0. !Jupp
      cd_tile      = 0. !Jupp
      ch_tile      = 0. !Jupp
      cd_std       = 0. !Jupp
      cd_sea       = 0. !Jupp
      ch_sea       = 0. !Jupp
      cd_ice       = 0. !Jupp
      ch_ice       = 0. !Jupp
      cd_miz       = 0. !Jupp
      ch_miz       = 0. !Jupp
      v_s_sea      = 0. !Jupp
      v_s_ice      = 0. !Jupp
      v_s_miz      = 0. !Jupp
      v_s_tile     = 0. !Jupp
      v_s_std      = 0. !Jupp
      recip_l_mo_sea = 0. !Jupp
      recip_l_mo_ice = 0. !Jupp
      recip_l_mo_miz = 0. !Jupp
      recip_l_mo_tile = 0. !Jupp


array_zero(:)=0.0
array_one(:)=1.0
array_negone(:)=-1.0
array_zero_int(:)=0

! MORUSES Initialise urban roughness array
z0_urban(:,:) = 0.0

DO j=1,rows
 DO i=1,row_length
  rhostar(i,j) = pstar(i,j) / ( r*tstar(i,j) )
!                        ... surface air density from ideal gas equation
 END DO
END DO


!-----------------------------------------------------------------------
! Land surface calculations
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
!  0. Initialise FTL_TILE and RIB_TILE on all tiles at all points,
!     to allow STASH to process these as diagnostics.
!-----------------------------------------------------------------------
DO n=1,ntiles
  DO l=1,land_pts
    ftl_tile(l,n) = 0.0
    rib_tile(l,n) = 0.0
    z0m_tile(l,n) = 0.0
    u_s_std_tile(l,n)=0.0
  END DO
END DO

DO j=1,rows
 DO i=1,row_length
  rhokm_land(i,j) = 0.
 END DO
END DO

!-----------------------------------------------------------------------
!!  2.  Calculate QSAT values required later.
!-----------------------------------------------------------------------


! Calculate temeprature and specific humidity for elevation bands

! DEPENDS ON: elevate
CALL elevate(                                                     &
 row_length,rows,land_pts,ntiles,tile_pts,land_index,tile_index,  &
 tl_1,qw_1,qs1,pstar,surf_hgt,t_elev,q_elev,                      &
 ltimer                                                           &
 )

! DEPENDS ON: qsat_mix
CALL qsat_mix(qs1,tl_1,pstar,row_length*rows,lq_mix_bl)
DO l=1,land_pts
  j=(land_index(l)-1)/row_length + 1
  i = land_index(l) - (j-1)*row_length
  pstar_land(l) = pstar(i,j)
END DO
DO n=1,ntiles
! DEPENDS ON: qsat_mix
  CALL qsat_mix(qstar_tile(:,n),tstar_tile(:,n),                  &
            pstar_land,land_pts,lq_mix_bl)
! DEPENDS ON: qsat_mix
  CALL qsat_mix(qs1_elev(:,n),t_elev(:,n),                        &
            pstar_land,land_pts,lq_mix_bl)
END DO

!-----------------------------------------------------------------------
!!  3. Calculation of transfer coefficients and surface layer stability
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!!  3.1 Calculate neutral roughness lengths
!-----------------------------------------------------------------------

! Land tiles
! Z0_TILE contains the appropriate value for land-ice points, but has to
! be modified for snow-cover on non-land-ice points
!Jupp
!$TAF LOOP = PARALLEL
DO n=1,ntiles
!Jupp
!$TAF LOOP = PARALLEL
  DO k=1,tile_pts(n)
    l = tile_index(k,n)
    IF ( snow_tile(l,n).gt.0. .AND.                               &
         smvcst(l) > EPSILON(smvcst(l)) ) THEN
      z0 = z0_tile(l,n) - 0.1 * snowdepth(l,n)
      zeta1 = MIN( 5.0e-4 , z0_tile(l,n)  )
!      zeta1 = softmin( 5.0e-4 , z0_tile(l,n),1.0  ) !Luke
      z0m_tile(l,n) = MAX( zeta1 , z0 )
!      z0m_tile(l,n) = softmax( zeta1 , z0 ,1.0) !Luke
    ELSE
      z0m_tile(l,n) = z0_tile(l,n)
    END IF

! MORUSES: z0m_tile (z0_tile previously set in sparm) reset with ztm
! (calculated in init_urban) to remove effects of snow. MORUSES does not
! yet contain a parametrisation for snow although snow should not affect
! the behaviour of bluff bodies rather it affects the  material roughness
! of the road & roof
! Note: The test on smvcst is used to alter just the roof tile and not the
!       land-ice tiles as the tile is shared at the moment. This will not
!       be required when flexible tiles are introduced.

    IF ( .NOT. l_aggregate                                        &
       .AND. l_moruses_rough                                      &
       .AND. smvcst(l) > EPSILON(smvcst)                          &
       .AND. ( n == urban_canyon .OR. n == urban_roof ) ) THEN
! Snow could be added here to the material roughness length for
! momentum before passing to urbanz0. Only the road & roof should be
! affected as the walls will be essentially snow-free
      z0_urban(l,n) = z0m_mat
      z0m_tile(l,n) = ztm(l)
    END IF

    z0h_tile(l,n) = z0h_z0m(n)*z0m_tile(l,n)
    rib_tile(l,n) = 0.
    db_tile(l,n) = 0.
    wind_profile_factor(l,n)=1.0
    z0m_eff_tile(l,n)=z0m_tile(l,n)
  END DO
END DO

! MORUSES: urbanz0 calculates r.l. for heat and updates z0h_tile, which is
! previously set above. Two calls are required; one for urban_canyon and one
! for urban_roof. It is done this way to remove the need for do loop over tile
! type and to avoid over-writing any land-ice tiles. If l_aggregate roughness
! lengths are already calculated: z0m in sparm & z0h above.
IF ( .NOT. l_aggregate ) THEN
  IF ( l_moruses_rough ) THEN
    n = urban_canyon
    DO k = 1,tile_pts(n)
      l = tile_index(k,n)
      j = ( land_index(l) - 1 ) / row_length + 1
      i = land_index(l) - ( j - 1 ) * row_length
      ! DEPENDS ON: urbanz0
      CALL urbanz0(                                                     &
         n, z1_uv(i,j), z1_tq(i,j), hgt(l), hwr(l), disp(l),            &
         z0m_mat, z0_urban(l,n), ztm(l),                                &
         z0h_tile(l,n) )
      ! DEPENDS ON: urbanz0
      CALL urbanz0(                                                     &
         urban_roof, z1_uv(i,j), z1_tq(i,j), hgt(l), hwr(l), disp(l),   &
         z0m_mat, z0_urban(l,urban_roof), ztm(l),                       &
         z0h_tile(l,urban_roof) )
    END DO

  ELSE IF ( l_urban2t .AND. .NOT. l_moruses_rough ) THEN
    ! Set values for URBAN-2T
    n = urban_canyon
    DO k = 1,tile_pts(n)
      l = tile_index(k,n)
      z0h_tile(l,n)          = z0h_z0m_c  * z0m_tile(l,n)
      z0h_tile(l,urban_roof) = z0h_z0m_rf * z0m_tile(l,urban_roof)
    END DO
  END IF

END IF


! Calculate orographic effective parameter for neutral conditions
! if using orographic roughness scheme
IF(formdrag ==  effective_z0) THEN
!Jupp
!$TAF LOOP = PARALLEL
  DO n=1,ntiles
! DEPENDS ON: sf_orog
    CALL sf_orog (                                                &
     row_length,rows,land_pts,tile_pts(n),                        &
     land_index,tile_index(:,n),                                  &
     fd_stab_dep,orog_drag_param,ltimer,                          &
     ho2r2_orog,rib_tile(:,n),sil_orog,z0m_tile(:,n),z1_uv,       &
     wind_profile_factor(:,n),z0m_eff_tile(:,n)                   &
     )
  END DO
END IF

!-----------------------------------------------------------------------
! Calculate RESFT with neutral CH and EPDT=0 for use in calculation
! of Richardson number. RESFT=1 for snow and land-ice.
!-----------------------------------------------------------------------

!Jupp
!$TAF LOOP = PARALLEL
DO n=1,ntiles
!Jupp
!$TAF LOOP = PARALLEL
  DO k=1,tile_pts(n)
    l = tile_index(k,n)
    j=(land_index(l)-1)/row_length + 1
    i = land_index(l) - (j-1)*row_length
    zetam = LOG ( (z1_uv(i,j) + z0m_tile(l,n))/z0m_tile(l,n) )
    zetah = LOG ( (z1_tq(i,j) + z0m_tile(l,n))/z0h_tile(l,n) )
    chn(l,n) = (vkman/zetah)*(vkman/zetam)* &
      wind_profile_factor(l,n)
    dq(l) = qw_1(i,j) - qstar_tile(l,n)
    epdt(l) = 0.0
  END DO

! DEPENDS ON: sf_resist
  CALL sf_resist (                                                &
   row_length,rows,land_pts,tile_pts(n),                          &
   land_index,tile_index(:,n),                                    &
   canopy(:,n),catch(:,n),chn(:,n),dq,epdt,flake(:,n),gc(:,n),    &
   snowdepth(:,n),vshr_land,fraca(:,n),resfs(:,n),resft(:,n),     &
   ltimer                                                         &
   )
END DO

! RESFT < 1 for snow on canopy if canopy snow model used
! N.B. chn is calculated in the preceding loop over tiles and
! used in the next loop over npft. This works only if the
! first npft tiles are the vegetated ones.
IF (.NOT. l_aggregate .AND. can_model.eq.4) THEN
!Jupp
!$TAF LOOP = PARALLEL
  DO n=1,npft
    IF ( cansnowtile(n) ) THEN
!Jupp
!$TAF LOOP = PARALLEL
      DO k=1,tile_pts(n)
        l = tile_index(k,n)
        j=(land_index(l)-1)/row_length + 1
        i = land_index(l) - (j-1)*row_length
        IF (snow_tile(l,n) .gt. 0.) THEN
          gc(l,n) = 0.06*snow_tile(l,n)**0.6*catch_snow(l,n)**0.4 &
                       * 2.06e-5*(tm/tstar_tile(l,n))**1.75       &
                       * (1.79 + 3*SQRT(vshr_land(i,j)))          &
                       / (2*rho_ice*5e-4**2)
          fraca(l,n) = 0.
          resfs(l,n) = gc(l,n)/(gc(l,n) + chn(l,n)*vshr_land(i,j))
          resft(l,n) = resfs(l,n)
        END IF
      END DO
    END IF
  END DO
END IF

!-----------------------------------------------------------------------
!  3.2 Calculate bulk Richardson number for the lowest model level.
!-----------------------------------------------------------------------

! Land tiles
!Jupp
!$TAF LOOP = PARALLEL
DO n=1,ntiles
! DEPENDS ON: sf_rib
  CALL sf_rib (                                                   &
   row_length,rows,land_pts,tile_pts(n),                          &
   land_index,tile_index(:,n),                                    &
   bq_1,bt_1,qstar_tile(:,n),q_elev(:,n),resft(:,n),t_elev(:,n),  &
   tstar_tile(:,n),vshr_land,z0h_tile(:,n),z0m_tile(:,n),         &
   z1_tq,z1_uv,                                                   &
   rib_tile(:,n),db_tile(:,n),ltimer                              &
   )
END DO

!-----------------------------------------------------------------------
!!  3.3 Calculate stability corrected effective roughness length.
!!  Stability correction only applies to land points.
!-----------------------------------------------------------------------

IF(formdrag ==  effective_z0) THEN
!Jupp
!$TAF LOOP = PARALLEL
  DO n=1,ntiles
! DEPENDS ON: sf_orog
    CALL sf_orog (                                                &
     row_length,rows,land_pts,tile_pts(n),                        &
     land_index,tile_index(:,n),                                  &
     fd_stab_dep,orog_drag_param,ltimer,                          &
     ho2r2_orog,rib_tile(:,n),sil_orog,z0m_tile(:,n),z1_uv,       &
     wind_profile_factor(:,n),z0m_eff_tile(:,n)                   &
     )
  END DO
END IF

!-----------------------------------------------------------------------
!!  3.4 Calculate CD, CH via routine FCDCH.
!-----------------------------------------------------------------------

! Land tiles
!Jupp
!$TAF LOOP = PARALLEL
DO n=1,ntiles
! DEPENDS ON: fcdch
  CALL fcdch (                                                    &
   row_length,rows,cor_mo_iter,land_pts,tile_pts(n),              &
   tile_index(:,n),land_index,                                    &
   db_tile(:,n),vshr_land,                                        &
   z0m_eff_tile(:,n),z0h_tile(:,n),zh,                            &
   z1_uv,z1_uv_top,z1_tq,z1_tq_top,wind_profile_factor(:,n),      &
   ddmfx,                                                         &
   cd_tile(:,n),ch_tile(:,n),cd_std(:,n),                         &
   v_s_tile(:,n),v_s_std(:,n),recip_l_mo_tile(:,n),               &
   u_s_iter_tile(:,n),                                            &
   ltimer                                                         &
   )
END DO

IF ( cor_mo_iter == use_correct_ustar ) THEN
!       Use correct "standard" ustar
  DO n=1,ntiles 
    DO k=1,tile_pts(n) 
      l = tile_index(k,n) 
      u_s_std_tile(l,n) = v_s_std(l,n)
    END DO
  END DO
ELSE
  !       Use ustar from mid-iteration
  DO n=1,ntiles 
    DO k=1,tile_pts(n) 
      l = tile_index(k,n) 
      u_s_std_tile(l,n) = u_s_iter_tile(l,n)
    END DO
  END DO
END IF

!-----------------------------------------------------------------------
!!  4.1 Recalculate RESFT using "true" CH and EPDT for land tiles
!-----------------------------------------------------------------------

!Jupp
!$TAF LOOP = PARALLEL
DO n=1,ntiles
!Jupp
!$TAF LOOP = PARALLEL
  DO k=1,tile_pts(n)
    l = tile_index(k,n)
    j=(land_index(l)-1)/row_length + 1
    i = land_index(l) - (j-1)*row_length
    dq(l) = qw_1(i,j) - qstar_tile(l,n)
    epdt(l) = - rhostar(i,j)*ch_tile(l,n)*vshr_land(i,j)          &
      *dq(l)*timestep
  END DO
! DEPENDS ON: sf_resist

!Jupp
!$TAF STORE fraca,resfs,resft  = tape_n, REC = iloopcount
!Jupp

  CALL sf_resist (                                                &
   row_length,rows,land_pts,tile_pts(n),                          &
   land_index,tile_index(:,n),                                    &
   canopy(:,n),catch(:,n),ch_tile(:,n),dq,epdt,flake(:,n),        &
   gc(:,n),snowdepth(:,n),vshr_land,fraca(:,n),                   &
   resfs(:,n),resft(:,n),                                         &
   ltimer)
END DO

IF (.NOT. l_aggregate .AND. can_model.eq.4) THEN
!Jupp
!$TAF LOOP = PARALLEL
  DO n=1,npft
    IF ( cansnowtile(n) ) THEN
!Jupp
!$TAF LOOP = PARALLEL
      DO k=1,tile_pts(n)
        l = tile_index(k,n)
        j=(land_index(l)-1)/row_length + 1
        i = land_index(l) - (j-1)*row_length
        IF (snow_tile(l,n) .gt. 0.) THEN
          fraca(l,n) = 0.
          resfs(l,n) = gc(l,n) /                                  &
                  (gc(l,n) + ch_tile(l,n)*vshr_land(i,j))
          resft(l,n) = resfs(l,n)
        END IF
      END DO
    END IF
  END DO
END IF

!-----------------------------------------------------------------------
! Calculate gridbox-means of transfer coefficients.
!-----------------------------------------------------------------------

DO j=1,rows
 DO i=1,row_length
  cd_land(i,j) = 0.
  ch_land(i,j) = 0.
 END DO
END DO

! Land tiles
DO n=1,ntiles
  DO k=1,tile_pts(n)
    l = tile_index(k,n)
    j=(land_index(l)-1)/row_length + 1
    i = land_index(l) - (j-1)*row_length
    cd_land(i,j) = cd_land(i,j) + tile_frac(l,n)*cd_tile(l,n)
    ch_land(i,j) = ch_land(i,j) + tile_frac(l,n)*ch_tile(l,n)
  END DO
END DO

!-----------------------------------------------------------------------
!!  4.3 Calculate the surface exchange coefficients RHOK(*) and
!       resistances for use in Sulphur Cycle
!       (Note that CD_STD, CH and VSHR should never = 0)
!     RHOSTAR * CD * VSHR stored for diagnostic output before
!     horizontal interpolation.
!-----------------------------------------------------------------------

! Land tiles
DO n=1,ntiles
  DO k=1,tile_pts(n)
    l = tile_index(k,n)
    j=(land_index(l)-1)/row_length + 1
    i = land_index(l) - (j-1)*row_length
    rhokm_1_tile(l,n) = rhostar(i,j)*cd_tile(l,n)*vshr_land(i,j)
!                                                         ! P243.124
    rhokm_land(i,j) = rhokm_land(i,j) +                           &
           tile_frac(l,n)*rhokm_1_tile(l,n)
    rhokh_1(l,n) = rhostar(i,j)*ch_tile(l,n)*vshr_land(i,j)
!                                                         ! P243.125
  END DO
END DO

!-----------------------------------------------------------------------
!!  Calculate local and gridbox-average surface fluxes of heat and
!!  moisture.
!-----------------------------------------------------------------------

DO j=1,rows
  DO i=1,row_length
    ftl_1(i,j) = 0.
    fqw_1(i,j) = 0.
  END DO
END DO

! Land tiles
DO n=1,ntiles
  DO l = 1,land_pts
    ftl_tile(l,n) = 0.
    fqw_tile(l,n) = 0.
  END DO
END DO

! Adjust ASHTF for sens. heat flux to ground beneath coniferous canopy
rhokh_can(:,:)=0.0
IF (.NOT. l_aggregate .AND. can_model.eq.4) THEN
  DO n=1,npft
    IF ( cansnowtile(n) ) THEN
      DO k=1,tile_pts(n)
        l = tile_index(k,n)
        j=(land_index(l)-1)/row_length + 1
        i = land_index(l) - (j-1)*row_length
        rhokh_can(l,n) = rhostar(i,j) * cp /                      &
                     (43. / (SQRT(cd_tile(l,n))*vshr_land(i,j)))
      END DO
    END IF
  END DO
END IF

! Initialise scaling_urban to 1.0 so that it only affects urban tiles when
! MORUSES used with no aggregation.
scaling_urban(:,:) = 1.0
IF ( .NOT. l_aggregate .AND. l_moruses_storage ) THEN
  n = urban_canyon
  DO l = 1, land_pts
    IF ( tile_frac(l,n) > 0.0 ) THEN
      scaling_urban(l,n) =                                          &
         ( tile_frac(l,n) + tile_frac(l,urban_roof) ) /             &
         tile_frac(l,n)
    END IF
  END DO
END IF

!Jupp
!$TAF LOOP = PARALLEL
DO n=1,ntiles
  lh0=lc
!Jupp
!$TAF LOOP = PARALLEL
  DO l = 1,land_pts
    j=(land_index(l)-1)/row_length + 1
    i = land_index(l) - (j-1)*row_length
    tsurf(l) = ts1(l) + t_elev(l,n) - tl_1(i,j)
    dzsurf(l) = dzsoil
    hcons_surf(l) = hcons(l)
    IF ((nsmax > 0).AND.(nsnow(l,n) > 0)) THEN
      tsurf(l) = tsnow(l,n,1)
      dzsurf(l) = ds(l,n,1)
      hcons_surf(l) = hcons_snow(l,n)
    END IF
  END DO
! DEPENDS ON: sf_flux
  CALL sf_flux (                                                  &
   row_length,rows,land_pts,tile_pts(n),flandg,                   &
   land_index,tile_index(:,n),                                    &
   nsnow(:,n),n,canhc_tile(:,n),dzsurf,hcons_surf,                &
   qs1_elev(:,n),qstar_tile(:,n),q_elev(:,n),                     &
   radnet_tile(:,n),resft(:,n),rhokh_1(:,n),smvcst,               &
   snowdepth(:,n),tile_frac(:,n),timestep,t_elev(:,n),tsurf,      &
   tstar_tile(:,n),vfrac_tile(:,n),rhokh_can(:,n),z0h_tile(:,n),  &
   z0m_eff_tile(:,n),z1_tq,lh0,emis_tile(:,n),emis_soil,          &
   1.0,anthrop_heat(:,n),scaling_urban(:,n),                      &
   fqw_1,ftl_1,                                                   &
   alpha1(:,n),ashtf_prime_tile(:,n),fqw_tile(:,n),               &
   epot_tile(:,n),ftl_tile(:,n),                                  &
   dtstar_tile(:,n),ltimer,0.0                                    &
 )
END DO



!-----------------------------------------------------------------------
!  4.4   Calculate the standard deviations of layer 1 turbulent
!        fluctuations of temperature and humidity using approximate
!        formulae from first order closure.
!-----------------------------------------------------------------------

DO j=1,rows
  DO i=1,row_length
    q1_sd(i,j) = 0.
    t1_sd(i,j) = 0.
  END DO
END DO

! Land tiles
DO n=1,ntiles
! DEPENDS ON: stdev1
  CALL stdev1 (                                                   &
   row_length,rows,land_pts,tile_pts(n),                          &
   land_index,tile_index(:,n),flandg,                             &
   bq_1,bt_1,fqw_tile(:,n),ftl_tile(:,n),rhokm_1_tile(:,n),       &
   rhostar,vshr_land,z0m_tile(:,n),z1_tq,tile_frac(:,n),          &
   q1_sd,t1_sd,ltimer                                             &
   )
END DO


!-----------------------------------------------------------------------
!! Call SFL_INT to calculate CDR10M and CHR1P5M - interpolation coeffs
!! used to calculate screen temperature, humidity and 10m winds.
!-----------------------------------------------------------------------

DO j=1,rows
  DO i=1,row_length
    cdr10m(i,j) = 0.
  END DO
END DO

! Land tiles
IF (su10 .OR. sv10 .OR. sq1p5 .OR. st1p5) THEN
  DO n=1,ntiles
! DEPENDS ON: sfl_int
    CALL sfl_int (                                                &
     row_length,rows,off_x,off_y,land_pts,tile_pts(n),            &
     tile_index(:,n),land_index,flandg,                           &
     vshr_land,cd_std(:,n),cd_tile(:,n),ch_tile(:,n),             &
     tile_frac(:,n),                                              &
     z0m_eff_tile(:,n),z0m_tile(:,n),z0h_tile(:,n),               &
     recip_l_mo_tile(1,n),                                        &
     v_s_tile(:,n),v_s_std(:,n),                                  &
     z1_uv,z1_tq,db_tile(:,n),                                    &
     su10,sv10,st1p5,sq1p5,                                       &
     cdr10m,chr1p5m(:,n),ltimer                                   &
     )
  END DO

END IF



!-----------------------------------------------------------------------
! Sea and sea-ice surface calculations
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!  Calculate height of lowest model level relative to sea.
!-----------------------------------------------------------------------

z1_tq_sea = z1_tq
IF (l_ctile) THEN
  WHERE( (flandg(:,:) > 0.0) .AND. (flandg(:,:) < 1.0) )
    z1_tq_sea(:,:)     = z1_tq(:,:) + z_land(:,:)
  END WHERE
ENDIF
IF (i_modiscopt == on) THEN
  ALLOCATE(z1_tq_top_sea(row_length,rows))
  z1_tq_top_sea = 0 !Jupp
  z1_tq_top_sea = z1_tq_top
  IF (l_ctile) THEN
    WHERE( (flandg(:,:) > 0.0) .AND. (flandg(:,:) < 1.0) )
      z1_tq_top_sea(:,:) = z1_tq_top(:,:) + z_land(:,:)
    END WHERE
  ENDIF
ELSE
! Default allocation
  ALLOCATE(z1_tq_top_sea(1,1))
  z1_tq_top_sea = 0 !
ENDIF

! DEPENDS ON: qsat_mix
CALL qsat_mix(qstar_sea,tstar_sea,pstar,row_length*rows           &
 ,lq_mix_bl)
! DEPENDS ON: qsat_mix
CALL qsat_mix(qstar_ice,tstar_sice,pstar,row_length*rows          &
 ,lq_mix_bl)


! Sea, sea-ice leads, sea-ice and marginal ice zone
hcons_sea(:)    = kappas
hcons_sice(:)   = kappai

!Jupp
!$TAF LOOP = PARALLEL
DO j=1,rows
!Jupp
!$TAF LOOP = PARALLEL
  DO i=1,row_length
    z0h_sea(i,j) = z0hsea
    z0_miz(i,j) = z0miz
    z0_ice(i,j) = z0sice
    rib_sea(i,j) = 0.
    rib_ice(i,j) = 0.
    db_sea(i,j) = 0.
    db_ice(i,j) = 0.
    rhokm_ssi(i,j) = 0.
    rhokm_ssi_nohalo(i,j) = 0.
    cd_ssi(i,j) = 0.
    ch_ssi(i,j) = 0.
    ftl_ice(i,j)=0.
    fqw_ice(i,j)=0.
    h_sea(i,j)=0.
    e_sea(i,j)=0.
  END DO
END DO


IF (iseaz0t == surfdivz0t) THEN

!       Composite formulation for thermal roughness lengths,
!       incoporating the smooth aerodynamic limit for low
!       wind speeds and a value based on surface divergence
!       theory for higher wind speeds.

!       The friction velocity is diagnosed in the surface
!       transfer scheme, using z0m from the previous time-step.
!       z0[T,q] is also required but depends on u_* in this
!       scheme. For practical purposes, it is sufficient to
!       infer it from u_* determined from z0m, but in general
!       a given value of z0m corresponds to two values of
!       u_*, so we need to know whether we are on the low or
!       high wind speed side of the minimum value of z0m.
!       If we are on the high side, z0[T,q] will be inversely
!       proportional to z0m, but on the low side it may follow
!       this relationship, or be aerodynamically smooth. In
!       the smooth case we iterate u_* from the expression for
!       the roughness length and then take the maximum of the
!       smooth and high-wind expressions for z0[T,q]. An
!       iteration for the low-wind value of u_*, USTR_L is
!       carried out. This will converge to the correct limit only
!       on the low-wind side of the minimum, and the standard
!       criterion that the gradient of a fixed-point iteration
!       should be less than 1 in modulus gievs a more precise
!       boundary, TOL_USTR_L. For consistency with earlier versions
!       of the modset, hard-wired values are retained for the
!       operational value of Charnock's parameter. An additional
!       check is required, since z0m can be large at low or at
!       high wind-speeds. This is less precise and a fixed
!       value of 0.07 is used to test USTR_N, which was determined
!       by inspection of a graph of z0m against u_*: it is
!       unlikely that this will need to be changed unless
!       Charnock's constant is greatly altered.

  IF (charnock == 0.018) THEN
    tol_ustr_l = 0.055
    tol_ustr_n = 0.07
  ELSE
    tol_ustr_l = 0.75*(1.54e-6*g/(2.0*charnock))**0.33333
    tol_ustr_n = 0.07
  END IF

!Jupp
!$TAF LOOP = PARALLEL
  DO j=1, rows
!Jupp
!$TAF LOOP = PARALLEL
    DO i=1, row_length
!           We need to infer u_* from Z0M.
      IF (vshr_ssi(i,j)  > 0.0) THEN
!             Compute u_* using neutral stratification.
!             stratification.
        ustr_n = vkman * vshr_ssi(i,j) /                          &
          LOG(1.0 + z1_uv(i,j) / z0msea(i,j) )
!             Compute u_* using low wind approximation.
        ustr_l = 1.54e-06 /  z0msea(i,j) - 1.0e-05
!             Since Z0M could be large for low and high u_*, we use
!             USTR_N as an additional check on the regime.
        IF ( (ustr_n < tol_ustr_n) .AND.                          &
             (ustr_l < tol_ustr_l) ) THEN
!               Iterate u_* for low winds.
          DO jits=1, 5
!Luke
!$TAF STORE ustr_l=tape_sfexch,REC=(iloopcount-1)*rows*row_length*5+(j-1)*row_length*5+(i-1)*5+jits
!Luke
            ustr_l=1.54e-06/(z0msea(i,j)-(charnock/g)*ustr_l**2)  &
              -1.0e-05
          END DO
!               Take the maximum of the smooth and high-wind values.
!               A lower limit is imposed on the friction velocity to
!               allow for the exceptional case of very low winds: the
!               value of 10^-5 is the same as the limit for the momentum
!               roughness length.
          z0h_sea(i,j) = MAX( 2.52e-6/(ustr_l+1.0e-05),           &
            2.56e-9/z0msea(i,j) )
!          z0h_sea(i,j) = softmax( 2.52e-6/(ustr_l+1.0e-05),           & !Luke
!            2.56e-9/z0msea(i,j) ,1.0)                                   !Luke
        ELSE
!               Take the high-wind value, but limit it to the molecular
!               mean free path (we should not hit this limit
!               in practice).
          z0h_sea(i,j) = MAX( 2.56e-9/z0msea(i,j), 7.0e-08 )
!          z0h_sea(i,j) = softmax( 2.56e-9/z0msea(i,j), 7.0e-08 ,1.0) !Luke
        END IF
      END IF
    END DO
  END DO

ELSE IF (iseaz0t == fixed_z0t) THEN

!       Use a fixed thermal roughness length.
  z0h_sea(1:row_length,1:rows) = z0hsea

END IF


IF ( l_spec_z0 ) THEN
   DO j = 1, rows
     DO i = 1, row_length
       IF (z0h_scm(i,j)  >   0.0) THEN

        ! Set Z0H from SCM namelist
        ! (if specified) for sea points
        z0h_sea(i,j) = z0h_scm(i,j)

       END IF  ! Z0H_SCM
     END DO  ! I
   END DO  ! J
END IF

!-----------------------------------------------------------------------
!!  3.2 Calculate bulk Richardson number for the lowest model level.
!-----------------------------------------------------------------------

! Sea, sea-ice and sea-ice leads
! DEPENDS ON: sf_rib
CALL sf_rib (                                                     &
 row_length,rows,ssi_pts,sea_pts,                                 &
 ssi_index,sea_index,                                             &
 bq_1,bt_1,qstar_sea,qw_1,array_one,tl_1,                         &
 tstar_sea,vshr_ssi,z0h_sea,z0msea,                               &
 z1_tq,z1_uv,                                                     &
 rib_sea,db_sea,ltimer                                            &
 )

! DEPENDS ON: sf_rib
CALL sf_rib (                                                     &
 row_length,rows,ssi_pts,sice_pts,                                &
 ssi_index,sice_index,                                            &
 bq_1,bt_1,qstar_ice,qw_1,array_one,tl_1,                         &
 tstar_sice,vshr_ssi,z0_ice,z0_ice,                               &
 z1_tq,z1_uv,                                                     &
 rib_ice,db_ice,ltimer                                            &
 )

!-----------------------------------------------------------------------
!!  3.4 Calculate CD, CH via routine FCDCH.
!-----------------------------------------------------------------------

! DEPENDS ON: fcdch
CALL fcdch (                                                      &
   row_length,rows,cor_mo_iter,ssi_pts,sice_pts,                  &
   sice_index,ssi_index,                                          &
   db_ice,vshr_ssi,                                               &
   z0_ice,z0_ice,zh,                                              &
   z1_uv,z1_uv_top,z1_tq_sea,z1_tq_top_sea,array_one,             &
   ddmfx,                                                         &
   cd_ice,ch_ice,cd_std_ice,                                      &
   v_s_ice,v_s_std_ice,recip_l_mo_ice,                            &
   u_s_std_ice,                                                   &
   ltimer                                                         &
   )


! DEPENDS ON: fcdch
CALL fcdch (                                                      &
   row_length,rows,cor_mo_iter,ssi_pts,sice_pts,                  &
   sice_index,ssi_index,                                          &
   db_ice,vshr_ssi,                                               &
   z0_miz,z0_miz,zh,                                              &
   z1_uv,z1_uv_top,z1_tq_sea,z1_tq_top_sea,array_one,             &
   ddmfx,                                                         &
   cd_miz,ch_miz,cd_std_miz,                                      &
   v_s_miz,v_s_std_miz,recip_l_mo_miz,                            &
   u_s_std_miz,                                                   &
   ltimer                                                         &
   )


! DEPENDS ON: fcdch
CALL fcdch (                                                      &
   row_length,rows,cor_mo_iter,ssi_pts,sea_pts,                   &
   sea_index,ssi_index,                                           &
   db_sea,vshr_ssi,                                               &
   z0msea,z0h_sea,zh,                                             &
   z1_uv,z1_uv_top,z1_tq_sea,z1_tq_top_sea,array_one,             &
   ddmfx,                                                         &
   cd_sea,ch_sea,cd_std_sea,                                      &
   v_s_sea,v_s_std_sea,recip_l_mo_sea,                            &
   u_s_std_sea,                                                   &
   ltimer                                                         &
   )

! z1_tq_top_sea is no longer required. 
DEALLOCATE(z1_tq_top_sea) 


! Sea and sea-ice
!Jupp
!$TAF LOOP = PARALLEL
DO j=1,rows
!Jupp
!$TAF LOOP = PARALLEL
  DO i=1,row_length
    IF (flandg(i,j).lt.1.0 ) THEN
      IF ( ice_fract(i,j) .lt. 0.7 ) THEN
        cd_ssi(i,j) = ( ice_fract(i,j)*cd_miz(i,j) +              &
              (0.7-ice_fract(i,j))*cd_sea(i,j) ) / 0.7  ! P2430.5
        ch_ssi(i,j) = ( ice_fract(i,j)*ch_miz(i,j) +              &
              (0.7-ice_fract(i,j))*ch_sea(i,j) ) / 0.7  ! P2430.4
       ELSE
         cd_ssi(i,j) = ( (1.0-ice_fract(i,j))*cd_miz(i,j) +       &
              (ice_fract(i,j)-0.7)*cd_ice(i,j) ) / 0.3  ! P2430.7
         ch_ssi(i,j) = ( (1.0-ice_fract(i,j))*ch_miz(i,j) +       &
              (ice_fract(i,j)-0.7)*ch_ice(i,j) ) / 0.3  ! P2430.7
       END IF
    END IF

  END DO
END DO

! Sea and sea-ice
!Jupp
!$TAF LOOP = PARALLEL
DO j=1,rows
!Jupp
!$TAF LOOP = PARALLEL
  DO i=1,row_length
    IF ( flandg(i,j).lt.1.0 ) THEN
      rhokm_ssi(i,j) = rhostar(i,j)*cd_ssi(i,j)*vshr_ssi(i,j)
!                                                          ! P243.124
      rhokm_ssi_nohalo(i,j) = rhokm_ssi(i,j)
      rhokh_1_sice(i,j) = rhostar(i,j)*ch_ssi(i,j)*vshr_ssi(i,j)
!                                                           ! P243.125
    ELSE
      rhokm_ssi(       i,j) = 0.0
      rhokm_ssi_nohalo(i,j) = 0.0
      rhokh_1_sice(    i,j) = 0.0
    END IF

  END DO
END DO


! Sea and sea-ice
lh0=lc
dzssi(:) = dzsea
! DEPENDS ON: sf_flux
CALL sf_flux (                                                    &
   row_length,rows,ssi_pts,sea_pts,fssi,                          &
   ssi_index,sea_index,                                           &
   array_zero_int,0,array_zero,dzssi,hcons_sea,                   &
   qs1,qstar_sea,qw_1,                                            &
   radnet,array_one,rhokh_1_sice,array_negone,array_zero,         &
   sea_frac,timestep,tl_1,tstar_sea,tstar_sea,                    &
   array_zero,array_zero,z0h_sea,z0msea,z1_tq,lh0,                &
   array_one,array_one,                                           &
   seasalinityfactor,array_zero,array_one,                        &
   fqw_1,ftl_1,                                                   &
   alpha1_sea,ashtf_prime_sea,e_sea,epot_sea,h_sea,               &
   dtstar,ltimer,1.0                                              &
 )

lh0=ls
dzssi(:)=de
! DEPENDS ON: sf_flux
CALL sf_flux (                                                    &
   row_length,rows,ssi_pts,sice_pts,fssi,                         &
   ssi_index,sice_index,                                          &
   array_zero_int,0,array_zero,dzssi,hcons_sice,                  &
   qs1,qstar_ice,qw_1,                                            &
   radnet,array_one,rhokh_1_sice,array_negone,array_zero,         &
   sice_frac,timestep,tl_1,ti,tstar_sice,                         &
   array_zero,array_zero,z0_ice,z0_ice,z1_tq,lh0,                 &
   array_one,array_one,                                           &
   1.0,array_zero,array_one,                                      &
   fqw_1,ftl_1,                                                   &
   alpha1_sice,ashtf_prime,fqw_ice,epot_ice,ftl_ice,              &
   dtstar,ltimer,0.0                                              &
 )


! DEPENDS ON: stdev1
CALL stdev1 (                                                     &
   row_length,rows,ssi_pts,sea_pts,                               &
   ssi_index,sea_index,fssi,                                      &
   bq_1,bt_1,e_sea,h_sea,rhokm_ssi_nohalo,                        &
   rhostar,vshr_ssi,z0msea,z1_tq_sea,sea_frac,                    &
   q1_sd,t1_sd,ltimer                                             &
   )

! DEPENDS ON: stdev1
CALL stdev1 (                                                     &
   row_length,rows,ssi_pts,sice_pts,                              &
   ssi_index,sice_index,fssi,                                     &
   bq_1,bt_1,fqw_ice,ftl_ice,rhokm_ssi_nohalo,                    &
   rhostar,vshr_ssi,z0_ice,z1_tq_sea,sice_frac,                   &
   q1_sd,t1_sd,ltimer                                             &
   )

!-----------------------------------------------------------------------
!!  4.6 For sea points, calculate the wind mixing energy flux and the
!!      sea-surface roughness length on the P-grid, using time-level n
!!      quantities.
!-----------------------------------------------------------------------

!Jupp
!$TAF LOOP = PARALLEL
DO j=1,rows
!Jupp
!$TAF LOOP = PARALLEL
 DO i=1,row_length
  fme(i,j) = 0.0
  IF (flandg(i,j).lt.1.0) THEN
    tau = rhokm_ssi(i,j) * vshr_ssi(i,j)             ! P243.130
    IF (ice_fract(i,j) .gt. 0.0)                                  &
      tau = rhostar(i,j) * cd_sea(i,j)                            &
        * vshr_ssi(i,j) * vshr_ssi(i,j)

    IF (sfme)                                                     &
      fme(i,j) = (1.0-ice_fract(i,j)) * tau * SQRT(tau/rhosea)
!                                                            ! P243.96
! Limit Z0MSEA to 0.154m for TAU very small
    IF( rhostar(i,j) > EPSILON(0.0))THEN
      z0msea(i,j) = 1.54e-6 / (SQRT(tau/rhostar(i,j)) + 1.0e-5)   &
                 +  (charnock/g) * (tau / rhostar(i,j))
      z0msea(i,j) = MAX ( z0hsea , z0msea(i,j) )
!      z0msea(i,j) = softmax ( z0hsea , z0msea(i,j) ,1.0) !Luke
!                                       ... P243.B6 (Charnock formula)
!                    TAU/RHOSTAR is "mod VS squared", see eqn P243.131
    ELSE
      z0msea(i,j) = z0hsea
    END IF
  END IF

 END DO
END DO


IF ( l_spec_z0 ) THEN
! Check for prescribed surface roughness lengths specified in SCM
! NAMELIST.  If specified in the &INPROF then they will be used
! instead of Model calculated values
  DO j=1,rows
  DO i=1,row_length
    IF ( z0m_scm(i,j) > 0.0 ) THEN
      ! Set z0m from SCM namelist for sea points
      z0msea(i,j)  = z0m_scm(i,j)
    END IF
    IF ( z0h_scm(i,j) > 0.0 ) THEN
      ! Set z0h from SCM namelist for sea points
      z0h_sea(i,j) = z0h_scm(i,j)
    END IF
  END DO
  END DO
END IF

!-----------------------------------------------------------------------
! If sea ice is present then set RECIP_L_MO_SEA to its value over
! the ice, a long-standing choice for the screen diagnostics.
! Note that RECIP_L_MO_SEA is also used in BDY_EXPL2 to diagnose
! shear-dominated boundary layer types.  To preserve bit
! reproducibility when screen diagnostics are switched on,
! this change has been moved outside the if-test on stash logicals
!-----------------------------------------------------------------------
!Jupp
!$TAF LOOP = PARALLEL
DO j=1,rows
!Jupp
!$TAF LOOP = PARALLEL
  DO i=1,row_length
    IF (flandg(i,j) <  1.0 .AND. ice_fract(i,j) >  0. ) THEN
      recip_l_mo_sea(i,j) = recip_l_mo_ice(i,j)
    END IF
  END DO
END DO


! Sea and sea-ice (leads ignored)
IF (su10 .OR. sv10 .OR. sq1p5 .OR. st1p5) THEN
! DEPENDS ON: sfl_int
  CALL sfl_int (                                                  &
     row_length,rows,off_x,off_y,ssi_pts,sea_pts,                 &
     sea_index,ssi_index,fssi,                                    &
     vshr_ssi,cd_std_sea,cd_sea,ch_sea,                           &
     sea_frac,                                                    &
     z0msea,z0msea,z0h_sea,                                       &
     recip_l_mo_sea,                                              &
     v_s_sea,v_s_std_sea,                                         &
     z1_uv,z1_tq_sea,db_sea,                                      &
     su10,sv10,st1p5,sq1p5,                                       &
     cdr10m,chr1p5m_sice,ltimer                                   &
     )

! DEPENDS ON: sfl_int
  CALL sfl_int (                                                  &
     row_length,rows,off_x,off_y,ssi_pts,sice_pts,                &
     sice_index,ssi_index,fssi,                                   &
     vshr_ssi,cd_std_ice,cd_ice,ch_ice,                           &
     sice_frac,                                                   &
     z0_ice,z0_ice,z0_ice,                                        &
     recip_l_mo_ice,                                              &
     v_s_ice,v_s_std_ice,                                         &
     z1_uv,z1_tq_sea,db_ice,                                      &
     su10,sv10,st1p5,sq1p5,                                       &
     cdr10m,chr1p5m_sice,ltimer                                   &
     )
END IF



!-----------------------------------------------------------------------
! GBM diagnstic calculations
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Calculate effective roughness lengths, orographic blending heights
! and gridbox-average Richardson numbers.
!-----------------------------------------------------------------------

!Jupp
!$TAF LOOP = PARALLEL
DO j=1,rows
!Jupp
!$TAF LOOP = PARALLEL
  DO i=1,row_length
    fz0(i,j) = 0.
    fz0h(i,j) = 0.
    rib(i,j) = 0.
    h_blend_orog(i,j) = h_blend_min
  END DO
END DO

!Jupp
!$TAF LOOP = PARALLEL
DO j=1,rows
!Jupp
!$TAF LOOP = PARALLEL
  DO i=1,row_length
    rib(i,j) = rib(i,j) +                                         &
               (1.0-flandg(i,j))*(1.0-ice_fract(i,j))*rib_sea(i,j)
    fz0(i,j) = fz0(i,j) + (1.0-flandg(i,j))*(1.0-ice_fract(i,j))  &
                    / (LOG(lb/z0msea(i,j))**2)
    fz0h(i,j) = fz0h(i,j) +                                       &
                    (1.0-flandg(i,j))*(1.0-ice_fract(i,j)) /      &
                    (LOG(lb/z0msea(i,j))*LOG(lb/z0hsea))
    rib(i,j) = rib(i,j) +                                         &
               (1.0-flandg(i,j))*ice_fract(i,j)*rib_ice(i,j)
    fz0(i,j) = fz0(i,j) + (1.0-flandg(i,j))*ice_fract(i,j) /      &
                    (LOG(lb/z0_ice(i,j))**2)
    fz0h(i,j) = fz0h(i,j) + (1.0-flandg(i,j))*ice_fract(i,j) /    &
                    (LOG(lb/z0_ice(i,j))*LOG(lb/z0_ice(i,j)))
  END DO
END DO

DO n=1,ntiles
  DO k=1,tile_pts(n)
    l = tile_index(k,n)
    j=(land_index(l)-1)/row_length + 1
    i = land_index(l) - (j-1)*row_length
    rib(i,j) = rib(i,j) + flandg(i,j)*tile_frac(l,n)*rib_tile(l,n)
    fz0(i,j) = fz0(i,j)                                           &
      + flandg(i,j)*tile_frac(l,n) / (LOG(lb/z0m_tile(l,n))**2)
    fz0h(i,j) = fz0h(i,j) + flandg(i,j)*tile_frac(l,n) /          &
                    (LOG(lb/z0m_tile(l,n))*LOG(lb/z0h_tile(l,n)))
  END DO
END DO

!Jupp
!$TAF LOOP = PARALLEL
DO j=1,rows
!Jupp
!$TAF LOOP = PARALLEL
  DO i=1,row_length
    z0m_gb(i,j) = lb * EXP( - SQRT(1./fz0(i,j)) )
    z0h_gb(i,j) = lb * EXP( - SQRT(fz0(i,j)) / fz0h(i,j))
    z0m_eff(i,j) = z0m_gb(i,j)
    z0h_eff(i,j) = z0h_gb(i,j)
  END DO
END DO

DO l = 1,land_pts
  j=(land_index(l)-1)/row_length + 1
  i = land_index(l) - (j-1)*row_length
  z0m_gb_land(l) = z0m_gb(i,j)
  z0h_gb_land(l) = z0h_gb(i,j)
END DO


IF(formdrag ==  effective_z0) THEN
! DEPENDS ON: sf_orog_gb
  CALL sf_orog_gb(                                                &
   row_length,rows,land_pts,land_index,                           &
   land_mask,fd_stab_dep,orog_drag_param,                         &
   ho2r2_orog,rib,sil_orog,z0m_gb_land,z1_uv,                     &
   h_blend_orog,z0m_eff,sz0heff,z0h_gb_land,z0h_eff,ltimer        &
   )
 END IF


!-----------------------------------------------------------------------
! Calculate gridbox-means of transfer coefficients.
!-----------------------------------------------------------------------

DO j=1,rows
  DO i=1,row_length
    cd(i,j) = flandg(i,j)*cd_land(i,j) +                          & 
                       (1.-flandg(i,j))*cd_ssi(i,j)
    ch(i,j) = flandg(i,j)*ch_land(i,j) +                          & 
                       (1.-flandg(i,j))*ch_ssi(i,j)
  END DO
END DO

!-----------------------------------------------------------------------
!! Set grid-box surface exchange coefficients
!-----------------------------------------------------------------------
DO j=1,rows
 DO i=1,row_length
  rhokm_1(i,j)= flandg(i,j) * rhokm_land(i,j) +                   &
                     (1.0-flandg(i,j)) * rhokm_ssi(i,j)
  rho_cd_modv1(i,j) = rhokm_1(i,j)  ! diagnostic required for VAR
 END DO
END DO


DO j=1,rows
  DO i=1,row_length
    IF( flandg(i,j) .lt. 1.0) THEN
      rhokh_gb(i,j) = (1.0 - flandg(i,j))*rhokh_1_sice(i,j)
    ELSE
      rhokh_gb(i,j) = 0.0
    END IF
  END DO
END DO

DO n=1,ntiles
  DO k=1,tile_pts(n)
    l = tile_index(k,n)
    j=(land_index(l)-1)/row_length + 1
    i = land_index(l) - (j-1)*row_length
    rhokh_gb(i,j) = rhokh_gb(i,j)                                 &
      + flandg(i,j)*tile_frac(l,n)*rhokh_1(l,n)
  END DO
END DO


!-----------------------------------------------------------------------
!! Calculate scaling parameters required for non-local BL scheme
!-----------------------------------------------------------------------

DO j=1,rows
  DO i=1,row_length
    u_s(i,j) = SQRT(                                              &
       flandg(i,j)*cd_land(i,j)*vshr_land(i,j)*vshr_land(i,j)     &
       +(1.-flandg(i,j))*cd_ssi(i,j)*vshr_ssi(i,j)*vshr_ssi(i,j)  &
                    )
!   !------------------------------------------------------
!   ! Limit the explicitly calculated surface stress,
!   ! used to scale the non-local parametrizations,
!   ! such that the implied stress gradient across the BL
!   ! is less than Max_Stress_Grad.
!   !------------------------------------------------------
    bl_stress_grad = u_s(i,j)*u_s(i,j) / zh(i,j)
    IF (bl_stress_grad > max_stress_grad) THEN
      u_s(i,j) = SQRT(zh(i,j) * max_stress_grad)
    END IF

    fb_surf(i,j) = g * ( bt_1(i,j)*ftl_1(i,j) +                   &
                     bq_1(i,j)*fqw_1(i,j) ) / rhostar(i,j)
  END DO
END DO

! Calculate parameters required for aerosols
! DEPENDS ON: sf_aero
CALL sf_aero (                                                    &
 row_length,rows,land_pts,ntiles,land_index,tile_index,tile_pts,  &
 ltimer,l_dust,                                                   &
 flandg,tile_frac,pstar,rhostar,tstar,vshr_land,vshr_ssi,         &
 cd_ssi,ch_ssi,cd_std,ch_tile,rhokh_gb,                           &
 rho_aresist,aresist,resist_b,rho_aresist_tile,aresist_tile,      &
 resist_b_tile,r_b_dust,cd_std_dust,rhokh_mix                     &
 )

! set these roughness lengths which otherwise are unspecified
z0mssi(:,:) = z0msea( :,:)
z0hssi(:,:) = z0h_sea(:,:)


IF (ltimer) THEN
! DEPENDS ON: timer
  CALL timer('SFEXCH  ',4)
END IF

IF (lhook) CALL dr_hook('SF_EXCH',zhook_out,zhook_handle)
RETURN
END
