!###############################################################################
!###############################################################################
! program jules
! Main program unit for JULES.

subroutine jules                                    !Jupp

  USE inout, ONLY :  &
!  imported scalars with intent(out)
    print_step, echo, nts                         !Jupp

  USE output_mod, ONLY :  &
!  imported procedures
    output

  USE spin_mod, ONLY :  &
!  imported scalars with intent(inout)
    ispin,spinUp

  USE time_loc, ONLY :   &
!  imported scalars with intent(out)
    date,time

  USE time_mod, ONLY :   &
!  imported procedures
    s_to_chhmmss

  use nstypes, only : urban

  use snow_param, only : ds

  USE trifctl, ONLY :  &
!  imported scalars with intent(out)
    asteps_since_triffid, cv, g_leaf_day, g_leaf_dr_out, resp_s_dr_out, lai_phen, lit_c, & !Jupp
    lit_c_mn,c_veg,npp_ft_acc, &
! edithere begins                                  !Jupp
!                                                  !Jupp
! user may want to use different variables here    !Jupp
    npp, resp_s , resp_p                           !Jupp
  USE fluxes, ONLY :  &                            !Jupp
     ftl_1,latent_heat,tstar                       !Jupp
! edithere ends                                    !Jupp

  USE update_mod, ONLY :  &
!  imported procedures
   drive_update,veg_update

  USE veg_io_vars, ONLY :  &
!  imported scalars with intent(out)
    vegVaryT

  use switches
  use prognostics                                                                                       !Jupp
  use drive_io_vars                                                                                     !Jupp
  use observation                                                                                       !Jupp
    use aero, only : aresist, aresist_tile,resist_b_tile,rho_aresist_tile                               !Jupp
  use spin_mod, only : spinend,spinvalold                                                               !Jupp
  use time_loc, only : datenext,endyear,ijulian,stepflag,timenext,utc_time_secs,newMonth                         !Jupp/Luke
  use trifctl, only : g_leaf_acc,g_leaf_phen_acc,gpp_ft,npp,npp_dr_out,npp_ft,npp_ft_acc,resp_s, &      !Jupp
       resp_p_ft,resp_s_acc,resp_w_dr_out,resp_w_ft,resp_w_ft_acc,gpp                                       !Jupp/Luke
  use veg_io_vars, only : notnextveg,vegdatain,vegdatastep,vegdatastepmax,vegdate,vegfile,vegfiledate,& !Jupp
       vegfilename,vegfilestep,vegfiletime,vegresetstep,vegresetstepprev,vegtime,vegunit,&              !Jupp
       vegupdatestep,vegupdatestepmax                                                                   !Jupp
  use ancil_info, only : frac,tile_index,tile_pts                                                       !Jupp
  use file_utils, only : irecprev,nunitmax                                                              !Jupp
  use p_s_parms, only : catch,catch_snow,cosz,infil_tile,sthf,sthu,z0_tile                              !Jupp
  use coastal, only : flandg,surf_ht_flux_land,tstar_land,tstar_sice,tstar_ssi                          !Jupp
  use fluxes, only : alb_tile,e_sea,ecan_tile,ei_tile,esoil,esoil_tile,ext,fqw_1,fqw_ice,fqw_tile,&     !Jupp
       ftl_1,ftl_ice,ftl_tile,h_sea,land_albedo,&                                                       !Jupp
       le_tile,melt_tile,radnet_tile,surf_roff,tot_tfall,tstar                                          !Jupp
  use top_pdm, only : dun_roff,fsat,gamtot,qbase,zw, sthzw , qbase                                      !Jupp
  use pftparm, only : rootd_ft                                                                          !Jupp
  use u_v_grid, only : u_0_p,u_1_p,v_0_p,v_1_p                                                          !Jupp
  use route_mod, only : routecount,routestep                                                            !Jupp
  use forcing, only : con_rain,con_snow,ls_rain,ls_snow,pstar,qw_1,sw_down,lw_down,tl_1,u_0,u_1,v_0,v_1 !Jupp
  use fomod, only :  iloopcount,nloopcount,spin_a_step,modts,monthstarts                                            !Jupp/Luke
  use surf_param, only : q10_leaf , diff_frac                                                           !Jupp
  use ozone_vars, only : o3
  use pftparm, only : nl0, alpha  

  use inout, only : nts, ts_bias, ts_scal

!-------------------------------------------------------------------------------

  IMPLICIT NONE

!  character(len=10), external :: s_to_chhmmss !Jupp

  INTEGER ::   &!  local SCALARS
    a_step      !  timestep counter

  LOGICAL ::  &!  local SCALARS
    endRun    !  TRUE at end of last timestep in run

  CHARACTER(len=10) ::  time_hms  ! Time in the form "hh:mm:ss H", used as a local variable
!                                 ! that is required because some compilers can not cope
!                                 ! with recursive write statements

!   real modts(nts)       !Jupp
   integer its              ! Jupp work variable


!-------------------------------------------------------------------------------
! Call the initialisation routine.
!-------------------------------------------------------------------------------
!Jupp  CALL init

!-------------------------------------------------------------------------------
! Loop over timesteps.
! Note that the number of timesteps is of unknown length at the start of run,
! if the model is to determine when it has spun up.
!-------------------------------------------------------------------------------

  a_step = 0
  endRun     = .false.        !Jupp
  do iloopcount=1,nloopcount  !Jupp
!Jupp
    if (echo) write(*,*) 'jules line 115: ',iloopcount
!$TAF STORE &
!$TAF& aresist,aresist_tile,asteps_since_triffid,canht_ft,canopy,catch,catch_snow,cs,c_veg, &
!$TAF& cv,date,datenext,drivedata,drivedatain,drivedatastep,drivedate, &
!$TAF& drivefile,drivefiledate,drivefilename,drivefileonunit,drivefilestep,drivefiletime,driveresetstep,driveresetstepprev,     &
!$TAF& drivetime,driveunit,ds,dun_roff,ecan_tile,ei_tile,esoil,esoil_tile,ext,flandg,fqw_1,fqw_ice,fqw_tile,frac,fsat,ftl_1,    &
!$TAF& ftl_ice,g_leaf_day,g_leaf_acc,g_leaf_phen_acc,g_leaf_dr_out,gamtot,gc,gpp_ft,gs,ijulian,infil_tile,irecprev,ispin,lai,   &
!$TAF& l_q10,lai_phen,lit_c,lit_c_mn,melt_tile,notnextdrive,notnextveg,npp,nsnow,npp_ft,npp_dr_out,npp_ft_acc,                  &
!$TAF& qbase,radnet_tile,resist_b_tile,resp_p_ft,resp_s_acc,resp_s_dr_out,resp_w_ft,resp_w_dr_out,resp_w_ft_acc,rgrain,rgrainl,&
!$TAF& rho_aresist_tile,rho_snow_grnd,rootd_ft,smc,smcl,snow_grnd,snow_tile,spinend,spinup,spinvalold,sthf,sthu,                &
!$TAF& sice,sliq,snowdepth,surf_ht_flux_land,surf_roff,sw_down,sthzw,tsnow,t_soil,ti,tile_index,tile_pts,timenext,tl_1,tstar,   &
!$TAF& tstar_land,tstar_sice,tstar_ssi,tstar_tile,u_1,utc_time_secs,v_1,vegdatain,vegdatastep,vegdatastepmax,vegdate,vegfile,   &
!$TAF& tot_tfall,urban,vegfiledate,vegfilename,vegfilestep,vegfiletime,vegresetstep,vegresetstepprev,vegtime,vegunit,           &
!$TAF& vegupdatestep,vegupdatestepmax,z0_tile,z0msea,zw,spin_a_step = tape_n, REC = iloopcount
!Jupp

!-------------------------------------------------------------------------------
!   Generate output if this is required at the start of a time period (i.e.
!   rarely).  The logical argument (endCall) is always false at this call.
!-------------------------------------------------------------------------------
    CALL output( a_step,.FALSE. )

!-------------------------------------------------------------------------------
!   Increment timestep counters.
!-------------------------------------------------------------------------------
   a_step = iloopcount                                             !Jupp
   if (endRun) print*,'122: nloopcount probably wrong!',nloopcount !Jupp 
    ASTEPS_SINCE_TRIFFID=ASTEPS_SINCE_TRIFFID+1

    IF ( MOD(a_step,print_step)==0 .OR. a_step==1 ) THEN
      time_hms = s_to_chhmmss( time )
      IF ( spinUp ) THEN
        if (echo) then                                              !Jupp
        WRITE(*,"(a,i7,tr2,a,i8,tr1,a,a,i3)") 'timestep:',a_step  &
                   ,'Start date and time: '  &
                    ,date,time_hms,' Spin up cycle: ',ispin
        end if                                                      !Jupp
      ELSE
        if (echo) then                                              !Jupp
        WRITE(*,"(a,i7,tr2,a,i8,tr1,a)") 'timestep:',a_step  &
                  ,'Start date and time: ',date,time_hms
        end if                                                      !Jupp
      ENDIF
    ENDIF

!-------------------------------------------------------------------------------
!   Update meteorological data.  2nd argument (next) is TRUE to indicate that
!   the "next" data in file are to be used.
!-------------------------------------------------------------------------------
    if (echo) write(*,*) 'jules line 167, about to call drive_update...'
    CALL drive_update( a_step,.TRUE.,-1 )                           !Jupp
    if (echo) write(*,*) 'jules line 169, finished call to drive_update'
!Jupp
    if (echo) write(*,*) 'jules line 171: ',iloopcount
!$TAF STORE &
!$TAF& asteps_since_triffid,canht_ft,canopy,catch,catch_snow,con_rain,con_snow,cs,date, &
!$TAF& datenext,diff_frac,drivedata,drivedatain,drivedatastep,drivedate,drivedateinit,drivefile,drivefiledate,drivefilename,&
!$TAF& drivefileonunit,drivefilestep,drivefiletime,drivetemplatet,drivetime,drivetimeinit,driveresetstep,driveresetstepprev,    &
!$TAF& drivetime,driveunit,endrun, endyear,flandg,fqw_tile,frac,fsat,     &
!$TAF& g_leaf_acc,g_leaf_phen_acc,gamtot,gc,gs,ijulian,infil_tile,irecprev,ispin,lai,ls_rain,ls_snow,lw_down,         &
!$TAF& notnextdrive,notnextveg,npp_dr_out,npp_ft_acc,o3,pstar,qw_1,resp_s_acc,resp_w_dr_out,resp_w_ft_acc,rgrain,rootd_ft,      &
!$TAF& routestore,smcl,snow_grnd,snow_tile,spinend,spinup,spinvalold,stepflag,sthf,sthu,surf_roff,sw_down,t_soil,ti,tile_index, &
!$TAF& tile_pts,  &
!$TAF& time,timenext,tl_1,tstar,tstar_land,tstar_sice,tstar_ssi,tstar_tile,u_0,u_1,u_1_p,u_0_p,utc_time_secs,v_0,     &
!$TAF& v_0_p,v_1,     &
!$TAF& v_1_p,vegdatain,vegdatastep,vegdatastepmax,vegdate,vegfile,vegfiledate,vegfilename,vegfilestep,vegfiletime,vegresetstep, &
!$TAF& vegresetstepprev,vegtime,vegunit,vegupdatestep,vegupdatestepmax,z0_tile,z0msea,zw &
!$TAF& = tape_n, REC = iloopcount
!Jupp    
    
!-------------------------------------------------------------------------------
!   Update prescribed vegetation fields.  2nd argument (next) is TRUE to
!   indicate that the "next" data in file are to be used.
!-------------------------------------------------------------------------------

    IF ( vegVaryT ) CALL veg_update( a_step,.TRUE.,-1 )            !Jupp

!Jupp
    if (echo) write(*,*) 'jules line 191: ',iloopcount
!$TAF STORE &
!$TAF& asteps_since_triffid,canht_ft,canopy,catch,catch_snow,cs,date, &
!$TAF& datenext,drivedata,drivedatain,drivedatastep,drivedate,drivefile,drivefiledate,drivefilename,drivefileonunit,   &
!$TAF& drivefilestep,drivefiletime,driveresetstep,driveresetstepprev,drivetime,driveunit,endrun, endyear,flandg,fqw_tile,frac,  &
!$TAF& fsat,     &
!$TAF& g_leaf_acc,g_leaf_phen_acc,gamtot,gc,gs, &
!$TAF& ijulian,infil_tile,irecprev,ispin,lai,l_q10,notnextdrive,notnextveg,         &
!$TAF& npp_dr_out,npp_ft_acc,qbase,resp_s_acc,resp_w_dr_out,resp_w_ft_acc,rgrain,rootd_ft,routecount,routestore,routestep,      &
!$TAF& smcl,snow_grnd,snow_tile,spinend,&
!$TAF& spinup,spinvalold,stepflag,sthf,sthu,sw_down,t_soil,ti,tile_index,tile_pts,time,timenext,tl_1,tstar,tstar_land,          &
!$TAF& tstar_sice,tstar_ssi,tstar_tile,u_1,utc_time_secs,v_1,vegdatain,vegdatastep,vegdatastepmax,vegdate,vegfile,vegfiledate,  &
!$TAF& vegfilename,vegfilestep,vegfiletime,vegresetstep,vegresetstepprev,vegtime,vegunit,vegupdatestep,             &
!$TAF& vegupdatestepmax,z0_tile,z0msea,zw &
!$TAF& = tape_n, REC = iloopcount
!Jupp



!-------------------------------------------------------------------------------
!   Call the main model routine.
!-------------------------------------------------------------------------------
    CALL control (a_step)

!Jupp
    if (echo) write(*,*) 'jules line 213: ',iloopcount
!$TAF STORE &
!$TAF& canht_ft,catch,catch_snow,date,datenext,drivedata,drivedatastep,drivedate,drivedateinit,drivefile,drivefiledate,         &
!$TAF& drivefilename,drivefilestep,drivefiletime,driveresetstep,driveresetstepprev,drivetemplatet,drivetime,drivetimeinit,      &
!$TAF& endrun,frac,ijulian,infil_tile,irecprev,ispin,lai,lw_down,notnextdrive,notnextveg,npp,pstar,qbase,qw_1,resp_s,rootd_ft,  &
!$TAF& routecount,routestore,routestep,     &
!$TAF& smcl,snow_grnd,snow_tile,spinend,spinend,spinup,spinvalold,surf_roff,sw_down,tile_index,tile_pts,tl_1,       &
!$TAF& timenext,u_0_p,u_1,u_1_p,utc_time_secs,v_0_p,v_1,v_1_p,vegdatain,vegdatastep,vegdatastepmax,vegdate,vegfile,vegfiledate, &
!$TAF& vegfilename,vegfilestep,       &
!$TAF& vegfiletime,vegresetstep,vegresetstepprev,vegtime,vegunit,vegupdatestep,vegupdatestepmax,z0_tile             &
!$TAF& = tape_n, REC = iloopcount
!Jupp

!-------------------------------------------------------------------------------
!   Generate output. This call (at the end of the timestep) is expected to
!   generate most of the output for a run.  The logical argument (endCall) is
!   always true at this call.
!-------------------------------------------------------------------------------
    CALL output( a_step,.TRUE. )

    if (spinUp) then
       spin_a_step = a_step                        ! overwrites to ensure last spin_up version of a_step is stored
    else 
! edithere begins                                                         !Jupp
! user may want to use different variables here                           !Jupp
       modts(a_step-spin_a_step,1) = (-npp(1) + resp_s(1,1) )  *1.0e3*1.0e6*.0832591         !Jupp convert units to umoles/m2/s
       modts(a_step-spin_a_step,2) = ftl_1(1,1)                                              !Jupp define a total of nts different fluxes
       modts(a_step-spin_a_step,3) = latent_heat(1,1)                                        !Jupp
       modts(a_step-spin_a_step,4) = tstar(1,1)                                              !Jupp
       modts(a_step-spin_a_step,5) = gpp(1)  *1.0e3*1.0e6*.0832591                           !Luke convert units to umoles/m2/s
       modts(a_step-spin_a_step,6) = (resp_s(1,1) + resp_p(1))*1.0e3*1.0e6*.0832591          !Luke convert units to umoles/m2/s
! edithere ends                                                           !Jupp

       monthstarts(a_step-spin_a_step) = newMonth				!Luke store whether timestep is the start of a month
!write(*,*) 'jules :',nts,ts_bias,ts_scal

       do its =1,nts
         modts(a_step-spin_a_step,its) = ts_scal(its) * (modts(a_step-spin_a_step,its) - ts_bias(its))             ! rescale modelled timeseries 
       end do

       call writemodts(a_step-spin_a_step) ! Jupp record modts to a binary file
    end if

!-------------------------------------------------------------------------------
!   Update time.
!-------------------------------------------------------------------------------

!Jupp
!write(*,*) 'jules line 249: ',iloopcount
!$TAF STORE &
!$TAF& alb_tile,canht_ft,canopy,catch,catch_snow,cosz,cs,diff_frac             &
!$TAF&  ,e_sea,ecan_tile,ext,flandg,fqw_1,fqw_tile,frac,fsat,ftl_1,ftl_tile    &
!$TAF&  ,g_leaf_acc,g_leaf_phen_acc,gamtot,gc,gs,h_sea,infil_tile,lai          &
!$TAF&  ,land_albedo,le_tile,lw_down,melt_tile,npp,npp_ft_acc,qbase            &
!$TAF&  ,radnet_tile,resp_s,resp_s_acc,resp_w_ft_acc,rgrain,smc,smcl           &
!$TAF&  ,snow_grnd,snow_tile,sthf,sthu,surf_ht_flux_land,sw_down,t_soil,ti     &
!$TAF&  ,tile_index,tile_pts,tstar_land,tstar_sice,tstar_ssi,tstar_tile,       &
!$TAF&  z0_tile,z0msea,zw                         &
!$TAF& = tape_n, REC = iloopcount
!Jupp

    CALL newTime( a_step,endRun )

!Jupp
!write(*,*) 'jules line 265: ',iloopcount
!$TAF STORE &
!$TAF& asteps_since_triffid,canht_ft,canopy,catch,catch_snow,cs,date, &
!$TAF& datenext,drivedata,drivedatain,drivedatastep,drivedate,drivefile,drivefiledate,drivefilename,drivefileonunit,   &
!$TAF& drivefilestep,drivefiletime,driveresetstep,driveresetstepprev,drivetime,driveunit,endrun, endyear,flandg,fqw_tile,frac,  &
!$TAF& fsat,     &
!$TAF& g_leaf_acc,g_leaf_phen_acc,gamtot,gc,gs,ijulian,infil_tile,irecprev,ispin,lai,notnextdrive,notnextveg,npp,     &
!$TAF& npp_dr_out,npp_ft_acc,pstar,qw_1,resp_s,resp_s_acc,resp_w_dr_out,resp_w_ft_acc,rgrain,rootd_ft,routestore,smcl,snow_grnd,&
!$TAF& snow_tile,spinend,spinup,spinvalold,stepflag,sthf,sthu,surf_roff,sw_down,t_soil,ti,tile_index,tile_pts,time,timenext,    &
!$TAF& tl_1,tstar,tstar_land,tstar_sice,tstar_ssi,tstar_tile,u_0_p,u_1,u_1_p,utc_time_secs,v_0_p,v_1,v_1_p,vegdatain,          &
!$TAF& vegdatastep,vegdatastepmax,vegdate,vegfile,vegfiledate,  &
!$TAF& vegfilename,vegfilestep,vegfiletime,vegresetstep,vegresetstepprev,vegtime,vegunit,vegupdatestep,             &
!$TAF& vegupdatestepmax,z0_tile,z0msea,zw &
!$TAF& = tape_n, REC = iloopcount
!Jupp

!-------------------------------------------------------------------------------
!   Check for end of run.
!-------------------------------------------------------------------------------
!write(*,*) 'jules line 284: ',iloopcount
!$TAF STORE endRun = tape_n, REC = iloopcount

!Jupp    IF ( endRun ) EXIT

  ENDDO  !  timestep loop

!Jupp
!write(*,*) 'jules line 292: ',1
!$TAF STORE a_step,npp,resp_s = tape_1, REC = 1
!Jupp

!-------------------------------------------------------------------------------
! End of model run.
!-------------------------------------------------------------------------------

  if (.not.endRun) print*,'nloopcount probably wrong!',nloopcount                 !Jupp

  time_hms = s_to_chhmmss( time )

  if (echo) then                                                                  !Jupp
  WRITE(*,"(/,a,i7,a,i8,tr1,a,/)") 'End of model run after ',a_step  &
      ,' timesteps. Date and time: ',date,time_hms
  end if                                                                          !Jupp
  if (echo) write(*,*) 'leaving subroutine jules'
END subroutine jules                                                              !Jupp

!###############################################################################




















!###############################################################################


subroutine postjules                          !Jupp
  USE inout, ONLY :  &                        !Jupp
      echo                                    !Jupp
!-----------------------------------------------------------!Jupp
! Call routine to dump final state, deallocate memory etc.  !Jupp
!-----------------------------------------------------------!Jupp
  CALL jules_final( 'final' )                   !Jupp
  if (echo) WRITE(*,"(/,a)")'End of postjules.' !Jupp
end subroutine postjules                        !Jupp
