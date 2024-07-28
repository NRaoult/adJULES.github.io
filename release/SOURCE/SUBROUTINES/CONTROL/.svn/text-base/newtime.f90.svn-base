!#################################################################################
!#################################################################################
!
! subroutine newTime
! Update date and time variables, taking account of spin up.
!
!#################################################################################
!#################################################################################
SUBROUTINE newTime( a_step,endRun )

  USE drive_io_vars, ONLY :  &
!   imported scalars with intent(in)
     driveDateInit,driveDataPer,driveDataStepInit  &
    ,driveTimeInit,driveTimeIndex  &
!   imported scalars with intent(inout)
    ,driveResetStep,driveResetStepPrev,driveDataStep,driveDate,driveTime,notNextDrive

  USE initial_mod, ONLY :  &
!  imported procedures
    dump_io

  USE inout, ONLY : &
!  imported scalars with intent(in)
     dumpFreq,nout  &
!   imported arrays with intent(inout)
    ,outFirstSection, echo

  USE misc_utils, ONLY :  &
!  imported procedures
     init_count 

  USE route_mod, ONLY :  &
!  imported scalars with intent(in)
     routeTimeStep  &
!  imported scalars with intent(inout)
    ,routeCount,routeStep

  USE spin_mod, ONLY :   &
!   imported scalars with intent(inout)
     ispin,nspin,spinEnd,spinUp

  USE switches, ONLY :   &
!   imported scalars with intent(in)
     l_360,route

  USE time_loc, ONLY :   &
!   imported scalars with intent(in)
     dateMainRun,dateRun,dateSpin,timeRun,timeStep  &
!   imported scalars with intent(inout)
    ,date,dateNext,datePrev,endMonth,endSec,endYear  &
    ,ijulian,newMonth,newSec,newYear,stepFlag,time,timeNext,timePrev,utc_time_secs

  USE time_mod, ONLY :  &
!  imported procedures
     dateToBits,dayOfYear,s_to_chhmmss,timeDate,timeDate_cmp

  USE update_mod, ONLY :  &
!   imported procedures
     calc_reset_step,data_init_vals

  USE veg_io_vars, ONLY :  &
!   imported scalars with intent(in)
     vegDataPer,vegDateInit,vegDataStepInit,vegTimeInit,vegTimeIndex,vegUpdateStepInit,vegVaryT  &
!   imported scalars with intent(inout)
    ,vegDataStep,vegDataStepMax,vegDate,vegResetStep,vegResetStepPrev  &
    ,vegTime,notNextVeg,vegUpdateStep


  use drive_io_vars, only :  &                                      !Jupp
      drivedata,drivedatain,drivefile,drivedate,&                   !Jupp
      drivefiledate,drivefileonunit, &                              !Jupp
      drivefilestep,drivefiletime,driveunit,drivefilename           !Jupp
  use file_utils, only : irecprev                                   !Jupp
  use forcing, only : sw_down                                       !Jupp
  use prognostics, only : canht_ft,lai                              !Jupp
  use spin_mod, only : spinvalold                                   !Jupp
  use veg_io_vars, only : vegdatain,vegfile,vegfiledate,&           !Jupp
      vegfilename,vegfilestep,vegfiletime,vegunit,&                 !Jupp
      vegunitfile,vegupdatestepmax                                  !Jupp
  use fomod, only : iloopcount,call_data_init_vals                  !Jupp

!-------------------------------------------------------------------------------

  IMPLICIT NONE
!  character(len=10), external :: s_to_chhmmss !Jupp
!  logical, external :: timedate_cmp        !Jupp
!  integer, external :: dayofyear           !Jupp

  LOGICAL, PARAMETER :: initCall = .FALSE.   !   argument to data_init_vals 
!              FALSE when call is not during initialisation (as now)

  INTEGER, INTENT(in) ::  &!  in SCALARS
   a_step         !  timestep number
!                      Zero is used in the call during initialisation to indicate 
!                      that spinCheck should not be called.

  INTEGER ::    &!  local SCALARS
   day,month,year  &!  parts of the date
  ,dayTmp       &!  work
  ,iStepMax     &!  work
  ,monthNext    &!  work
  ,monthPrev    &!  work
  ,yearNext     &! work
  ,yearPrev      ! work

  LOGICAL, INTENT(out) ::  &!  out scalars
    endRun    !   TRUE after the last timestep in the run.

  LOGICAL ::  &!  local SCALARS
    endSpinCycle     !  T at end of any cycle of spin up

  CHARACTER(len=10) ::  time_hms  ! Time in the form "hh:mm:ss H", used as a local variable
!                                 ! that is required because some compilers can not cope
!                                 ! with recursive write statements

  integer :: idummy = 0 !Jupp

!-----------------------------------------------------------------------
! Initialise.
!-----------------------------------------------------------------------
  newMonth = .FALSE.
  newYear = .FALSE.
  newSec = .FALSE.
  endRun= .FALSE. ! Jupp

  IF ( endMonth ) newMonth=.TRUE.
  IF ( endYear ) newYear=.TRUE.
  IF ( endSec ) newSec=.TRUE.

  endMonth = .FALSE.
  endYear = .FALSE.
  endSec = .FALSE.
  IF ( a_step > 0 ) stepFlag = 0

!-----------------------------------------------------------------------
! Get new previous time (the start of the current timestep, soon to be 
! the start of the previous timestep).
!-----------------------------------------------------------------------
  timePrev = time
  datePrev = date

! Get month and year at start of current timestep.
  CALL dateToBits( datePrev,dayTmp,monthPrev,yearPrev,l_360,'newtime' )

! Get new date and time, valid at the start of the next timestep.
! These may change if we are at the end of a spin-up cycle.
  time = timeNext
  date = dateNext

! Get time and date for end of next timestep. These may change if we 
! are at the end of a spin-up cycle.
  CALL timeDate( time,date,NINT(timeStep),'sec',l_360,timeNext,dateNext,'newtime' )
  CALL dateToBits( dateNext,dayTmp,monthNext,yearNext,l_360,'newtime' )

!-----------------------------------------------------------------------
! Look for the approaching end of a spin-up cycle (check if time at end
! of next timestep=end of a cycle).
!-----------------------------------------------------------------------
  IF ( spinUp .AND. (dateNext==dateSpin(2) .AND. timeNext==timeRun(1)) ) endSec = .TRUE.

!-----------------------------------------------------------------------
! Check if we are at the end of a spin-up cycle.
! XX Since the end of this cycle could be foreseen, we could set a flag 
! when it is forseen (eg stepFlag) and then test that flag here. XX
!-----------------------------------------------------------------------
  endSpinCycle = .FALSE.
  IF ( spinUp .AND.  &
       ( (date==dateSpin(2) .AND. time==timeRun(1)) .OR. a_step==0 ) ) THEN

!   A cycle of spin up has just been completed (or, if a_step=0, this is
!   a call during initialisation, before spin up).
    endSpinCycle = .TRUE.

    IF ( a_step > 0 ) THEN
      time_hms = s_to_chhmmss( time )
      if (echo) then !Luke
      WRITE(*,"(a,i5,a,i9,a,i9,tr1,a)")'Spin up cycle #',ispin  &
          ,' ended after timestep #',a_step,' End date and time:',date,time_hms
      end if
      IF ( nspin>0 .AND. ispin==nspin .AND. echo ) WRITE(*,"(50('#'),/,a)")  &  !Luke
        'Model has now completed the maximum number of spin up cycles requested.'
!     Check if spin up is complete (or store initial values at start of 
!     a spin up cycle other than the first).
      CALL spin_check
    ENDIF

!Jupp    
!$TAF STORE irecprev,spinend,spinvalold = tape_n, REC = iloopcount
!Jupp

    IF ( spinEnd ) THEN
!     Spin up has just ended. Time at start of next timestep is the 
!     start of the main run.
      spinEnd = .FALSE.
      date = dateMainRun(1)
      time = timeRun(1)
!     Set flag to indicate that next timestep is first in main section of run.
      stepFlag = 3
!     Update 'next' times.
      CALL timeDate( time,date,NINT(timeStep),'sec',l_360,timeNext,dateNext,'newtime' )
    ELSE

!     At the end of one cycle and the start of another cycle of spin 
!     up (or the start of first cycle of spin up).
      date = dateSpin(1)
      time = timeRun(1)
      ispin = ispin + 1
      stepFlag = 1
!     Update 'next' times.
      CALL timeDate( time,date,NINT(timeStep),'sec',l_360,timeNext,dateNext,'newtime' )
      
      if (echo) then
      WRITE(*,"(a)") 'Starting another cycle of spin up....'
      end if

!-----------------------------------------------------------------------
!     Deal with the case of spin-up of modelled-determined length, when 
!     we now know that another cycle of spin up is required (having 
!     previously assumed that it would not be required). Not an issue if
!     spin up and main run start at same time.  Note also that test on
!     a_step ensures that this is not used during call from init_time.
!-----------------------------------------------------------------------
      IF ( a_step>0 .AND. nspin>0 .AND. dateSpin(1)/=dateMainRun(1) ) THEN
!       Reinitialise data, to the state that was set at t=0.

!       Driving data.
!       Pass istepMax to the dummy argument dataStepMax, which is declared with
!       intent(inout) but is actually intent(in) for driving data.
!       Admittedly this is rather untidy.....
        istepMax = driveDataPer
 call_data_init_vals = 1 !Jupp
    CALL data_init_vals( driveDateInit,driveDataPer,istepmax,driveDataStepInit & !Jupp
                        ,driveTimeInit,driveDataStep,driveDate,driveTime  &  !Jupp
                        ,driveTimeIndex,.false.,notNextDrive,'drive'  &              !Jupp
                        ,idummy,0)                                           !Jupp

!  SUBROUTINE data_init_vals( dataDateInit,dataPer,dataStepMax,dataStepInit  &
!                            ,dataTimeInit,dataStep,dataDate,dataTime &
!                            ,dataTimeIndex,initCall,notNext,callType  &
!                            ,dataUpdateStep,dataUpdateStepInit )


!Jupp
!$TAF STORE &
!$TAF&  canht_ft, &
!$TAF&  drivedata,drivedatain,drivedatastep,drivedate,drivefile,drivefiledate,     &
!$TAF&  drivefilename,drivefileonunit,drivefilestep,drivefiletime,drivetime,driveunit,      &
!$TAF&  irecprev,lai,notnextdrive,notnextveg,spinend,spinvalold,sw_down,vegdatain,&
!$TAF&  vegdatastep,vegdatastepmax,vegdate,vegfile,vegfiledate,vegfilename,vegfilestep,     &
!$TAF&  vegfiletime,vegtime,vegunit,vegunitfile,vegupdatestep,vegupdatestepmax              &
!$TAF&   = tape_n, REC = iloopcount
!Jupp

!       Vegetation data.
 call_data_init_vals = 2 !Jupp
 
        IF ( vegVaryT ) CALL data_init_vals(  &
                             vegDateInit,vegDataPer,vegDataStepMax,vegDataStepInit  &
                            ,vegTimeInit,vegDataStep,vegDate,vegTime  &
                            ,vegTimeIndex,initCall,notNextVeg,'veg'  &
                            ,vegUpdateStep,vegUpdateStepInit)


!Jupp
!$TAF STORE &
!$TAF&  canht_ft, &
!$TAF&  drivedata,drivedatain,drivedatastep,drivedate,drivefile,drivefiledate,     &
!$TAF&  drivefilename,drivefileonunit,drivefilestep,drivefiletime,drivetime,driveunit,      &
!$TAF&  irecprev,lai,notnextdrive,notnextveg,spinend,spinvalold,sw_down,vegdatain,&
!$TAF&  vegdatastep,vegdatastepmax,vegdate,vegfile,vegfiledate,vegfilename,vegfilestep,     &
!$TAF&  vegfiletime,vegtime,vegunit,vegunitfile,vegupdatestep,vegupdatestepmax              &
!$TAF&   = tape_n, REC = iloopcount
!Jupp

      ENDIF

!-----------------------------------------------------------------------
!     Work out when the next "resets" are required.  Note that these
!     procedures are called separately at a_step=0 (needed to wait until
!     e.g. init_drive has calculated some dates and initialised driveResetStep).
!-----------------------------------------------------------------------
      IF ( a_step > 0 ) THEN
        CALL calc_reset_step( a_step,driveResetStep,driveResetStepPrev,'drive' )
        IF ( vegVaryT ) CALL calc_reset_step( a_step,vegResetStep,vegResetStepPrev,'veg' )
      ENDIF

    ENDIF  !  spinEnd

  ENDIF   !   end of a cycle of spin up (or a_astep=0 before spin up)

!-----------------------------------------------------------------------
! Set other counters at start of run or at the end of a cycle of spin up.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Routing.
! Not called during initialisation (routing timestep not known then) - 
! there is a separate call from init_route. Also do not reset if this is
! the end of spin up and dates of main run follow those of spin up.
!-----------------------------------------------------------------------
!  IF ( route .AND. endSpinCycle .AND. (.NOT. ( spinEnd .AND. dateSpin(2)==dateMainRun(1) )) ) THEN
  IF ( route .AND. a_step>0 .AND. endSpinCycle .AND.   &
      .NOT. (spinEnd .AND. dateSpin(2)==dateMainRun(1) ) ) THEN
!   In all cases the date to use is that at the start of the run 
!   (dateRun(1)).  This is only true because we have discounted the case
!   of spinEnd .AND. dateSpin(2)==dateMainRun(1) via the IF clause above
!   (that would use require the use of dateMainRun).
    CALL init_count( routeTimeStep,dateRun(1),timeRun(1),routeStep )
    routeCount = 0
  ENDIF

!-----------------------------------------------------------------------
! At this point:
!  * date and time are the date/time at the start of the next timestep
!  * dateNext and timeNext are the date/time at the end of the next 
!    timestep (aka start of the timestep after next)
!-----------------------------------------------------------------------

! At present I'm still carrying around two identical time variables.....!xx
  utc_time_secs = time

!-----------------------------------------------------------------------
! Establish if we are at the start of the last timstep in the run, and 
! set flag showing the end of a section.  This condition is also true 
! at the end of spin up (when endSec is already true).
!-----------------------------------------------------------------------
  IF ( .NOT.spinUp .AND. timeDate_cmp(timeNext,dateNext,'>=',timeRun(2),dateRun(2),'newTime') ) endSec=.TRUE.


!-----------------------------------------------------------------------
! Get day of year for end of current timestep, and also split date into
! parts.
!-----------------------------------------------------------------------
  CALL dateToBits( date,day,month,year,l_360,'newTime' )
  ijulian = dayOfYear( day,month,year,l_360 )

! Split next date into parts.
  CALL dateToBits( dateNext,day,monthNext,yearNext,l_360,'newTime' )

!-----------------------------------------------------------------------
! Test for end of month or year.  Note that the month and year can 
! change at the end of a cycle of spinUp. Although the month/year need 
! not really have been completed (the end is just being skipped), it is 
! treated as the end of a month/year.
!-----------------------------------------------------------------------
  IF ( year/=yearNext ) THEN
    endYear = .TRUE.
    endMonth = .TRUE.
  ELSEIF ( month /= monthNext ) THEN
    endMonth = .TRUE.
  ENDIF

!  Test for end of run.
   endRun = .FALSE.
   IF ( timeDate_cmp(time,date,'>=',timeRun(2),dateRun(2),'newTime') .AND. .NOT.spinUp )  &
      endRun = .TRUE.

!-----------------------------------------------------------------------
! Test if it is time to write a dump.  Note that none of these are TRUE 
! during the call from init_time (i.e. nothing is done during
! initialisation).  The order of the IF clauses establishes precedence 
! if more than once case is true (e.g. end of run coincides with end of
! year).
!-----------------------------------------------------------------------
  IF ( endRun .AND. dumpFreq/=0 ) THEN
!   The final dump is written by jules_final.
  ELSEIF ( stepFlag==3 .AND. (dumpFreq==3 .OR. dumpFreq==4) ) THEN
!   Dump after spin up has completed.
    CALL dump_io( .TRUE., dumpTypeArg='spunup' )
  ELSEIF ( newYear .AND. dumpFreq==4 .AND. a_step>1 ) THEN
!   Dump after each calendar year - but not on first timestep.
    CALL dump_io( .TRUE. )
  ENDIF

!-----------------------------------------------------------------------
! At start of each section of the run, set output flag. Not done during 
! initialisation, since at the time of the call from init_time, 
! outFirstSection is not yet available.
!-----------------------------------------------------------------------
  IF ( newSec .AND. a_step>0 .AND. nout>0 ) outFirstSection(:)=.TRUE.

END SUBROUTINE newTime


!#################################################################################
!#################################################################################
