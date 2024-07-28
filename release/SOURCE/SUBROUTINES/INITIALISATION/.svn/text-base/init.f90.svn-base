!#######################################################################
!#######################################################################
! subroutine init
! Subroutine to read input data and initialise the model.

  SUBROUTINE init

  USE init_grid_mod, ONLY :  &
!  imported procedures
     init_grid

  USE initial_mod, ONLY :  &
!  imported procedures
     dump_io,init_ic

  USE inout, ONLY :  &
!  imported scalars with intent(in)
     dumpFreq,stdIn,formatAsc  &
!  imported scalars with intent(out)
    ,jinUnit

  USE spin_mod, ONLY :  &
!  imported scalars with intent(in)
     spinUp

  USE veg_io_vars, ONLY :  &
!  imported scalars with intent(in)
     vegVaryT

  USE file_utils, ONLY : fileUnit,openFile,closeFile

  USE inout, ONLY : echo, ts_bias, ts_scal !Jupp

! Import procedures needed to read arguments
! This line is only required for the NAG compiler
!  USE F90_UNIX_ENV, ONLY : IARGC,GETARG

!-----------------------------------------------------------------------

  IMPLICIT NONE

! Declare local variables
  CHARACTER(len=250) :: jinFilename

!-----------------------------------------------------------------------
! To enable passing of the control file as an argument rather than
! piping standard input, un-comment the code below. This makes
! attaching certain debuggers easier, particularly those integrated with
! IDES (e.g. Photran for Eclipse).
! This code causes problems compiling with the Oracle Studio (Sun)
! and Portland Group compilers, so is disabled by default.
!
! This code is written with the expectation that either:
!  * One argument is supplied that contains a path to a formatted file
!  * No argument is given and we read from standard input that should be
!    redirected to a formatted file
!-----------------------------------------------------------------------
!  IF ( IARGC() > 0 ) THEN
! The first argument will be the filename
!    CALL GETARG(1, jinFilename)
!    WRITE(*,"(50('-'),/,a)")'Reading model control file from file '  &
!                         // TRIM(jinFilename)                        &
!                         // '...'

! Get a unit for the control file specified and open it
!    jinUnit = fileUnit(formatAsc)
!    CALL openFile(1,.FALSE.,jinUnit,'READ',formatAsc,jinFilename,'old')
!  ELSE
      if (echo) then !Jupp
    WRITE(*,*)'If this program appears to be waiting for input,'
    WRITE(*,*)'that''s because it is intended to be used with input'
    WRITE(*,*)'from a file, and expects a particular format.'
    WRITE(*,*)'Use with standard input redirected,'
    WRITE(*,*)'e.g. xjules < jules.in'
    WRITE(*,"(50('-'),/,a)")'Reading model control file from stdin...'
      end if !Jupp
    jinUnit = stdIn
!  ENDIF

  ts_bias(:) = 0.0  ! by default no bias
  ts_scal(:) = 1.0  ! and no rescaling factor

!-----------------------------------------------------------------------
! Read model options and misc other values.
!-----------------------------------------------------------------------
  CALL init_opts

!-----------------------------------------------------------------------
! Read date, time and location information
!-----------------------------------------------------------------------
  CALL init_time

!-----------------------------------------------------------------------
! Read details of model grid and allocate arrays.
!-----------------------------------------------------------------------
  CALL init_grid

!-----------------------------------------------------------------------
! Read fractional coverages.
!-----------------------------------------------------------------------
  CALL init_frac

!-----------------------------------------------------------------------
! Read soil layer thicknesses, and hydraulic and thermal characteristics.
!-----------------------------------------------------------------------
  CALL init_soil

!-----------------------------------------------------------------------
! Read TOPMODEL parameters.
!-----------------------------------------------------------------------
  CALL init_top

!-----------------------------------------------------------------------
! Read PDM parameters.
!-----------------------------------------------------------------------
  CALL init_pdm

!-----------------------------------------------------------------------
! Read surface heights
!-----------------------------------------------------------------------.
  CALL init_hgt

!-----------------------------------------------------------------------
! Read PFT parameters.
!-----------------------------------------------------------------------
  CALL init_veg

!-----------------------------------------------------------------------
! Read parameters for non-veg types.
!-----------------------------------------------------------------------
  CALL init_nonveg

!-----------------------------------------------------------------------
! Read urban parameters and calculate disp and ztm from MacDonald (1998)
! formulation
!-----------------------------------------------------------------------
  CALL init_urban

!-----------------------------------------------------------------------
! Read snow parameters.
!-----------------------------------------------------------------------
  CALL init_snow

!-----------------------------------------------------------------------
! Read TRIFFID parameters.
!-----------------------------------------------------------------------
  CALL init_trif

!-----------------------------------------------------------------------
! Read agricultural fraction.
!-----------------------------------------------------------------------
  CALL init_agric

!-----------------------------------------------------------------------
! Read miscellaneous surface and carbon/veg parameters.
!-----------------------------------------------------------------------
  CALL init_misc

!-----------------------------------------------------------------------
! Read runoff routing parameters.
!-----------------------------------------------------------------------
  call init_route

!-----------------------------------------------------------------------
! Read details of meteorological driving data.
!-----------------------------------------------------------------------
  CALL init_drive

!-----------------------------------------------------------------------
! Read the initial state.
!-----------------------------------------------------------------------
  CALL init_ic

!-----------------------------------------------------------------------
! Setup the output.
!-----------------------------------------------------------------------
  CALL init_out

  if (echo) WRITE(*,"(60('-'),/,a)")'Finished reading control file.' !Jupp

!-----------------------------------------------------------------------
! Temporary (i.e. for this version) initialisation of variables.
!-----------------------------------------------------------------------
  CALL INIT_VARS_TMP

!-----------------------------------------------------------------------
! Set index arrays and initialise other variables.
!-----------------------------------------------------------------------
  CALL INIT_PARMS

!-----------------------------------------------------------------------
! Save initial state if spinning up. Arrays are allocated here.
!-----------------------------------------------------------------------
  IF ( spinUp ) CALL init_spin_check !Jupp

!-----------------------------------------------------------------------
! Write an initial dump (if requested).
! If veg fields vary with time, wait until these are definitely updated.
!-----------------------------------------------------------------------
  IF ( .NOT.vegVaryT .AND. dumpFreq>1 ) CALL dump_io( .TRUE., dumpTypeArg='init' )

!-----------------------------------------------------------------------
! Deallocate space that was only needed during intialisation.
!-----------------------------------------------------------------------
  CALL deallocate_arrays( 'init_end' )

!-----------------------------------------------------------------------
! Close the jin file if we were reading from a file other than stdin
!-----------------------------------------------------------------------
  IF ( jinUnit /= stdIn ) THEN
    CALL closeFile(jinUnit, formatAsc)
! Set jinUnit to a value that cannot possibly be a file unit to avoid future collisions
    jinUnit = -1
  END IF

  if (echo) WRITE(*,"(70('#'),/,a,/,70('#'))") & 
  'Initialisation complete. Start of run.' !Jupp

  END SUBROUTINE init

!#######################################################################
!#######################################################################
