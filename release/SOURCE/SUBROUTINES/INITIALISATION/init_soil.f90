! Read parameters for soil, including layer thicknesses.

  SUBROUTINE init_soil()

  USE ancil_info, ONLY :  &
!  imported scalars with intent(in)
     land_pts,sm_levels  &
!  imported scalars with intent(out)
    ,soil_pts  &
!  imported arrays with intent(out)
    ,soil_index

  USE file_utils, ONLY :  &
!  imported procedures
     closeFile,fileUnit,findTag,openFile  &
!  imported arrays with intent(inout)
    ,irecPrev

  USE inout, ONLY :  &
!  imported scalar parameters
     formatAsc,formatBin,formatLen,formatNc,formatPP,jinUnit,tagAscBin,tagNc  &
!  imported scalars with intent(in)
    ,echo,nxIn,nyIn  &
!  imported arrays with intent(in)
    ,mapInLand

  USE jules_netcdf, ONLY :  &
!  imported scalars with intent(in)
     ncType

  USE misc_utils, ONLY :  &
!  imported procedures
     checkVarPos,checkVars,read_list,repeatVal,varInfo,varList,varValue

  USE p_s_parms, ONLY :  &
!  imported arrays with intent(out)
    albsoil,b,hcap,hcon,satcon,sathh  &
   ,smvccl,smvcst,smvcwt

  USE readwrite_mod, ONLY :  &
!  imported procedures
     readvar3dcomp

  USE soil_param, ONLY :  &
!  imported arrays with intent(out)
     dzsoil

  USE switches, ONLY :  &
!  imported scalars with intent(in)
     l_triffid,routeOnly  &
!  imported scalars with intent(out)
    ,l_q10,l_soil_sat_down,l_vg_soil,soilhc_method

!-------------------------------------------------------------------------------
  IMPLICIT NONE

  INTEGER, PARAMETER ::  &!  local SCALARS
    nvarMax = 10  !  the number of possible soil characteristic variables
!                         This may be larger than the number needed for any
!                         particular configuration.
!                         Possible variables:
!                         b,sathh,satcon,smvccl,smvcst,smvcwt,hcap,hcon,albsoil,soilType

  INTEGER ::  &!  local SCALARS
    i,ierr       &!  loop counters/work
   ,inUnit       &!  unit used to connect to input file
   ,iorder       &!  work
!  Every possible variable (nvarMax) must have an "iposX" variables declared here
   ,iposAlb      &!  position in list of all possible variables of albsoil
   ,iposB        &!  position in list of all possible variables of b exponent
   ,iposHcap     &!  position in list of all possible variables of hcap
   ,iposHcon     &!  position in list of all possible variables of hcon
   ,iposSatcon   &!  position in list of all possible variables of satcon
   ,iposSathh    &!  position in list of all possible variables of sathh
   ,iposSMCrit   &!  position in list of all possible variables of critical soil moisture
   ,iposSMSat    &!  position in list of all possible variables of saturated soil moisture
   ,iposSMWilt   &!  position in list of all possible variables of wilting point soil moisture
   ,iposSoilType &!  position in list of all possible variables of soil type (number)
   ,ivar,jvar    &!  work
   ,l            &!  work
   ,nfieldFile   &!  number of fields per time in a file
   ,nheaderField &!  number of header records before each field in file
   ,nheaderFile  &!  number of header records at start of file
   ,nheaderT     &!  number of headers at start of each time
   ,nlineField   &!  work
   ,nsoilVar     &!  number of soil variables required for chosen configuration
!XX Do we need nsoilVar?? Can we just use nvarIn?
   ,nvarIn   &!  number of soil variables that are read in
!                      Often this =nsoilVar, but may be < nsoilVar if we can derive a field
!                      from other fields. or set by some other assumption.
   ,nz           &!  number of levels of data to read
   ,nzz          &!  work
   ,readT        &!  time level to be read from file
   ,useIndex      !  index in irecPrev

  REAL :: albSoilConst   !  a constant (in space and time) value of soil albedo, for use
!                               when useSoilType=TRUE.

  INTEGER :: &!  local arrays
    ival(nvarMax)           &!  work
   ,soilType(land_pts)      &!  soil type (class)
   ,varFlag(nvarMax)        &!  flag indicating source of data.
!                       At present this must either be zero (variable not used - little point!)
!                       or >0 (location of first level of each variable in input file, in terms of
!                       field number, or number of xy planes. Set to 1 for SDF files).
   ,varInSort(nvarMax)    &!  a list of which variables in master list are to be read in,
!                                sorted into ascending order of location in file.
!                                Values 1:varIn give a list of variables that are to be
!                                read from file (in terms of location in masterlist of all possible
!                                variables), sorted into ascending order of location
!                                of data in file.
   ,varFlagTmp(nvarMax)        &!  work: flags as read in
   ,varStashCode(nvarMax)      &!  STASH code for each variable
   ,varUse(nvarMax)             !  flag indicating if a variable
!                        is required for current setup, and how it is used
!                        0: not required
!                        1: needed. Can be set indirectly (from other variables).
!                        2: only needed indirectly to set other variables.

  REAL ::  &!  local ARRAYS
    tmpval(land_pts,sm_levels)   !  workspace, large enough for all levels of a variable

  LOGICAL ::  &!  local SCALARS
    checkNames,checkPos  &!  work
   ,constZ       &!  flag indicating that soil characteristics are constant with depth
   ,errFound     &!  flag indicating an error
   ,levels       &!  work
   ,readFile     &!  flag indicating if another file is to be read
   ,sdfFile      &!  TRUE if dealing with an SDF (self-describing file)
   ,summary      &!  work
   ,useSoilType  &!  flag indicating if a map of soil types is to be read
!                      T read a map of soil types, and characteristics for each type
!                      F read maps of soil characteristics
   ,zrev          !  T means the order of the soil layers in the input data will be reversed.
                  !    Soil levels start from that at the ground surface and increase with depth (top to bottom).
                  !    F means the input soil data are arranged top to bottom
                  !    T means the input soil data are arranged bottom to top.

  LOGICAL ::  &!  arrays
    done(nvarMax)      &!  work
   ,foundVar(nvarMax)   !  flag indicating if a variable is listed in run control file

  CHARACTER(len=formatLen) ::  &! local SCALARS
    fileFormat   !  format of file

  CHARACTER(len=150) ::  &! local SCALARS
    fileName     &!  the name of a file
   ,soilLUTfile   !  file containing look-uo table of soil characteristics

  CHARACTER(len=100) ::  &! local ARRAYS
    varDesc(nvarMax)      &!  description of each variable
   ,varExtra(nvarMax)     &!  "extra" information for of each variable
   ,varName(nvarMax)      &!  names of all possible variables - used to identify
!                                    variables in this routine
   ,varNameTmp(nvarMax)   &!  work: variable names as read in
   ,varNameSDF(nvarMax)   &!  names of variables in input file (SDF=self-describing file)
   ,varNameSDFTmp(nvarMax) !  work: names as read in

!------------------------------------------------------------------------------------------

! If data are not needed, nothing to do.
  IF ( routeOnly ) RETURN

  if (echo) WRITE(*,"(50('-'),/,a)") 'init_soil' !Jupp

! Locate the start of this section in input file.
  CALL findTag( jinUnit,'init_soil','>INIT_SOIL' )

! Start to read from run control file.
  READ(jinUnit,*) l_vg_soil
  READ(jinUnit,*) l_soil_sat_down
  READ(jinUnit,*) l_q10
  READ(jinUnit,*) soilhc_method
  READ(jinUnit,*) useSoilType
  READ(jinUnit,*) constZ,zrev

! Establish where data are to be read from.
  READ(jinUnit,*) readFile
  READ(jinUnit,*) fileFormat
  READ(jinUnit,*) fileName
  IF ( useSoilType ) READ(jinUnit,*) soilLUTfile

! Set file format if reading from run control file.
  IF ( .NOT. readFile ) fileFormat = formatAsc

! Insist on zrev=FALSE if reading from run control file.
  IF ( .NOT.readFile .AND. zrev ) THEN
    WRITE(*,*)'ERROR: init_soil: NOT readFile requires zrev=FALSE.'
    STOP
  ENDIF

! If reading a map of soil types, insist on zrev=FALSE (for clarity, hopefully).
  IF ( useSoilType .AND. .NOT.zrev ) THEN
    WRITE(*,*)'ERROR: init_soil: if using a map of soil types,'
    WRITE(*,*)'zrev must be FALSE.'
    STOP
  ENDIF

! Check soilHc_method recognised.
  IF ( soilHc_method<1 .OR. soilHc_method>2 ) THEN
    WRITE(*,*)'ERROR: init_soil: soilHc_method must be 1 or 2.'
    STOP
  ENDIF

!-------------------------------------------------------------------------------
! Set up list of all possible variables.
! Delayed until now so we know value of l_vg_soil.
! Note that some alternative STASH codes could be used - e.g. STASH 43 is
! saturation moisture after timestep.
!------------------------------------------------------------------------------
  i = 0

  i=i+1; iposB=i; varName(i)='b'
  varStashcode(i) = 8288      !  PP code 1380
  varDesc(i)='Clapp-Hornberger b exponent'
  IF ( l_vg_soil ) THEN
    varStashcode(i) = 8288   !  288 is really Clapp-Hornberger b. PP code 1381
    varDesc(i)='exponent, b=1/(n-1) (m)'
  ENDIF

  i=i+1; iposSathh=i; varName(i)='sathh'
  varStashcode(i) = 8224      !  PP code 342
  varDesc(i)='Saturated soil water pressure (m)'
  IF ( l_vg_soil ) varDesc(i)='1/alpha'

  i=i+1; iposSatcon=i; varName(i)='satcon'
  varStashcode(i) = 8214    !  PP code 333
  varDesc(i)='Saturated hydraulic conductivity (kg m-2 s-1)'

  i=i+1; iposSMSat=i; varName(i)='sm_sat'
  varStashcode(i) = 8213  !  PP code 332
  varDesc(i)='Volumetric soil moisture content at saturation'

  i=i+1; iposSMCrit=i; varName(i)='sm_crit'
  varStashcode(i) = 8211  !  PP code 330
  varDesc(i)='Volumetric soil moisture content at critical point'

  i=i+1; iposSMWilt=i; varName(i)='sm_wilt'
  varStashcode(i) = 8210  !  PP code 329
  varDesc(i)='Volumetric soil moisture content at wilting point'

  i=i+1; iposHcap=i; varName(i)='hcap'
  varStashcode(i) = 8216   !  PP code 335
  varDesc(i)='Soil heat capacity (J K-1 m-3)'

  i=i+1; iposHcon=i; varName(i)='hcon'
  varStashcode(i) = 8217   !  PP code 336
  varDesc(i)='Soil thermal conductivity (W m-1 K-1)'

  i=i+1; iposAlb=i; varName(i)='albsoil'
  varStashcode(i) = 220  !  PP code 1395
  varDesc(i)='Soil albedo'

  i=i+1; iposSoilType=i; varName(i)='soilType'
  varStashcode(i) = -1   !  not known
  varDesc(i)='Soil type number'

!-------------------------------------------------------------------------------
! Indicate the variables that are required for the selected configuration.
!------------------------------------------------------------------------------

! Initialise to show that no variables are required.
  varUse(:) = 0

! Variables that are always required.
  varUse(iposAlb) = 1
  varUse(iposB) = 1
  varUse(iposHcap) = 1
  varUse(iposHcon) = 1
  varUse(iposSatcon) = 1
  varUse(iposSathh) = 1
  varUse(iposSMcrit) = 1
  varUse(iposSMsat) = 1
  varUse(iposSMwilt) = 1

! Variables that are only required under some configurations.
  IF ( useSoilType ) varUse(iposSoilType) = 1

! Count number of variables required for chosen configuration.
! Note that this curently includes "temporary" variables such as soil type.
  nsoilVar = COUNT( varUse(:) > 0 )

!-------------------------------------------------------------------------------
! Only read parameters for the file format indicated.

  SELECT CASE ( fileFormat )

    CASE ( formatAsc,formatBin,formatPP )
!     Locate the information in run control file.
      CALL findTag( jinUnit,'init_soil',tagAscBin,preInit=.TRUE. )
      READ(jinUnit,*) nheaderFile,nheaderField

!     Read details of variables. Read 2 values from a blank-delimited list.
      CALL read_list( jinUnit,2,nvarMax,'>VARS','>ENDVARS',' ','init_soil'  &
                     ,nvarIn,cvar1=varNameTmp,cvar1Pos=1,ivar=varFlagTmp,ivarPos=2 )
      sdfFile = .FALSE.

    CASE ( formatNc )
!     Locate the information in run control file.
      CALL findTag( jinUnit,'init_soil',tagNc,preInit=.TRUE. )

!     Read details of variables. Read 2 values from a blank-delimited list.
      CALL read_list( jinUnit,2,nvarMax,'>VARS','>ENDVARS',' ','init_soil'  &
                     ,nvarIn,cvar1=varNameTmp,cvar1Pos=1,cvar2=varNameSDFTmp,cvar2Pos=2 )

      sdfFile = .TRUE.

    CASE default
      WRITE(*,*)'ERROR: init_soil: no code for fileFormat=',TRIM(fileFormat)
      STOP

  END SELECT

!-------------------------------------------------------------------------------

! If using run control file, set number of headers to zero.
  IF ( .NOT. readFile ) THEN
    nheaderFile = 0
    nheaderField = 0
  ENDIF

! Check that there are no repeated names in input list.
  IF ( repeatVal( varNameTmp(1:nvarIn) ) ) THEN
    WRITE(*,*)'ERROR: init_soil: repeated variable names.'
    STOP
  ENDIF

! Work out what variables are indicated.
  CALL varList( nvarIn,sdfFile,varNameTmp,varName  &
               ,errFound,foundVar  &
               ,varFlagIn=varFlagTmp,varNameSDFin=varNameSDFtmp  &
               ,varFlag=varFlag,varNameSDF=varNameSDF )
  IF ( errFound ) THEN
    WRITE(*,*)'ERROR: init_soil: error returned by varList'
    STOP
  ENDIF

! If using a map of soil type, indicate that all other variables will be "derived",
! meaning set from soil characteristic file.
  IF ( useSoilType ) THEN
    WRITE(*,*)'WARNING: init_soil: as a soil type map is being used,'
    WRITE(*,*)'all other variables will be set based on soil type.'
    DO ivar=1,nvarMax
      IF ( varName(ivar) /= varName(iposSoilType) ) THEN
        varFlag(ivar) = -2
        varExtra(ivar) = 'using characteristics for each soil type'
       ENDIF
    ENDDO
  ENDIF

! Check that all necessary variables are provided.
  CALL checkVars( nvarMax,.FALSE.,.FALSE.,varUse,foundVar,varDesc,varName  &
                 ,nvarIn,varFlag,errFound )
! Stop if an error was detected.
  IF ( errFound ) THEN
    WRITE(*,*)'ERROR: init_soil: an error was found by checkVars.'
    STOP
  ENDIF

!-------------------------------------------------------------------------------
! Get a sorted list, giving input variables according to position in input file(s).
! This is needed for reading from run control file (and makes ASCII reading easier)
! and is always done.
! Note that we now insist that variables from run control file are listed in the
! correct order, but this sorting code has been left - easier for now!
  done(:) = .TRUE.
  varInSort(:) = 0
! Only consider variables that are to be read in (flag>0). SDF variables have flag set >0.
  WHERE ( varFlag(:) > 0 ) done(:)=.FALSE.
  DO ivar=1,nvarIn
!    IF ( readFile ) THEN
      ival(1:1) = MINLOC( varFlag(:), .NOT.done(:) )
!    ELSE
!     Hack so we can use existing code. If reading from run control file, don't
!     sort the list.
!      ival(1) = ivar
!    ENDIF
    i = ival(1)
    done(i) = .TRUE.
    varInSort(ivar) = i
  ENDDO

!-------------------------------------------------------------------------------
! Some output to screen.
  IF ( echo ) THEN
    DO ivar=1,nvarMax
      IF ( varUse(ivar) > 0 ) CALL varInfo( varName(ivar)  &
                      ,varFlag(ivar),varStashcode(ivar),varNameSDF(ivar)  &
                      ,fileFormat,varDesc=varDesc(ivar) )
    ENDDO
  ENDIF
  IF ( echo ) THEN
    DO ivar=1,nvarMax
      IF ( varUse(ivar) /= 0 ) CALL varInfo( varName(ivar)  &
                      ,varFlag(ivar),varStashcode(ivar),varNameSDF(ivar)  &
                      ,fileFormat,varDesc=varDesc(ivar),extraDesc=varExtra(ivar) )
    ENDDO
  ENDIF
!-------------------------------------------------------------------------------

! Check we have an acceptable file format, and set flags indicating what other
! variables to check.
  checkNames = .FALSE.
  checkPos = .FALSE.
  SELECT CASE ( fileFormat )
    CASE ( formatAsc )
!     ASCII. Need positions of variables.
      checkPos = .TRUE.
    CASE ( formatBin )
!     GrADS. For now, need positions of variables.
      checkPos = .TRUE.
    CASE ( formatNc )
!     netCDF. Need names of variables.
      checkNames = .TRUE.
    CASE ( formatPP )
!     PP format. For now, need positions of variables.
      checkPos = .TRUE.
    CASE default
      WRITE(*,*)'ERROR: init_soil: no code for fileFormat=',TRIM(fileFormat)
      STOP
  END SELECT

  IF ( checkPos ) THEN
!   Check positions of variables within file.
!   Check for repeated locations.
!XX   Note that we should really be checking that there is no overlap if nz>1 and varPos is FIELD num (not var num).

!   First argument to checkVarPos indicates how important the order of the variables is.
!   0 means order is not important. If reading from run control file, insist that variables
!   are 1,2,3,.... - indicated by iorder=2. Note that for run control file, we interpret
!   varFlag as variable number, not field number, since this is required for current code
!   to pass checkVarPos with iorder=2 (want vars to appear adjacent, which wouldn't be if
!   had multi-level data and gave non-consecutive field numbers. Admittedly, this is
!   becoming a bit of a faff, and needs to be rewritten, one day.).
!   In all cases, values <1 are ignored.
    iorder = 0
    IF ( .NOT. readFile ) iorder = 2

!   We need to load file locations in sorted order.
    ival(:) = 0
    DO ivar=1,nvarIn
      ival(ivar) = varFlag( varInSort(ivar) )
    ENDDO

    ierr = checkVarPos( iorder,ival(:),' init_soil: ival' )
    IF ( ierr < 0 ) THEN
      WRITE(*,*)'ERROR: init_soil: error from checkvarPos.'
      WRITE(*,*)'If error was repeated use of same varPos, but you do in fact want to reuse the'
      WRITE(*,*)'same data for more than one variable, comment out this stop!'
      STOP
    ENDIF
  ENDIF

  IF ( checknames ) THEN
!   Check names only for variables that are to be read in (varFlag>0).
    DO ivar=1,nvarMax
      DO jvar=ivar+1,nvarMax
        IF ( ( varFlag(ivar)>0 .AND. varFlag(jvar)>0 ) .AND.  &
             ( varNameSDF(ivar)==varNameSDF(jvar) ) ) THEN
          WRITE(*,*)'ERROR: init_soil: repeated varNameSDF: ',TRIM(varNameSDF(ivar))
          WRITE(*,*)'If you really do want to use the same variable from file to set values'
          WRITE(*,*)'of more than one FORTRAN variable, comment out this check!'
          WRITE(*,*)'Stopping in init_soil'
          STOP
        ENDIF
      ENDDO
    ENDDO
  ENDIF

!------------------------------------------------------------------------------
! Read layer thicknesses.
!------------------------------------------------------------------------------

! Find section in the run control file.
  inUnit = jinUnit
  CALL findTag( inUnit,'init_soil dzsoil','>DATA_DZSOIL',preInit=.TRUE. )

! Read data.
  READ(jinUnit,*) dzsoil(:)

! Read constant soil albedo, if required.
  IF ( useSoilType ) READ(jinUnit,*) albSoilConst

!------------------------------------------------------------------------------
! Open file.
  IF ( readFile ) THEN
    inUnit = fileUnit( fileFormat )  !  get unit
    CALL openFile( 1,.FALSE.,inUnit,'read',fileFormat,fileName,'old','init_soil',ncType )
  ELSE
    if (echo) WRITE(*,*)'Reading soil characteristics from the run control file.'
    if (echo) WRITE(*,*)'Data must be in fields 1,2,3,.... (i.e. sequential from 1).'
    inUnit = jinUnit
    CALL findTag( inUnit,'init_soil char','>DATA',preInit=.TRUE. )
  ENDIF

! Set the number of levels of data to read.
  nz = sm_levels
  IF ( constZ ) nz = 1

!-------------------------------------------------------------------------------

! Read data.
! All levels of a variable are read into temporary space, then loaded into final variables.
! Only data for the required points are returned.
! Only need to do anything for variables that are to be read in, since currently all (required)
! variables must be read in (no option to set as constant).
! For non-SDF files, variables are read in the order in which they appear in file - this is
! done so that values can be read from the run control file (which is possibly open on stdIn and
! therefore can't be backspaced).

  DO ivar=1,nvarIn

!   Get location in master list of next variable to be read.
    jvar = varInSort(ivar)

    IF ( varUse(jvar)>0 .AND. varFlag(jvar)>0 ) THEN

!     The netCDF reading code can't cope with a call from here to a soil with more
!     than one dimension (Land). So, for now, stop if hoping to read more than one level!
      IF ( fileFormat==formatNC .AND. .NOT.constZ ) THEN
        WRITE(*,*)'ERROR: init_soil: can''t read a netCDF file with more than one'
        WRITE(*,*)'dimension from init_soil. Needs more code! Sorry.'
        STOP
      ENDIF

      nzz = nz
      IF ( jvar == iposAlb ) nzz=1

      IF ( inUnit==jinUnit .AND. nxIn*nyIn==1 .AND. fileFormat==formatAsc) THEN
!       If the input grid is a single point and reading from run control file, expect no
!       new line between fields (eg all on one line).
!       Calling readVar means we could cope with headers in the run control file.
!       But there's no need since annotation is already simple in this case.
        READ(jinUnit,*) tmpval(:,1:nzz)

      ELSE

!       Simplifying assumptions regarding input file. Otherwise have to read these in.
        readT = 1             !  time level to read from file
        nfieldFile = varFlag(jvar)+nzz-1 !  # of fields in file.
!                                           Set to last field number - OK while readT=1
        nheaderT = 0          !  no headers at top of each time
        nlineField = 0        !  0 means will not attempt to read ASCII line-by-line

!       Set index to use for irecPrev with netCDF files - this irecPrev isn't changed,
!       but need to keep index within bounds.
        useIndex = inUnit
        IF ( fileFormat == formatNC ) useIndex = 1

        CALL readVar3dComp( readT,varFlag(jvar),varStashcode(jvar),.FALSE.,irecPrev(useIndex)  &
                         ,nfieldFile,nheaderFile,nheaderT  &
                         ,nheaderField,nlineField,nxIn,nyIn,nzz,zrev  &
                         ,inUnit,varNameSDF(jvar)  &
                         ,mapInLand(:),(/(i,i=1,land_pts)/)  &
                         ,fileFormat,tmpval(:,1:nzz),'init_soil','init_soil',ncType )
      ENDIF

!     Copy data into final variable, if necessary also filling all levels with
!     values from a single level.
      IF ( jvar == iposAlb ) THEN
        albSoil(:) = tmpval(:,1)
      ELSEIF ( jvar == iposB ) THEN
        b(:,1:nzz) = tmpval(:,1:nzz)
        b(:,nzz+1:sm_levels) = SPREAD( b(:,1), 2, sm_levels-nzz )
      ELSEIF ( jvar == iposHcap ) THEN
        hcap(:,1:nzz) = tmpval(:,1:nzz)
        hcap(:,nzz+1:sm_levels) = SPREAD( hcap(:,1), 2, sm_levels-nzz )
      ELSEIF ( jvar == iposHcon ) THEN
        hcon(:,1:nzz) = tmpval(:,1:nzz)
        hcon(:,nzz+1:sm_levels) = SPREAD( hcon(:,1), 2, sm_levels-nzz )
      ELSEIF ( jvar == iposSatcon ) THEN
        satcon(:,1:nzz) = tmpval(:,1:nzz)
        satcon(:,nzz+1:sm_levels) = SPREAD( satcon(:,1), 2, sm_levels-nzz )
      ELSEIF ( jvar == iposSathh ) THEN
        sathh(:,1:nzz) = tmpval(:,1:nzz)
        sathh(:,nzz+1:sm_levels) = SPREAD( sathh(:,1), 2, sm_levels-nzz )
      ELSEIF ( jvar == iposSMCrit ) THEN
        smvccl(:,1:nzz) = tmpval(:,1:nzz)
        smvccl(:,nzz+1:sm_levels) = SPREAD( smvccl(:,1), 2, sm_levels-nzz )
      ELSEIF ( jvar == iposSMSat ) THEN
        smvcst(:,1:nzz) = tmpval(:,1:nzz)
        smvcst(:,nzz+1:sm_levels) = SPREAD( smvcst(:,1), 2, sm_levels-nzz )
      ELSEIF ( jvar == iposSMWilt ) THEN
        smvcwt(:,1:nzz) = tmpval(:,1:nzz)
        smvcwt(:,nzz+1:sm_levels) = SPREAD( smvcwt(:,1), 2, sm_levels-nzz )
      ELSEIF ( jvar == iposSoilType ) THEN
        soilType(:) = NINT( tmpval(:,1) )
      ELSE
        WRITE(*,*)'ERROR: init_soil: no code to load jvar=',jvar
        STOP
      ENDIF

!   Here we would have endif for varpos<0 (meaning derive or otherwise set field)
    ENDIF   !  read variable

  ENDDO  !  variables

! Close file if it is not the JULES control file
  IF ( inUnit /= jinUnit ) CALL closeFile( inUnit,fileFormat )

!-------------------------------------------------------------------------------
! Read look-up table of characteristics for each soil type and set fields.
!------------------------------------------------------------------------------
  IF ( useSoilType ) CALL init_soil_lut( land_pts,nsoilVar,sm_levels  &
                          ,albSoilConst,soilLUTfile,soilType )

!-------------------------------------------------------------------------------
! Set surface values.
  hcon(:,0) = hcon(:,1)
  satcon(:,0) = satcon(:,1)

! Check that sathh>=0 - a common error!
  IF ( ANY( sathh(:,:)<0.0 ) ) THEN
    WRITE(*,*)'sathh < 0.0 detected. Should be > 0.'
    WRITE(*,*)'For JULES, sathh is abs(saturated head).'
    WRITE(*,*)'Stopping in init_soil'
    STOP
  ENDIF

!-------------------------------------------------------------------------------
! Detect soil points.
! If top layer saturation moisture content>0, this is a soil point.
! Note that land ice points are no longer assigned here.
!-------------------------------------------------------------------------------
  SOIL_PTS=0
  soil_index(:) = 0
  DO L=1,LAND_PTS
    IF( SMVCST(L,1) > EPSILON(SMVCST(L,1)) ) THEN
      SOIL_PTS=SOIL_PTS+1
      SOIL_INDEX(SOIL_PTS)=L
    ENDIF
  ENDDO

  if (echo) WRITE(*,*)'Number of soil points=',soil_pts

  IF ( soil_pts == 0 ) THEN
    WRITE(*,"(10('*'),a,10('*'))")' WARNING: init_soil: there are no soil points. '
    WRITE(*,"(10('*'),a,10('*'))")' Any land points are land ice points. '
  ENDIF

!------------------------------------------------------------------------------
! Set l_q10 to TRUE if not using TRIFFID.
! This is anyway done later in MICROBE, so do here too for ease.
!------------------------------------------------------------------------------
  IF ( .NOT. l_triffid ) l_q10 = .TRUE.

!------------------------------------------------------------------------------
! Optional write to screen.
!------------------------------------------------------------------------------
  IF ( echo ) THEN

    IF ( l_vg_soil ) WRITE(*,*) 'van Genuchten model will be used'
    IF ( l_soil_sat_down ) THEN
      WRITE(*,*) 'l_soil_sat_down=TRUE, meaning excess water is pushed down'
    ELSE
      WRITE(*,*) 'l_soil_sat_down=FALSE, meaning excess water is pushed up'
    ENDIF
    IF ( soilHc_method == 1 ) THEN
      WRITE(*,*)'soilHc_method=1:  following Cox et al (1999)'
    ELSE
      WRITE(*,*)'soilHc_method=2:  following simplified Johansen (1975)'
    ENDIF
    IF ( l_q10 ) THEN
      WRITE(*,*) 'Q10 equation will be used for soil respiration.'
    ELSE
      WRITE(*,*) 'RothC equations will be used for soil respiration.'
    ENDIF

    WRITE(*,*)'Soil layer thicknesses (m):'
    DO i=1,sm_levels
      WRITE(*,"(a,i2,a,(10f5.2))")'iz=',i,' dzsoil=',dzsoil(i)
    ENDDO

!-------------------------------------------------------------------------------
!   Write fields to screen.
!-------------------------------------------------------------------------------
    DO i=1,2
!     i = 1 writes full field, level by level
!     i = 2 writes summary statistics for whole field
      IF ( nxIn*nyIn==1 .AND. i==2 ) EXIT
      IF ( i == 1 ) THEN
        summary = .FALSE.
        levels = .TRUE.
        if (echo) WRITE(*,*)'###############################################'
      ELSE
        summary = .TRUE.
        levels = .FALSE.
        if (echo) WRITE(*,*)'###############################################'
        if (echo) WRITE(*,*)'Summary of input.'
        if (echo) WRITE(*,*)'NB The ranges below include any ice points.'
      ENDIF

      DO ivar=1,nvarMax

        IF ( varUse(ivar) == 1 ) THEN

          IF ( ivar == iposAlb ) THEN
            CALL varValue( summary,albsoil,varFormat='f6.3',varName=varName(ivar) )

          ELSEIF ( ivar == iposB ) THEN
            CALL varValue( levels,summary,b,varFormat='f7.3',varName=varName(ivar) )

          ELSEIF ( ivar == iposHcap ) THEN
            CALL varValue( levels,summary,hcap,varFormat='es10.2',varName=varName(ivar) )

          ELSEIF ( ivar == iposHcon ) THEN
            CALL varValue( levels,summary,hcon,varFormat='f6.3',varName=varName(ivar) )

          ELSEIF ( ivar == iposSatcon ) THEN
            CALL varValue( levels,summary,satcon,varFormat='es10.2',varName=varName(ivar) )
            IF ( soil_pts /= land_pts ) WRITE(*,"(/,a,/)")  &
                   'NB satcon will be reset to zero at land ice points.'

          ELSEIF ( ivar == iposSathh ) THEN
            CALL varValue( levels,summary,sathh,varFormat='f7.3',varName=varName(ivar) )

          ELSEIF ( ivar == iposSMsat ) THEN
            CALL varValue( levels,summary,smvcst,varFormat='f6.3',varName=varName(ivar) )

          ELSEIF ( ivar == iposSMcrit ) THEN
            CALL varValue( levels,summary,smvccl,varFormat='f6.3',varName=varName(ivar) )

          ELSEIF ( ivar == iposSMwilt ) THEN
            CALL varValue( levels,summary,smvcwt,varFormat='f6.3',varName=varName(ivar) )

          ENDIF

        ENDIF  !  varUse

      ENDDO  !  ivar
    ENDDO  !  i

  ENDIF  !  echo

  END SUBROUTINE init_soil
!###############################################################################
!###############################################################################
!###############################################################################
