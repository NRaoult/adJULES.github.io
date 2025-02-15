!$TAF MODULE jules_netcdf SUBROUTINE opencdf INPUT = 1
!$TAF MODULE jules_netcdf SUBROUTINE opencdf OUTPUT =  2
!$TAF MODULE jules_netcdf SUBROUTINE opencdf ACTIVE = 
!$TAF MODULE jules_netcdf SUBROUTINE opencdf DEPEND = 

!$TAF MODULE jules_netcdf SUBROUTINE check_nc_dims INPUT = 1, 2
!$TAF MODULE jules_netcdf SUBROUTINE check_nc_dims OUTPUT = 
!$TAF MODULE jules_netcdf SUBROUTINE check_nc_dims ACTIVE = 
!$TAF MODULE jules_netcdf SUBROUTINE check_nc_dims DEPEND = 

!$TAF MODULE jules_netcdf SUBROUTINE readVar2dReal_ncVector_gswp2 INPUT = 1, 2, 3, 4, 5, 6, 7, 8,   10
!$TAF MODULE jules_netcdf SUBROUTINE readVar2dReal_ncVector_gswp2 OUTPUT =                        9
!$TAF MODULE jules_netcdf SUBROUTINE readVar2dReal_ncVector_gswp2 ACTIVE = 
!$TAF MODULE jules_netcdf SUBROUTINE readVar2dReal_ncVector_gswp2 DEPEND = 

!$TAF MODULE jules_netcdf SUBROUTINE closecdf INPUT = 1
!$TAF MODULE jules_netcdf SUBROUTINE closecdf OUTPUT = 
!$TAF MODULE jules_netcdf SUBROUTINE closecdf ACTIVE = 
!$TAF MODULE jules_netcdf SUBROUTINE closecdf DEPEND = 



  MODULE jules_netcdf

! Contains procedures used to read/write from netCDF files.

  USE inout, ONLY :  &
!  imported scalar parameters
     formatNc  &
!  imported scalars
    ,undefOut

  USE netcdf, ONLY :  &
!  imported procedures
     nf90_def_dim,nf90_def_var,nf90_enddef   &
    ,nf90_float,nf90_get_var,nf90_global,nf90_inq_dimid  &
    ,nf90_inq_varid,nf90_put_att,nf90_put_var  &
!  imported scalar parameters
    ,nf90_noErr,nf90_noWrite,nf90_unlimited

  USE rwErr_mod, ONLY : &
!  imported procedures
     rwErr

  IMPLICIT NONE
!-------------------------------------------------------------------------------

! Global scalar parameters.
  CHARACTER(len=7), PARAMETER :: ncTypeGswp2 = 'gswp2'  !  value of ncType (qv)
!                                                      indicating GSWP2 format
  CHARACTER(len=7), PARAMETER :: ncTypeJules = 'jules'  !  value of ncType (qv)
!                                                      indicating JULES format
  CHARACTER(len=7), PARAMETER :: ncTypePilps2e = 'pilps2e'  !  value of ncType
!                                             (qv) indicating PILPS2e format
  CHARACTER(len=7), PARAMETER :: ncTypePrincet = 'princet'  !  value of ncType
!                                             (qv) indicating Princeton format
  CHARACTER(len=7), PARAMETER :: ncTypeTseries = 'tseries'  !  value of ncType
!                                             (qv) indicating time series format.
  CHARACTER(len=7), PARAMETER :: ncTypeWatch = 'watch'  !  value of ncType (qv)
!                                                      indicating WATCH format

! Global scalars.
  CHARACTER(len=7) :: ncType  !  indicates what "type" of netCDF files are to be read
!         Does not apply to driving (meteorological) data.
!         Only used if netCDF files are to be read.
!         This flag is used so that netCDF files from different experiments can be
!         read by the same not-really-good netCDF-reading code.

  CHARACTER(len=7) :: ncTypeDrive  !  as ncType, but for driving (meteorological) data

  CONTAINS
!###############################################################################
!###############################################################################
!###############################################################################
  SUBROUTINE checkNcType( inType,errMess )

! Simply checks that the declared "type" (i.e. format) of netCDf files is
! recognised, and a does a few other checks.

  USE inout, ONLY :  &
!  imported scalars with intent(in)
     nxIn,nyIn

  IMPLICIT NONE

! Scalar arguments with intent(in)
  CHARACTER(len=*), INTENT(in) :: inType  !  indicated "type" of netCDf files
  CHARACTER(len=*), INTENT(in) :: errMess !  message to be printed on error

  LOGICAL :: errFound  !  error flag
!-------------------------------------------------------------------------------

  errFound = .FALSE.
  SELECT CASE ( inType )

    CASE ( ncTypePilps2e,ncTypePrincet )
!     Recognised, and no restrictions.

    CASE ( ncTypeGswp2,ncTypeWatch )
!     Can only be used with vector input.
      IF ( nyIn /= 1 ) THEN
        errFound = .TRUE.
        WRITE(*,*)'ERROR: GSWP2- or WATCH-style netCDF files must be used with nyIn=1.'
      ENDIF

    CASE ( ncTypeTseries )
!     Can only be used with single point input.
      IF ( nxIn*nyIn /= 1 ) THEN
        errFound = .TRUE.
        WRITE(*,*)'ERROR: "Simple time series netCDF files must be used with nxIn=nyIn=1.'
      ENDIF

    CASE default
      errFound = .TRUE.
      WRITE(*,*)'ERROR: do not recognise ncType=',inType
      
  END SELECT

  IF ( errFound ) THEN
    WRITE(*,*)'ERROR in checkNcType.'
    WRITE(*,*)'ErrMess=',errMess
    WRITE(*,*)'Stopping in subroutine checkNcType.'
    STOP
  ENDIF

  END SUBROUTINE checkNcType
!###############################################################################
!###############################################################################
! subroutine createNcFile
! Internal procedure in module jules_netcdf.
! Set up a new, but already existing, netCDF file (create dimensions etc).

  SUBROUTINE createNcFile( iout,ncID,gridNx,gridNy,pointsOut,nvar  &
                          ,gridDx,gridDy,gridX1,gridY1,forGrads  &
                          ,haveSCpool,havePFT,haveSnow  &
                          ,haveSoil,haveTile,haveType   &
                          ,outCompress,callType,compressGridFile  &
                          ,dateTimeString,fileName,globalAtt,compressMap  &
                          ,varName,varNcAtt,varType,varID )

  USE ancil_info, ONLY :  &
!  imported scalars with intent(in)
      dim_cs1,nsmax,nsoil=>sm_levels,ntiles

  USE inout, ONLY :  &
!  imported arrays with intent(out)
     outTimeID

  USE nstypes, ONLY :  &
!  imported scalars with intent(in)
     npft,ntype

  USE time_loc, ONLY :  &
!  imported scalars with intent(in)
     timestep

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Scalar arguments with intent(in)
!-------------------------------------------------------------------------------
  INTEGER, INTENT(in) :: iout                !  output profile number (only
!                                               used for callType='diag')
  INTEGER, INTENT(in) :: ncID                !  netCDF ID of file
  INTEGER, INTENT(in) :: gridNx           !  x size of full output grid
  INTEGER, INTENT(in) :: gridNy           !  y size of full output grid
  INTEGER, INTENT(in) :: pointsOut        !  number of output points
  INTEGER, INTENT(in) :: nvar             !  number of variables to be written
  REAL, INTENT(in) :: gridDx              !  gridbox size in x (deg lon)
  REAL, INTENT(in) :: gridDy              !  gridbox size in y (deg lat)
  REAL, INTENT(in) :: gridX1              !  x coord of point (1,1) in
!                                               output grid (deg lon)
  REAL, INTENT(in) :: gridY1              !  y coord of point (1,1) in
!                                               output grid (deg lat)
  LOGICAL, INTENT(in) :: forGrads    !  T means netCDF files should be readable
!                                       by GrADS
!                                    !  F no requirement (butmight be readable)
  LOGICAL, INTENT(in) :: haveSCpool, havePFT,haveSnow,haveSoil,haveTile,haveType
!               Flags indicating what "types" of variables are to be output.
  LOGICAL, INTENT(in) :: outCompress         !  T if output is a compressed grid
  CHARACTER(len=*), INTENT(in) :: callType         !  type of call
!              This should be 'diag' or 'dump'.
  CHARACTER(len=*), INTENT(in) :: compressGridFile !  name of compression
!       mapping data file (if used) (for information only)
  CHARACTER(len=*), INTENT(in) :: dateTimeString   !  date and time as
!                                                      yyyy-mm-dd hh:mm:ss
  CHARACTER(len=*), INTENT(in) :: fileName   !  name of file (for diagnostic
!                                               purposes only)
  CHARACTER(len=*), INTENT(in) :: globalAtt  !  a line to use as a global
!                                               attribute (for a "note")

!-------------------------------------------------------------------------------
! Array arguments with intent(in)
!-------------------------------------------------------------------------------
  INTEGER, INTENT(in) :: compressMap(pointsOut)  !  index in grid of output points
  CHARACTER(len=*), INTENT(in) :: varName(:)     !  name of each output variable
  CHARACTER(len=*), INTENT(in) :: varNcAtt(:,:)  !  attributes for each variable
  CHARACTER(len=*), INTENT(in) :: varType(:)     !  type of each output variable

!-------------------------------------------------------------------------------
! Array arguments with intent(out)
!-------------------------------------------------------------------------------
  INTEGER, INTENT(out) :: varID(:)    !  netCDF ID for each created variable

!-------------------------------------------------------------------------------
! Local scalar parameters.
!-------------------------------------------------------------------------------
  INTEGER, PARAMETER :: ndimMax = 6     !  number of possible "vertical"
!        dimensions. These are pft,csPool,snow,soil,tile,type.
  INTEGER, PARAMETER :: nvarDimMax = 5  !  max possible number of netCDF
!                         for any one variable. These are x,y,z,pseudo and t.

!-------------------------------------------------------------------------------
! Local scalar variables.
!-------------------------------------------------------------------------------
  INTEGER :: idim,ierrLast   !  loop counters/work
  INTEGER :: ierrSum    !  work
  INTEGER :: ierr       !  work
  INTEGER :: indexDimID !  netCDF ID for "index" dimension
  INTEGER :: indexID    !  netCDF ID for "index" variable
  INTEGER :: idimPft    !  index for PFT dimension
  INTEGER :: idimSCpool !  index for soil carbon pool dimension
  INTEGER :: idimSnow   !  index for snow dimension
  INTEGER :: idimSoil   !  index for soil dimension
  INTEGER :: idimTile   !  index for tile dimension
  INTEGER :: idimType   !  index for type dimension
  INTEGER :: ivar,ix,iy,iz !  loop counters/work
  INTEGER :: kvar  !  work
  INTEGER :: latID      !  netCDF ID for latitude variable
  INTEGER :: lonID      !  netCDF ID for longitude variable
  INTEGER :: ndim       !  # of netCDF dimensions for a variable
  INTEGER :: nvarCreate !  number of variables created
  INTEGER :: timeDimID  !  netCDF ID for time dimension
  INTEGER :: timeID     !  netCDF ID for a time variable
  INTEGER :: vID      !  netCDF ID for a variable
  INTEGER :: xDimID     !  netCDF ID for x dimension
  INTEGER :: yDimID     !  netCDF ID for y dimension
  CHARACTER(len=100) :: useName !  work
  CHARACTER(len=100) :: cwork   !  workspace - hopefully large enough!

!-------------------------------------------------------------------------------
! Local array variables.
!-------------------------------------------------------------------------------
  INTEGER :: dimID(ndimMax)         !  netCDF ID of each possible "levels" dimension
  INTEGER :: dimSize(ndimMax)       !  size of each possible "levels" dimension
  INTEGER :: dimVarID(ndimMax)      !  ID for variables related to "levels" dimensions
  INTEGER :: varDims(nvarDimMax)    !  netCDF IDs for dimensions for variables
  REAL :: lat(gridNx,gridNy)  !  latitudes of points in full output grid
  REAL :: lon(gridNx,gridNy)  !  longitudes of points in full output grid
  LOGICAL :: useDim(ndimMax)        !  flag indicating if a dimension used
  CHARACTER(len=10) :: dimName(ndimMax)    !  name of each possible "levels" dimension
  CHARACTER(len=30) :: dimLongName(ndimMax)!  name of each possible "levels" dimension

!-------------------------------------------------------------------------------
! Of course we should probably try to follow the CF conventions...but I'm in a
! hurry. Instead I'm basing this on GSWP2 and WATCH Forcing Data files.
!-------------------------------------------------------------------------------

! Set up all possible "levels" dimensions.
  idim = 0

  idim=idim+1; idimPFT=idim; dimName(idim)='pft'
  dimLongName(idim)  = 'plant functional types'
  dimSize(idim) = npft

  idim=idim+1; idimSCpool=idim; dimName(idim)='SCpool'
  dimLongName(idim)  = 'soil carbon pools'
  dimSize(idim) = dim_cs1

  idim=idim+1; idimSnow=idim; dimName(idim)='snow'
  dimLongName(idim)  = 'snow layers'
  dimSize(idim) = nsmax

  idim=idim+1; idimSoil=idim; dimName(idim)='soil'
  dimLongName(idim)  = 'soil layers'
  dimSize(idim) = nsoil

  idim=idim+1; idimTile=idim; dimName(idim)='tile'
  dimLongName(idim)  = 'surface tiles'
  dimSize(idim) = ntiles

  idim=idim+1; idimType=idim; dimName(idim)='type'
  dimLongName(idim)  = 'surface types'
  dimSize(idim) = ntype

!-------------------------------------------------------------------------------
! Calculate lat and lon for every gridpoint (in the full, uncompressed output grid).
!-------------------------------------------------------------------------------
  DO iy=1,gridNy
    DO ix=1,gridNx
      lon(ix,iy) = gridX1 + REAL(ix-1) * gridDx
      lat(ix,iy) = gridY1 + REAL(iy-1) * gridDy
    ENDDO
  ENDDO

!-------------------------------------------------------------------------------
! Create dimensions.
! Allow a few errors to accumulate before stopping!
!-------------------------------------------------------------------------------
  ierrSum = 0
  ierrLast = 0

  IF ( callType == 'diag' ) THEN
!   Unlimited dimension.
    ierr = nf90_def_dim( ncID,'tstep',nf90_unlimited,timeDimID )
  ELSE
    ierr = nf90_def_dim( ncID,'tstep',1,timeDimID )
  ENDIF
  ierrSum = ierrSum + ierr
  IF ( ierr /= nf90_noErr ) ierrLast = ierr

! x dimension.
  ierr = nf90_def_dim( ncID,'x',gridNx,xDimID )
  ierrSum = ierrSum + ierr
  IF ( ierr /= nf90_noErr ) ierrLast = ierr

! y dimension.
  ierr = nf90_def_dim( ncID,'y',gridNy,yDimID )
  ierrSum = ierrSum + ierr
  IF ( ierr /= nf90_noErr ) ierrLast = ierr

! Index dimension (e.g. land points) - only used if grid is compressed.
  IF ( outCompress ) THEN
    ierr = nf90_def_dim( ncID,'index',pointsOut,indexDimID )
    ierrSum = ierrSum + ierr
    IF ( ierr /= nf90_noErr ) ierrLast = ierr
  ENDIF

!-------------------------------------------------------------------------------
! Create vertical/levels/pseudo dimensions.
! Loop over all possible vertical dimensions.
! This isn't great but works. Keeping everything in arrays reduces repetition,
! but still requires an "IF haveXX" test.
!-------------------------------------------------------------------------------
  useDim(:) = .FALSE.
  DO idim=1,ndimMax
    IF ( idim==idimPFT .AND. havePFT ) THEN
      useDim(idim) = .TRUE.
    ELSEIF ( idim==idimSCpool .AND. haveSCpool ) THEN
      useDim(idim) = .TRUE.
    ELSEIF ( idim==idimSnow .AND. haveSnow ) THEN
      useDim(idim) = .TRUE.
    ELSEIF ( idim==idimSoil .AND. haveSoil ) THEN
      useDim(idim) = .TRUE.
    ELSEIF ( idim==idimTile .AND. haveTile ) THEN
      useDim(idim) = .TRUE.
    ELSEIF ( idim==idimType .AND. haveType ) THEN
      useDim(idim) = .TRUE.
    ENDIF
    IF ( useDim(idim) ) THEN
!     Create dimension.
      ierr = nf90_def_dim( ncID,TRIM(dimName(idim)),dimSize(idim),dimID(idim) )
      ierrSum = ierrSum + ierr
      IF ( ierr /= nf90_noErr ) ierrLast = ierr
    ENDIF
  ENDDO

! Snow layer variables also require a tile dimension, unless
! writing for GrADS. useDim=T means it has already been defined.
  IF ( haveSnow .AND. .NOT.forGrads .AND. .NOT.useDim(idimTile) ) THEN
!   Create tile dimension.
    idim = idimTile
    ierr = nf90_def_dim( ncID,TRIM(dimName(idim)),dimSize(idim),dimID(idim) )
    ierrSum = ierrSum + ierr
    IF ( ierr /= nf90_noErr ) ierrLast = ierr
  ENDIF

! If error, call error routine, indicating a stop.
  IF ( ierrSum /= 0 ) CALL rwErr( formatNc,.TRUE.  &
            ,ierr=ierrLast,fileName=fileName,ncID=ncid   &
            ,errMess1='createNcFile nf90_def_dim - last error reported' )

!-------------------------------------------------------------------------------
! Create variables to describe the grid.
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Create time variable.
!-------------------------------------------------------------------------------
  ierr = nf90_def_var( ncID,'time',nf90_float,(/timeDimID/),timeID )
! Save ID for variable.
  IF ( callType == 'diag' ) outTimeID(iout,1) = timeID
  IF ( ierr /= 0 ) CALL rwErr( formatNc,.TRUE.  &
          ,ierr=ierr,fileName=fileName,ncID=ncid,errMess1='createNcFile nf90_def_var time' )

! Write attributes.

! Units.
  IF ( callType == 'diag' ) THEN
    cwork = 'seconds since ' // TRIM(dateTimeString)
  ELSE
    cwork = TRIM(dateTimeString)
  ENDIF
  ierr = nf90_put_att( ncID,timeID,'units',TRIM(cwork) )
  ierrSum = ierrSum + ierr
  IF ( ierr /= nf90_noErr ) ierrLast = ierr

! Title
  ierr = nf90_put_att( ncID,timeID,'title','Time' )
  ierrSum = ierrSum + ierr
  IF ( ierr /= nf90_noErr ) ierrLast = ierr

! Long name.
  ierr = nf90_put_att( ncID,timeID,'long_name','Time axis' )
  ierrSum = ierrSum + ierr
  IF ( ierr /= nf90_noErr ) ierrLast = ierr

! Time origin.
  ierr = nf90_put_att( ncID,timeID,'time_origin',TRIM(dateTimeString) )
  ierrSum = ierrSum + ierr
  IF ( ierr /= nf90_noErr ) ierrLast = ierr

! If error, call error routine, indicating a stop.
  IF ( ierrSum /= 0 ) CALL rwErr( formatNc,.TRUE.  &
          ,ierr=ierrLast,fileName=fileName,ncID=ncid   &
          ,errMess1='createNcFile nf90_put_att, last error for var= time' )

!-------------------------------------------------------------------------------
! Create time step variable.
!-------------------------------------------------------------------------------
! Get ID for variable.
  ierr = nf90_def_var( ncID,'timestp  ',nf90_float,(/timeDimID/),timeID )
! Save ID for variable.
  IF ( callType == 'diag' ) outTimeID(iout,2) = timeID
  IF ( ierr /= 0 ) CALL rwErr( formatNc,.TRUE.  &
          ,ierr=ierr,fileName=fileName,ncID=ncid  &
          ,errMess1='createNcFile nf90_def_var timestp' )

! Write attributes.

! Units.
  cwork = 'timesteps starting ' // TRIM(dateTimeString)
  ierr = nf90_put_att( ncID,timeID,'units',TRIM(cwork) )
  ierrSum = ierrSum + ierr
  IF ( ierr /= nf90_noErr ) ierrLast = ierr

! Title
  ierr = nf90_put_att( ncID,timeID,'title','Time steps' )
  ierrSum = ierrSum + ierr
  IF ( ierr /= nf90_noErr ) ierrLast = ierr

! Long name.
  ierr = nf90_put_att( ncID,timeID,'long_name','Time step axis' )
  ierrSum = ierrSum + ierr
  IF ( ierr /= nf90_noErr ) ierrLast = ierr

! Time origin.
  ierr = nf90_put_att( ncID,timeID,'time_origin',TRIM(dateTimeString) )
  ierrSum = ierrSum + ierr
  IF ( ierr /= nf90_noErr ) ierrLast = ierr

! Timestep length.
  ierr = nf90_put_att( ncID,timeID,'tstep_sec',timestep )
  ierrSum = ierrSum + ierr
  IF ( ierr /= nf90_noErr ) ierrLast = ierr

! If error, call error routine, indicating a stop.
  IF ( ierrSum /= 0 ) CALL rwErr( formatNc,.TRUE.  &
          ,ierr=ierrLast,fileName=fileName,ncID=ncid   &
          ,errMess1='createNcFile nf90_put_att, last error for var= timestp' )

!-------------------------------------------------------------------------------
! Create longitude variable.
! This is longitude(x,y).
!-------------------------------------------------------------------------------
! Create variable.
  ierr = nf90_def_var( ncID,'lon',nf90_float,(/xDimID,yDimID/),lonID )
  IF ( ierr /= 0 ) CALL rwErr( formatNc,.TRUE.  &
          ,ierr=ierr,fileName=fileName,ncID=ncid,errMess1='createNcFile nf90_def_var lon' )

! Write attributes.

! Units
  ierr = nf90_put_att( ncID,lonID,'units','pixel centre degrees east' )
  ierrSum = ierrSum + ierr
  IF ( ierr /= nf90_noErr ) ierrLast = ierr

! Min
  ierr = nf90_put_att( ncID,lonID,'valid_min',lon(1,1) )
  ierrSum = ierrSum + ierr
  IF ( ierr /= nf90_noErr ) ierrLast = ierr

! Max
  ierr = nf90_put_att( ncID,lonID,'valid_max',lon(gridNx,1) )
  ierrSum = ierrSum + ierr
  IF ( ierr /= nf90_noErr ) ierrLast = ierr

! Long name
  ierr = nf90_put_att( ncID,lonID,'long_name','Longitude' )
  ierrSum = ierrSum + ierr
  IF ( ierr /= nf90_noErr ) ierrLast = ierr

! If error, call error routine, indicating a stop.
  IF ( ierrSum /= 0 ) CALL rwErr( formatNc,.TRUE.  &
          ,ierr=ierrLast,fileName=fileName,ncID=ncid   &
          ,errMess1='createNcFile nf90_put_att, last error for var= lon' )

!-------------------------------------------------------------------------------
! Create latitude variable.
! This is lat(x,y).
!-------------------------------------------------------------------------------
! Create variable.
  ierr = nf90_def_var( ncID,'lat',nf90_float,(/xDimID,yDimID/),latID )
  IF ( ierr /= 0 ) CALL rwErr( formatNc,.TRUE.  &
          ,ierr=ierr,fileName=fileName,ncID=ncid,errMess1='createNcFile nf90_def_var lat' )

! Write attributes.

! Units
  ierr = nf90_put_att( ncID,latID,'units','pixel centre degrees north' )
  ierrSum = ierrSum + ierr
  IF ( ierr /= nf90_noErr ) ierrLast = ierr

! Min
  ierr = nf90_put_att( ncID,latID,'valid_min',lat(1,1) )
  ierrSum = ierrSum + ierr
  IF ( ierr /= nf90_noErr ) ierrLast = ierr

! Max
  ierr = nf90_put_att( ncID,latID,'valid_max',lat(1,gridNy) )
  ierrSum = ierrSum + ierr
  IF ( ierr /= nf90_noErr ) ierrLast = ierr

! Long name
  ierr = nf90_put_att( ncID,latID,'long_name','Latitude' )
  ierrSum = ierrSum + ierr
  IF ( ierr /= nf90_noErr ) ierrLast = ierr

! If error, call error routine, indicating a stop.
  IF ( ierrSum /= 0 ) CALL rwErr( formatNc,.TRUE.  &
          ,ierr=ierrLast,fileName=fileName,ncID=ncid  &
          ,errMess1='createNcFile nf90_put_att, last error for var=latitude' )

  IF ( outCompress ) THEN
!-------------------------------------------------------------------------------
!   Grid index (point number). Only needed for a compressed grid (and not used
!   by GrADS at present, which uses a pdef file).
!   Really integer, but leave as real for now.
!-------------------------------------------------------------------------------
!   Create index variable.
    ierr = nf90_def_var( ncID,'index',nf90_float,(/indexDimID/),indexID )
    IF ( ierr /= 0 ) CALL rwErr( formatNc,.TRUE.  &
            ,ierr=ierr,fileName=fileName,ncID=ncid,errMess1='createNcFile nf90_def_var index' )

!   Write attributes.

!   Compress
    ierr = nf90_put_att( ncID,indexID,'compress','y x' )
    ierrSum = ierrSum + ierr
    IF ( ierr /= nf90_noErr ) ierrLast = ierr

!   Notes
    ierr = nf90_put_att( ncID,indexID,'note','index in grid' )
    ierrSum = ierrSum + ierr
    IF ( ierr /= nf90_noErr ) ierrLast = ierr

!   If error, call error routine, indicating a stop.
    IF ( ierrSum /= 0 ) CALL rwErr( formatNc,.TRUE.  &
          ,ierr=ierrLast,fileName=fileName,ncID=ncid   &
          ,errMess1='createNcFile nf90_put_att, last error for var=index' )

  ENDIF  !  outCompress

!-------------------------------------------------------------------------------
! Loop over all possible "layers" dimensions, creating a variable if required.
! Currently using an index - really integer, but leave as real for now.
!-------------------------------------------------------------------------------
  DO idim=1,ndimMax
    IF ( useDim(idim) ) THEN

!     Create variable.
      ierr = nf90_def_var( ncID,TRIM(dimName(idim)),nf90_float,(/dimID(idim)/),dimVarID(idim) )
      IF ( ierr /= 0 ) CALL rwErr( formatNc,.TRUE.  &
            ,ierr=ierr,fileName=fileName,ncID=ncid  &
            ,errMess1='createNcFile nf90_def_var var='//TRIM(dimName(idim)) )

!     Write attributes for variable.

!     Title
      ierr = nf90_put_att( ncID,dimVarID(idim),'title',TRIM(dimName(idim)) )
      ierrSum = ierrSum + ierr
      IF ( ierr /= nf90_noErr ) ierrLast = ierr

!     Long name.
      ierr = nf90_put_att( ncID,dimVarID(idim),'long_name',TRIM(dimLongName(idim)) )
      ierrSum = ierrSum + ierr
      IF ( ierr /= nf90_noErr ) ierrLast = ierr

!     If error, call error routine, indicating a stop.
      IF ( ierrSum /= 0 ) CALL rwErr( formatNc,.TRUE.  &
              ,ierr=ierrLast,fileName=fileName,ncID=ncid   &
              ,errMess1='createNcFile nf90_put_att, last error for var='//TRIM(dimName(idim)) )

    ENDIF  !  useDim
  ENDDO  !  idim

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! Create variables and attributes for the "data" variables.
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

  DO ivar=1,nvar

    nvarCreate = 1                  !  by default, each variable generates one netCDF var

!-------------------------------------------------------------------------------
!   Decide on dimensions for this variable.
!-------------------------------------------------------------------------------
    SELECT CASE  ( varType(ivar) )

      CASE ( 'LA', 'RG', 'SI' )
        IF ( outCompress ) THEN
          ndim = 2   !  index,t
          varDims(1:ndim) = (/ indexDimID,timeDimID /)
        ELSE
          ndim = 3   !  x,y,t
          varDims(1:ndim) = (/ xDimID,ydimID,timeDimID /)
        ENDIF

      CASE ( 'PF' )
        IF ( outCompress ) THEN
          ndim = 3   !  index,pft,t
          varDims(1:ndim) = (/ indexDimID,dimID(idimPFT),timeDimID /)
        ELSE
          ndim = 4   !  x,y,pseudo,t
          varDims(1:ndim) = (/ xDimID,ydimID,dimID(idimPFT),timeDimID /)
        ENDIF

      CASE ( 'SC' )
        IF ( outCompress ) THEN
          ndim = 3   !  index,pool,t
          varDims(1:ndim) = (/ indexDimID,dimID(idimSCpool),timeDimID /)
        ELSE
          ndim = 4   !  x,y,pool,t
          varDims(1:ndim) = (/ xDimID,ydimID,dimID(idimSCpool),timeDimID /)
        ENDIF

      CASE ( 'SO' )
        IF ( outCompress ) THEN
          ndim = 3   !  index,z,t
          varDims(1:ndim) = (/ indexDimID,dimID(idimSoil),timeDimID /)
        ELSE
          ndim = 4   !  x,y,z,t
          varDims(1:ndim) = (/ xDimID,ydimID,dimID(idimSoil),timeDimID /)
        ENDIF

      CASE ( 'SN' )
!       Snow layer variables are awkward!
        IF ( .NOT. forGrads ) THEN
!         Keep layer and pseudo dimensions.
          IF ( outCompress ) THEN
            ndim = 4   !  index,z,tile,t
            varDims(1:ndim) = (/ indexDimID,dimID(idimSnow)  &
                                ,dimID(idimTile),timeDimID /)
          ELSE
            ndim = 5   !  x,y,z,tile,t
            varDims(1:ndim) = (/ xDimID,ydimID,dimID(idimSnow)  &
                                ,dimID(idimTile),timeDimID /)
          ENDIF
        ELSE
!         Each tile (pseudo dimension) is represented as a separate variable.
          nvarCreate = ntiles
          IF ( outCompress ) THEN
            ndim = 3   !  index,z,t
            varDims(1:ndim) = (/ indexDimID,dimID(idimSnow),timeDimID /)
          ELSE
            ndim = 4   !  x,y,z,t
            varDims(1:ndim) = (/ xDimID,ydimID,dimID(idimSnow),timeDimID /)
          ENDIF
        ENDIF  !  forGrads

      CASE ( 'TI' )
        IF ( outCompress ) THEN
          ndim = 3   !  index,tile,t
          varDims(1:ndim) = (/ indexDimID,dimID(idimTile),timeDimID /)
        ELSE
          ndim = 4   !  x,y,tile,t
          varDims(1:ndim) = (/ xDimID,ydimID,dimID(idimTile),timeDimID /)
        ENDIF

      CASE ( 'TY' )
        IF ( outCompress ) THEN
          ndim = 3   !  index,type,t
          varDims(1:ndim) = (/ indexDimID,dimID(idimType),timeDimID /)
        ELSE
          ndim = 4   !  x,y,type,t
          varDims(1:ndim) = (/ xDimID,ydimID,dimID(idimType),timeDimID /)
        ENDIF

      CASE default
        WRITE(*,*)'ERROR: createNcFile: ndim: varType=',varType(ivar)
        WRITE(*,*)'No code for this varType.'
        STOP
    END SELECT

!-------------------------------------------------------------------------------
!   Create one or more netCDF variables.
!   This is a painful way to get snow layer variables that GrADS can read!
!   Otherwise we don't need the kvar loop.
!-------------------------------------------------------------------------------
    DO kvar=1,nvarCreate

      IF ( nvarCreate>1 .AND. varType(ivar)/='SN' ) THEN
        WRITE(*,*)'ERROR: createNcFile:  nvarCreate>1 .AND. varType(ivar)/=SN'
        STOP
      ENDIF

!-------------------------------------------------------------------------------
!     Get ID for each created variable.
!-------------------------------------------------------------------------------
      useName = TRIM(varName(ivar))
      IF ( nvarCreate>1 .AND. varType(ivar)=='SN' ) THEN
!       For snow layer variables, add tile number to name.
        IF ( nvarCreate < 10 ) THEN
          WRITE(cwork,"(i1.1)") kvar
        ELSE
          WRITE(cwork,"(i2.2)") kvar
        ENDIF
        useName = TRIM(useName) // TRIM(cwork)
      ENDIF

      ierr = nf90_def_var( ncID,TRIM(useName),nf90_float  &
                          ,varDims(1:ndim),vID )

!     A hack for snow layers vars with gradsNC=T: only save the
!     var ID for first level.
      IF ( kvar == 1 ) varID(ivar) = vID

      IF ( ierr /= nf90_noErr ) THEN
         WRITE(*,*)'Error for variable=',TRIM(useName)
         CALL rwErr( formatNc,.TRUE.  &
            ,ierr=ierr,fileName=fileName,ncID=ncid   &
            ,errMess1='createNcFile nf90_def_var' )
      ENDIF

!-------------------------------------------------------------------------------
!     Write attributes for variable.
!-------------------------------------------------------------------------------
      ierr = nf90_put_att( ncID,vID,'axis',TRIM(varNcAtt(ivar,1)) )
      ierrSum = ierrSum + ierr
      IF ( ierr /= nf90_noErr ) ierrLast = ierr

      ierr = nf90_put_att( ncID,vID,'units',TRIM(varNcAtt(ivar,2)) )
      ierrSum = ierrSum + ierr
      IF ( ierr /= nf90_noErr ) ierrLast = ierr

      ierr = nf90_put_att( ncID,vID,'missing_value',undefOut )
      ierrSum = ierrSum + ierr
      IF ( ierr /= nf90_noErr ) ierrLast = ierr

      ierr = nf90_put_att( ncID,vID,'title',TRIM(useName) )
      ierrSum = ierrSum + ierr
      IF ( ierr /= nf90_noErr ) ierrLast = ierr

!     Here's another painful consequence of hacking snow variables for GrADS -
!     varNcAtt has an entry for each var as shown in ctl file.
      ierr = nf90_put_att( ncID,vID,'long_name',TRIM(varNcAtt(ivar+kvar-1,4))  )
      ierrSum = ierrSum + ierr
      IF ( ierr /= nf90_noErr ) ierrLast = ierr

!     If error, call error routine, indicating a stop.
      IF ( ierrSum /= 0 ) THEN
         WRITE(*,*)'Error for variable=',TRIM(varName(ivar))
         CALL rwErr( formatNc,.TRUE.  &
            ,ierr=ierrLast,fileName=fileName,ncID=ncid   &
            ,errMess1='createNcFile nf90_put_att, last error' )
      ENDIF

    ENDDO  !  kvar created variables

  ENDDO !  ivar

!-------------------------------------------------------------------------------
! Global attributes.
!-------------------------------------------------------------------------------

  ierr = nf90_put_att( ncID,nf90_global,'title','Output from the JULES model.' )
  ierrSum = ierrSum + ierr
  IF ( ierr /= nf90_noErr ) ierrLast = ierr

  ierr = nf90_put_att( ncID,nf90_global,'note',TRIM(globalAtt) )
  ierrSum = ierrSum + ierr
  IF ( ierr /= nf90_noErr ) ierrLast = ierr

  IF ( outCompress ) THEN
    cwork = 'Mapping information held in ' // TRIM(compressGridFile)
    ierr = nf90_put_att( ncID,nf90_global,'note1',TRIM(cwork) )
    ierrSum = ierrSum + ierr
    IF ( ierr /= nf90_noErr ) ierrLast = ierr
  ENDIF

!-------------------------------------------------------------------------------
! Finished definitions, leave define mode.
!-------------------------------------------------------------------------------
  ierr = nf90_enddef( ncID )
  ierrSum = ierrSum + ierr
  IF ( ierr /= nf90_noErr ) ierrLast = ierr

! If error, call error routine, indicating a stop.
  IF ( ierrSum /= 0 ) CALL rwErr( formatNc,.TRUE.  &
          ,ierr=ierrLast,fileName=fileName,ncID=ncid   &
          ,errMess1='createNcFile last error')

!-------------------------------------------------------------------------------
! Write grid variables.
!-------------------------------------------------------------------------------

! Write lat and lon variables. These are for the full (uncompressed) output grid.

  ierr = nf90_put_var( ncID,lonID,lon(:,:) )
  ierrSum = ierrSum + ierr
  IF ( ierr /= nf90_noErr ) ierrLast = ierr

  ierr = nf90_put_var( ncID,latID,lat(:,:) )
  ierrSum = ierrSum + ierr
  IF ( ierr /= nf90_noErr ) ierrLast = ierr

! If error, call error routine, indicating a stop.
  IF ( ierrSum /= 0 ) CALL rwErr( formatNc,.TRUE.  &
          ,ierr=ierrLast,fileName=fileName,ncID=ncid   &
          ,errMess1='createNcFile last error from write lat/lon' )

! Write index variable.
  IF ( outCompress ) THEN
    ierr = nf90_put_var( ncID,indexID,REAL( compressMap ) )
    ierrSum = ierrSum + ierr
    IF ( ierr /= nf90_noErr ) ierrLast = ierr

!   If error, call error routine, indicating a stop.
    IF ( ierrSum /= 0 ) CALL rwErr( formatNc,.TRUE.  &
          ,ierr=ierrLast,fileName=fileName,ncID=ncid   &
          ,errMess1='createNcFile error from write indexVar' )
  ENDIF

!-------------------------------------------------------------------------------
! Write layer variables.
! Loop over all possible "layers" dimensions.
! Currently using an index - really integer, but leave as real for now.
!-------------------------------------------------------------------------------
  DO idim=1,ndimMax
    IF ( useDim(idim) ) THEN
      ierr = nf90_put_var( ncID,dimVarID(idim),(/ (iz,iz=1,dimSize(idim)) /) )

!     If error, call error routine, indicating a stop.
      IF ( ierr /= nf90_noErr ) CALL rwErr( formatNc,.TRUE.  &
              ,ierr=ierr,fileName=fileName,ncID=ncid   &
              ,errMess1='createNcFile error from write levels for var='//TRIM(dimName(idim)) )
    ENDIF
  ENDDO

  END SUBROUTINE createNcFile

!###############################################################################
!###############################################################################
!###############################################################################
! subroutine readVar2dReal_ncVector_gswp2
! Internal procedure in module jules_netcdf.
! Read a pseudo-2D field from netCDF file - a temporary (hopefully) fix for
! GSWP2 and WATCH files. In fact, reads a vector (eg compressed to land points).
! I have combined these GSWP2 and WATCH cases because they are so similar,
! but it does make this routine slightly more complicated.

  SUBROUTINE readVar2dReal_ncVector_gswp2( ncCallType,ncTypeArg,ncID,varName  &
                                          ,t,z,npoints,outval,start )

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Scalar arguments with intent(in).
!-------------------------------------------------------------------------------
  INTEGER, INTENT(in) ::  &
    ncID      &!  the ID (number) of the netCDF file
   ,npoints   &!  number of points to read
   ,t         &!  time number to read
   ,z          !  level number to read

  CHARACTER(len=*), INTENT(in) ::  &
    ncCallType  &!  type (location) of call
   ,ncTypeArg   &!  type (format) of netCDF files
   ,varName      !  the name of the netCDF variable

!-------------------------------------------------------------------------------
! Array arguments with intent(out)
!-------------------------------------------------------------------------------
  REAL, INTENT(out) :: outval(npoints)  !  output data

!-------------------------------------------------------------------------------
! Optional scalar arguments with intent(in).
!-------------------------------------------------------------------------------
  INTEGER, INTENT(in), OPTIONAL :: start !   starting point number

!-------------------------------------------------------------------------------
! Local scalar variables.
!-------------------------------------------------------------------------------
  INTEGER ::  &
    ierr   &!  error flag
   ,varID  &!  the ID (number) of the netCDF variable
   ,xstart  !  starting point number

  LOGICAL :: errFound   !  error flag

  CHARACTER(len=len(ncCallType)+6) :: callType ! local version of
!     ncCallType, used to select code where GSWP2 and WATCH differ.
!     Length is sufficient for ncCallType + _gswp2 or _watch.

!-------------------------------------------------------------------------------

! Look for optional arguments.
  xstart = 1
  IF ( PRESENT(start) ) xstart=start

! Get the ID of the variable.
  ierr = nf90_inq_varid( ncID,varName,varID )

! If error, call error routine, indicating a stop.
  IF( ierr /= nf90_noErr ) CALL rwErr( formatNc,.TRUE.  &
                         ,ierr=ierr,ncID=ncID,varName=varName  &
                         ,errMess1='readVar2dReal_ncVector_gswp2 nf90_inq_varid'  &
                         ,errMess2=TRIM(ncCallType) )

!-------------------------------------------------------------------------------
! Create a new variable that can be used to select code for GSWP2 or WATCH
! "formats" when these need different code.
!-------------------------------------------------------------------------------
  errFound = .FALSE.
  callType = ncCallType   !  default

  SELECT CASE ( ncCallType )

!   Cases for which identical code can be used for GSWP2 and WATCH.
!   Nothing more to do.
    CASE ( 'init_agric','init_grid','init_hgt','init_pdm','init_soil'  &
          ,'init_top','veg_read' )

!   Cases that are only considered for GSWP2.
    CASE ( 'drive_read','init_ic' )
      if ( ncTypeArg /= ncTypeGSWP2 ) errFound = .TRUE.

!   Cases that require different code for GSWP2 and WATCH.
    CASE ( 'init_frac' )
      if ( ncTypeArg == ncTypeGSWP2 ) then
        callType = trim(ncCallType) // '_gswp2'
      elseif ( ncTypeArg == ncTypeWatch ) then
        callType = trim(ncCallType) // '_watch'
      else
        errFound = .TRUE.
      endif

    CASE default
      errFound = .TRUE.      

  END SELECT  !  ncCallType

!-------------------------------------------------------------------------------
! Stop if there is no code for this case.
!-------------------------------------------------------------------------------
  if ( errFound ) then
    write(*,*)'ERROR: readVar2dReal_ncVector_gswp2: no code for these options.'
    write(*,*)'ncCallType=',trim(ncCallType)
    write(*,*)'ncTypeArg=',trim(ncTypeArg)
    stop
  endif

!-------------------------------------------------------------------------------
! Read data.
!-------------------------------------------------------------------------------

  SELECT CASE ( callType )

!-------------------------------------------------------------------------------
!   First, cases other than 'init_ic'.
!-------------------------------------------------------------------------------
!   1-D cases.

    CASE ( 'init_agric','init_grid','init_pdm','init_soil','init_top' )
!     Read a vector with single dimension ('Land' or 'land').
      ierr = nf90_get_var( ncID,varID,outval,(/xstart/) )

!   2-D cases.

    CASE ( 'drive_read' )
!     Read a vector from 1st dimension ('land'),
!     for given value of 2nd dimension ('tstep').
      ierr = nf90_get_var( ncID,varID,outval,(/xstart,t/),(/npoints,1/) )

    CASE ( 'init_frac_watch','init_hgt' )
!     Read a vector from 1st dimension ('Land' or 'land'),
!     for given value of 2nd dimension ('Psuedo' or 'pseudo').
      ierr = nf90_get_var( ncID,varID,outval,(/xstart,z/),(/npoints,1/) )

!   3-D cases.
    CASE ( 'init_frac_gswp2','veg_read' )
!     Read a vector from 1st dimension ('Land' or 'land'),
!     for given value of 2nd dimension ('Psuedo' or 'pseudo')
!     and 3rd dimension ('Time' or 'tstep').
      ierr = nf90_get_var( ncID,varID,outval,(/xstart,z,t/),(/npoints,1,1/) )

!-------------------------------------------------------------------------------
!   Now, init_ic cases, selected on variable name.
!-------------------------------------------------------------------------------
    CASE ( 'init_ic' )

      SELECT CASE ( varName )

        CASE ( 'cs', 'gs' )
!         Read a vector with single dimension ('Land').
          ierr = nf90_get_var( ncID,varID,outval,(/xstart/) )

        CASE ( 'sthuf', 't_soil' )
!         Read a vector from 1st dimension ('Land'),
!         for given value of 2nd dimension ('Soil').
          ierr = nf90_get_var( ncID,varID,outval,(/xstart,z/),(/npoints,1/) )

        CASE ( 'canopy', 'frac', 'rgrain', 'snow_tile', 'tstar_tile' )
!         Read a vector from 1st dimension ('Land'),
!         for given value of 2nd dimension ('Psuedo').
          ierr = nf90_get_var( ncID,varID,outval,(/xstart,z/),(/npoints,1/) )

        CASE default
          WRITE(*,*)'ERROR: readVar2dReal_ncVector_gswp2: no code!'
          WRITE(*,*)'callType=',TRIM(callType),' varName=',TRIM(varName)
          STOP

      END SELECT
!-------------------------------------------------------------------------------

    CASE default
      WRITE(*,*)'ERROR: readVar2dReal_ncVector_gswp2: no code for callType.'
      WRITE(*,*)'callType=',TRIM(callType)
      WRITE(*,*)'ncCallType=',TRIM(ncCallType)
      WRITE(*,*)'ncTypeArg=',TRIM(ncTypeArg)
      STOP

  END SELECT  !  callType

!-------------------------------------------------------------------------------
! If error, call error routine, indicating a stop.
!-------------------------------------------------------------------------------
  IF ( ierr /= nf90_noErr ) CALL rwErr( formatNc,.TRUE.  &
                ,ierr=ierr,ncID=ncID,varName=varName,varID=varID  &
                ,t=t,xstart=xstart,z=z  &
                ,errMess1='readVar2dReal_ncVector_gswp2 nf90_get_var'  &
                ,errMess2=TRIM(callType) )

  END SUBROUTINE readVar2dReal_ncVector_gswp2

!###############################################################################
!###############################################################################
!###############################################################################
! subroutine readVar2dReal_nc_pilps2e
! Internal procedure in module jules_netcdf.
! Read a 2D field from netCDF file - a temporary (hopefully) fix for PILPS2e
! data.
!
  SUBROUTINE readVar2dReal_nc_pilps2e( ncCallType,ncID,varName  &
                                          ,nx,xstart,ny,ystart,t,outval )

  IMPLICIT NONE

  INTEGER, INTENT(in) ::  &!  in scalars
    ncID      &!  the ID (number) of the netCDF file
   ,nx        &!  size in x of data range to be read
   ,ny        &!  size in y of data range to be read
   ,t         &!  time number to read
   ,xstart    &!  first x location to read
   ,ystart     !  first x location to read

  INTEGER ::  &!  local scalars
    ierr   &!  error flag
   ,varID   !  the ID (number) of the netCDF variable

  REAL, INTENT(out) :: &!  out arrays
    outval(nx*ny)

  CHARACTER(len=*), INTENT(in) ::  &!  in scalars
    ncCallType  &!  type of call
   ,varName      !  the name of the netCDF variable

!-------------------------------------------------------------------------------

! Get the ID of the variable.
  ierr = nf90_inq_varid( ncID,varName,varID )

! If error, call error routine, indicating a stop.
  IF( ierr /= nf90_noErr ) CALL rwErr( formatNc,.TRUE.  &
                         ,ierr=ierr,ncID=ncID,varName=varName  &
                         ,errMess1='readVar2dReal_nc_pilps2e nf90_inq_varid'  &
                         ,errMess2=TRIM(ncCallType) )

  SELECT CASE ( ncCallType )
!-------------------------------------------------------------------------------
!   Only driving (meteorological) data can be read.
    CASE ( 'drive_read' )
!     Read a grid of nx*ny points from dimensions #1 and 2 ('x' and 'y'), for given
!     value of 3rd dimension ('tstep').
      ierr = nf90_get_var( ncID,varID,outval,(/xstart,ystart,t/),(/nx,ny,1/) )
!--------------------------------------------------
    CASE default
      WRITE(*,*)'ERROR: readVar2dReal_nc_pilps2e: no code for ncCallType.'
      WRITE(*,*)'ncCallType=',TRIM(ncCallType)
      STOP
  END SELECT

! If error, call error routine, indicating a stop.
  IF ( ierr /= nf90_noErr ) CALL rwErr( formatNc,.TRUE.  &
                ,ierr=ierr,ncID=ncID,varName=varName,varID=varID,t=t,xstart=xstart  &
                ,ystart=ystart,errMess1='readVar2dReal_nc_pilps2e nf90_get_var'  &
                ,errMess2=TRIM(ncCallType) )

  END SUBROUTINE readVar2dReal_nc_pilps2e
!###############################################################################
!###############################################################################
!###############################################################################
! subroutine readVar2dReal_nc_princet
! Internal procedure in module jules_netcdf.
! Read a 2D field from netCDF file - a temporary (hopefully) fix for Princeton
! data (Sheffield et al., 2006, J. Climate.)
! data.
!
  SUBROUTINE readVar2dReal_nc_princet( ncCallType,ncID,varName  &
                                          ,nx,xstart,ny,ystart,t,outval )

  IMPLICIT NONE

! Local scalar parameters.
  INTEGER, PARAMETER :: z = 1  !  index to use in "z" dimension

  INTEGER, INTENT(in) ::  &!  in scalars
    ncID      &!  the ID (number) of the netCDF file
   ,nx        &!  size in x of data range to be read
   ,ny        &!  size in y of data range to be read
   ,t         &!  time number to read
   ,xstart    &!  first x location to read
   ,ystart     !  first x location to read

  INTEGER ::  &!  local scalars
    ierr   &!  error flag
   ,varID   !  the ID (number) of the netCDF variable

  REAL, INTENT(out) :: &!  out arrays
    outval(nx*ny)

  CHARACTER(len=*), INTENT(in) ::  &!  in scalars
    ncCallType  &!  type of call
   ,varName      !  the name of the netCDF variable

!-------------------------------------------------------------------------------

! Get the ID of the variable.
  ierr = nf90_inq_varid( ncID,varName,varID )

! If error, call error routine, indicating a stop.
  IF( ierr /= nf90_noErr ) CALL rwErr( formatNc,.TRUE.  &
                         ,ierr=ierr,ncID=ncID,varName=varName  &
                         ,errMess1='readVar2dReal_nc_princet nf90_inq_varid'  &
                         ,errMess2=TRIM(ncCallType) )

  SELECT CASE ( ncCallType )
!-------------------------------------------------------------------------------
!   Only driving (meteorological) data can be read.
    CASE ( 'drive_read' )
!     Read a grid of nx*ny points from dimensions #1 and 2 ('longitude' and
!     'latitude'), for given values of 3rd and 4th dimensions ('z','time').
      ierr = nf90_get_var( ncID,varID,outval,(/xstart,ystart,z,t/),(/nx,ny,1,1/) )
!--------------------------------------------------
    CASE default
      WRITE(*,*)'ERROR: readVar2dReal_nc_princet: no code for ncCallType.'
      WRITE(*,*)'ncCallType=',TRIM(ncCallType)
      STOP
  END SELECT

! If error, call error routine, indicating a stop.
  IF ( ierr /= nf90_noErr ) CALL rwErr( formatNc,.TRUE.  &
                ,ierr=ierr,ncID=ncID,varName=varName,varID=varID,t=t,xstart=xstart  &
                ,ystart=ystart,z=z,errMess1='readVar2dReal_nc_princet nf90_get_var'  &
                ,errMess2=TRIM(ncCallType) )

  END SUBROUTINE readVar2dReal_nc_princet

!###############################################################################
!###############################################################################
!###############################################################################
! subroutine readVar2dReal_nc_tseries
! Internal procedure in module jules_netcdf.
! Read a 2D (to JULES) field from a netCDF file that contains a single space point.
!
  SUBROUTINE readVar2dReal_nc_tseries( ncCallType,ncID,varName,t  &
                                      ,outval )

  IMPLICIT NONE

  INTEGER, INTENT(in) ::  &!  in scalars
    ncID      &!  the ID (number) of the netCDF file
   ,t          !  time number to read

  INTEGER ::  &!  local scalars
    ierr   &!  error flag
   ,varID   !  the ID (number) of the netCDF variable

  REAL, INTENT(out) :: &!  out arrays
    outval(1)

  CHARACTER(len=*), INTENT(in) ::  &!  in scalars
    ncCallType  &!  type of call
   ,varName      !  the name of the netCDF variable

!-------------------------------------------------------------------------------

! Get the ID of the variable.
  ierr = nf90_inq_varid( ncID,varName,varID )

! If error, call error routine, indicating a stop.
  IF( ierr /= nf90_noErr ) CALL rwErr( formatNc,.TRUE.  &
                         ,ierr=ierr,ncID=ncID,varName=varName  &
                         ,errMess1='readVar2dReal_nc_tseries nf90_inq_varid'  &
                         ,errMess2=TRIM(ncCallType) )

  SELECT CASE ( ncCallType )
!-------------------------------------------------------------------------------
!   Only driving (meteorological) data can be read.
    CASE ( 'drive_read' )
!     Read a single value from dimension #1 ( time ).
      ierr = nf90_get_var( ncID,varID,outval,(/t/),(/1/) )
!--------------------------------------------------
    CASE default
      WRITE(*,*)'ERROR: readVar2dReal_nc_tseries: no code for ncCallType.'
      WRITE(*,*)'ncCallType=',TRIM(ncCallType)
      STOP
  END SELECT

! If error, call error routine, indicating a stop.
  IF ( ierr /= nf90_noErr ) CALL rwErr( formatNc,.TRUE.  &
                ,ierr=ierr,ncID=ncID,varName=varName,varID=varID,t=t  &
                ,errMess1='readVar2dReal_nc_tseries nf90_get_var'  &
                ,errMess2=TRIM(ncCallType) )

  END SUBROUTINE readVar2dReal_nc_tseries

!###############################################################################
!###############################################################################
! subroutine check_nc_dims
! Internal procedure in module jules_netcdf.
! Checks that the dimensions found in an existing netCDF file match those
! expected.
! Note that this is far from being a full check - it will establish if the
! dimensions exist at all in the file, but not if the required variables have
! these dimensions. This is bit of a faff, and could alternatively be dealt with
! by adding to error diagnostics if an error occurs on read.

  SUBROUTINE check_nc_dims( ncID,ncCallType,ncFileType )

  IMPLICIT NONE

! Local scalar parameters.
  INTEGER, PARAMETER :: ndimMax = 4     !  maximum possible number of dimensions

! Scalar arguments with intent(in)
  INTEGER, INTENT(in) :: ncID                !  ID (number) of an existing and
!                                               open netCDf file
  CHARACTER(len=*), INTENT(in) :: ncCallType !  flag indicating what type of
!                                               data to be read from this file
!                            This is used to identify what dimensions are expected.
  CHARACTER(len=*), INTENT(in) :: ncFileType !  indicates what "type" of netCDF
!                                               files are to be read

! Local scalar variables.
  INTEGER ::  &
    dimID  &!  dimension ID (number)
   ,i      &!  loop counter
   ,ierr   &!  error flag
   ,ndim    !  number of dimensions for a particular case

  LOGICAL :: errFound         !   flag indicating if an error has been found

! Local array variables.
  CHARACTER(len=10) :: dimName(ndimMax)       !  names of the netCDF dimensions

!-------------------------------------------------------------------------------

  errFound = .FALSE.

! Set up the number and names of dimensions in each case.
! Note that the Sun (Solaris) compiler currently requires "array constructor values"
! for character variables to all be of the same length - hence some of the
! trailing blanks in dimName below.


  SELECT CASE ( ncFileType )

!-------------------------------------------------------------------------------
    CASE ( ncTypeGswp2 )

!     NB The "pseudo" dimension in GSWP2 files was mistakenly called "Psuedo".
!     To allow this code to be used with existing files, we have to indicate
!     the misspelled name.

      SELECT CASE ( ncCallType )

        CASE ( 'init_agric','init_grid','init_soil','init_top' )
          ndim = 1
          dimName(1:ndim) = (/ 'Land' /)

        CASE ( 'drive_read' )
          ndim = 2
          dimName(1:ndim) = (/ 'land ', 'tstep' /)

        CASE ( ' init_hgt' )
          ndim = 2
          dimName(1:ndim) = (/ 'Land  ', 'Psuedo' /)

        CASE ( 'init_frac', 'veg_read' )
          ndim = 3
          dimName(1:ndim) = (/ 'Land  ', 'Psuedo', 'Time  ' /)

        CASE ( 'init_ic' )
          ndim = 3
          dimName(1:ndim) = (/ 'Land  ', 'Psuedo', 'Soil  ' /)

        CASE default
          errFound = .TRUE.

      END SELECT

!-------------------------------------------------------------------------------
    CASE ( ncTypeJules )
      SELECT CASE ( ncCallType )

        CASE ( 'dump_io' )
!         In fact we do nothing here as this is handled in dump i/o code.
!         Just leave the routine (rather untidy!)
          RETURN

        CASE default
          errFound = .TRUE.

      END SELECT
!-------------------------------------------------------------------------------
    CASE ( ncTypePilps2e )

      SELECT CASE ( ncCallType )

        CASE ( 'drive_read' )
          ndim = 3
          dimName(1:ndim) = (/ 'x    ', 'y    ', 'tstep' /)

        CASE default
          errFound = .TRUE.

      END SELECT

!-------------------------------------------------------------------------------
    CASE ( ncTypePrincet )

      SELECT CASE ( ncCallType )

        CASE ( 'drive_read' )
          ndim = 4
          dimName(1:ndim) = (/ 'longitude', 'latitude ', 'z        ', 'time     ' /)

        CASE default
          errFound = .TRUE.

      END SELECT

!-------------------------------------------------------------------------------
    CASE ( ncTypeTseries )

      SELECT CASE ( ncCallType )

        CASE ( 'drive_read' )
          ndim = 1
          dimName(1:ndim) = (/ 'time' /)

        CASE default
          errFound = .TRUE.

      END SELECT

!-------------------------------------------------------------------------------
    CASE ( ncTypeWatch )

!     Similar to ncTypeGSWP2 but with relatively subtle but important differences!

      SELECT CASE ( ncCallType )

        CASE ( 'init_agric', 'init_grid', 'init_soil', 'init_top' )
          ndim = 1
          dimName(1:ndim) = (/ 'land' /)

        CASE ( 'init_frac',' init_hgt' )
          ndim = 2
          dimName(1:ndim) = (/ 'land  ', 'pseudo' /)

        CASE ( 'veg_read' )
          ndim = 3
          dimName(1:ndim) = (/ 'land  ', 'pseudo', 'tstep ' /)

        CASE default
          errFound = .TRUE.

      END SELECT

!-------------------------------------------------------------------------------
    CASE default
      errFound = .TRUE.

  END SELECT

! Stop if error found.
  IF ( errFound ) THEN
    WRITE(*,*) 'ERROR: check_nc_dims: no code for this combination:'
    WRITE(*,*) 'ncFileType=',TRIM(ncFileType),' ncCallType=',TRIM(ncCallType)
    WRITE(*,*)'This code is checking that an existing netCDF file is of the'
    WRITE(*,*)'type claimed, with the expected DIMENSION names.'
    WRITE(*,*)'To introduce a new "type" of netCDF file, add more code!'
    WRITE(*,*)'Stopping in subroutine check_nc_dims'
    STOP
  ENDIF

!-------------------------------------------------------------------------------
! Check that the dimensions exist in file.
!-------------------------------------------------------------------------------

  DO i=1,ndim
    ierr = nf90_inq_dimid( ncID,TRIM(dimName(i)),dimID )
!   If error, call error routine.
    IF ( ierr /= nf90_noErr ) THEN
      errFound = .TRUE.
      WRITE(*,*)'ERROR: check_nc_dims: error checking for dimensions.'
      CALL rwErr( formatNc,.FALSE.,ierr=ierr,ncID=ncID,dimName=dimName(i)  &
                 ,errMess1='nf90_inq_dimid' )
    ENDIF
  ENDDO

! Stop if error found.
  IF ( errFound ) THEN
    WRITE(*,*)
    WRITE(*,*)'The code expected to find the following dimensions in the file:'
    DO i=1,ndim
      WRITE(*,*) TRIM( dimName(i) )
    ENDDO
    WRITE(*,*)'Stopping in subroutine check_nc_dims'
    STOP
  ENDIF

  END SUBROUTINE check_nc_dims
!###############################################################################
!###############################################################################

  SUBROUTINE readVarNcDump( ncID,varName,ncCount,outval )

! Read a variable from a netCDF dump file.
! Module procedure in module jules_netcdf.

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Scalar arguments with intent(in)
!-------------------------------------------------------------------------------
  INTEGER, INTENT(in) :: ncID      !  ID of netCDF dataset
  CHARACTER(len=*), INTENT(in) :: varName  !  netCDF variable name

!-------------------------------------------------------------------------------
! Array arguments with intent(in)
!-------------------------------------------------------------------------------
  INTEGER, INTENT(in) :: ncCount(:)  !  counts for variable

!-------------------------------------------------------------------------------
! Array arguments with intent(out)
!-------------------------------------------------------------------------------
  REAL, INTENT(out) :: outval(:,:)   !  data read from file
!      Note that a 3-D variable is read into this 2-D space, not because that
!      is sensible, but because that means that existing 2-D code can be reused.

!-------------------------------------------------------------------------------
! Local scalar variables.
!-------------------------------------------------------------------------------
  INTEGER :: ierr   !  return code
  INTEGER :: varID  !  netCDF ID of a variable

 outval(:,:) = 0.0 ! Jupp - initialise output variable

!-------------------------------------------------------------------------------
! Get ID of variable.
!-------------------------------------------------------------------------------
  ierr = nf90_inq_varid( ncID,varName,varID )

! If error, call error routine, indicating a stop.
  IF( ierr /= nf90_noErr ) CALL rwErr( formatNc,.TRUE.  &
                         ,ierr=ierr,ncID=ncID,varName=varName  &
                         ,errMess1='Error from  nf90_inq_varid'  &
                         ,errMess2='Called from readVarNcDump' )

!-------------------------------------------------------------------------------
! Read data. By default start=1.
!-------------------------------------------------------------------------------
  ierr = nf90_get_var( ncID,varID,outval,count=ncCount )

! If error, call error routine, indicating a stop.
  IF( ierr /= nf90_noErr ) CALL rwErr( formatNc,.TRUE.  &
                         ,ierr=ierr,ncID=ncID,varName=varName  &
                         ,errMess1='Error from nf90_get_var'  &
                         ,errMess2='Called from readVarNcDump' )

  END SUBROUTINE readVarNcDump

!###############################################################################
!###############################################################################

END MODULE jules_netcdf

