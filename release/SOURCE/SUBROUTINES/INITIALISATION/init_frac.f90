!###############################################################################
!###############################################################################
! Read fractional coverage for surface types.
! NB A check that sum(frac)=1 is performed in init_ic().
!###############################################################################
!###############################################################################
SUBROUTINE init_frac()

  USE ancil_info, ONLY :  &
!  imported scalars with intent(in)
     land_pts  &
!  imported arrays with intent(out)
    ,frac

  USE file_utils, ONLY:  &
!  imported procedures
     closeFile,fileUnit,findTag,openFile  &
!  imported arrays with intent(out)
    ,irecPrev

  USE inout, ONLY :  &
!  imported scalar parameters
     formatLen,formatAsc,formatBin,formatNc,formatPP,jinUnit,tagAscBin,tagNc  &
!  imported scalars with intent(in)
    ,echo,nxIn,nyIn  &
!  imported scalars with intent(out)
    ,readFracIC  &
!  imported arrays with intent(in)
    ,mapInLand

  USE jules_netcdf, ONLY :  &
!  imported scalars with intent(in)
     ncType

  USE nstypes, ONLY :  &
!  imported scalars with intent(in)
     ntype

  USE readwrite_mod, ONLY :  &
!  imported procedures
     readvar3dComp

  USE switches, ONLY :  &
!  imported scalars with intent(in)
     l_veg_compete,routeOnly
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER, PARAMETER ::  &!  scalar parameters
   stashFrac = 216  !  STASH code for fractional coverage field. PP code = 1391.

  INTEGER ::  &!  local SCALARS
    fieldNum     &!  field number in file of first field in frac
   ,i            &!  loop counter
   ,inUnit       &!  unit used to connect to input file
   ,nfieldFile   &!  number of fields per time in a file
   ,nheaderField &!  number of header records before each field in file
   ,nheaderFile  &!  number of header records at start of file
   ,nheaderT     &!  number of headers at start of each time
   ,nlineField   &!  work
   ,readT        &!  time level to be read from file
   ,useIndex      !  index in irecPrev

  LOGICAL ::  &!  local SCALARS
    readFile   !  flag indicating if another file is to be read

  CHARACTER(len=formatLen) ::  &! local SCALARS
    fileFormat   !  format of file

  CHARACTER(len=150) ::  &! local SCALARS
    fileName  &!  the name of a file
   ,varName    !  name of variable in input file

!------------------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
! If data are not needed, nothing to do.
!-------------------------------------------------------------------------------
  IF ( routeOnly ) RETURN

  if (echo) WRITE(*,"(50('-'),/,a)") 'init_frac' !Jupp

!-------------------------------------------------------------------------------
! Locate the start of this section in input file and establish whether
! fractional cover is to be read here or with initial condition.
!-------------------------------------------------------------------------------
  CALL findTag( jinUnit,'init_frac','>INIT_FRAC' )
  READ(jinUnit,*) readFracIC

!-------------------------------------------------------------------------------
! For runs with competing vegetation, insist that frac is read as initial condition.
!-------------------------------------------------------------------------------
  IF ( l_veg_compete .AND. .NOT.readFracIC ) THEN
    if (echo) then !Jupp
    WRITE(*,*)'ERROR: init_frac: A run with competing vegetation must read frac with initial condition. Set readFracIC=T.'
    end if !Jupp
    STOP
  ENDIF

  IF ( readFracIC ) THEN
    if (echo) then !Jupp
    WRITE(*,*)'readFracIC=T, meaning fractional cover will be read with the initial condition.'
    WRITE(*,*)'frac will NOT be read by init_frac'
    end if !Jupp
    RETURN
  ENDIF

!-------------------------------------------------------------------------------
! Establish where data are to be read from, and other details of format.  Only
! read parameters for the file format indicated.
!-------------------------------------------------------------------------------
  READ(jinUnit,*) readFile

  IF ( readFile ) THEN
!-------------------------------------------------------------------------------
!   An external file will be read.
!-------------------------------------------------------------------------------
    READ(jinUnit,*) fileFormat
    READ(jinUnit,*) fileName

    SELECT CASE ( fileFormat )

      CASE ( formatAsc,formatBin,formatPP )
!       Locate the information in run control file.
        CALL findTag( jinUnit,'init_frac',tagAscBin )
        READ(jinUnit,*) nheaderFile,nheaderField
        READ(jinUnit,*) fieldNum

      CASE ( formatNc )
!       Locate the information in run control file.
        CALL findTag( jinUnit,'init_grid',tagNc,preInit=.TRUE. )
        READ(jinUnit,*) varName

      CASE default
        if (echo) WRITE(*,*) & !Jupp
	'ERROR: init_frac: no code for fileFormat=',TRIM(fileFormat) !Jupp
        STOP

    END SELECT

  ELSE   !  NOT readFile
!-------------------------------------------------------------------------------
!   Data will be read from run control file.
!   The first field will be read, no headers expected. Field number is
!   redundant for stdIn, but is used to set nfieldFile.
!-------------------------------------------------------------------------------
    fileFormat = formatAsc
    nheaderFile = 0
    nheaderField = 0
    fieldNum = 1

  ENDIF

!-------------------------------------------------------------------------------
! Open file.
!-------------------------------------------------------------------------------
  IF ( readFile ) THEN
    inUnit = fileUnit( fileFormat )  !  get unit
!   Use first arg to openFile to set recl (unformatted file) to be enough for a single value.
    CALL openFile( 1,.FALSE.,inUnit,'read',fileFormat,fileName,'old','init_frac',ncType )
  ELSE
    if (echo) then !Jupp
    WRITE(*,*)'Reading fractional cover from the run control file.'
    WRITE(*,*)'Data must be the first field encountered.'
    end if !Jupp
    inUnit = jinUnit
!   Locate the start of the data in the run control file.
    CALL findTag( inUnit,'init_frac','>DATA',preInit=.TRUE. )
  ENDIF

!-------------------------------------------------------------------------------
! Read data.
!-------------------------------------------------------------------------------
  IF ( inUnit==jinUnit .AND. nxIn*nyIn==1 ) THEN
!   If only need to read one space point from run control file, expect no new
!   line between fields (eg all on one line).  Calling readVar means we could
!   cope with headers in the run control file!  But there's no need since
!   annotation is already simple in this case.
    READ(jinUnit,*) frac(:,:)
  ELSE
!   Simplifying assumptions regarding input file. Otherwise have to read these in.
    readT      = 1                !  time level to read from file
    nfieldFile = fieldNum+ntype-1 !  # of fields in file. Set to last level of required field - OK while readT=1
    nheaderT   = 0                !  no headers at top of each time
    nlineField = 0                !  0 means will not attempt to read ASCII line-by-line

!   Set index to use for irecPrev with netCDF files - this irecPrev isn't changed,
!   but need to keep index within bounds.
    useIndex = inUnit
    IF ( fileFormat == formatNC ) useIndex = 1

    CALL readVar3dComp( readT,fieldNum,stashFrac,.FALSE.,irecPrev(useIndex)  &
                       ,nfieldFile,nheaderFile  &
                       ,nheaderT,nheaderField,nlineField,nxIn,nyIn,ntype,.FALSE. &
                       ,inUnit,varName  &
                       ,mapInLand(:),(/(i,i=1,land_pts)/)  &
                       ,fileFormat,frac(:,:),'init_frac','init_frac',ncType )
  ENDIF

!-------------------------------------------------------------------------------
! Close file if it is not the JULES control file
!-------------------------------------------------------------------------------
  IF ( inUnit /= jinUnit ) CALL closeFile( inUnit,fileFormat )

!------------------------------------------------------------------------------
! If requested, write to stdout the fractions for each gridbox.
!-------------------------------------------------------------------------------
  IF ( echo ) THEN
    DO i=1,ntype
      WRITE(*,"(a,i3,a,(10f6.3))") 'type ',i,' frac=',frac(:,i)
    ENDDO
  ENDIF

END SUBROUTINE init_frac

