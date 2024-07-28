!Jupp
!$TAF MODULE rwErr_mod SUBROUTINE rwErr INPUT = 1
!$TAF MODULE rwErr_mod SUBROUTINE rwErr OUTPUT = 
!$TAF MODULE rwErr_mod SUBROUTINE rwErr ACTIVE = 
!$TAF MODULE rwErr_mod SUBROUTINE rwErr DEPEND = 
!Jupp

! module rwErr_mod
!
! Contains error routine for reading and writing routines (subroutine rwErr).
!
!###############################################################################
!###############################################################################

  MODULE rwErr_mod

  USE inout, ONLY :  &
!  imported scalar parameters
    formatAsc,formatBin,formatNc

!Jupp  USE netcdf, ONLY :  &
!Jupp!  imported procedures
!Jupp     nf90_inquire_dimension,nf90_strerror

  IMPLICIT NONE

  CONTAINS

!#######################################################################
!#######################################################################
! subroutine rwErr
! Internal procedure in readWrite_mod.
! Reports an error on read or write.

  SUBROUTINE rwErr( fileFormat,terminate  &
                   ,dimID,ierr,irec,irecPrev,ncID,nfieldFile,nval  &
                   ,field,fieldCode,t,z,unit,varID,xstart,ystart  &
                   ,dimName,errMess1,errMess2,fileName,varName  &
                   ,ncCount,ncMap,ncStart )

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Scalar arguments with intent(in)
!-------------------------------------------------------------------------------
  CHARACTER(len=*), INTENT(in) ::  &
    fileFormat  !  format of file

  LOGICAL, INTENT(in) ::  &
    terminate  !  TRUE means end run, FALSE means continue

!--------------------------------------------------
! Optional scalar arguments with intent(in)
!-------------------------------------------------------------------------------
  INTEGER, INTENT(in), OPTIONAL ::  &
    dimID    &!  ID (number) of a netCDF dimension
   ,nfieldFile &!  number of fields in file
   ,ierr     &!  error value
   ,irec     &!  requested record number (accumulated fields)
   ,irecPrev &!  previous record number (accumulated fields)
   ,ncID     &!  ID (number) of a netCDF file
   ,nval     &!  size of a data field (number of values)
   ,field    &!  requested field number
   ,fieldCode   &!  field code  (e.g. STASH or PP)
   ,t        &!  time level requested
   ,z        &!  z level requested
   ,unit     &!  unit
   ,varID    &!  ID (number) of a netCDF variable
   ,xstart   &!  start point for netCDF in x
   ,ystart    !  start point for netCDF in y

  CHARACTER(len=*), INTENT(in), OPTIONAL ::  &
    dimName             &!  name of a netCDF dimension
   ,errMess1,errMess2   &!  messages printed on error
   ,fileName            &!  name of file
   ,varName              !  name of a variable (netCDF only)

!-------------------------------------------------------------------------------
! Optional array arguments with intent(in)
!-------------------------------------------------------------------------------
  integer, intent(in), optional :: ncCount(:)  !  count argument for netCDF
  integer, intent(in), optional :: ncMap(:)    !  map argument for netCDF
  integer, intent(in), optional :: ncStart(:)  !  start argument for netCDF

!--------------------------------------------------
! Local scalars variables.
!-------------------------------------------------------------------------------
  INTEGER ::  &
    dimLen    &!  length of a netCDF dimension
   ,i          !  work

  CHARACTER(len=200) ::  &
    workChar   !  work (various uses)
!-------------------------------------------------------------------------------

  WRITE(*,"(/,a)")'ERROR: rwErr: error on read or write.'
  IF ( PRESENT(errMess1) ) WRITE(*,*) TRIM(errMess1)
  IF ( PRESENT(errMess2) ) WRITE(*,*) TRIM(errMess2)
  WRITE(*,*)'fileFormat=',TRIM(fileFormat)

  IF ( PRESENT(unit) ) THEN
    INQUIRE( unit, name=workChar )
    WRITE(*,*)'unit=',unit,' file=',TRIM(workChar)
  ENDIF

  IF ( PRESENT(fieldCode) ) WRITE(*,*)'Field code=',fieldCode
  IF ( PRESENT(field) ) WRITE(*,*)'Field number=',field
  IF ( PRESENT(nfieldFile) ) WRITE(*,*)'Number of fields in file (nfieldFile)=',nfieldFile
  IF ( PRESENT(irec) ) WRITE(*,*)'Record number (irec)=',irec
  IF ( PRESENT(irecPrev) ) WRITE(*,*)'Previous record number (irecPrev)=',irecPrev
  IF ( PRESENT(ierr) .AND. fileFormat/=formatNc) THEN
    WRITE(*,*)'ierr=',ierr
    IF ( ierr < 0 ) WRITE(*,*)'End of file.'
  ENDIF
  IF ( PRESENT(nval) ) WRITE(*,*)'Size of field (nval)=',nval

!-------------------------------------------------------------------------------
! Options for netCDF files.
  IF ( fileFormat == formatNc ) THEN

!Jupp    IF ( PRESENT(fileName) ) WRITE(*,*)'fileName=',fileName
!Jupp    IF ( PRESENT(ncID) ) WRITE(*,*)'ncID=',ncID
!Jupp    IF ( PRESENT(dimName) ) WRITE(*,*)'dimName=',dimName
!Jupp    IF ( PRESENT(dimID) ) THEN
!Jupp      WRITE(*,*)'dimID=',dimID
!Jupp      IF ( PRESENT(ncID) ) THEN
!Jupp        i = nf90_inquire_dimension(ncID,dimID,workChar,dimLen)
!Jupp        IF ( .NOT. PRESENT(dimName) ) WRITE(*,*)'Dimension name is',workChar
!Jupp        WRITE(*,*)'Dimension length=',dimLen
!Jupp      ENDIF
!Jupp    ENDIF
!Jupp    IF ( PRESENT(varName) ) WRITE(*,*)'varName=',varname
!Jupp    IF ( PRESENT(varID) ) WRITE(*,*)'varID=',varID
!Jupp    IF ( PRESENT(xstart) ) WRITE(*,*)'xstart=',xstart
!Jupp    IF ( PRESENT(ystart) ) WRITE(*,*)'ystart=',ystart
!Jupp    IF ( PRESENT(t) ) WRITE(*,*)'t=',t
!Jupp    IF ( PRESENT(z) ) WRITE(*,*)'z=',z
!Jupp    IF ( PRESENT(ierr) ) PRINT*,'netCDf error: ',TRIM(  nf90_strerror(ierr) )
!Jupp
!Jupp    IF ( PRESENT(ncStart) ) WRITE(*,*)'start=',ncStart
!Jupp    IF ( PRESENT(ncCount) ) WRITE(*,*)'count=',ncCount
!Jupp    IF ( PRESENT(ncMap) ) WRITE(*,*)'map=',ncMap

  ENDIF  !  formatNc

  IF ( terminate ) THEN
    WRITE(*,*)'Stopping in rwErr'
    STOP
  ENDIF

  END SUBROUTINE rwErr
!###############################################################################
!###############################################################################
  END MODULE rwErr_mod
