C
C NEV event lines read
C Want to read NEV lines of the form
C
C  EVID     DXEV        DYEV
C  DPRK1    2.346512   -0.719774
C  DPRK2    0.000000    0.000000
C  DPRK3   -0.365993   -0.306754
C  DPRK4   -0.711693    0.348541
C  DPRK5   -0.158407    0.394550
C  DPRK6   -0.090634    0.049051
C
C These are in kilometers - all Cartesian - we will convert to
C lat and lon eventually.
C
C We read from logical file unit LUIN (could be 5 for standard input)
C
C We return with an error if the end of the input is reached
C without NEV events being read.
C NEVMAX is also specified as the dimensions of these arrays.
C The name of an event can have up to 24 characters
C We read in CHARACTER*(24) CEVARR( NEVMAX )
C and        INTEGER        LEVARR( NEVMAX )
C and        REAL*8         DRXARR( NEVMAX )
C and        REAL*8         DRYARR( NEVMAX )
C
      SUBROUTINE NEVELR( IERR, LUIN, NEVMAX, NEV,
     1                   CEVARR, LEVARR, DRXARR, DRYARR )
      IMPLICIT NONE
C
      INTEGER        IERR
      INTEGER        LUIN
      INTEGER        NEVMAX
      INTEGER        NEV
      CHARACTER*(24) CEVARR( NEVMAX )
      INTEGER        LEVARR( NEVMAX )
      REAL*8         DRXARR( NEVMAX )
      REAL*8         DRYARR( NEVMAX )
C
C     Variables for the routine CSARGM
C
      INTEGER     IARG
      INTEGER     I1
      INTEGER     I2
      INTEGER     NARGM
      PARAMETER ( NARGM = 3 )
      INTEGER     NARGS, CSLEN, IARGL( NARGM, 2 )
C
      INTEGER         IEV
      INTEGER         LEV
      CHARACTER*(200) CHARG
      CHARACTER*(200) CMESS
      CHARACTER*(24)  CEV
      REAL*8          DRX
      REAL*8          DRY
C
      IERR   = 0
      CMESS  = ' '
      IF ( LUIN.LT.7 .AND. LUIN.NE.5 ) THEN
        WRITE (6,*) 'NEVELR: LUIN = ', LUIN
        IERR    = 1
        RETURN
      ENDIF
      IF ( NEV.GT.NEVMAX .OR. NEV.LE.0 ) THEN
        WRITE (6,*) 'NEVELR: NEV    = ', NEV
        WRITE (6,*) 'NEVELR: NEVMAX = ', NEVMAX
        IERR    = 1
        RETURN
      ENDIF
C
      IEV    = 0
 30   CONTINUE
      CHARG  = ' '
      CMESS  = 'Error reading in event line'
      READ ( 5, '(A)', ERR=99, END=99 ) CHARG
      IF ( CHARG(1:1).EQ.'#' ) GOTO 30
      IF ( CHARG(1:1).EQ.'*' ) GOTO 30
      CSLEN  = 200
      NARGS  = 3
      CALL CSARGM( CHARG, NARGS, NARGM, CSLEN, IARGL, IERR )
      IF ( IERR.NE.0 ) THEN
        CMESS  = 'Error from CSARGM '
        WRITE (6,*) 'CSARGM returned IERR = ', IERR
        GOTO 99
      ENDIF
C
C Need to find CEV
C
      IARG   = 1
      I1     = IARGL( IARG, 1 )
      I2     = IARGL( IARG, 2 )
      LEV    = I2 - I1 + 1
      IF ( LEV.GT.24 ) THEN
        WRITE (6,*) 'Invalid length for event ', CHARG(I1:I2)
        GOTO 99
      ENDIF
      CEV    = ' '
      CEV    = CHARG(I1:I2)
C
C Need to find RX
C
      IARG   = 2
      I1     = IARGL( IARG, 1 )
      I2     = IARGL( IARG, 2 )
      CMESS  = 'Error reading RX'
      READ ( CHARG(I1:I2), *, ERR=99, END=99 ) DRX
C
C Need to find RY
C
      IARG   = 3
      I1     = IARGL( IARG, 1 )
      I2     = IARGL( IARG, 2 )
      CMESS  = 'Error reading RY'
      READ ( CHARG(I1:I2), *, ERR=99, END=99 ) DRY
C
C (have successfully read in CEV, LEV, DRX, DRY. Now need to
C enter them into the appropriate arrays)
C
      IF ( IEV.EQ.NEVMAX ) THEN
        WRITE (6,*) 'NEVELR: IEV    = ', IEV
        WRITE (6,*) 'NEVELR: NEVMAX = ', NEVMAX
        WRITE (6,*) 'IEV about to exceed NEVMAX '
        GOTO 99
      ENDIF
C
      IEV    = IEV + 1
      CEVARR( IEV ) = CEV
      LEVARR( IEV ) = LEV
      DRXARR( IEV ) = DRX
      DRYARR( IEV ) = DRY
      IF ( IEV.EQ.NEV ) RETURN
      GOTO 30
C note that we can comment out this 40 CONTINUE as we should never
C get there - as any reaching the end of the file should exit
C with error
c40   CONTINUE
c     RETURN
C
 99   CONTINUE
      WRITE (6,*) 'NEVELR: Error '
      WRITE (6,*) CMESS
      IERR   = 1
      RETURN
      END
C
