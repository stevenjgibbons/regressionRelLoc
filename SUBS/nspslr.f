C
C NSP station and phase lines read
C Want to read NSP lines of the form
C STAT PHASE lat lon lat lon SX SY
C
C
C We read from logical file unit LUIN (could be 5 for standard input)
C
C We return with an error if the end of the input is reached
C without NSP station and phases being read.
C NSPMAX is also specified as the maximum dimension of the arrays.
C
      SUBROUTINE NSPSLR( IERR, LUIN, NSPMAX, NSP,
     1                   CSTARR, LSTARR, CPHARR, LPHARR,
     2                   DSTLAT, DSTLON, DRFLAT, DRFLON,
     3                   DSXARR, DSYARR )
      IMPLICIT NONE
C
      INTEGER       IERR
      INTEGER       LUIN
      INTEGER       NSPMAX
      INTEGER       NSP
      CHARACTER*(8) CSTARR( NSPMAX )
      INTEGER       LSTARR( NSPMAX )
      CHARACTER*(8) CPHARR( NSPMAX )
      INTEGER       LPHARR( NSPMAX )
      REAL*8        DSTLAT( NSPMAX )
      REAL*8        DSTLON( NSPMAX )
      REAL*8        DRFLAT( NSPMAX )
      REAL*8        DRFLON( NSPMAX )
      REAL*8        DSXARR( NSPMAX )
      REAL*8        DSYARR( NSPMAX )
C
      CHARACTER*(8) CSTAT
      INTEGER       LST
      CHARACTER*(8) CPHAS
      INTEGER       LPH
      REAL*8        DLAT1
      REAL*8        DLON1
      REAL*8        DLAT2
      REAL*8        DLON2
      REAL*8        DSX
      REAL*8        DSY
C
      CHARACTER*(200) CHARG
      CHARACTER*(80)  CMESS
C
C     Variables for the routine CSARGM
C
      INTEGER     IARG
      INTEGER     I1
      INTEGER     I2
      INTEGER     NARGM
      PARAMETER ( NARGM = 8 )
      INTEGER     NARGS, CSLEN, IARGL( NARGM, 2 )
C
      INTEGER     ISP
C
      IERR   = 0
      CMESS  = ' '
      IF ( LUIN.LT.7 .AND. LUIN.NE.5 ) THEN
        WRITE (6,*) 'NSPSLR: LUIN = ', LUIN
        IERR    = 1
        RETURN
      ENDIF
      IF ( NSP.GT.NSPMAX .OR. NSP.LT.0 ) THEN
        WRITE (6,*) 'NSPSLR: NSP    = ', NSP
        WRITE (6,*) 'NSPSLR: NSPMAX = ', NSPMAX
        IERR    = 1
        RETURN
      ENDIF
C
      CMESS   = ' '
      ISP    = 0
 30   CONTINUE
      CHARG  = ' '
      CMESS  = 'Error reading in slowness line'
      READ ( 5, '(A)', ERR=99, END=99 ) CHARG
      IF ( CHARG(1:1).EQ.'#' ) GOTO 30
      IF ( CHARG(1:1).EQ.'*' ) GOTO 30
      CSLEN  = 200
      NARGS  = 8
      CALL CSARGM( CHARG, NARGS, NARGM, CSLEN, IARGL, IERR )
      IF ( IERR.NE.0 ) THEN
        CMESS  = 'Error from CSARGM '
        WRITE (6,*) 'CSARGM returned IERR = ', IERR
        GOTO 99
      ENDIF
C
C Need to find STAT
C
      IARG   = 1
      I1     = IARGL( IARG, 1 )
      I2     = IARGL( IARG, 2 )
      LST    = I2 - I1 + 1
      IF ( LST.GT.8 ) THEN
        WRITE (6,*) 'Invalid length for station ', CHARG(I1:I2)
        GOTO 99
      ENDIF
      CSTAT  = ' '
      CSTAT  = CHARG(I1:I2)
C
C Need to find PHAS
C
      IARG   = 2
      I1     = IARGL( IARG, 1 )
      I2     = IARGL( IARG, 2 )
      LPH    = I2 - I1 + 1
      IF ( LPH.GT.8 ) THEN
        WRITE (6,*) 'Invalid length for station ', CHARG(I1:I2)
        GOTO 99
      ENDIF
      CPHAS  = ' '
      CPHAS  = CHARG(I1:I2)
C
C Need to find DLAT1 (station)
C
      IARG   = 3
      I1     = IARGL( IARG, 1 )
      I2     = IARGL( IARG, 2 )
      CMESS  = 'Error reading DLAT1'
      READ ( CHARG(I1:I2), *, ERR=99, END=99 ) DLAT1
C
C Need to find DLON1 (station)
C
      IARG   = 4
      I1     = IARGL( IARG, 1 )
      I2     = IARGL( IARG, 2 )
      CMESS  = 'Error reading DLON1'
      READ ( CHARG(I1:I2), *, ERR=99, END=99 ) DLON1
C
C Need to find DLAT2 (reference)
C
      IARG   = 5
      I1     = IARGL( IARG, 1 )
      I2     = IARGL( IARG, 2 )
      CMESS  = 'Error reading DLAT2'
      READ ( CHARG(I1:I2), *, ERR=99, END=99 ) DLAT2
C
C Need to find DLON2 (reference)
C
      IARG   = 6
      I1     = IARGL( IARG, 1 )
      I2     = IARGL( IARG, 2 )
      CMESS  = 'Error reading DLON2'
      READ ( CHARG(I1:I2), *, ERR=99, END=99 ) DLON2
C
C Need to find SX
C
      IARG   = 7
      I1     = IARGL( IARG, 1 )
      I2     = IARGL( IARG, 2 )
      CMESS  = 'Error reading SX'
      READ ( CHARG(I1:I2), *, ERR=99, END=99 ) DSX
C
C Need to find SY
C
      IARG   = 8
      I1     = IARGL( IARG, 1 )
      I2     = IARGL( IARG, 2 )
      CMESS  = 'Error reading SY'
      READ ( CHARG(I1:I2), *, ERR=99, END=99 ) DSY
C
C OK so we have successfully read in all the terms we need ...
C
      IF ( ISP.EQ.NSPMAX ) THEN
        WRITE (6,*) 'NSPSLR: ISP    = ', ISP
        WRITE (6,*) 'NSPSLR: NSPMAX = ', NSPMAX
        WRITE (6,*) 'ISP about to exceed NSPMAX '
        GOTO 99
      ENDIF
C
      ISP    = ISP + 1
      CSTARR( ISP ) = CSTAT
      LSTARR( ISP ) = LST
      CPHARR( ISP ) = CPHAS
      LPHARR( ISP ) = LPH
      DSTLAT( ISP ) = DLAT1
      DSTLON( ISP ) = DLON1
      DRFLAT( ISP ) = DLAT2
      DRFLON( ISP ) = DLON2
      DSXARR( ISP ) = DSX
      DSYARR( ISP ) = DSY
C
      IF ( ISP.EQ.NSP ) RETURN
      GOTO 30
C note that we can comment out this 40 CONTINUE as we should never
C get there - as any reaching the end of the file should exit
C with error
c40   CONTINUE
c     RETURN
C
 99   CONTINUE
      WRITE (6,*) 'NSPSLR: Error '
      WRITE (6,*) CMESS
      IERR   = 1
      RETURN
      END
C
