C DTLNRD Differential Time Lines Read
C Reads from file unit LUIN lines of the following format:
C
C CEV1
C CEV2
C CHTIM1
C CHTIM2
C CSTAT
C CPHASE
C CCV
C 
C There are at most NRELVM of these lines and we return
C NRELVL - the number of lines actually read in.
C We assume that we continue reading until the end of the file
C of the end of standard input.
C (i.e. that there is nothing left to read once all our lines are
C read in.)
C
C We need to read NEV, NSP (we don't really need the absolute
C limits NEVMAX, NSPMAX of these arrays as they are all linear)
C Neither do we need coordinates or slownesses etc.
C We only need the names of the events and the station/phase pairs
C and their lengths.
C
C i.e. we need the following arrays:
C
C            CHARACTER*(24) CEVARR( NEVmax )
C            INTEGER        LEVARR( NEVmax )
C            CHARACTER*(8)  CSTARR( NSPmax )
C            INTEGER        LSTARR( NSPmax )
C            CHARACTER*(8)  CPHARR( NSPmax )
C            INTEGER        LPHARR( NSPmax )
C
C For each line read we need to check that both events are
C among those read and that the station and phase combination
C is present. If not we return with an error.
C
C We need to fill the arrays
C
C            INTEGER        IE1ARR( NRELVM ) - index of event1
C            INTEGER        IE2ARR( NRELVM ) - index of event1
C            REAL*8         DE1ARR( NRELVM ) - epoch time for ev1
C            REAL*8         DE2ARR( NRELVM ) - epoch time for ev2
C            INTEGER        ISPARR( NRELVM ) - index of station/phase pair
C            REAL*8         DCCARR( NRELVM ) - quality measure (e.g. CC)
C
      SUBROUTINE DTLNRD( IERR, LUIN, NEV, NSP, NRELVL, NRELVM,
     1                   CEVARR, LEVARR, CSTARR, LSTARR, CPHARR, LPHARR,
     2                   IE1ARR, IE2ARR, DE1ARR, DE2ARR, ISPARR, DCCARR)
      IMPLICIT NONE
C
      INTEGER        IERR
      INTEGER        LUIN
      INTEGER        NEV
      INTEGER        NSP
      INTEGER        NRELVL
      INTEGER        NRELVM
C
      CHARACTER*(24) CEVARR( NEV )
      INTEGER        LEVARR( NEV )
      CHARACTER*(8)  CSTARR( NSP )
      INTEGER        LSTARR( NSP )
      CHARACTER*(8)  CPHARR( NSP )
      INTEGER        LPHARR( NSP )
C
      INTEGER        IE1ARR( NRELVM )
      INTEGER        IE2ARR( NRELVM )
      REAL*8         DE1ARR( NRELVM )
      REAL*8         DE2ARR( NRELVM )
      INTEGER        ISPARR( NRELVM )
      REAL*8         DCCARR( NRELVM )
C
C     Variables for the routine CSARGM
C
      INTEGER     IARG
      INTEGER     I0
      INTEGER     I1
      INTEGER     I2
      INTEGER     NARGM
      PARAMETER ( NARGM = 7 )
      INTEGER     NARGS, CSLEN, IARGL( NARGM, 2 )
C
C Variables for SAC routines
C
      INTEGER     ISACTM( 6 )
      INTEGER     IMON
      INTEGER     IDOM
      INTEGER     IJUL
      INTEGER     IYYYY
      REAL*8      DSECS
C
      INTEGER     I
      INTEGER     ISP
      INTEGER     IEV1
      INTEGER     IEV2
      REAL*8      DTIME1
      REAL*8      DTIME2
      REAL*8      DCC
C
      INTEGER       ILEN
      INTEGER       ILENE
      INTEGER       ILENS
      INTEGER       ILENP
      CHARACTER*(8) CSTAT
      CHARACTER*(8) CPHAS
C
      CHARACTER*(80)  CMESS
      CHARACTER*(200) CHARG
C
      IERR   = 0
      CMESS  = ' '
      IF ( LUIN.LT.7 .AND. LUIN.NE.5 ) THEN
        WRITE (6,*) 'DTLNRD: LUIN = ', LUIN
        IERR    = 1
        RETURN
      ENDIF
C
      IF ( NEV.LT.2 .OR. NSP.LT.3 ) THEN
        WRITE (6,*) 'DTLNRD: NEV    = ', NEV   
        WRITE (6,*) 'DTLNRD: NSP    = ', NSP   
        IERR    = 1
        RETURN
      ENDIF
C
      IF ( NRELVM.LT.5 ) THEN
        WRITE (6,*) 'DTLNRD: NRELVM = ', NRELVM
        IERR    = 1
        RETURN
      ENDIF
C
      NRELVL = 0
 30   CONTINUE
      CHARG  = ' '
      CMESS  = 'Error reading in differential time line'
      READ ( 5, '(A)', ERR=99, END=40 ) CHARG
      IF ( CHARG(1:1).EQ.'#' ) GOTO 30
      IF ( CHARG(1:1).EQ.'*' ) GOTO 30
      CSLEN  = 200
      NARGS  = 7
      CALL CSARGM( CHARG, NARGS, NARGM, CSLEN, IARGL, IERR )
      IF ( IERR.NE.0 ) THEN
        CMESS  = 'Error from CSARGM '
        WRITE (6,*) 'CSARGM returned IERR = ', IERR
        GOTO 99
      ENDIF
C
C Assume first that we do not have IEV1, IEV2, ISP
C
      IEV1   = 0
      IEV2   = 0
      ISP    = 0
C
C Find IEV1
C
      IARG   = 1
      I1     = IARGL( IARG, 1 )
      I2     = IARGL( IARG, 2 )
      ILENE  = I2 - I1 + 1
      DO I = 1, NEV
        IF ( ILENE.EQ.LEVARR( I ) ) THEN
          IF ( CEVARR(I)(1:ILENE).EQ.CHARG(I1:I2) ) IEV1 = I
        ENDIF
      ENDDO
      IF ( IEV1.EQ.0 ) THEN
        CMESS  = 'Event not found. '
        WRITE (6,*) 'Event specified = ', CHARG(I1:I2)
        GOTO 99
      ENDIF
C
C Find IEV2
C
      IARG   = 2
      I1     = IARGL( IARG, 1 )
      I2     = IARGL( IARG, 2 )
      ILENE  = I2 - I1 + 1
      DO I = 1, NEV
        IF ( ILENE.EQ.LEVARR( I ) ) THEN
          IF ( CEVARR(I)(1:ILENE).EQ.CHARG(I1:I2) ) IEV2 = I
        ENDIF
      ENDDO
      IF ( IEV2.EQ.0 ) THEN
        CMESS  = 'Event not found. '
        WRITE (6,*) 'Event specified = ', CHARG(I1:I2)
        GOTO 99
      ENDIF
C
C Ignore this line is IEV1.eq.IEV2 (this is an autocorrelation
C and typically used for quality control but not useful to us)
C
      IF ( IEV1.EQ.IEV2 ) GOTO 30
C
C Find ISP
C
      IARG   = 5
      I1     = IARGL( IARG, 1 )
      I2     = IARGL( IARG, 2 )
      ILENS  = I2 - I1 + 1
      IF ( ILENS.GT.8 ) THEN
        CMESS = 'Invalid length of station name'
        GOTO 99
      ENDIF
      CSTAT          = ' '
      CSTAT(1:ILENS) = CHARG(I1:I2)
C
      IARG   = 6
      I1     = IARGL( IARG, 1 )
      I2     = IARGL( IARG, 2 )
      ILENP  = I2 - I1 + 1
      IF ( ILENP.GT.8 ) THEN
        CMESS = 'Invalid length of phase name'
        GOTO 99
      ENDIF
      CPHAS          = ' '
      CPHAS(1:ILENP) = CHARG(I1:I2)
C
      DO I = 1, NSP
        IF ( ILENS.EQ.LSTARR( I ) .AND. ILENP.EQ.LPHARR( I ) ) THEN
          IF ( CSTARR(I)(1:ILENS).EQ.CSTAT(1:ILENS)  .AND.
     1         CPHARR(I)(1:ILENP).EQ.CPHAS(1:ILENP)  )  ISP = I
        ENDIF
      ENDDO
      IF ( ISP.EQ.0 ) THEN
        CMESS  = 'Station/phase combination not found. '
        WRITE (6,*) 'Station specified = ', CSTAT(1:ILENS)
        WRITE (6,*) 'Phase specified   = ', CPHAS(1:ILENP)
        GOTO 99
      ENDIF
C
C OK. So we have the events and station/phase combination.
C Now we just need the times and the quality value.
C
C     .
C     . Now need to try to read the epoch time for event 1
C     .
      IARG   = 3
      I1     = IARGL( IARG, 1 )
      I2     = IARGL( IARG, 2 )
      ILEN   = I2 - I1 + 1
      IF ( ILEN.LT.23 ) THEN
        WRITE (6,*) 'Invalid string for time ', CHARG(I1:I2)
        GOTO 99
      ENDIF
      I0     = I1 - 1
      READ ( CHARG(I0+ 1:I0+ 4), '(I4)', END=99, ERR=99 ) ISACTM( 1 )
      READ ( CHARG(I0+ 6:I0+ 7), '(I2)', END=99, ERR=99 ) IMON
      READ ( CHARG(I0+ 9:I0+10), '(I2)', END=99, ERR=99 ) IDOM
      IYYYY  = ISACTM( 1 )
      CALL MD2DOY( IYYYY, IMON, IDOM, IJUL, IERR )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'Error from MD2DOY: ', IYYYY, IMON, IDOM, IJUL
        GOTO 99
      ENDIF
      ISACTM( 2 ) = IJUL
      READ ( CHARG(I0+12:I0+13), '(I2)', END=99, ERR=99 ) ISACTM( 3 )
      READ ( CHARG(I0+15:I0+16), '(I2)', END=99, ERR=99 ) ISACTM( 4 )
      READ ( CHARG(I0+18:I0+24), *,      END=99, ERR=99 ) DSECS
      ISACTM( 5 ) = INT( DSECS )
      ISACTM( 6 ) = INT( (DSECS - DBLE( INT( DSECS ) ))*1000.0d0 )
      CALL SACH2E( ISACTM, DTIME1 )
C
C     .
C     . Now need to try to read the epoch time for event 2
C     .
      IARG   = 4
      I1     = IARGL( IARG, 1 )
      I2     = IARGL( IARG, 2 )
      ILEN   = I2 - I1 + 1
      IF ( ILEN.LT.23 ) THEN
        WRITE (6,*) 'Invalid string for time ', CHARG(I1:I2)
        GOTO 99
      ENDIF
      I0     = I1 - 1
      READ ( CHARG(I0+ 1:I0+ 4), '(I4)', END=99, ERR=99 ) ISACTM( 1 )
      READ ( CHARG(I0+ 6:I0+ 7), '(I2)', END=99, ERR=99 ) IMON
      READ ( CHARG(I0+ 9:I0+10), '(I2)', END=99, ERR=99 ) IDOM
      IYYYY  = ISACTM( 1 )
      CALL MD2DOY( IYYYY, IMON, IDOM, IJUL, IERR )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'Error from MD2DOY: ', IYYYY, IMON, IDOM, IJUL
        GOTO 99
      ENDIF
      ISACTM( 2 ) = IJUL
      READ ( CHARG(I0+12:I0+13), '(I2)', END=99, ERR=99 ) ISACTM( 3 )
      READ ( CHARG(I0+15:I0+16), '(I2)', END=99, ERR=99 ) ISACTM( 4 )
      READ ( CHARG(I0+18:I0+24), *,      END=99, ERR=99 ) DSECS
      ISACTM( 5 ) = INT( DSECS )
      ISACTM( 6 ) = INT( (DSECS - DBLE( INT( DSECS ) ))*1000.0d0 )
      CALL SACH2E( ISACTM, DTIME2 )
C
C Finally, read the weight in the 7th column
C
      IARG   = 7
      I1     = IARGL( IARG, 1 )
      I2     = IARGL( IARG, 2 )
      READ ( CHARG(I1:I2), *, ERR=99, END=99 ) DCC
C
C Now we have all of the values we need!
C So we can increment NRELVL
C
      IF ( NRELVL.EQ.NRELVM ) THEN
        CMESS  = 'NRELVL about to be exceed ... '
        GOTO 99
      ENDIF
      NRELVL = NRELVL + 1
C
      IE1ARR( NRELVL ) = IEV1
      IE2ARR( NRELVL ) = IEV2
      DE1ARR( NRELVL ) = DTIME1
      DE2ARR( NRELVL ) = DTIME2
      ISPARR( NRELVL ) = ISP
      DCCARR( NRELVL ) = DCC
C
      GOTO 30
 40   CONTINUE
      RETURN
C
 99   CONTINUE
      WRITE (6,*) 'DTLNRD: Error '
      WRITE (6,*) CMESS
      IERR   = 1
      RETURN
      END
C
