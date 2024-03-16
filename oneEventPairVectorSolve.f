C 
C Steven J Gibbons
C NGI
C
C One Event Pair Vector Solve
C
C Takes 4 input arguments NEV NSP cheva chevb
C
      PROGRAM oneEventPairVectorSolve
      IMPLICIT NONE
C
      INTEGER     NIARGS
      INTEGER     IARGC
      INTEGER     IARG
C
      INTEGER     NRELVL
      INTEGER     NRELVM
      PARAMETER ( NRELVM = 50000 )
      INTEGER     NSP
      INTEGER     NSPMAX
      PARAMETER ( NSPMAX = 120 )
      INTEGER     NEV
      INTEGER     NEVMAX
      PARAMETER ( NEVMAX =  60 )
C
      CHARACTER*(200) CHARG
      CHARACTER*(80)  CMESS
C
      CHARACTER*(8) CSTARR( NSPMAX )
      INTEGER       LSTARR( NSPMAX )
      CHARACTER*(8) CPHARR( NSPMAX )
      INTEGER       LPHARR( NSPMAX )
      REAL*8        DSXARR( NSPMAX )
      REAL*8        DSYARR( NSPMAX )
      REAL*8        DSTLAT( NSPMAX )
      REAL*8        DSTLON( NSPMAX )
      REAL*8        DRFLAT( NSPMAX )
      REAL*8        DRFLON( NSPMAX )
C
      CHARACTER*(24) CEVARR( NEVMAX )
      INTEGER        LEVARR( NEVMAX )
      REAL*8         DRXARR( NEVMAX )
      REAL*8         DRYARR( NEVMAX )
C
      INTEGER        IE1ARR( NRELVM )
      INTEGER        IE2ARR( NRELVM )
      REAL*8         DE1ARR( NRELVM )
      REAL*8         DE2ARR( NRELVM )
      INTEGER        ISPARR( NRELVM )
      REAL*8         DCCARR( NRELVM )
C
      INTEGER        NUMEVO( NEVMAX )
      INTEGER        NUMSPO( NSPMAX )
      INTEGER        NLEV
      INTEGER        NLSP
      INTEGER        INDLEV( NEVMAX )
      INTEGER        INDLSP( NSPMAX )
C
C LDA is the maximum number of observation pairs
C
      INTEGER        LDA
      PARAMETER    ( LDA  = 20000 )
      INTEGER        ND
C
      INTEGER        LWORK
      PARAMETER    ( LWORK  = 2*NRELVM )
      INTEGER        LWOPT
      INTEGER        NM
      PARAMETER    ( NM = 3 )
      REAL*8         DWORK1( LWORK )
      REAL*8         DAMAT( LDA, NM )
      REAL*8         DDVEC( LDA )
      REAL*8         DWEIG( LDA )
      REAL*8         DRESV( LDA )
      REAL*8         DOLDRV( LDA )
      REAL*8         DTEMP1( LDA )
      INTEGER        IOBS( LDA )
      INTEGER        JOBS( LDA )
C
      INTEGER        I
      INTEGER        ILEVA
      INTEGER        ILEVB
      LOGICAL        OSOLVE
      REAL*8         DXBMXA
      REAL*8         DYBMYA
      REAL*8         DRESVN
C
C CTAREA is the name of event A
C LTAREA is the length of the character string CTAREA
      CHARACTER*(24) CTAREA
      INTEGER        LTAREA
C CTAREB is the name of event B
C LTAREB is the length of the character string CTAREB
      CHARACTER*(24) CTAREB
      INTEGER        LTAREB
C
      INTEGER        IERR
      INTEGER        LUIN
C
      CMESS   = ' '
      NIARGS  = IARGC()
      IF ( NIARGS.NE.4 ) THEN
        WRITE (6,*) 'Usage:  NSP     NEV     EVENTA    EVENTB     '
        WRITE (6,*) '        111      6      DPRK3      DPRK4      '
        CALL EXIT(1)
      ENDIF
C
      CHARG  = ' '
      IARG   = 1
      CALL GETARG( IARG, CHARG )
      CMESS  = 'Error reading integer NSP'
      READ ( CHARG, *, ERR=99, END=99 ) NSP
      IF ( NSP.LT.1 .OR. NSP.GT.NSPMAX ) THEN
        WRITE (6,*) 'NSP    = ', NSP
        WRITE (6,*) 'NSPMAX = ', NSPMAX
        CMESS  = 'NSPMAX exceeded'
        GOTO 99
      ENDIF
C
      CHARG  = ' '
      IARG   = 2
      CALL GETARG( IARG, CHARG )
      CMESS  = 'Error reading integer NEV'
      READ ( CHARG, *, ERR=99, END=99 ) NEV
      IF ( NEV.LT.1 .OR. NEV.GT.NEVMAX ) THEN
        WRITE (6,*) 'NEV    = ', NEV
        WRITE (6,*) 'NEVMAX = ', NEVMAX
        CMESS  = 'NEVMAX exceeded'
        GOTO 99
      ENDIF
C
      CHARG  = ' '
      IARG   = 3
      CALL GETARG( IARG, CHARG )
      LTAREA = 24
      DO I = 2, 24
        IF ( CHARG(I:I).EQ.' ' .AND. LTAREA.EQ.24 ) LTAREA = I-1
      ENDDO
      CTAREA           = '                        '
      CTAREA(1:LTAREA) = CHARG(1:LTAREA)
C
      CHARG  = ' '
      IARG   = 4
      CALL GETARG( IARG, CHARG )
      LTAREB = 24
      DO I = 2, 24
        IF ( CHARG(I:I).EQ.' ' .AND. LTAREB.EQ.24 ) LTAREB = I-1
      ENDDO
      CTAREB           = '                        '
      CTAREB(1:LTAREB) = CHARG(1:LTAREB)
C
      print *,' nsp     = ', nsp
      print *,' nev     = ', nev
      print *,' ltarea  = ', ltarea
      print *,' ctarea  = ', ctarea
      print *,' ltareb  = ', ltareb
      print *,' ctareb  = ', ctareb
C
C Now read those lines of the input file for the NSP slowness vectors
C
      LUIN   = 5
      CALL NSPSLR( IERR, LUIN, NSPMAX, NSP,
     1             CSTARR, LSTARR, CPHARR, LPHARR,
     2             DSTLAT, DSTLON, DRFLAT, DRFLON,
     3             DSXARR, DSYARR )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'NSPSLR returned IERR = ', IERR
        CMESS  = 'Error from NSPSLR '
        GOTO 99
      ENDIF
      print *,' NSP slowness vectors read.'
C
C Now read those lines of the input file for the NEV events
C
      CALL NEVELR( IERR, LUIN, NEVMAX, NEV,
     1             CEVARR, LEVARR, DRXARR, DRYARR )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'NEVELR returned IERR = ', IERR
        CMESS  = 'Error from NEVELR '
        GOTO 99
      ENDIF
      print *,' NEV events read.'
C
C Now read the remaining lines of the input file for the NRELVL
C observations.
C
      CALL DTLNRD( IERR, LUIN, NEV, NSP, NRELVL, NRELVM,
     1             CEVARR, LEVARR, CSTARR, LSTARR, CPHARR, LPHARR,
     2             IE1ARR, IE2ARR, DE1ARR, DE2ARR, ISPARR, DCCARR)
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'DTLNRD returned IERR = ', IERR
        CMESS  = 'Error from DTLNRD '
        GOTO 99
      ENDIF
      print *,' NRELVL observations read. NRELVL = ', NRELVL
C
C Now we have read in all of our observations, we need to
C calculate which of our events and slowness vectors are "live"
C i.e. we may have read in some of the NEV and NSP that do not
C have (sufficient) observations.
C So we calculate NLEV and NLSP - the numbers of those that do.
C
      CALL NLSPEF( IERR, NEV, NSP, NRELVL,
     1             IE1ARR, IE2ARR, ISPARR, NUMEVO, NUMSPO,
     2             NLEV, NLSP, INDLEV, INDLSP )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'NLSPEF returned IERR = ', IERR
        CMESS  = 'Error from NLSPEF '
        GOTO 99
      ENDIF
      print *,' NLEV   = ', NLEV
      print *,' NLSP   = ', NLSP
C
C Now we have to calculate the numbers of the "live" events
C that CTAREA and CTAREB correspond to.
C If they are not found and not distinct then we have to abort.
C
      CALL INDLEF( IERR, ILEVA, LTAREA, CTAREA, NLEV, NEV, NEVMAX,
     1             INDLEV, CEVARR, LEVARR )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'INDLEF returned IERR = ', IERR
        CMESS  = 'Error from INDLEF '
        GOTO 99
      ENDIF
      print *,' ILEVA  = ', ILEVA
C
      CALL INDLEF( IERR, ILEVB, LTAREB, CTAREB, NLEV, NEV, NEVMAX,
     1             INDLEV, CEVARR, LEVARR )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'INDLEF returned IERR = ', IERR
        CMESS  = 'Error from INDLEF '
        GOTO 99
      ENDIF
      print *,' ILEVB  = ', ILEVB
C
C Now solve for the vector distance difference between 
C event A and event B (i.e. evB - evA)
C
      CALL OSEVPS( IERR, NLEV, ILEVA, ILEVB, NEV, INDLEV, NSP,
     1             NRELVL, IE1ARR, IE2ARR, ISPARR,
     2             DE1ARR, DE2ARR, DCCARR, DSXARR, DSYARR,
     3             LDA, ND, DAMAT, DDVEC, DWEIG, IOBS, JOBS,
     4             LWORK, LWOPT, DWORK1, DRESV, DOLDRV, DTEMP1,
     5             DXBMXA, DYBMYA, OSOLVE, DRESVN )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'OSEVPS returned IERR = ', IERR
        CMESS  = 'Error from OSEVPS '
        GOTO 99
      ENDIF
      IF ( .NOT. OSOLVE ) THEN
        WRITE (6,*) 'OSEVPS returned OSOLVE = ', OSOLVE
        CMESS  = 'Failed to solve for DXBMXA, DYBMYA '
        GOTO 99
      ENDIF
      WRITE (6,81) CTAREB(1:LTAREB), CTAREA(1:LTAREA),
     1             DXBMXA, DYBMYA, ND, DRESVN
 81   FORMAT(A,' minus ',A,1X,f10.4,1X,f10.4,1X,I4,1X,f20.4)
C
      CALL EXIT(0)
 99   CONTINUE
      WRITE (6,'(A)') CMESS
      CALL EXIT(1)
      END
C
