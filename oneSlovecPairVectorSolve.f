C 
C Steven J Gibbons
C NGI
C
C One Slovec Pair Vector Solve
C
C Takes 6 input arguments NEV NSP cstat1 cphas1 cstat2 cphas2
C
      PROGRAM oneSlovecPairVectorSolve
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
      INTEGER        NUMOU( NRELVM )
      REAL*8         DCRES( NRELVM )
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
      INTEGER        ITER
      INTEGER        ILSPI
      INTEGER        ILSPJ
      LOGICAL        OSOLVE
      REAL*8         DXJMXI
      REAL*8         DYJMYI
      REAL*8         DRESVN
C
C CTARS1 is the name of station one
C LTARS1 is the length of the character string CTARS1
      CHARACTER*(8) CTARS1
      INTEGER       LTARS1
C CTARP1 is the name of phase one
C LTARP1 is the length of the character string CTARP1
      CHARACTER*(8) CTARP1
      INTEGER       LTARP1
C
C CTARS2 is the name of station two
C LTARS2 is the length of the character string CTARS2
      CHARACTER*(8) CTARS2
      INTEGER       LTARS2
C CTARP2 is the name of phase two
C LTARP2 is the length of the character string CTARP2
      CHARACTER*(8) CTARP2
      INTEGER       LTARP2
C
      INTEGER        IERR
      INTEGER        LUIN
C
      CMESS   = ' '
      NIARGS  = IARGC()
      IF ( NIARGS.NE.6 ) THEN
        WRITE (6,*) 'Usage: NSP  NEV  CSTAT1 CPHAS1 CSTAT2 CPHAS2 '
        WRITE (6,*) '       111  6    KEV     P1    SGF     S1'
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
      LTARS1 = 8
      DO I = 2, 8
        IF ( CHARG(I:I).EQ.' ' .AND. LTARS1.EQ.8 ) LTARS1 = I-1
      ENDDO
      CTARS1           = '        '
      CTARS1(1:LTARS1) = CHARG(1:LTARS1)
C
      CHARG  = ' '
      IARG   = 4
      CALL GETARG( IARG, CHARG )
      LTARP1 = 8
      DO I = 2, 8
        IF ( CHARG(I:I).EQ.' ' .AND. LTARP1.EQ.8 ) LTARP1 = I-1
      ENDDO
      CTARP1           = '        '
      CTARP1(1:LTARP1) = CHARG(1:LTARP1)
C
      CHARG  = ' '
      IARG   = 5
      CALL GETARG( IARG, CHARG )
      LTARS2 = 8
      DO I = 2, 8
        IF ( CHARG(I:I).EQ.' ' .AND. LTARS2.EQ.8 ) LTARS2 = I-1
      ENDDO
      CTARS2           = '        '
      CTARS2(1:LTARS2) = CHARG(1:LTARS2)
C
      CHARG  = ' '
      IARG   = 6
      CALL GETARG( IARG, CHARG )
      LTARP2 = 8
      DO I = 2, 8
        IF ( CHARG(I:I).EQ.' ' .AND. LTARP2.EQ.8 ) LTARP2 = I-1
      ENDDO
      CTARP2           = '        '
      CTARP2(1:LTARP2) = CHARG(1:LTARP2)
C
      print *,' nsp     = ', nsp
      print *,' nev     = ', nev
      print *,' ltars1  = ', ltars1
      print *,' ctars1  = ', ctars1
      print *,' ltarp1  = ', ltarp1
      print *,' ctarp1  = ', ctarp1
      print *,' ltars2  = ', ltars2
      print *,' ctars2  = ', ctars2
      print *,' ltarp2  = ', ltarp2
      print *,' ctarp2  = ', ctarp2
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
C Initialize the arrays NUMOU and DCRES
C
      DO I = 1, NRELVL
        NUMOU( I ) = 0
        DCRES( I ) = 0.0d0
      ENDDO
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
      CALL INDLSF( IERR, ILSPI, LTARS1, CTARS1, LTARP1, CTARP1,
     1             NLSP, NSP, NSPMAX,
     1             INDLSP, CSTARR, LSTARR, CPHARR, LPHARR )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'INDLSP returned IERR = ', IERR
        CMESS  = 'Error from INDLSF '
        GOTO 99
      ENDIF
      print *,' ILSPI  = ', ILSPI
C
      CALL INDLSF( IERR, ILSPJ, LTARS2, CTARS2, LTARP2, CTARP2,
     1             NLSP, NSP, NSPMAX,
     1             INDLSP, CSTARR, LSTARR, CPHARR, LPHARR )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'INDLSP returned IERR = ', IERR
        CMESS  = 'Error from INDLSF '
        GOTO 99
      ENDIF
      print *,' ILSPJ  = ', ILSPJ
C
C Now solve for the vector distance difference between 
C statphase 1 and statphase 2 (i.e. sp2 - sp1)
C
      CALL OSSPPS( IERR, NLSP, ILSPI, ILSPJ, NSP, INDLSP, NEV,
     1       ITER, NRELVL, IE1ARR, IE2ARR, ISPARR,
     2             DE1ARR, DE2ARR, DCCARR, DRXARR, DRYARR,
     3             LDA, ND, DAMAT, DDVEC, DWEIG, IOBS, JOBS,
     4             LWORK, LWOPT, DWORK1, DRESV, DOLDRV, DTEMP1,
     5             DXJMXI, DYJMYI, OSOLVE, DRESVN, NUMOU, DCRES )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'OSSPPS returned IERR = ', IERR
        CMESS  = 'Error from OSSPPS '
        GOTO 99
      ENDIF
      IF ( .NOT. OSOLVE ) THEN
        WRITE (6,*) 'OSSPPS returned OSOLVE = ', OSOLVE
        CMESS  = 'Failed to solve for DXJMXI, DYJMYI '
        GOTO 99
      ENDIF
      WRITE (6,81) CTARS2(1:LTARS2), CTARP2(1:LTARP2),
     1             CTARS1(1:LTARS1), CTARP1(1:LTARP1),
     1             DXJMXI, DYJMYI, ND, DRESVN
 81   FORMAT(A,1X,A,' minus ',A,1X,A,1X,
     1             f10.4,1X,f10.4,1X,I4,1X,f20.4)
C
      CALL EXIT(0)
 99   CONTINUE
      WRITE (6,'(A)') CMESS
      CALL EXIT(1)
      END
C
