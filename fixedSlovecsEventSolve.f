C 
C Steven J Gibbons
C NGI
C
C fixed slowness vectors event solve
C
C Takes 3 input arguments NEV NSP chrefev
C
      PROGRAM fixedSlovecsEventSolve
      IMPLICIT NONE
C
      INTEGER     NIARGS
      INTEGER     IARGC
      INTEGER     IARG
      INTEGER     IFIXLE
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
      INTEGER        NPAIRM
      PARAMETER    ( NPAIRM = NEVMAX*(NEVMAX-1) )
      INTEGER        LDATCM
      PARAMETER    ( LDATCM = NEVMAX )
      REAL*8         DATCVM( LDATCM, LDATCM )
      INTEGER        IABSVL( NPAIRM )
      INTEGER        JABSVL( NPAIRM )
      REAL*8         DRELVX( NPAIRM )
      REAL*8         DRELVY( NPAIRM )
      REAL*8         DABSVX( NEVMAX )
      REAL*8         DABSVY( NEVMAX )
      REAL*8         DWGHTA( NPAIRM )
      REAL*8         DRELRX( NPAIRM )
      REAL*8         DRELRY( NPAIRM )
      REAL*8         DWORK2( NPAIRM )
      REAL*8         DOLDR2( NPAIRM )
C
      INTEGER        I
      INTEGER        IEV
      INTEGER        ITER
C
C CTARER is the name of the reference event
C LTARER is the length of the character string CTARER
      CHARACTER*(24) CTARER
      INTEGER        LTARER
C
      INTEGER        IERR
      INTEGER        LUIN
C
      CMESS   = ' '
      NIARGS  = IARGC()
      IF ( NIARGS.NE.3 ) THEN
        WRITE (6,*) 'Usage:  NSP     NEV     EVENTA    '
        WRITE (6,*) '        111      6      DPRK2     '
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
      LTARER = 24
      DO I = 2, 24
        IF ( CHARG(I:I).EQ.' ' .AND. LTARER.EQ.24 ) LTARER = I-1
      ENDDO
      CTARER           = '                        '
      CTARER(1:LTARER) = CHARG(1:LTARER)
C
      print *,' nsp     = ', nsp
      print *,' nev     = ', nev
      print *,' ltarer  = ', ltarer
      print *,' ctarer  = ', ctarer
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
C that CTARER and CTAREB correspond to.
C If they are not found and not distinct then we have to abort.
C
      CALL INDLEF( IERR, IFIXLE, LTARER, CTARER, NLEV, NEV, NEVMAX,
     1             INDLEV, CEVARR, LEVARR )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'INDLEF returned IERR = ', IERR
        CMESS  = 'Error from INDLEF '
        GOTO 99
      ENDIF
      print *,' IFIXLE = ', IFIXLE
C
      ITER   = 1
      CALL EABSLF( IERR, NLEV, IFIXLE, NEV, INDLEV, NSP, NPAIRM,
     1       ITER, NRELVL, IE1ARR, IE2ARR, ISPARR, DE1ARR, DE2ARR,
     2             DCCARR, DSXARR, DSYARR, LDA, ND, DAMAT, DDVEC,
     3             DWEIG, IOBS, JOBS, LWORK, LWOPT, DWORK1, DRESV,
     4             DOLDRV, DTEMP1, IABSVL, JABSVL, DRELVX, DRELVY,
     5             DABSVX, DABSVY, DWGHTA, DRELRX, DRELRY,
     6             LDATCM, DATCVM, DWORK2, DOLDR2, NUMOU, DCRES )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'EABSLF returned IERR = ', IERR
        CMESS  = 'Error from EABSLF '
        GOTO 99
      ENDIF
      DO I = 1, NRELVL
        IF ( NUMOU(I).GT.0 ) THEN
          WRITE (6,82) I, NUMOU(I), DCRES(I), 
     1                 DCRES(I)/DBLE( NUMOU(I) )
        ENDIF
      ENDDO
 82   FORMAT('Observation ',I6,' uses ',I8,' cres ',f20.4,
     1        ' avg ',f20.4)
      DO I = 1, NLEV
        IEV = INDLEV( I )
        WRITE (6,81) CEVARR( IEV )(1:LEVARR( IEV ) ),
     1               DABSVX( I ), DABSVY( I )
      ENDDO
 81   FORMAT(A,' ',f10.4,1X,f10.4 )
c     WRITE (6,81) CTAREB(1:LTAREB), CTARER(1:LTARER),
c    1             DXBMXA, DYBMYA, ND, DRESVN
c81   FORMAT(A,' minus ',A,1X,f10.4,1X,f10.4,1X,I4,1X,f20.4)
C
      CALL EXIT(0)
 99   CONTINUE
      WRITE (6,'(A)') CMESS
      CALL EXIT(1)
      END
C
