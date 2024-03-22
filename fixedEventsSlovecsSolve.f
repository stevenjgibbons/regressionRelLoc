C 
C Steven J Gibbons
C NGI
C
C fixed events slowness vectors solve
C
C Takes 2 input arguments NEV NSP 
C
      PROGRAM fixedEventsSlovecsSolve
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
      INTEGER        NPAIRM
      PARAMETER    ( NPAIRM = NSPMAX*(NSPMAX-1) )
      INTEGER        LDATCM
      PARAMETER    ( LDATCM = NSPMAX )
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
      REAL*8         DCVSVX( NEVMAX )
      REAL*8         DCVSVY( NEVMAX )
C
      INTEGER        I
      INTEGER        ISP
      INTEGER        ITER
C
      INTEGER        IERR
      INTEGER        LUIN
C
      REAL*8         DCOVEL
      REAL*8         DREQMX
      REAL*8         DREQMY
      REAL*8         DSCALE
C
      CMESS   = ' '
      NIARGS  = IARGC()
      IF ( NIARGS.NE.2 ) THEN
        WRITE (6,*) 'Usage:  NSP     NEV     '
        WRITE (6,*) '        111      6      '
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
      print *,' nsp     = ', nsp
      print *,' nev     = ', nev
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
C Now we need to calculate the current means of the
C "live" slowness vectors
C
      DREQMX = 0.0d0
      DREQMY = 0.0d0
      DSCALE = 1.0d0/DBLE( NLSP )
      DO I = 1, NLSP
        ISP    = INDLSP( I )
        DREQMX = DREQMX + DSCALE*DSXARR( ISP )
        DREQMY = DREQMY + DSCALE*DSYARR( ISP )
      ENDDO
C
      ITER   = 1
      CALL ABSSVF( IERR, NLSP, NSP, INDLSP, NEV, NPAIRM,
     1       ITER, NRELVL, IE1ARR, IE2ARR, ISPARR, DE1ARR, DE2ARR,
     2             DCCARR, DRXARR, DRYARR, LDA, ND, DAMAT, DDVEC,
     3             DWEIG, IOBS, JOBS, LWORK, LWOPT, DWORK1, DRESV,
     4             DOLDRV, DTEMP1, IABSVL, JABSVL, DRELVX, DRELVY,
     5             DABSVX, DABSVY, DWGHTA, DRELRX, DRELRY,
     6             LDATCM, DATCVM, DWORK2, DOLDR2, NUMOU, DCRES,
     7             DREQMX, DREQMY, DCVSVX, DCVSVY )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'ABSSVF returned IERR = ', IERR
        CMESS  = 'Error from ABSSVF '
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
c    1             CSTARR, LSTARR, CPHARR, LPHARR,
c    2             DSTLAT, DSTLON, DRFLAT, DRFLON,

      DO I = 1, NLSP
        ISP = INDLSP( I )
        DCOVEL = DSQRT( DCVSVX( I )*DCVSVX( I ) +
     1                  DCVSVY( I )*DCVSVY( I )  )
        WRITE (6,81) CSTARR( ISP )(1:LSTARR( ISP ) ),
     1               CPHARR( ISP )(1:LPHARR( ISP ) ),
     2               DSTLAT( ISP ), DSTLON( ISP ),
     3               DRFLAT( ISP ), DRFLON( ISP ),
     4               DABSVX( I ), DABSVY( I ),
     5               DCOVEL
      ENDDO
 81   FORMAT(A8,1X,A8,1X,f10.5,1X,f11.5,1X,f10.5,1X,f11.5,1X,
     1               f13.8,1X,f13.8,1X,f13.8 )
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
