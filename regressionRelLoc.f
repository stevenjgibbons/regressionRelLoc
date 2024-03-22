C 
C Steven J Gibbons
C NGI
C
C regression Relative Location
C
C Takes 5 input arguments NEV NSP chrefev dtol mxiter
C
      PROGRAM regressionRelLoc
      IMPLICIT NONE
C
      INTEGER     NIARGS
      INTEGER     IARGC
      INTEGER     IARG
      INTEGER     IFIXLE
      INTEGER     MXITER
      REAL*8      DTOL
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
      REAL*8        DSXAPR( NSPMAX )
      REAL*8        DSYAPR( NSPMAX )
      REAL*8        DSXADF( NSPMAX )
      REAL*8        DSYADF( NSPMAX )
C
      CHARACTER*(24) CEVARR( NEVMAX )
      INTEGER        LEVARR( NEVMAX )
      REAL*8         DRXARR( NEVMAX )
      REAL*8         DRYARR( NEVMAX )
      REAL*8         DRXAPR( NEVMAX )
      REAL*8         DRYAPR( NEVMAX )
      REAL*8         DRXADF( NEVMAX )
      REAL*8         DRYADF( NEVMAX )
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
c     PARAMETER    ( NPAIRM = NEVMAX*(NEVMAX-1) )
      PARAMETER    ( NPAIRM = NSPMAX*(NSPMAX-1) )
      INTEGER        LDATCM
c     PARAMETER    ( LDATCM = NEVMAX )
      PARAMETER    ( LDATCM = NSPMAX )
      REAL*8         DATCVM( LDATCM, LDATCM )
      INTEGER        IABSVL( NPAIRM )
      INTEGER        JABSVL( NPAIRM )
      REAL*8         DRELVX( NPAIRM )
      REAL*8         DRELVY( NPAIRM )
      REAL*8         DABSEX( NEVMAX )
      REAL*8         DABSEY( NEVMAX )
      REAL*8         DABSSX( NSPMAX )
      REAL*8         DABSSY( NSPMAX )
      REAL*8         DWGHTA( NPAIRM )
      REAL*8         DRELRX( NPAIRM )
      REAL*8         DRELRY( NPAIRM )
      REAL*8         DWORK2( NPAIRM )
      REAL*8         DOLDR2( NPAIRM )
      REAL*8         DCVMEX( NEVMAX )
      REAL*8         DCVMEY( NEVMAX )
      REAL*8         DCVMSX( NSPMAX )
      REAL*8         DCVMSY( NSPMAX )
C
      INTEGER        I
      INTEGER        IEV
      INTEGER        ISP
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
      REAL*8         DREQMX
      REAL*8         DREQMY
      REAL*8         DSCALE
C
      REAL*8         DCOVEL
      REAL*8         DTOTNM
      REAL*8         DNRM2
      INTEGER        INCX
      PARAMETER    ( INCX = 1 )
C
      CMESS   = ' '
      NIARGS  = IARGC()
      IF ( NIARGS.NE.5 ) THEN
        WRITE (6,*) 'Usage:  NSP  NEV  EVENTA  DTOL  MXITER  '
        WRITE (6,*) '        111   6   DPRK2   0.001  100    '
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
      CHARG  = ' '
      IARG   = 4
      CMESS  = 'Error reading real value DTOL'
      CALL GETARG( IARG, CHARG )
      READ ( CHARG, *, ERR=99, END=99 ) DTOL
C
      CHARG  = ' '
      IARG   = 5
      CMESS  = 'Error reading integer MXITER'
      CALL GETARG( IARG, CHARG )
      READ ( CHARG, *, ERR=99, END=99 ) MXITER
C
      print *,' nsp     = ', nsp
      print *,' nev     = ', nev
      print *,' ltarer  = ', ltarer
      print *,' ctarer  = ', ctarer
      print *,' dtol    = ', dtol
      print *,' mxiter  = ', mxiter
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
C Start iteration
C
      ITER   = 0
 50   CONTINUE
      ITER   = ITER + 1
C     .
C     . First we need to save the current iterations of
C     . DSXARR and DSYARR in DSXAPR and DSYAPR
C     .
      DO ISP = 1, NSP
        DSXAPR( ISP ) = DSXARR( ISP )
        DSYAPR( ISP ) = DSYARR( ISP )
      ENDDO
C     .
C     . Now save the current iterations of DRXARR and DRYARR in
C     . DRXAPR and DRYAPR
C     .
      DO IEV = 1, NEV
        DRXAPR( IEV ) = DRXARR( IEV )
        DRYAPR( IEV ) = DRYARR( IEV )
      ENDDO
C     .
      CALL OIESVF( IERR, NLEV, NLSP, IFIXLE, NEV, INDLEV, NSP,
     1             INDLSP, NPAIRM, ITER, NRELVL, IE1ARR, IE2ARR, ISPARR,
     2             DE1ARR, DE2ARR, DCCARR, DSXARR, DSYARR, DRXARR,
     3             DRYARR, LDA, ND, DAMAT, DDVEC, DWEIG, IOBS, JOBS,
     4             LWORK, LWOPT, DWORK1, DRESV, DOLDRV, DTEMP1, IABSVL,
     5             JABSVL, DRELVX, DRELVY, DABSEX, DABSEY, DABSSX,
     6             DABSSY, DWGHTA, DRELRX, DRELRY, LDATCM, DATCVM,
     7             DWORK2, DOLDR2, NUMOU, DCRES, DREQMX, DREQMY,
     8             DCVMEX, DCVMEY, DCVMSX, DCVMSY )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'OIESVF returned IERR = ', IERR
        CMESS  = 'Error from OIESVF '
        GOTO 99
      ENDIF
C
C Now we need to calculate the differences between the
C previous and the current iterations
C
      DO ISP = 1, NSP
        DSXADF( ISP ) = DSXARR( ISP ) - DSXAPR( ISP )
        DSYADF( ISP ) = DSYARR( ISP ) - DSYAPR( ISP )
      ENDDO
      DO IEV = 1, NEV
        DRXADF( IEV ) = DRXARR( IEV ) - DRXAPR( IEV )
        DRYADF( IEV ) = DRYARR( IEV ) - DRYAPR( IEV )
      ENDDO
C
      DTOTNM = 0.0d0
      DTOTNM = DTOTNM + DNRM2( NSP, DSXADF, INCX )
      DTOTNM = DTOTNM + DNRM2( NSP, DSYADF, INCX )
      DTOTNM = DTOTNM + DNRM2( NEV, DRXADF, INCX )
      DTOTNM = DTOTNM + DNRM2( NEV, DRYADF, INCX )
C
      DO I = 1, NRELVL
        IF ( NUMOU(I).GT.0 ) THEN
          WRITE (6,82) I, NUMOU(I), DCRES(I), 
     1                 DCRES(I)/DBLE( NUMOU(I) )
        ENDIF
      ENDDO
 82   FORMAT('Observation ',I6,' uses ',I8,' cres ',f20.4,
     1        ' avg ',f20.4)
C
      DO ISP = 1, NSP
        DCOVEL = DSQRT( DCVMSX( ISP )*DCVMSX( ISP ) +
     1                  DCVMSY( ISP )*DCVMSY( ISP )  )
        WRITE (6,71) ITER,
     1               CSTARR( ISP )(1:LSTARR( ISP ) ),
     2               CPHARR( ISP )(1:LPHARR( ISP ) ),
     3               DSTLAT( ISP ), DSTLON( ISP ),
     4               DRFLAT( ISP ), DRFLON( ISP ),
     5               DSXARR( ISP ), DSYARR( ISP ),
     6               DCOVEL
      ENDDO
 71   FORMAT(I5,1X,A8,1X,A8,1X,f10.5,1X,f11.5,1X,f10.5,1X,f11.5,1X,
     1               f13.8,1X,f13.8,1X,f13.8)
C
      DO IEV = 1, NLEV
        DCOVEL = DSQRT( DCVMEX( IEV )*DCVMEX( IEV ) +
     1                  DCVMEY( IEV )*DCVMEY( IEV )  )
        WRITE (6,81) ITER, CEVARR( IEV )(1:LEVARR( IEV ) ),
     1               DRXARR( IEV ), DRYARR( IEV ),
     2               DCOVEL
      ENDDO
 81   FORMAT('Iter ',I5,1X,A,1X,f10.4,1X,f10.4,1X,f10.4)
C
      IF ( ITER.GE.MXITER ) THEN
        WRITE (6,*) 'MXITER iterations exceeded.'
        GOTO 60
      ENDIF
      IF ( DTOTNM.LT.DTOL ) THEN
        WRITE (6,*) 'Convergence reached.'
        GOTO 60
      ENDIF
C
      GOTO 50
C
 60   CONTINUE
C
C Any final output?
C
      CALL EXIT(0)
 99   CONTINUE
      WRITE (6,'(A)') CMESS
      CALL EXIT(1)
      END
C
