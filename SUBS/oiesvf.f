C
C One Iteration Event and Slowness Vector Find.
C 2024-03-18
C
C Performs one iteration where we start off with a set of slowness
C vectors and find the loci of events that best fits them (using
C EABSLF) and we then find the new set of slowness vectors that
C best fit our new set of events (using ABSSVF).
C This routine is really just a wrapper for those two routines,
C carrying out a little book-keeping.
C
C IFIXLE is the index of the live event that we want to fix to zero.
C NPAIRM is the maximum number of pairs for our relative to absolute
C calculation. Now if we have NLEV live events and NLSP live slowness
C vectors then NPAIRM needs to be at least the greater of the
C two quantities NLEV*(NLEV-1) and NLSP*(NLSP-1)
C
C
      SUBROUTINE OIESVF( IERR, NLEV, NLSP, IFIXLE, NEV, INDLEV, NSP,
     1             INDLSP, NPAIRM, ITER, NRELVL, IE1ARR, IE2ARR, ISPARR,
     2             DE1ARR, DE2ARR, DCCARR, DSXARR, DSYARR, DRXARR,
     3             DRYARR, LDA, ND, DAMAT, DDVEC, DWEIG, IOBS, JOBS,
     4             LWORK, LWOPT, DWORK1, DRESV, DOLDRV, DTEMP1, IABSVL,
     5             JABSVL, DRELVX, DRELVY, DABSEX, DABSEY, DABSSX,
     6             DABSSY, DWGHTA, DRELRX, DRELRY, LDATCM, DATCVM,
     7             DWORK2, DOLDR2, NUMOU, DCRES, DREQMX, DREQMY,
     8          DCVMEX, DCVMEY, DCVMSX, DCVMSY, DSXAR0, DSYAR0, DSLOWM )
      IMPLICIT NONE
C
      INTEGER            IERR
      INTEGER            NLEV
      INTEGER            NLSP
      INTEGER            IFIXLE
      INTEGER            NEV
      INTEGER            INDLEV( NLEV )
      INTEGER            NSP
      INTEGER            INDLSP( NLSP )
      INTEGER            NPAIRM
      INTEGER            ITER
      INTEGER            NRELVL
      INTEGER            IE1ARR( NRELVL )
      INTEGER            IE2ARR( NRELVL )
      INTEGER            ISPARR( NRELVL )
      REAL*8             DE1ARR( NRELVL )
      REAL*8             DE2ARR( NRELVL )
      REAL*8             DCCARR( NRELVL )
      REAL*8             DSXARR( NSP )
      REAL*8             DSYARR( NSP )
      REAL*8             DRXARR( NEV )
      REAL*8             DRYARR( NEV )
      INTEGER            LDA
      INTEGER            ND
      REAL*8             DAMAT( LDA, 3 )
      REAL*8             DDVEC( LDA )
      REAL*8             DWEIG( LDA )
      INTEGER            IOBS( LDA )
      INTEGER            JOBS( LDA )
      INTEGER            LWORK
      INTEGER            LWOPT
      REAL*8             DWORK1( LWORK )
      REAL*8             DRESV( LDA )
      REAL*8             DOLDRV( LDA )
      REAL*8             DTEMP1( LDA )
      INTEGER            IABSVL( NPAIRM )
      INTEGER            JABSVL( NPAIRM )
      REAL*8             DRELVX( NPAIRM )
      REAL*8             DRELVY( NPAIRM )
      REAL*8             DABSEX( NLEV )
      REAL*8             DABSEY( NLEV )
      REAL*8             DABSSX( NLSP )
      REAL*8             DABSSY( NLSP )
      REAL*8             DWGHTA( NPAIRM )
      REAL*8             DRELRX( NPAIRM )
      REAL*8             DRELRY( NPAIRM )
      REAL*8             DWORK2( NPAIRM )
      REAL*8             DOLDR2( NPAIRM )
      INTEGER            LDATCM
      REAL*8             DATCVM( LDATCM, LDATCM )
      INTEGER            NUMOU( NRELVL )
      REAL*8             DCRES( NRELVL )
      REAL*8             DREQMX
      REAL*8             DREQMY
      REAL*8             DCVMEX( NLEV )
      REAL*8             DCVMEY( NLEV )
      REAL*8             DCVMSX( NLSP )
      REAL*8             DCVMSY( NLSP )
      REAL*8             DSXAR0( NSP )
      REAL*8             DSYAR0( NSP )
      REAL*8             DSLOWM
C
      INTEGER            IEV
      INTEGER            ILEV
      INTEGER            ISP
      INTEGER            ILSP
      REAL*8             DLEN
      REAL*8             DLENTL
      REAL*8             DLENTA
      REAL*8             DRATIO
C
      REAL*8             DSX
      REAL*8             DSUMSX
      REAL*8             DELSX
      REAL*8             DSY
      REAL*8             DSUMSY
      REAL*8             DELSY
      REAL*8             DTORQ
C
      IERR   = 0
C
C First we calculate the total length of the slowness vectors.
C We will ultimately scale the output slowness vectors to that
C the total length is unchanged.
C
      DLENTL = 0.0d0
      DO ISP = 1, NSP
        DLEN   = DSQRT(   DSXARR(ISP)*DSXARR(ISP)      +
     1                    DSYARR(ISP)*DSYARR(ISP)   )
        DLENTL = DLENTL + DLEN
      ENDDO
C
C Now we need to call EABSLF:
C
      CALL EABSLF( IERR, NLEV, IFIXLE, NEV, INDLEV, NSP, NPAIRM,
     1             ITER, NRELVL, IE1ARR, IE2ARR, ISPARR, DE1ARR, DE2ARR,
     2             DCCARR, DSXARR, DSYARR, LDA, ND, DAMAT, DDVEC,
     3             DWEIG, IOBS, JOBS, LWORK, LWOPT, DWORK1, DRESV,
     4             DOLDRV, DTEMP1, IABSVL, JABSVL, DRELVX, DRELVY,
     5             DABSEX, DABSEY, DWGHTA, DRELRX, DRELRY,
     6             LDATCM, DATCVM, DWORK2, DOLDR2, NUMOU, DCRES,
     7             DCVMEX, DCVMEY )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'Subroutine OIESVF. Error from EABSLF.'
        IERR   = 1
        RETURN
      ENDIF
C
C We have the new absolute values of the event coordinates
C in DABSEX and DABSEY. We need to transfer them to DRXARR and
C DRYARR so that we can use them to calculate the next iteration
C of slowness vectors (using ABSSVF).
C
C Store DCVMEX and DCVMEY temporarily in DATCVM(1,) and DATCVM(2,)
C and then put the values back into DCVMEX and DCVMEY in the correct order
C
      DO ILEV = 1, NLEV
        DATCVM( 1, ILEV ) = DCVMEX( ILEV )
        DATCVM( 2, ILEV ) = DCVMEY( ILEV )
      ENDDO
C
      DO ILEV = 1, NLEV
        IEV           = INDLEV( ILEV )
        DRXARR( IEV ) = DABSEX( ILEV )
        DRYARR( IEV ) = DABSEY( ILEV )
        DCVMEX( IEV ) = DATCVM( 1, ILEV )
        DCVMEY( IEV ) = DATCVM( 2, ILEV )
      ENDDO
C
C Now we call ABSSVF
C
      CALL ABSSVF( IERR, NLSP, NSP, INDLSP, NEV, NPAIRM,
     1             ITER, NRELVL, IE1ARR, IE2ARR, ISPARR, DE1ARR, DE2ARR,
     2             DCCARR, DRXARR, DRYARR, LDA, ND, DAMAT, DDVEC,
     3             DWEIG, IOBS, JOBS, LWORK, LWOPT, DWORK1, DRESV,
     4             DOLDRV, DTEMP1, IABSVL, JABSVL, DRELVX, DRELVY,
     5             DABSSX, DABSSY, DWGHTA, DRELRX, DRELRY,
     6             LDATCM, DATCVM, DWORK2, DOLDR2, NUMOU, DCRES,
     7             DREQMX, DREQMY, DCVMSX, DCVMSY )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'Subroutine OIESVF. Error from ABSSVF.'
        IERR   = 1
        RETURN
      ENDIF
C
C We now have the new absolute values of the slowness vectors
C in DABSSX and DABSSY. Need to transfer them to the correct locations
C in DSXARR and DSYARR ready for the next iteration.
C
      DO ILSP = 1, NLSP
        DATCVM( 1, ILSP ) = DCVMSX( ILSP )
        DATCVM( 2, ILSP ) = DCVMSY( ILSP )
      ENDDO
C
      DTORQ  = 0.0d0
      DSUMSX = 0.0d0
      DSUMSY = 0.0d0
      DO ILSP = 1, NLSP
        ISP           = INDLSP( ILSP )
        DSX           = DSXARR( ISP )
        DSUMSX        = DSUMSX         + DSX
        DELSX         = DABSSX( ILSP ) - DSX
        DSY           = DSYARR( ISP )
        DSUMSY        = DSUMSY         + DSY
        DELSY         = DABSSY( ILSP ) - DSY
        DTORQ         = DTORQ + DSX*DELSY - DSY*DELSX
        DCVMSX( ISP ) = DATCVM( 1, ILSP )
        DCVMSY( ISP ) = DATCVM( 2, ILSP )
      ENDDO
      PRINT *,'bef TORQUE, DSUMSX, DSUMSY = ', DTORQ, DSUMSX, DSUMSY
C
C Now try to "align" the new values with the old ...
C
      CALL ANSVWO( IERR, NSP, NLSP, INDLSP,
     1             DSXARR, DSYARR, DABSSX, DABSSY,
     2             DSXAR0, DSYAR0, DSLOWM )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'Subroutine OIESVF. Error from ANSVWO.'
        IERR   = 1
        RETURN
      ENDIF
C
C Now recalculate sums and torque and refill DSXARR, DSYARR
C
      DO ILSP = 1, NLSP
        ISP           = INDLSP( ILSP )
        DSXARR( ISP ) = DABSSX( ILSP )
        DSYARR( ISP ) = DABSSY( ILSP )
      ENDDO
C
C Now we calculate the total length of the new slowness vectors.
C
      DLENTA = 0.0d0
      DO ISP = 1, NSP
        DLEN   = DSQRT(   DSXARR(ISP)*DSXARR(ISP)      +
     1                    DSYARR(ISP)*DSYARR(ISP)   )
        DLENTA = DLENTA + DLEN
      ENDDO
C
      DRATIO = DLENTL/DLENTA
C
C Now we scale the vectors by this factor
C
c     DO ISP = 1, NSP
c       DSXARR( ISP ) = DSXARR( ISP )*DRATIO
c       DSYARR( ISP ) = DSYARR( ISP )*DRATIO
c     ENDDO
C
      RETURN
      END
C
