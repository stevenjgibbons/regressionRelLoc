C
C Absolute Slowness Vectors find.
C 2024-03-18
C
C NPAIRM is the maximum number of pairs for our relative to absolute
C calculation. Now if we have NLSP live slowness vectors then NPAIRM needs to
C be at least NLSP*(NLSP-1)
C
C Since we are not fixing any of the values, we wish to preserve the means
C of both the X values and the Y values. These are specified as DREQMX and
C DREQMY.
C
      SUBROUTINE ABSSVF( IERR, NLSP, NSP, INDLSP, NEV, NPAIRM,
     1             ITER, NRELVL, IE1ARR, IE2ARR, ISPARR, DE1ARR, DE2ARR,
     2                   DCCARR, DRXARR, DRYARR, LDA, ND, DAMAT, DDVEC,
     3                   DWEIG, IOBS, JOBS, LWORK, LWOPT, DWORK1, DRESV,
     4                   DOLDRV, DTEMP1, IABSVL, JABSVL, DRELVX, DRELVY,
     5                   DABSVX, DABSVY, DWGHTA, DRELRX, DRELRY,
     6                   LDATCM, DATCVM, DWORK2, DOLDR2, NUMOU, DCRES,
     7                   DREQMX, DREQMY )
      IMPLICIT NONE
C
      INTEGER            IERR
      INTEGER            NLSP
      INTEGER            NSP
      INTEGER            INDLSP( NLSP )
      INTEGER            NEV
      INTEGER            NPAIRM
      INTEGER            ITER
      INTEGER            NRELVL
      INTEGER            IE1ARR( NRELVL )
      INTEGER            IE2ARR( NRELVL )
      INTEGER            ISPARR( NRELVL )
      REAL*8             DE1ARR( NRELVL )
      REAL*8             DE2ARR( NRELVL )
      REAL*8             DCCARR( NRELVL )
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
      REAL*8             DABSVX( NLSP )
      REAL*8             DABSVY( NLSP )
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
C
      INTEGER            IPAIR
      INTEGER            NPAIRS
      INTEGER            ILSPI
      INTEGER            ILSPJ
      INTEGER            NABSVL
      LOGICAL            OSOLVE
      REAL*8             DRESVN
      REAL*8             DXJMXI
      REAL*8             DYJMYI
      INTEGER            MXITER
      PARAMETER        ( MXITER = 10000 )
      REAL*8             DTOL
      PARAMETER        ( DTOL = 1.0d-6 )
C
      INTEGER            I
      REAL*8             DSCALE
      REAL*8             DTRUEM
C
      INTEGER            IFIXLS
      PARAMETER        ( IFIXLS = 0 )
C
      IERR   = 0
C
      IPAIR = 0
      DO ILSPI = 1, NLSP
        DO ILSPJ = 1, NLSP
          IF ( ILSPI.NE.ILSPJ ) THEN
C           .
C           . We want to calculate DXJ-DXI and DYJ-DYI
C           .
            CALL OSSPPS( IERR, NLSP, ILSPI, ILSPJ, NSP, INDLSP, NEV,
     1             ITER, NRELVL, IE1ARR, IE2ARR, ISPARR,
     2             DE1ARR, DE2ARR, DCCARR, DRXARR, DRYARR,
     3             LDA, ND, DAMAT, DDVEC, DWEIG, IOBS, JOBS,
     4             LWORK, LWOPT, DWORK1, DRESV, DOLDRV, DTEMP1,
     5             DXJMXI, DYJMYI, OSOLVE, DRESVN, NUMOU, DCRES )
            IF ( IERR.NE.0 ) THEN
              WRITE (6,*) 'Subroutine ABSSVF.'
              WRITE (6,*) 'OSSPPS returned IERR = ', IERR
              IERR   = 1
              RETURN
            ENDIF
            IF ( OSOLVE ) THEN
              IPAIR            = IPAIR + 1
              WRITE (6,81) ILSPI, ILSPJ, DXJMXI, DYJMYI
 81   FORMAT('I= ',I4,' J= ',I4,' dxj-dxi = ',f10.4,' dyj-dyi = ',
     1       f10.4)
              IABSVL( IPAIR  ) = ILSPI
              JABSVL( IPAIR  ) = ILSPJ
              DRELVX( IPAIR  ) = DXJMXI
              DRELVY( IPAIR  ) = DYJMYI
              DWGHTA( IPAIR  ) = 1.0d0/( DRESVN + 0.1d0 )
            ENDIF
C           .
          ENDIF
        ENDDO
      ENDDO
      NPAIRS = IPAIR
C
C     OK we should have all our relative X and Y values now.
C
      NABSVL = NLSP
      CALL IRLSRA( IERR, NABSVL, NPAIRS, LDATCM, IFIXLS,
     1             IABSVL, JABSVL, DRELVX, DWGHTA, DRELRX,
     2             DABSVX, DATCVM,
     3             MXITER, DTOL, DWORK2, DOLDR2 )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'Subroutine ABSSVF.'
        WRITE (6,*) 'IRLSRA for SX returned IERR = ', IERR
        IERR   = 1
        RETURN
      ENDIF
C
C Calculate the true mean and add the required mean.
C (I think the mean should be zero out of IRLSRA - but it doesn't
C hurt to calculate it again.)
C
      DSCALE = 1.0d0/DBLE( NABSVL )
      DTRUEM = 0.0d0
      DO I = 1, NABSVL
        DTRUEM = DTRUEM + DSCALE*DABSVX( I )
      ENDDO
      DO I = 1, NABSVL
        DABSVX( I ) = DABSVX( I ) - DTRUEM + DREQMX
      ENDDO
C
c     NABSVL = NLSP
      CALL IRLSRA( IERR, NABSVL, NPAIRS, LDATCM, IFIXLS,
     1             IABSVL, JABSVL, DRELVY, DWGHTA, DRELRY,
     2             DABSVY, DATCVM,
     3             MXITER, DTOL, DWORK2, DOLDR2 )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'Subroutine ABSSVF.'
        WRITE (6,*) 'IRLSRA for SY returned IERR = ', IERR
        IERR   = 1
        RETURN
      ENDIF
C
      DSCALE = 1.0d0/DBLE( NABSVL )
      DTRUEM = 0.0d0
      DO I = 1, NABSVL
        DTRUEM = DTRUEM + DSCALE*DABSVY( I )
      ENDDO
      DO I = 1, NABSVL
        DABSVY( I ) = DABSVY( I ) - DTRUEM + DREQMY
      ENDDO
C
      RETURN
      END
C


