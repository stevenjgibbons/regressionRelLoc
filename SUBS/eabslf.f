C
C Event Absolute Locations Find.
C 2024-03-17
C
C IFIXLE is the index of the live event that we want to fix to zero.
C NPAIRM is the maximum number of pairs for our relative to absolute
C calculation. Now if we have NLEV live events then NPAIRM needs to
C be at least NLEV*(NLEV-1)
C
C
      SUBROUTINE EABSLF( IERR, NLEV, IFIXLE, NEV, INDLEV, NSP, NPAIRM,
     1             ITER, NRELVL, IE1ARR, IE2ARR, ISPARR, DE1ARR, DE2ARR,
     2                   DCCARR, DSXARR, DSYARR, LDA, ND, DAMAT, DDVEC,
     3                   DWEIG, IOBS, JOBS, LWORK, LWOPT, DWORK1, DRESV,
     4                   DOLDRV, DTEMP1, IABSVL, JABSVL, DRELVX, DRELVY,
     5                   DABSVX, DABSVY, DWGHTA, DRELRX, DRELRY, 
     6                   LDATCM, DATCVM, DWORK2, DOLDR2, NUMOU, DCRES,
     7                   DCVSVX, DCVSVY )
      IMPLICIT NONE
C
      INTEGER            IERR
      INTEGER            NLEV
      INTEGER            IFIXLE
      INTEGER            NEV
      INTEGER            INDLEV( NLEV )
      INTEGER            NSP
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
      REAL*8             DABSVX( NLEV )
      REAL*8             DABSVY( NLEV )
      REAL*8             DWGHTA( NPAIRM )
      REAL*8             DRELRX( NPAIRM )
      REAL*8             DRELRY( NPAIRM )
      REAL*8             DWORK2( NPAIRM )
      REAL*8             DOLDR2( NPAIRM )
      INTEGER            LDATCM
      REAL*8             DATCVM( LDATCM, LDATCM )
      INTEGER            NUMOU( NRELVL )
      REAL*8             DCRES( NRELVL )
      REAL*8             DCVSVX( NLEV )
      REAL*8             DCVSVY( NLEV )
C
      INTEGER            I
      INTEGER            IPAIR
      INTEGER            NPAIRS
      INTEGER            ILEVA
      INTEGER            ILEVB
      INTEGER            NABSVL
      LOGICAL            OSOLVE
      REAL*8             DSCALE
      REAL*8             DRESVN
      REAL*8             DXBMXA
      REAL*8             DYBMYA
      INTEGER            MXITER
      PARAMETER        ( MXITER = 10000 )
      REAL*8             DTOL
      PARAMETER        ( DTOL = 1.0d-6 )
C
      IERR   = 0
      IF ( IFIXLE.LT.1 .OR. IFIXLE.GT.NLEV ) THEN
        WRITE (6,*) 'Subroutine EABSLF.'
        WRITE (6,*) 'IFIXLE = ', IFIXLE
        WRITE (6,*) 'NLEV   = ', NLEV  
        IERR   = 1
        RETURN
      ENDIF
C
      IPAIR = 0
      DO ILEVA = 1, NLEV
        DO ILEVB = 1, NLEV
          IF ( ILEVA.NE.ILEVB ) THEN
C           .
C           . We want to calculate DXB-DXA and DYB-DYA
C           .
            CALL OSEVPS( IERR, NLEV, ILEVA, ILEVB, NEV, INDLEV, NSP,
     1             ITER, NRELVL, IE1ARR, IE2ARR, ISPARR,
     2             DE1ARR, DE2ARR, DCCARR, DSXARR, DSYARR,
     3             LDA, ND, DAMAT, DDVEC, DWEIG, IOBS, JOBS,
     4             LWORK, LWOPT, DWORK1, DRESV, DOLDRV, DTEMP1,
     5             DXBMXA, DYBMYA, OSOLVE, DRESVN, NUMOU, DCRES )
            IF ( IERR.NE.0 ) THEN
              WRITE (6,*) 'Subroutine EABSLF.'
              WRITE (6,*) 'OSEVPS returned IERR = ', IERR
              IERR   = 1
              RETURN
            ENDIF
            IF ( OSOLVE ) THEN
              IPAIR            = IPAIR + 1
c             WRITE (6,81) ILEVA, ILEVB, DXBMXA, DYBMYA
c81   FORMAT('A= ',I4,' B= ',I4,' dxb-dxa = ',f10.4,' dyb-dya = ',
c    1       f10.4)
              IABSVL( IPAIR  ) = ILEVA
              JABSVL( IPAIR  ) = ILEVB
              DRELVX( IPAIR  ) = DXBMXA
              DRELVY( IPAIR  ) = DYBMYA
              DWGHTA( IPAIR  ) = 1.0d0/( DRESVN + 0.1d0 )
            ENDIF
C           .
          ENDIF
        ENDDO
      ENDDO
      NPAIRS = IPAIR
      NABSVL = NLEV
      DSCALE = 1.0d0/DBLE( NABSVL )
C
C     OK we should have all our relative X and Y values now.
C
      CALL IRLSRA( IERR, NABSVL, NPAIRS, LDATCM, IFIXLE,
     1             IABSVL, JABSVL, DRELVX, DWGHTA, DRELRX,
     2             DABSVX, DATCVM,
     3             MXITER, DTOL, DWORK2, DOLDR2 )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'Subroutine EABSLF.'
        WRITE (6,*) 'IRLSRA for RX returned IERR = ', IERR
        IERR   = 1
        RETURN
      ENDIF
C
C Put the cumulative residual relating to each absolute value
C
       DO I = 1, NABSVL
         DCVSVX( I ) = 0.0d0
       ENDDO
       DO IPAIR = 1, NPAIRS
         ILEVA  = IABSVL( IPAIR  )
         ILEVB  = JABSVL( IPAIR  )
         DCVSVX( ILEVA ) = DCVSVX( ILEVA ) +
     1                       DABS( DRELRX( IPAIR ) )*DSCALE
         DCVSVX( ILEVB ) = DCVSVX( ILEVB ) +
     1                       DABS( DRELRX( IPAIR ) )*DSCALE
       ENDDO
cC
cC Now put the sqrt( diagonals ) of the covariance matrix into DCVSVX
cC
c      DO I = 1, NABSVL
c        DCVSVX( I ) = DSQRT( DATCVM( I, I ) )
c      ENDDO
C
c     NABSVL = NLEV
      CALL IRLSRA( IERR, NABSVL, NPAIRS, LDATCM, IFIXLE,
     1             IABSVL, JABSVL, DRELVY, DWGHTA, DRELRY,
     2             DABSVY, DATCVM,
     3             MXITER, DTOL, DWORK2, DOLDR2 )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'Subroutine EABSLF.'
        WRITE (6,*) 'IRLSRA for RY returned IERR = ', IERR
        IERR   = 1
        RETURN
      ENDIF
C
C Put the cumulative residual relating to each absolute value
C
       DO I = 1, NABSVL
         DCVSVY( I ) = 0.0d0
       ENDDO
       DO IPAIR = 1, NPAIRS
         ILEVA  = IABSVL( IPAIR  )
         ILEVB  = JABSVL( IPAIR  )
         DCVSVY( ILEVA ) = DCVSVY( ILEVA ) +
     1                       DABS( DRELRY( IPAIR ) )*DSCALE
         DCVSVY( ILEVB ) = DCVSVY( ILEVB ) +
     1                       DABS( DRELRY( IPAIR ) )*DSCALE
       ENDDO
cC
cC Now put the sqrt( diagonals ) of the covariance matrix into DCVSVY
cC
c      DO I = 1, NABSVL
c        DCVSVY( I ) = DSQRT( DATCVM( I, I ) )
c      ENDDO
C
      RETURN
      END
C
