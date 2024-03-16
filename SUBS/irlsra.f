C
C Steve Gibbons
C 2023-12-07
C
C Iteratively Reweighted Least Squares Relative to Absolute.
C
C The routine VDCR2A solves for NABSVL absolute values in the
C array DABSVA given NRELVL relative values in the array DRELVA
C The array DWGHTA contains an inital set of weights (with high
C weight indicating good quality)
C
C Here we call VDCR2A repeatedly with a weight
C
C DWORK1( I ) = 1.0d0/( DEPS + DSQRT( DABS( DRESID(I) ) ) )
C 
C where DRESID(I) is the residual from the previous iteration.
C
C We stop when ITER exceeds MXITER or when the
C residual from the previous iteration is identical to the residual
C from the current iteration.
C
C See VDCR2A for details of the inputs.
C
C We have the additional integer variable MXITER
C the additional real*8 variables DTOL
C and 2 work arrays of length NRELVL:
C  DWORK1, DOLDRV
C
      SUBROUTINE IRLSRA( IERR, NABSVL, NRELVL, LDATCM,         IFIXVL,
     1                   IABSVL, JABSVL, DRELVA, DWGHTA, DRELRA,
     2                   DABSVA, DATCVM,
     3                   MXITER, DTOL, DWORK1, DOLDRV )
      IMPLICIT NONE
C
      INTEGER     IERR
      INTEGER     NABSVL
      INTEGER     NRELVL
      INTEGER     LDATCM
      INTEGER     IFIXVL
C
      INTEGER     IABSVL( NRELVL )
      INTEGER     JABSVL( NRELVL )
C
      REAL*8      DRELVA( NRELVL )
      REAL*8      DWGHTA( NRELVL )
      REAL*8      DRELRA( NRELVL )
C
      REAL*8      DABSVA( NABSVL )
C
      REAL*8      DATCVM( LDATCM, LDATCM )
C
C ---------
      INTEGER     MXITER
      REAL*8      DTOL
      REAL*8      DWORK1( NRELVL )
      REAL*8      DOLDRV( NRELVL )
C ---------
      INTEGER     I
      INTEGER     NITER
      INTEGER     IWGHTF
C
      INTEGER     INCX
      PARAMETER ( INCX = 1      )
C
      REAL*8      DEPS
      PARAMETER ( DEPS = 1.0d-4 )
C
      REAL*8      DDIFF
C
      REAL*8      DNRM2
C
      IWGHTF = 1
      IERR   = 0
C
C First simply copy the original weights into DWORK1
C and then normalize and then solve for the initial set of
C residuals in DRELRA
C
      DO I = 1, NRELVL
        DWORK1( I ) = DWGHTA( I )
      ENDDO
      CALL MDPVUN( NRELVL, DWORK1 )
C
      CALL VDCR2A( IERR, NABSVL, NRELVL, LDATCM, IWGHTF, IFIXVL,
     1             IABSVL, JABSVL, DRELVA, DWORK1, DRELRA,
     2             DABSVA, DATCVM )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'Subroutine IRLSRA'
        WRITE (6,*) 'VDCR2A returned IERR = ', IERR
        RETURN
      ENDIF
C
C Now copy the vector of residuals into DOLDRV
C
      DO I = 1, NRELVL
        DOLDRV( I ) = DRELRA( I )
      ENDDO
C
C Now start our loop around the iterations
C
      DO NITER = 1, MXITER
C       .
C       . First construct the vector of weights
C       . from the previous set of residuals
C       .
        DO I = 1, NRELVL
          DWORK1( I ) = 1.0d0/( DEPS + DSQRT( DABS( DOLDRV(I) ) ) )
        ENDDO
        CALL MDPVUN( NRELVL, DWORK1 )
C       .
        CALL VDCR2A( IERR, NABSVL, NRELVL, LDATCM, IWGHTF, IFIXVL,
     1               IABSVL, JABSVL, DRELVA, DWORK1, DRELRA,
     2               DABSVA, DATCVM )
        IF ( IERR.NE.0 ) THEN
          WRITE (6,*) 'Subroutine IRLSRA'
          WRITE (6,*) 'VDCR2A returned IERR = ', IERR
          RETURN
        ENDIF
C       .
C       . Put the difference between the old residuals and the
C       . new into DWORK1 as we do not need this array right now
C       .
        DO I = 1, NRELVL
          DWORK1( I ) = DRELRA( I ) - DOLDRV( I )
        ENDDO
        DDIFF = DNRM2( NRELVL, DWORK1, INCX )

        WRITE (6,81) NITER, DDIFF
 81     FORMAT('Iteration ',I8,' : resid_diff = ', f20.6)
        IF ( DDIFF.LT.DTOL ) RETURN
C       .
C       . Now need to copy the new residuals into the old
C       . for the next iteration.
C       .
        DO I = 1, NRELVL
          DOLDRV( I ) = DRELRA( I )
        ENDDO
C       .
      ENDDO
C
      WRITE (6,*) 'ILRSRA: MXITER exceeded without convergence'
C
      RETURN
      END
C
