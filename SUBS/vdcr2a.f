C
C  Steve Gibbons
C  Van Decar and Crosson Relative 2 Absolute routine
C  2023-12-06
C
C  Algorithm based upon that of
C
C title = {Determination of teleseismic relative phase arrival
C times using multi-channel cross-correlation and least squares},
C journal = {Bulletin of the Seismological Society of America},
C author = {van Decar, J C and Crosson, R S},
C year = {1990},
C volume = {80},
C pages = {150--169},
C
C hereafter labelled VDC.
C
C The only principal difference is that VDC add a constraint of zero
C mean whereas I will set (optionally) a single specified value to zero.
C
C We have NABSVL - the number of absolute values wanted.
C                  These can be e.g. times or distances.
C                    These are V(1), V(2), ... , V( NABSVL )
C                  These are the numbers we want to solve for.
C
C         NRELVL - number of relative value measurements.
C                  These are estimates of the values
C                      V( J ) - V( I )
C                  obtained using some or other procedure.
C
C         IABSVL - Integer array with dimension NRELVL
C                  such that 
C                  IABSVL( K ) is a value between 1 and NABSVL.
C         JABSVL - Integer array with dimension NRELVL
C                  such that 
C                  JABSVL( K ) is a value between 1 and NABSVL.
C                   For all K (1 to NRELVL) IABSVL(K) and JABSVL(K)
C                    must be different.
C
C         DRELVA - real*8 array of dimension NRELVL such that
C                  DRELVA( k ) contains the estimate of the value
C                   VJ(k) - VI(k) where I and J are given by
C                      IABSVL and JABSVL.
C
C         DABSVA - real*8 array of dimension NABSVL
C                  Returned with estimates of V(1) --- V( NABSVL )
C
C         DWGHTA - real*8 array of dimension NRELVL
C                  DWGHTA( k ) contains a weighting for the
C                  relative value measurement k.
C         IWGHTF - weight flag.
C                  IF IWGHTF is zero then all measurements are
C                    weighed equally.
C                  IF IWGHTF is above zero then we have set
C                    DWGHTA(k) to a large value if the measurement
C                     is to be weighted highly.
C                  IF IWGHTF is below zero then we have set
C                    DWGHTA(k) to a small value if the measurement
C                     is to be weighted highly.
C                  (i.e. IWGHTF.eq.+1 --> DWGHTA(k) indicator of goodness
C                        IWGHTF.eq.-1 --> DWGHTA(k) indicator of badness )
C
C         IFIXVL - integer between 1 and NABSVL indicating which value
C                   should be fixed to zero.
C                  If IFIXVL is 0 then we do not fix any element to zero.
C                 
C         DRELRA - real*8 array of dimension NRELVL such that
C                   DRELRA( k ) contains the observed minus predicted
C                    value residual i.e.
C                     (v(J)-V(I))_obs - ( v_pred(J) - V_pred(I) )
C                  DRELVA( k ) contains the estimate of the value
C
C         LDATCM leading dimension of work array DATCVM
C                 must be at least equal to NABSVL STEVE
C
C         DATCVM - real*8 array ( LDATCM * LDATCM )
C                  Output with data covariance matrix.
C                  Used as a work array internally.
C                  Is not set on input (contents overwritten).
C
C         IERR   - error flag
C
      SUBROUTINE VDCR2A( IERR, NABSVL, NRELVL, LDATCM, IWGHTF, IFIXVL,
     1                   IABSVL, JABSVL, DRELVA, DWGHTA, DRELRA,
     2                   DABSVA, DATCVM )
      IMPLICIT NONE
C
      INTEGER     IERR
      INTEGER     NABSVL
      INTEGER     NRELVL
      INTEGER     LDATCM
      INTEGER     IWGHTF
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
C
      CHARACTER *(1)   UPLO
      PARAMETER      ( UPLO = 'U' )
      INTEGER          NRHS
      PARAMETER      ( NRHS = 1 )
      REAL*8           DLOW
      PARAMETER      ( DLOW = 1.0d-7 )
C
      INTEGER          IABSV
      INTEGER          IRELVL
      INTEGER          I
      INTEGER          J
      INTEGER          IC
      INTEGER          IR
      INTEGER          INFO
C
      REAL*8           DVAL
      REAL*8           DPREDR
C
C
C --- start executable part of code
C
      IERR   = 0
C
C Return with error if IFIXVL is not valid
C
      IF ( IFIXVL.LT.0 .OR. IFIXVL.GT.NABSVL ) THEN
        WRITE (6,*) 'Subroutine VDCR2A'
        WRITE (6,*) 'NABSVL = ', NABSVL
        WRITE (6,*) 'IFIXVL = ', IFIXVL
        IERR  = 99
        RETURN
      ENDIF
C
C Return with error if we have more unknowns than relative
C observations
C
      IF ( NABSVL.GE.NRELVL ) THEN
        WRITE (6,*) 'Subroutine VDCR2A'
        WRITE (6,*) 'NABSVL = ', NABSVL
        WRITE (6,*) 'NRELVL = ', NRELVL
        IERR  = 99
        RETURN
      ENDIF
C
C Return with error if the DATCVM is too small
C
      IF ( NABSVL.GT.LDATCM ) THEN
        WRITE (6,*) 'Subroutine VDCR2A'
        WRITE (6,*) 'NABSVL = ', NABSVL
        WRITE (6,*) 'LDATCM = ', LDATCM
        IERR  = 99
        RETURN
      ENDIF
C
C Initialize the work matrix
C
      DO J = 1, LDATCM
        DO I = 1, J
          DATCVM( I, J ) = 1.0d0
        ENDDO
      ENDDO
C
C Set array DABSVA to zero
C
      DO IABSV  = 1, NABSVL
        DABSVA( IABSV  ) = 0.0d0
      ENDDO
C
C But now use DABSVA to check that we have at least
C one relative measurement that relates to this absolute
C measurement.
C
      DO IRELVL = 1, NRELVL
        I      = IABSVL( IRELVL )
        J      = JABSVL( IRELVL )
        IF (        I.LT.1       .OR.
     1              J.LT.1       .OR.
     2              I.GT.NABSVL  .OR.
     3              J.GT.NABSVL  .OR.
     4              I.EQ.J           ) THEN
          WRITE (6,*) 'Subroutine VDCR2A'
          WRITE (6,*) 'IRELVL  = ', IRELVL
          WRITE (6,*) 'NABSVL  = ', NABSVL
          WRITE (6,*) 'I       = ', I
          WRITE (6,*) 'J       = ', J
          IERR   = 99
          RETURN
        ENDIF
        DABSVA( I ) = DABSVA( I ) + 0.5d0
        DABSVA( J ) = DABSVA( J ) + 0.5d0
      ENDDO
      DO IABSV  = 1, NABSVL
        IF ( DABSVA( IABSV  ).LT.DLOW ) THEN
          WRITE (6,*) 'Subroutine VDCR2A'
          WRITE (6,*) 'No measurement for abs val ', IABSV 
          IERR   = 99
          RETURN
        ENDIF
        DABSVA( IABSV  ) = 0.0d0
      ENDDO
C
C We now want to make sure that DWGHTA contains
C only positive weights and that the weights are larger
C if the variable is to be weighed heavily.
C
      DO IRELVL = 1, NRELVL
        IF ( DWGHTA( IRELVL ).LT.DLOW ) IWGHTF = 0
      ENDDO
C     .
C     . Invert the weights if they are "upside down"
C     .
      IF ( IWGHTF.LT.0 ) THEN
        DO IRELVL = 1, NRELVL
          DWGHTA( IRELVL ) = 1.0d0/DWGHTA( IRELVL )
        ENDDO
      ENDIF
C
C Set to unity if we do not want to weigh
C
      IF ( IWGHTF.EQ.0 ) THEN
        DO IRELVL = 1, NRELVL
          DWGHTA( IRELVL ) = 1.0d0
        ENDDO
      ENDIF
C
C Fill Right Hand Side and matrix elements
C
      DO IRELVL = 1, NRELVL
        DVAL        = DWGHTA( IRELVL )*DRELVA( IRELVL )
        I           = IABSVL( IRELVL )
        J           = JABSVL( IRELVL )
        DABSVA( I ) = DABSVA( I ) - DVAL
        DABSVA( J ) = DABSVA( J ) + DVAL
C       .
C       . Now replace DVAL with the weight for that measurement
C       .
        DVAL        = DWGHTA( IRELVL )
        IC          = MAX( I, J )
        IR          = MIN( I, J )
        DATCVM(  I,  I ) = DATCVM(  I,  I )  + DVAL
        DATCVM(  J,  J ) = DATCVM(  J,  J )  + DVAL
        DATCVM( IR, IC ) = DATCVM( IR, IC )  - DVAL
C       .
      ENDDO
C
      CALL DPOSV( UPLO, NABSVL, NRHS, DATCVM, LDATCM, DABSVA,
     1            NABSVL, INFO )
      IF ( INFO.NE.0 ) THEN
        WRITE ( 6, * ) 'Subroutine VDCR2A'
        WRITE ( 6, * ) 'DPOSV gave INFO = ', INFO
        IERR   = 99
        RETURN
      ENDIF
C
C Now calculate the predicted relative values
C and the residuals for each of the observations
C
      DO IRELVL = 1, NRELVL
        I                = IABSVL( IRELVL )
        J                = JABSVL( IRELVL )
        DPREDR           = DABSVA( J ) - DABSVA( I )
        DRELRA( IRELVL ) = DRELVA( IRELVL ) - DPREDR
      ENDDO
C
C Make sure that the fixed absolute value is set to zero
C
      IF ( IFIXVL.GT.0 ) THEN
        DVAL   = DABSVA( IFIXVL )
        DO IABSV = 1, NABSVL
          DABSVA( IABSV ) = DABSVA( IABSV ) - DVAL
        ENDDO
      ENDIF
C
C Now calculate the data Covariance matrix.
C We need to recalculate the matrix
C
      DO J = 1, LDATCM
        DO I = 1, J
          DATCVM( I, J ) = 1.0d0
        ENDDO
      ENDDO
      DO IRELVL = 1, NRELVL
        I           = IABSVL( IRELVL )
        J           = JABSVL( IRELVL )
        DVAL        = DWGHTA( IRELVL )
        IC          = MAX( I, J )
        IR          = MIN( I, J )
        DATCVM(  I,  I ) = DATCVM(  I,  I )  + DVAL
        DATCVM(  J,  J ) = DATCVM(  J,  J )  + DVAL
        DATCVM( IR, IC ) = DATCVM( IR, IC )  - DVAL
C       .
      ENDDO
C     ... LAPACK Cholesky factorization
      CALL DPOTRF( UPLO, NABSVL, DATCVM, LDATCM, INFO )
      IF ( INFO.NE.0 ) THEN
        WRITE ( 6, * ) 'Subroutine VDCR2A'
        WRITE ( 6, * ) 'DPOTRF gave INFO = ', INFO
        IERR = 6
        RETURN
      ENDIF
C     ... LAPACK inversion
      CALL DPOTRI( UPLO, NABSVL, DATCVM, LDATCM, INFO )
      IF ( INFO.NE.0 ) THEN
        WRITE ( 6, * ) 'Subroutine VDCR2A'
        WRITE ( 6, * ) 'DPOTRI gave INFO = ', INFO
        IERR = 7
        RETURN
      ENDIF
      DO J = 1, NABSVL
        DO I = 1, NABSVL
          IF ( J.LT.I ) DATCVM( I, J ) = DATCVM( J, I )
        ENDDO
      ENDDO
C
      RETURN
      END
C
