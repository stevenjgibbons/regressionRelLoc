C
C Calculate Residual Arrays
C StG 2024/03/22
C
C Zeros out the integer arrays NRESPE( NEV ) and NRESPS( NSP )
C and the double precision arrays DRESPE( NEV ) and DRESPS( NSP )
C and fills the arrays.
C
      SUBROUTINE CALCRA( IERR, NLEV, NEV,
     1                   INDLEV, NSP, NRELVL, IE1ARR, IE2ARR, ISPARR,
     2                   DE1ARR, DE2ARR, DSXARR, DSYARR,
     3                   DRXARR, DRYARR, DRESPE, DRESPS,
     4                   NRESPE, NRESPS )
      IMPLICIT NONE
C
      INTEGER     IERR
      INTEGER     NLEV
      INTEGER     NEV
      INTEGER     INDLEV( NLEV )
      INTEGER     NSP
      INTEGER     NRELVL
      INTEGER     IE1ARR( NRELVL )
      INTEGER     IE2ARR( NRELVL )
      INTEGER     ISPARR( NRELVL )
      REAL*8      DE1ARR( NRELVL )
      REAL*8      DE2ARR( NRELVL )
      REAL*8      DSXARR( NSP )
      REAL*8      DSYARR( NSP )
      REAL*8      DRXARR( NEV )
      REAL*8      DRYARR( NEV )
      REAL*8      DRESPE( NEV )
      REAL*8      DRESPS( NSP )
      INTEGER     NRESPE( NEV )
      INTEGER     NRESPS( NSP )
C
      INTEGER     IEV
      INTEGER     ISP
      INTEGER     ILEVA
      INTEGER     ILEVB
C
      IERR   = 0
C
      DO IEV = 1, NEV
        NRESPE( IEV ) = 0
        DRESPE( IEV ) = 0.0d0
      ENDDO
C
      DO ISP = 1, NSP
        NRESPS( ISP ) = 0
        DRESPS( ISP ) = 0.0d0
      ENDDO
C
      DO ILEVA = 1, NLEV
        DO ILEVB = 1, NLEV
          IF ( ILEVA.NE.ILEVB ) THEN
            CALL OSEVRI( IERR, NLEV, ILEVA, ILEVB, NEV,
     1                   INDLEV, NSP, NRELVL, IE1ARR, IE2ARR, ISPARR,
     2                   DE1ARR, DE2ARR, DSXARR, DSYARR,
     3                   DRXARR, DRYARR, DRESPE, DRESPS,
     4                   NRESPE, NRESPS )
            IF ( IERR.NE.0 ) THEN
              WRITE (6,*) 'Error in CALCRA'
              WRITE (6,*) 'Error from OSEVRI'
              IERR   = 1
              RETURN
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
      RETURN
      END
C
