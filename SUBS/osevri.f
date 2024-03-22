C
C OSEVRI - One single event pair Residual Increment
C 
C  We have the double precision array
C  (residual per event)
C  DRESPE( NEV )
C  and it adds to the correct columns the absolute values.
C  NRESPR( NEV ) is the number of observations contributing
C  to this residual.
C  We also have the double precision array
C  (residual per slowness vector)
C  DRESPS( NSP ) and it adds to the right location.
C  NRESPS( NSP ) is the number of observations contributing
C 
      SUBROUTINE OSEVRI( IERR, NLEV, ILEVA, ILEVB, NEV,
     1                   INDLEV, NSP, NRELVL, IE1ARR, IE2ARR, ISPARR,
     2                   DE1ARR, DE2ARR, DSXARR, DSYARR,
     3                   DRXARR, DRYARR, DRESPE, DRESPS,
     4                   NRESPE, NRESPS )
      IMPLICIT NONE
C
      INTEGER     IERR
      INTEGER     NLEV
      INTEGER     ILEVA
      INTEGER     ILEVB
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
C Note that the dimension of DRESV has to be at least ND
C but we do not pass this argument - we calculate it again.
C However, we follow exactly the same recipe as before so this should be
C ok. It has dimension LDA in the level above.
C
      INTEGER     IRELVL
      INTEGER     JRELVL
      REAL*8      DRXA
      REAL*8      DRXB
      REAL*8      DRYA
      REAL*8      DRYB
      REAL*8      DSXI
      REAL*8      DSXJ
      REAL*8      DSYI
      REAL*8      DSYJ
      REAL*8      DTIA
      REAL*8      DTIB
      REAL*8      DTJA
      REAL*8      DTJB
      REAL*8      DIFFX
      REAL*8      DIFFY
      REAL*8      DRHS
      REAL*8      DLHS
      REAL*8      DRESID
      INTEGER     ISPI
      INTEGER     ISPJ
C
      INTEGER     IEVA
      INTEGER     IEVB
      INTEGER     ND
C
      IERR   = 0
      IF ( ILEVA.EQ.ILEVB .OR. ILEVA.LT.1 .OR. ILEVA.GT.NLEV .OR.
     1     ILEVB.LT.1     .OR. ILEVB.GT.NLEV ) THEN
        WRITE (6,*) 'Error in routine OSEVPS )'
        WRITE (6,*) 'ILEVA  = ', ILEVA
        WRITE (6,*) 'ILEVB  = ', ILEVB
        WRITE (6,*) 'NLEV   = ', NLEV
        IERR   = 1
        RETURN
      ENDIF
      IEVA   = INDLEV( ILEVA )
      IEVB   = INDLEV( ILEVB )
      IF(     IEVA.LT.1 .OR. IEVA.GT.NEV  .OR.
     1        IEVB.LT.1 .OR. IEVB.GT.NEV ) THEN
        WRITE (6,*) 'Error in routine OSEVPS )'
        WRITE (6,*) 'IEVA  = ', IEVA
        WRITE (6,*) 'IEVB  = ', IEVB
        WRITE (6,*) 'NEV   = ', NEV
        IERR   = 1
        RETURN
      ENDIF
C
      DRXA   = DRXARR( IEVA )
      DRXB   = DRXARR( IEVB )
      DRYA   = DRYARR( IEVA )
      DRYB   = DRYARR( IEVB )
C
      ND     = 0
      DO IRELVL = 1, NRELVL
        IF (     IE1ARR( IRELVL ).EQ.IEVA       .AND.
     1           IE2ARR( IRELVL ).EQ.IEVB       ) THEN
          DTIA   = DE1ARR( IRELVL )
          DTIB   = DE2ARR( IRELVL )
          ISPI   = ISPARR( IRELVL )
          DSXI   = DSXARR( ISPI )
          DSYI   = DSYARR( ISPI )
          DO JRELVL = 1, NRELVL
            IF (     IE1ARR( JRELVL ).EQ.IEVA       .AND.
     1               IE2ARR( JRELVL ).EQ.IEVB       .AND.
     2               ISPARR( JRELVL ).NE.ISPI    ) THEN
              DTJA   = DE1ARR( JRELVL )
              DTJB   = DE2ARR( JRELVL )
              ISPJ   = ISPARR( JRELVL )
              DSXJ   = DSXARR( ISPJ )
              DSYJ   = DSYARR( ISPJ )
              ND             = ND + 1
              DIFFX  = DSXJ - DSXI
              DIFFY  = DSYJ - DSYI
              DRHS   = DTIB - DTJB - DTIA + DTJA
              DLHS   = DIFFX*(DRXB-DRXA) + DIFFY*(DRYB-DRYA)
              DRESID = DABS( DRHS - DLHS )
              DRESPE( IEVA ) = DRESPE( IEVA ) + DRESID
              DRESPE( IEVB ) = DRESPE( IEVB ) + DRESID
              DRESPS( ISPI ) = DRESPS( ISPI ) + DRESID
              DRESPS( ISPJ ) = DRESPS( ISPJ ) + DRESID
              NRESPE( IEVA ) = NRESPE( IEVA ) + 1
              NRESPE( IEVB ) = NRESPE( IEVB ) + 1
              NRESPS( ISPI ) = NRESPS( ISPI ) + 1
              NRESPS( ISPJ ) = NRESPS( ISPJ ) + 1
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
      RETURN
      END
C
