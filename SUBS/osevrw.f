C
C OSEVRW - One single event pair Residual write
C 
      SUBROUTINE OSEVRW( IERR, ITER, LUOUT, NLEV, ILEVA, ILEVB, NEV,
     1                   INDLEV, NSP, NRELVL, IE1ARR, IE2ARR, ISPARR,
     2                   DE1ARR, DE2ARR, DCCARR, DSXARR, DSYARR, DRESV )
      IMPLICIT NONE
C
      INTEGER     IERR
      INTEGER     ITER
      INTEGER     LUOUT
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
      REAL*8      DCCARR( NRELVL )
      REAL*8      DSXARR( NSP )
      REAL*8      DSYARR( NSP )
      REAL*8      DRESV( NRELVL )
C
      INTEGER     IRELVL
      INTEGER     JRELVL
      REAL*8      DCCI
      REAL*8      DCCJ
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
      REAL*8      DWGHT
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
      ND     = 0
      DO IRELVL = 1, NRELVL
        IF (     IE1ARR( IRELVL ).EQ.IEVA       .AND.
     1           IE2ARR( IRELVL ).EQ.IEVB       ) THEN
          DTIA   = DE1ARR( IRELVL )
          DTIB   = DE2ARR( IRELVL )
          DCCI   = DCCARR( IRELVL )
          ISPI   = ISPARR( IRELVL )
          DSXI   = DSXARR( ISPI )
          DSYI   = DSYARR( ISPI )
          DO JRELVL = 1, NRELVL
            IF (     IE1ARR( JRELVL ).EQ.IEVA       .AND.
     1               IE2ARR( JRELVL ).EQ.IEVB       .AND.
     2               ISPARR( JRELVL ).NE.ISPI    ) THEN
              DTJA   = DE1ARR( JRELVL )
              DTJB   = DE2ARR( JRELVL )
              DCCJ   = DCCARR( JRELVL )
              ISPJ   = ISPARR( JRELVL )
              DSXJ   = DSXARR( ISPJ )
              DSYJ   = DSYARR( ISPJ )
              ND             = ND + 1
              DIFFX  = DSXJ - DSXI
              DIFFY  = DSYJ - DSYI
              DRHS   = DTIB - DTJB - DTIA + DTJA
              DWGHT  = 0.5d0*( DABS(DCCI)+DABS(DCCJ) ) + 0.01d0
              WRITE ( LUOUT, 81 ) ND, ITER, IEVA, IEVB, ISPI, ISPJ,
     1                            DIFFX, DIFFY, DRHS, DRESV( ND ),
     2                            DWGHT, IRELVL, JRELVL
 81   FORMAT(I6,1X,I7,1X,I4,1X,I4,1X,I4,1X,I4,f12.6,1X,f12.6,1X,
     1       f20.6,1X,f20.6,1X,f10.4,1X,I6,1x,I6)
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
      RETURN
      END
C
