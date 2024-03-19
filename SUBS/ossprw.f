C
C OSSPRW - One single station/phase pair Residual write
C 
      SUBROUTINE OSSPRW( IERR, ITER, LUOUT, NLSP, ILSPI, ILSPJ, NSP,
     1                   INDLSP, NEV, NRELVL, IE1ARR, IE2ARR, ISPARR,
     2                   DE1ARR, DE2ARR, DCCARR, DRXARR, DRYARR, DRESV )
      IMPLICIT NONE
C
      INTEGER     IERR
      INTEGER     ITER
      INTEGER     LUOUT
      INTEGER     NLSP
      INTEGER     ILSPI
      INTEGER     ILSPJ
      INTEGER     NSP
      INTEGER     INDLSP( NLSP )
      INTEGER     NEV
      INTEGER     NRELVL
      INTEGER     IE1ARR( NRELVL )
      INTEGER     IE2ARR( NRELVL )
      INTEGER     ISPARR( NRELVL )
      REAL*8      DE1ARR( NRELVL )
      REAL*8      DE2ARR( NRELVL )
      REAL*8      DCCARR( NRELVL )
      REAL*8      DRXARR( NEV )
      REAL*8      DRYARR( NEV )
      REAL*8      DRESV( * )
C
C Note that the dimension of DRESV has to be at least ND
C but we do not pass this argument - we calculate it again.
C However, we follow exactly the same recipe as before so this should be
C ok. It has dimension LDA in the level above.
C
      INTEGER     IRELVL
      INTEGER     JRELVL
      REAL*8      DCCI
      REAL*8      DCCJ
      REAL*8      DRXA
      REAL*8      DRXB
      REAL*8      DRYA
      REAL*8      DRYB
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
      IF ( ILSPI.EQ.ILSPJ .OR. ILSPI.LT.1 .OR. ILSPI.GT.NLSP .OR.
     1     ILSPJ.LT.1     .OR. ILSPJ.GT.NLSP ) THEN
        WRITE (6,*) 'Error in routine OSSPRW )'
        WRITE (6,*) 'ILSPI  = ', ILSPI
        WRITE (6,*) 'ILSPJ  = ', ILSPJ
        WRITE (6,*) 'NLSP   = ', NLSP
        IERR   = 1
        RETURN
      ENDIF
      ISPI   = INDLSP( ILSPI )
      ISPJ   = INDLSP( ILSPJ )
      IF(     ISPI.LT.1 .OR. ISPI.GT.NSP  .OR.
     1        ISPJ.LT.1 .OR. ISPJ.GT.NSP ) THEN
        WRITE (6,*) 'Error in routine OSSPRW )'
        WRITE (6,*) 'ISPI  = ', ISPI
        WRITE (6,*) 'ISPJ  = ', ISPJ
        WRITE (6,*) 'NSP   = ', NSP
        IERR   = 1
        RETURN
      ENDIF
C
      ND     = 0
      DO IRELVL = 1, NRELVL
        IF (     ISPARR( IRELVL ).EQ.ISPI       ) THEN
          DTIA   = DE1ARR( IRELVL )
          DTIB   = DE2ARR( IRELVL )
          DCCI   = DCCARR( IRELVL )
          IEVA   = IE1ARR( IRELVL )
          IEVB   = IE2ARR( IRELVL )
          DRXA   = DRXARR( IEVA )
          DRYA   = DRYARR( IEVA )
          DRXB   = DRXARR( IEVB )
          DRYB   = DRYARR( IEVB )
          DO JRELVL = 1, NRELVL
            IF (     IE1ARR( JRELVL ).EQ.IEVA       .AND.
     1               IE2ARR( JRELVL ).EQ.IEVB       .AND.
     2               ISPARR( JRELVL ).EQ.ISPJ    ) THEN
              DTJA   = DE1ARR( JRELVL )
              DTJB   = DE2ARR( JRELVL )
              DCCJ   = DCCARR( JRELVL )
              ND             = ND + 1
              DIFFX  = DRXB - DRXA
              DIFFY  = DRYB - DRYA
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
