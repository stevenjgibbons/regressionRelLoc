C
C OSSPPS
C One Single Station and Phase Pair Solve.
C
C We have two event identifiers ILSPI and ILSPJ where
C ILSPI and ILSPJ are distinct and between 1 and NLSP.
C We seek to calculate the two values
C   (    DSX( ILSPJ ) - DSX( ILSPI )   ) 
C and
C   (    DSY( ILSPJ ) - DSY( ILSPI )   ) 
C
C  Now a and b are identifiers of seismic events
C i and j are identifiers of seismic station and phase combinations
C
C ( ti,b - tj,b) - (ti,a - tj,a ) = - (Si-Sj).(Rb-Ra)       [1]
C
C Where Si = (sxi,syi)
C       Sj = (sxj,syj)
C Where Ra = (rxa,rya)
C       Rb = (rxb,ryb)
C
C ti,a is the reference epoch time for event a for station/phase i
C ti,b is the (correlation) epoch time for event b for station/phase i
C with respect to the waveform from event a on station/phase i
C
C tj,a is the reference epoch time for event a for station/phase j
C tj,b is the (correlation) epoch time for event b for station/phase j
C with respect to the waveform from event a on station/phase j
C
C We want to solve for (rxb-rxa) and (ryb-rya) using assumed
C SXI and SXJ and measured ( ti,b - tj,b) - (ti,a - tj,a )
C
C So [1] gives us 
C
C (Sj-Si).(Rb-Ra) = ( ti,b - tj,b) - (ti,a - tj,a )
C
C  (sxj-sxi)*(rxb-rxa) + (syj-syi)*(ryb-rya) = ( ti,b - tj,b) - (ti,a - tj,a )
C
C  So we solve a least square problem of the form
C
C  ( Ax, Ay, 1 ) * ( sxj-sxi , syj-syi , C )^T = ( ti,b - tj,b) - (ti,a - tj,a )
C  where our matrix elements Ax and Ay are given by
C
C  Ax = (rxb-rxa)
C  Ay = (ryb-rya)
C
C We will then need to solve for sxi, syi etc. later using
C VanDeCar and Crosson.
C
C Now we have NLSP live station/phase pairs
C
C The logical flag OSOLVE is returned .FALSE. to indicate
C that no solution was found (i.e. fewer than 4 observations)
C
C We calculate the 2-norm of the residual vector (DRESVN) for convenience
C (even though that could be calculated later)
C
      SUBROUTINE OSSPPS( IERR, NLSP, ILSPI, ILSPJ, NSP, INDLSP, NEV,
     1                   NRELVL, IE1ARR, IE2ARR, ISPARR,
     2                   DE1ARR, DE2ARR, DCCARR, DRXARR, DRYARR,
     3                   LDA, ND, DAMAT, DDVEC, DWEIG, IOBS, JOBS,
     4                   LWORK, LWOPT, DWORK1, DRESV, DOLDRV, DTEMP1,
     5                   DXBMXA, DYBMYA, OSOLVE, DRESVN )
      IMPLICIT NONE
C
      INTEGER     IERR
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
      INTEGER     LDA
      INTEGER     ND
      REAL*8      DAMAT( LDA, 3 )
      REAL*8      DDVEC( LDA )
      REAL*8      DWEIG( LDA )
      INTEGER     IOBS( LDA )
      INTEGER     JOBS( LDA )
      INTEGER     LWORK
      INTEGER     LWOPT
      REAL*8      DWORK1( LWORK )
      REAL*8      DRESV( LDA )
      REAL*8      DOLDRV( LDA )
      REAL*8      DTEMP1( LDA )
      REAL*8      DXBMXA
      REAL*8      DYBMYA
      LOGICAL     OSOLVE
      REAL*8      DRESVN
C
      INTEGER     MXITER
      PARAMETER ( MXITER = 10000 )
      REAL*8      DTOL
      PARAMETER ( DTOL = 1.0d-6 )
      INTEGER     NMMAX
      PARAMETER ( NMMAX = 3 )
      INTEGER     NM
      PARAMETER ( NM = 3 )
      REAL*8      DMVEC( NM )
      REAL*8      DWORK2( NMMAX, NM )
      REAL*8      DWORK3( NM )
      INTEGER     IWORK1( NM )
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
      INTEGER     ISPI
      INTEGER     ISPJ
C
      INTEGER     IEVA
      INTEGER     IEVB
C
      REAL*8      DNRM2
      INTEGER     INCX
      PARAMETER ( INCX = 1 )
C
      OSOLVE = .FALSE.
      IERR   = 0
      IF ( ILSPI.EQ.ILSPJ .OR. ILSPI.LT.1 .OR. ILSPI.GT.NLSP .OR.
     1     ILSPJ.LT.1     .OR. ILSPJ.GT.NLSP ) THEN
        WRITE (6,*) 'Error in routine OSSPPS )'
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
        WRITE (6,*) 'Error in routine OSSPPS )'
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
              IF ( ND.GE.LDA ) THEN
                WRITE (6,*) 'Error in routine OSSPPS )'
                WRITE (6,*) 'ND     = ', ND
                WRITE (6,*) 'LDA    = ', LDA 
                WRITE (6,*) 'LDA about to be exceeded.'
                IERR   = 1
                RETURN
              ENDIF
              ND             = ND + 1
              DDVEC( ND )    = DTIB - DTJB - DTIA + DTJA
              DAMAT( ND, 1 ) = DRXB - DRXA
              DAMAT( ND, 2 ) = DRYB - DRYA
              DAMAT( ND, 3 ) = 1.0d0
              DWEIG( ND )    = 0.5d0*( DABS(DCCI)+DABS(DCCJ) ) + 0.01d0
              IOBS( ND )     = IRELVL
              JOBS( ND )     = JRELVL
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C     .
C     . At this point we need to return if we have
C     . fewer than 4 observations - since we have 3 unknowns.
C     .
      IF ( ND.LT.4 ) THEN
        WRITE (6,*) 'ND     = ', ND,' for '
        WRITE (6,*) 'ILSPI  = ', ILSPI
        WRITE (6,*) 'ILSPJ  = ', ILSPJ
        RETURN
      ENDIF
C     .
C  ( Ax, Ay, 1 ) * ( sxj-sxi , syj-syi , C )^T = ( ti,b - tj,b) - (ti,a - tj,a )
C  where our matrix elements Ax and Ay are given by
C
C  Ax = (rxb-rxa)
C  Ay = (ryb-rya)
C  Now we solve the linear system
C
      CALL IRWMPS( IERR, LDA, ND, NM, NMMAX,
     1             DDVEC, DAMAT, DWEIG, DMVEC, DRESV,
     2             LWORK, LWOPT, DWORK1, DWORK2, DWORK3,
     3             IWORK1,
     4             MXITER, DTOL, DTEMP1, DOLDRV )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'Error in routine OSSPPS from IRWMPS'
        WRITE (6,*) 'IERR  = ', IERR
        IERR   = 1
        RETURN
      ENDIF
C
      DXBMXA = DMVEC( 1 )
      DYBMYA = DMVEC( 2 )
      OSOLVE = .TRUE.
      DRESVN = DNRM2( ND, DRESV, INCX )
C
      RETURN
      END
C
