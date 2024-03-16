C
C Number of "Live" station/phase and events find.
C OK. We read in NEV events with initial coordinates.
C We also read in NSP station and phase combinations with
C  corresponging slowness vectors.
C We then read in a load of lines containing 
C EV1 EV2 time1 time2 STAT PHAS qval
C
C We now want to know which events and station/phase combinations
C are "live" i.e. represented by a sensible number of observations.
C
C This is to make the whole program more robust to suddenly eliminating
C sets of observations. It is easier to just remove the observations
C and leave the lists of slowness vectors/events unchanged.
C
C We inherit the values NEV and NSP
C together with NRELVL integers
C
C   IE1ARR( NRELVL ) - index of event1
C   IE2ARR( NRELVL ) - index of event1
C   ISPARR( NRELVL ) - index of station/phase pair
C
C And we fill the arrays
C
C   NUMEVO( NEV ) - number of event observations 
C   NUMSPO( NSP ) - number of station/phase observations 
C
C Then we calculate NLEV and NLSP (the numbers of "live" events and
C station/phase combinations) and fill the arrays
C
C  INDLEV( NLEV ) the index of each of the live events
C  INDLSP( NLSP ) the index of each of the live station/phases
C
C Let's say for now that we need at least 3 observations of
C either event or station/phase to make it "live"
C
      SUBROUTINE NLSPEF( IERR, NEV, NSP, NRELVL,
     1                   IE1ARR, IE2ARR, ISPARR, NUMEVO, NUMSPO,
     2                   NLEV, NLSP, INDLEV, INDLSP )
      IMPLICIT NONE
C . inputs
      INTEGER            IERR
      INTEGER            NEV
      INTEGER            NSP
      INTEGER            NRELVL
      INTEGER            IE1ARR( NRELVL )
      INTEGER            IE2ARR( NRELVL )
      INTEGER            ISPARR( NRELVL )
C . outputs
      INTEGER            NUMEVO( NEV )
      INTEGER            NUMSPO( NSP )
      INTEGER            NLEV
      INTEGER            NLSP
      INTEGER            INDLEV( NEV )
      INTEGER            INDLSP( NSP )
C . working variables
      INTEGER            IEV
      INTEGER            ISP
      INTEGER            IRELVL
      INTEGER            NOCMIN
      PARAMETER        ( NOCMIN = 3 )
C
      IERR   = 0
C
C Initialize the arrays NUMEVO, NUMSPO
C
      DO IEV = 1, NEV
        NUMEVO( IEV ) = 0
      ENDDO
C
      DO ISP = 1, NSP
        NUMSPO( ISP ) = 0
      ENDDO
C
C Now accumulate all the times that a given event or
C station/phase pair is referred to.
C
      DO IRELVL = 1, NRELVL
        IEV           = IE1ARR( IRELVL )
        NUMEVO( IEV ) = NUMEVO( IEV ) + 1
        IEV           = IE2ARR( IRELVL )
        NUMEVO( IEV ) = NUMEVO( IEV ) + 1
        ISP           = ISPARR( IRELVL )
        NUMSPO( ISP ) = NUMSPO( ISP ) + 1
      ENDDO
C
C First find the number of live events and fill INDLEV
C
      NLEV   = 0
      DO IEV = 1, NEV
        IF ( NUMEVO( IEV ).GT.NOCMIN ) THEN
          NLEV           = NLEV + 1
          INDLEV( NLEV ) = IEV
        ENDIF
      ENDDO
C
C Now find the number of live station/phase pairs and fill INDLSP
C
      NLSP   = 0
      DO ISP = 1, NSP
        IF ( NUMSPO( ISP ).GT.NOCMIN ) THEN
          NLSP           = NLSP + 1
          INDLSP( NLSP ) = ISP
        ENDIF
      ENDDO
C
      RETURN
      END
C

