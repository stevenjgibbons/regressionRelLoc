C
C StG 20240316
C INDLEF
C INDex of Live Event Find
C
C Returns ILEV  - the number of the live event with
C character ID CTARE with length LTARE
C Returns ILEV = 0 if not found ...
C
      SUBROUTINE INDLEF( IERR, ILEV, LTARE, CTARE, NLEV, NEV, NEVMAX,
     1                   INDLEV, CEVARR, LEVARR )
      IMPLICIT NONE
C
      INTEGER        IERR
      INTEGER        ILEV
      INTEGER        LTARE
      CHARACTER*(24) CTARE
      INTEGER        NLEV
      INTEGER        NEV
      INTEGER        NEVMAX
      INTEGER        INDLEV( NEVMAX )
      CHARACTER*(24) CEVARR( NEVMAX )
      INTEGER        LEVARR( NEVMAX )
C
      INTEGER        JLEV
      INTEGER        JEV
C
      IERR   = 0
      ILEV   = 0
      DO JLEV = 1, NLEV
        JEV    = INDLEV( JLEV )
        IF ( JEV.LT.1 .OR. JEV.GT.NEV .OR. JEV.GT.NEVMAX ) THEN
          WRITE (6,*) 'Error from INDLEF: JEV = ', JEV
          WRITE (6,*) 'Error from INDLEF: NEV = ', NEV
          IERR   = 1
          RETURN
        ENDIF
        IF ( LEVARR( JEV ).EQ.LTARE ) THEN
          IF ( CTARE(1:LTARE).EQ.CEVARR( JEV )(1:LTARE) ) ILEV = JLEV
        ENDIF
      ENDDO
C
      RETURN
      END
C
