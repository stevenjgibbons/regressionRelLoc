C
C StG 20240316
C INDLSF
C INDex of Live Slowness Vector Find
C (Slowness Vector = station + phase)
C
C Returns ILSP  - the number of the live slowness vector with
C station character ID CTARS with length LTARS and
C phase character ID   CTARP with length LTARP and
C Returns ILSP = 0 if not found ...
C
      SUBROUTINE INDLSF( IERR, ILSP, LTARS, CTARS, LTARP, CTARP,
     1                   NLSP, NSP, NSPMAX,
     1                   INDLSP, CSTARR, LSTARR, CPHARR, LPHARR )
      IMPLICIT NONE
C
      INTEGER       IERR
      INTEGER       ILSP
      INTEGER       LTARS
      CHARACTER*(8) CTARS
      INTEGER       LTARP
      CHARACTER*(8) CTARP
      INTEGER       NLSP
      INTEGER       NSP
      INTEGER       NSPMAX
      INTEGER       INDLSP( NSPMAX )
      CHARACTER*(8) CSTARR( NSPMAX )
      INTEGER       LSTARR( NSPMAX )
      CHARACTER*(8) CPHARR( NSPMAX )
      INTEGER       LPHARR( NSPMAX )
C
      INTEGER       JLSP
      INTEGER       JSP
C
      IERR   = 0
      ILSP   = 0
      DO JLSP = 1, NLSP
        JSP    = INDLSP( JLSP )
        IF ( JSP.LT.1 .OR. JSP.GT.NSP .OR. JSP.GT.NSPMAX ) THEN
          WRITE (6,*) 'Error from INDLSF: JSP = ', JSP
          WRITE (6,*) 'Error from INDLSF: NSP = ', NSP
          IERR   = 1
          RETURN
        ENDIF
        IF ( LSTARR( JSP ).EQ.LTARS .AND. LPHARR( JSP ).EQ.LTARP ) THEN
          IF ( CTARS(1:LTARS).EQ.CSTARR( JSP )(1:LTARS) .AND.
     1         CTARP(1:LTARP).EQ.CPHARR( JSP )(1:LTARP) ) ILSP = JLSP
        ENDIF
      ENDDO
C
      RETURN
      END
C
