C
C  Steve Gibbons
C  2024-04-06
C
C  ANSVWO
C
C  Align New Slowness Vectors With Old.
C
C  Our previous set of slowness vectors are in
C  the arrays DSXARR( NSP ) and DSYARR( NSP )
C
C  In the arrays DABSSX( NLSP ) and DABSSY( NLSP ) we
C  have new values of Sx and Sy solved for by ABSSVF.
C
C  The ISP indices of the NLSP live slowness vectors
C  are held in the integer array INDLSP.
C
C  However, we do not want the new Sx and Sy to be further
C  than DSLOWM from the origin Sx and Sy, so we allow them
C  to move in the direction solved for but only as far
C  as DSLOWM from sx0 and sy0.
C
C  We output updated DABSSX and DABSSY - so will need to
C  refill DSXARR and DSYARR outside of this routine.
C
C  We however only move those points that are less than
C  DSLOWM away from their original locations in
C  DSXAR0 and DSYAR0
C
      SUBROUTINE ANSVWO( IERR, NSP, NLSP, INDLSP,
     1                   DSXARR, DSYARR, DABSSX, DABSSY,
     2                   DSXAR0, DSYAR0, DSLOWM )
      IMPLICIT NONE
C
      INTEGER            IERR
      INTEGER            NSP
      INTEGER            NLSP
      INTEGER            INDLSP( NLSP )
      REAL*8             DSXARR( NSP )
      REAL*8             DSYARR( NSP )
      REAL*8             DABSSX( NLSP )
      REAL*8             DABSSY( NLSP )
      REAL*8             DSXAR0( NSP )
      REAL*8             DSYAR0( NSP )
      REAL*8             DSLOWM
C
      REAL*8      DXC
      REAL*8      DYC
      REAL*8      DRC
      REAL*8      DX0
      REAL*8      DY0
      REAL*8      DX1
      REAL*8      DY1
C
      INTEGER     ISP
      INTEGER     ILSP
C
      IERR   = 0
C
C First fill up DAMAT, DDVEC, and DWEIG
C
      DO ILSP = 1, NLSP
        ISP              = INDLSP( ILSP )
        DXC              = DSXAR0( ISP )
        DYC              = DSYAR0( ISP )
        DRC              = DSLOWM
        DX0              = DSXARR( ISP )
        DY0              = DSYARR( ISP )
        DX1              = DABSSX( ILSP )
        DY1              = DABSSY( ILSP )
        CALL CIRCVC( IERR, DXC, DYC, DRC, DX0, DY0, DX1, DY1 )
        IF ( IERR.NE.0 ) THEN
          WRITE (6,*) 'Subroutine ANSVWO'
          WRITE (6,*) 'Error from CIRCVC'
          IERR   = 1
        ENDIF
        DABSSX( ILSP )   = DX1
        DABSSY( ILSP )   = DY1
      ENDDO
C
      RETURN
      END
C
