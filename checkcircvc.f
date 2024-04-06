C
      PROGRAM checkcircvc
      IMPLICIT NONE
C
      INTEGER     IERR
      REAL*8      DXC
      REAL*8      DYC
      REAL*8      DRC
      REAL*8      DX0
      REAL*8      DY0
      REAL*8      DX1
      REAL*8      DY1
C
      DXC    =  1.0d0
      DYC    =  0.0d0
      DRC    = 1.0d0
      DX0    = 0.9d0
      DY0    = 0.0d0
      DX1    = -3.0d0
      DY1    = 0.0d0
C
      CALL CIRCVC( IERR, DXC, DYC, DRC, DX0, DY0, DX1, DY1 )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'Error from CIRCVC'
        CALL EXIT(1)
      ENDIF
C
      PRINT *,' DX1,DY1 = ', DX1, DY1
      CALL EXIT(0)
      END
C
