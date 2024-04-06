C
C Circle Vector intersect.
C
C  We have a circle centered at (DXC,DYC) with radius DRC.
C  We have a location (DX0,DY0) that must lie inside the circle
C  to start with. (if it is outside the circle we return with
C  an error!)
C
C  We provide a second location (DX1,DY1) that does not necessary
C  lie inside the circle. If it does lie inside the circle then 
C  we just return the values DX1 and DY1,
C  If it lies outside the circle then we draw a line between (DX0,DY0)
C  and (DX1,DY1) and we return a point just inside the circle along the
C  line.
C
C  This is coded up as described on
C
C https://math.stackexchange.com/questions/311921/get-location-of-vector-circle-intersection
C
C Input:
C
C  DXC - x-coordinate of circle center
C  DYC - y-coordinate of circle center
C  DRC - radius of circle fixed on (DXC,DYX)
C
C  DX0 - x-coordinate of starting point of line
C  DY0 - y-coordinate of starting point of line
C
C  DX1 - x-coordinate of ending point of line
C  DY1 - y-coordinate of ending point of line
C
C Output:
C
C  DX1 - unchanged if (DX1,DY1) is inside the circle
C        replaced with (almost) the furthest point along the line
C        that is still inside the circle
C  DY1 - unchanged if (DX1,DY1) is inside the circle
C        replaced with (almost) the furthest point along the line
C        that is still inside the circle
C
C  IERR - 0 for successful exit
C  IERR - 1 if DRC is less than a very small tolerance or
C         (DX0,DY0) lies outside the circle.
C         or if something goes wrong with the formular (should not happen!)
C
      SUBROUTINE CIRCVC( IERR, DXC, DYC, DRC, DX0, DY0, DX1, DY1 )
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
      REAL*8      DISTX
      REAL*8      DISTY
      REAL*8      DISTR
C
      REAL*8      DA
      REAL*8      DB
      REAL*8      DC
C
      REAL*8      DNUMER
      REAL*8      DDENOM
      REAL*8      DBB4AC
C
      REAL*8      DX1NEW
      REAL*8      DY1NEW
C
      REAL*8      DT
      REAL*8      DTREQ
C
      REAL*8      DTOL
      PARAMETER ( DTOL = 1.0d-7 )
C
      IERR   = 0
      IF ( DRC.LT.DTOL ) THEN
        WRITE (6,*) 'Subroutine CIRCVC'
        WRITE (6,*) 'DRC = ', DRC
        IERR  = 1
        RETURN
      ENDIF
C
      DISTX  = DX0 - DXC
      DISTY  = DY0 - DYC
      DISTR  = DSQRT( DISTX*DISTX + DISTY*DISTY )
      IF ( DISTR.GT.DRC ) THEN
        WRITE (6,*) 'Subroutine CIRCVC'
        WRITE (6,*) '(DX0,DY0) outside of circle'
        WRITE (6,*) 'DXC     = ', DXC
        WRITE (6,*) 'DYC     = ', DYC
        WRITE (6,*) 'DRC     = ', DRC
        WRITE (6,*) 'DX0     = ', DX0
        WRITE (6,*) 'DY0     = ', DY0
        WRITE (6,*) 'DISTR   = ', DISTR
        IERR  = 1
        RETURN
      ENDIF
C
C A quick exit if (DX1,DY1) is inside circle.
C
      DISTX  = DX1 - DXC
      DISTY  = DY1 - DYC
      DISTR  = DSQRT( DISTX*DISTX + DISTY*DISTY )
      IF ( DISTR.LE.DRC ) RETURN
C
C OK. So (DX1,DY1) is outside of the circle.
C So we need to parametrize the line with
C
C   dx(t) = (DX1-DX0).t + DX0
C   dy(t) = (DY1-DY0).t + DY0
C
C See https://en.wikipedia.org/wiki/Quadratic_equation#Alternative_quadratic_formula
C assuming that (DX1,DY1) is inside the circle and (DX0,DY0) is outside
c     DA     = (DX1-DX0)**2 + (DY1-DY0)**2
c     DB     = 2.0d0*(DX1-DX0)*(DX0-DXC) +
c    1          2.0d0*(DY1-DY0)*(DY0-DYC)
c     DC     = (DX0-DXC)**2 + (DY0-DYC)**2 - DRC*DRC
C
C assuming that (DX1,DY1) is inside the circle and (DX0,DY0) is outside
      DA     = (DX0-DX1)**2 + (DY0-DY1)**2
      DB     = 2.0d0*(DX0-DX1)*(DX1-DXC) +
     1          2.0d0*(DY0-DY1)*(DY1-DYC)
      DC     = (DX1-DXC)**2 + (DY1-DYC)**2 - DRC*DRC
C
      DBB4AC = DB*DB - 4.0d0*DA*DC
      IF ( DBB4AC.LE.0.0d0 ) THEN
        WRITE (6,*) 'Subroutine CIRCVC'
        WRITE (6,*) 'DBB4AC = ', DBB4AC
        WRITE (6,*) 'This should be positive!'
        IERR   = 1
        RETURN
      ENDIF
C
      DNUMER = 2.0d0*DC
      DDENOM = DSQRT( DBB4AC ) - DB
      IF ( DABS( DDENOM ).LT.DTOL ) THEN
        WRITE (6,*) 'Subroutine CIRCVC'
        WRITE (6,*) 'Division by zero imminent.'
        IERR   = 1
        RETURN
      ENDIF
C
      DT     = DNUMER/DDENOM
      IF ( DT.LT.0.0d0 .OR. DT.GT.1.0d0 ) THEN
        WRITE (6,*) 'Subroutine CIRCVC'
        WRITE (6,*) 'DT = ', DT
        WRITE (6,*) 'It should be between zero and unity!'
        IERR   = 1
        RETURN
      ENDIF
C
c     print *,' dt = ', dt
      DTREQ  = DT*1.01d0
      DX1NEW = (DX0-DX1)*DTREQ + DX1
      DY1NEW = (DY0-DY1)*DTREQ + DY1
      DX1    = DX1NEW
      DY1    = DY1NEW
C
      RETURN
      END
C
