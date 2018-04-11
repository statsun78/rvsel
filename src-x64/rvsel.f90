      SUBROUTINE nexsub(ni,in,ncard,j)
      IMPLICIT NONE
      INTEGER ni,j,ncard,in(ni)     
      j=1
      IF (MODULO(ncard, 2) == 0) GO TO 10030
10040 j=j+1
      IF (in(j-1) == 0) GO TO 10040
      IF (j > ni) j=ni
10030 in(j)=1-in(j)
      ncard=ncard+2*in(j)-1
      END SUBROUTINE nexsub

      SUBROUTINE maxcorr(no,ni,x,y,re,lq,wts,zs,out,jerr)
      IMPLICIT NONE
      INTEGER jerr,ierr,j,i,lq,no,ni,ncard,re(ni)
      DOUBLE PRECISION x(no,ni),y(no),wts(ni),out(ni)
      DOUBLE PRECISION zs, zss
      DOUBLE PRECISION, EXTERNAL :: scorr      
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: wx      
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: nx            
      ALLOCATE(wx(ni), STAT=jerr)
      IF (jerr /= 0) RETURN      
      ALLOCATE(nx(no), STAT=ierr)
      jerr=jerr+ierr
      IF (jerr /= 0) RETURN
      ncard=1.0
      j=1
      DO 10777 i=1,lq
      wx(1:ni)=re(1:ni)*wts(1:ni)
      nx(1:no)=MATMUL(x,wx)
      zss=0.0
      zss=scorr(nx,y,no)
      IF (abs(zss) <= abs(zs)) GO TO 10778
      zs=zss
      out(1:ni)=re(1:ni)
10778 CONTINUE
      CALL nexsub(ni,re,ncard,j)
10777 CONTINUE
      DEALLOCATE(wx, nx, STAT=jerr)
      END SUBROUTINE maxcorr
 
      SUBROUTINE maxcorr2(no,ni,x,y,re,lq,zs,out,jerr)
      IMPLICIT NONE
      INTEGER jerr,j,i,lq,no,ni,k,ncard,re(ni)
      DOUBLE PRECISION x(no,ni),y(no),out(ni)
      DOUBLE PRECISION zs, zss
      DOUBLE PRECISION, EXTERNAL :: scorr      
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: nx      
      ALLOCATE(nx(no), STAT=jerr)
      IF (jerr /= 0) RETURN
      ncard=1
      j=1
      DO 20777 i=1,lq
      nx(1:no)=MATMUL(x, re)
      DO 20799 k=1,no
      IF (nx(k)==0.0) GO TO 20791
      nx(k)=1.0
20791 CONTINUE
20799 CONTINUE     
      zss=0.0
      zss=scorr(nx,y,no)
      IF (abs(zss) <= abs(zs)) GO TO 20778
      zs=zss
      out(1:ni)=re(1:ni)
20778 CONTINUE
      CALL nexsub(ni,re,ncard,j)
20777 CONTINUE
      DEALLOCATE(nx, STAT=jerr)
      END SUBROUTINE maxcorr2

      FUNCTION scorr(sx,y,no)             
      IMPLICIT NONE
      INTEGER no
      DOUBLE PRECISION sx(no),y(no)
      DOUBLE PRECISION dx,ddx,scorr
      DOUBLE PRECISION, ALLOCATABLE :: nx(:)
      ALLOCATE(nx(no))
      nx=sx-SUM(sx)/no
      dx=DOT_PRODUCT(y,nx)
      ddx=DOT_PRODUCT(y,y)*DOT_PRODUCT(nx,nx)
      scorr=dx/SQRT(ddx)
      DEALLOCATE(nx)
      END FUNCTION scorr
