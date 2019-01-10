!  subroutine  newton(n,x,y,m,t,ty)
!  Input  parameters  :
!       1.  n  节点个数
!       2.  x 节点自变量值 N维向量
!       3.  y 节点因变量值  N维向量
!       4.  m  要计算点的个数
!       5.  t  要计算的点   M向量
!  Output parameters  :
!       1.  ty  计算结果，为M维向量
    
program main
    implicit none
    integer,parameter::n=6,m=1
    real*8:: x(n),y(n),t(m),ty(m)
    x(1)=0.01d0
    y(1)=56.0877d0
    x(2)=0.02d0
    y(2)=27.6709d0
    x(3)=0.03d0
    y(3)=18.1869d0
    x(4)=0.04d0
    y(4)=13.4381d0
    x(5)=0.05d0
    y(5)=10.5827d0
    x(6)=0.06d0
    y(6)=8.6751d0
    write(*,*)'enter x'
    read(*,*)t(1)
    call newton(n,x,y,m,t,ty)
    write(*,*)ty(1)
end program