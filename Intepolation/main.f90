!  subroutine  newton(n,x,y,m,t,ty)
!  Input  parameters  :
!       1.  n  �ڵ����
!       2.  x �ڵ��Ա���ֵ Nά����
!       3.  y �ڵ������ֵ  Nά����
!       4.  m  Ҫ�����ĸ���
!       5.  t  Ҫ����ĵ�   M����
!  Output parameters  :
!       1.  ty  ��������ΪMά����
    
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