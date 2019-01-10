subroutine  newton(n,x,y,m,t,ty)
!---------------------------------subroutine  comment
!  Version   :  V1.0    
!  Coded by  :  syz 
!  Date      :  
!-----------------------------------------------------
!  Purpose   :  ����������
!               ��ֱ�ӶԶ���ֵ
!-----------------------------------------------------
!  Input  parameters  :
!       1.  n  �ڵ����
!       2.  x �ڵ��Ա���ֵ Nά����
!       3.  y �ڵ������ֵ  Nά����
!       4.  m  Ҫ�����ĸ���
!       5.  t  Ҫ����ĵ�   M����
!  Output parameters  :
!       1.  ty  ��������ΪMά����
!       2.
!  Common parameters  :
!
!----------------------------------------------------
!  Post Script :
!       1.   ע�������N  ��ֱ��ָ�ڵ�ĸ���
!       2.   ���Ӻ���  new�� N ��ָ�ڵ������ 1 
!----------------------------------------------------

implicit real*8(a-z)
integer::n,m

integer::i,j,k

real*8::x(n),y(n),t(m),ty(m)

do i=1,m

call new(n-1,x,y,t(i),ty(i))

end do

end subroutine newton




subroutine new(n,x,y,t,ty)
!---------------------------------subroutine  comment
!  Version   :  V1.0    
!  Coded by  :  syz 
!  Date      :  
!-----------------------------------------------------
!  Purpose   :  �����ֵ����
!               �˺���Ŀ��Ϊ solve������
!-----------------------------------------------------
!  Input  parameters  :
!       1.
!       2.   t ����
!  Output parameters  :
!       1.   
!       2.    ty ����
!  Common parameters  :
!
!----------------------------------------------------
!  Post Script :
!       1.    N �ǽڵ���� �� 1�� ���繲4���ڵ�  ��N=3
!       2.
!----------------------------------------------------
implicit real*8(a-z)
integer::n

integer::i,j,k

real*8::x(0:n),y(0:n)


real*8::b(n+1)
real*8::Q(0:n,0:n)


do i=0,n

Q(i,0)=y(i)

end do


do i=1,n
  
  do j=1,i
   
   Q(i,j)=(Q(i,j-1)-Q(i-1,j-1))/(x(i)-x(i-j))
  
  end do

end do


b(n+1)=Q(n,n)


do k=n,1,-1
  
  b(k)=Q(k-1,k-1)+b(k+1)*(t-x(k-1))

end do

ty=b(1)

end subroutine new
