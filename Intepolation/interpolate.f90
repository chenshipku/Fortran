subroutine  newton(n,x,y,m,t,ty)
!---------------------------------subroutine  comment
!  Version   :  V1.0    
!  Coded by  :  syz 
!  Date      :  
!-----------------------------------------------------
!  Purpose   :  方法主函数
!               可直接对多点插值
!-----------------------------------------------------
!  Input  parameters  :
!       1.  n  节点个数
!       2.  x 节点自变量值 N维向量
!       3.  y 节点因变量值  N维向量
!       4.  m  要计算点的个数
!       5.  t  要计算的点   M向量
!  Output parameters  :
!       1.  ty  计算结果，为M维向量
!       2.
!  Common parameters  :
!
!----------------------------------------------------
!  Post Script :
!       1.   注意这里的N  就直接指节点的个数
!       2.   而子函数  new中 N 是指节点个数减 1 
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
!  Purpose   :  单点插值程序
!               此函数目的为 solve所调用
!-----------------------------------------------------
!  Input  parameters  :
!       1.
!       2.   t 标量
!  Output parameters  :
!       1.   
!       2.    ty 标量
!  Common parameters  :
!
!----------------------------------------------------
!  Post Script :
!       1.    N 是节点个数 减 1， 比如共4个节点  则N=3
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
