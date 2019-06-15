program main
implicit none

integer::myid,error=0,ierr=0
character(len=512)::infile
type maindata
  integer::nt
  real*8::t0,vp,theta,x1,y1,z1,vx1,vy1,vz1,w1,wvp
end type
type(maindata)::n(500000)

do myid=0,23
  write(infile,*)myid
  open(10,file='D:\data\'//Trim(AdjustL(infile))//'.dat',status='replace')
  open(11,file='D:\data\e2.dat',access='direct',form='formatted',recl=16,status='old')
  read(10,*,iostat=ierr)n(j)%nt,n(j)%t0,n(j)%vp,n(j)%theta,n(j)%x1,n(j)%y1,n(j)%z1,n(j)%vx1,n(j)%vy1,n(j)%vz1,n(j)%w1,n(j)%wvp
  read(11,fmt='(F15.9)',rec=6*nt-5,iostat=error)x2
  read(11,fmt='(F15.9)',rec=6*nt-4,iostat=error)y2
  read(11,fmt='(F15.9)',rec=6*nt-3,iostat=error)z2
  read(11,fmt='(F15.9)',rec=6*nt-2,iostat=error)vx2
  read(11,fmt='(F15.9)',rec=6*nt-1,iostat=error)vy2
  read(11,fmt='(F15.9)',rec=6*nt,iostat=error)vz2 
  myid=myid+1
end do

end
