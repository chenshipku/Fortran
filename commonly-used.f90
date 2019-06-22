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

program main
  implicit none
  real*8::freq,t,bigt,E0,Et,phi,pi,gamma,ip1,kappa1,zc1,nstar,e,cnl,vp,wadk,wvp
  integer::n
  open(10,file='field.dat')
  pi=3.1415926535897932384626d0
  E0=0.0535d0
  freq=0.05695d0
  bigt=2d0*pi/freq
  n=3
  phi=0.5*pi
  ip1=0.44576d0   !Xe: 12.129874eV
  kappa1=dsqrt(2d0*ip1)
  zc1=1d0
  nstar=zc1/kappa1
  e=2.7182818284d0
  cnl=(2d0*e/nstar)**nstar/dsqrt(2d0*pi*nstar)
  vp=0d0
  t=0d0
  do while(t<3d0*bigt)
    Et=-E0*dsin((freq*t)/(2d0*real(n)))**2d0*dsin(freq*t+phi)
    wadk=cnl**2d0*ip1*(2d0*kappa1**3d0/dabs(Et))**(2d0*nstar-1)*dexp(-2d0*kappa1**3d0/(3d0*dabs(Et)))
    wvp=(vp*dsqrt(2d0*ip1)/dabs(Et)/pi)*dexp(-dsqrt(2d0*ip1)*vp**2d0/dabs(Et))
    write(10,*)t/bigt,Et,wadk
    t=t+1
  end do
end

