    !test for two nodes
    program main
    use mpi
    implicit none
    real::zzz,z,r1,r2,energy,rho,phi,pz,prho,pphi,y(6)
    integer::i,myid,numprocs,rc,ierr,error,nnkk
    character(len=512)::tfile
    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD,myid,ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD,numprocs,ierr)
    write(*,*)'myid,numprocs'
    write(*,*)myid,numprocs
    open(1,file='/home/chenjing_pkuhpc/lustre2/chenshi/H2/perp/data/read/etwo.dat',access='direct',form='formatted',recl=12,status='old')
    write(tfile,*)myid
    tfile='/home/chenjing_pkuhpc/lustre2/chenshi/H2/test/write/'//Trim(AdjustL(tfile))//'.dat'
    open(2,file=tfile,status='replace')
    zzz=1.0d0
    nnkk=myid+1                        !!!!!!!!!!!
    write(*,*)'nnkk,myid,numprocs'
    write(*,*)nnkk,myid,numprocs
    do i=1,10000                !262:continue
        nnkk=nnkk+numprocs
        read(1,fmt='(F11.9)',rec=6*nnkk-5,iostat=error)z
        read(1,fmt='(F11.9)',rec=6*nnkk-4,iostat=error)rho
        read(1,fmt='(F11.9)',rec=6*nnkk-3,iostat=error)phi
        read(1,fmt='(F11.9)',rec=6*nnkk-2,iostat=error)pz
        read(1,fmt='(F11.9)',rec=6*nnkk-1,iostat=error)prho
        read(1,fmt='(F11.9)',rec=6*nnkk,iostat=error)pphi
        y(1)=rho*cos(phi)
        y(2)=rho*sin(phi)
        y(3)=z
        y(4)=prho*cos(pphi)
        y(5)=prho*sin(pphi)
        y(6)=pz
        r1=sqrt(y(1)**2+y(2)**2+(y(3)-1)**2)
        r2=sqrt(y(1)**2+y(2)**2+(y(3)+1)**2)
        energy=-zzz/r1-zzz/r2+(y(4)**2+y(5)**2+y(6)**2)/2
        write(*,*)'energy'
        write(*,*)energy
        write(2,*)energy,myid
    end do
    close(1)
    close(2)
    call MPI_FINALIZE(ierr)
    end
