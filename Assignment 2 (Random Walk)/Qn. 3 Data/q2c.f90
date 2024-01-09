program q2c
implicit none

real :: a(1000000),r,x, xbar,x2bar
integer :: nsteps, nwalks,i,j,k

nsteps=1000
nwalks=10**6

open(unit=1,file='output1.txt')

do k=1,5
    xbar=0.0
    x2bar=0.0
    do i=1,nwalks
        x=0.0

        do j=1,nsteps
            call random_number(r)
            if (r<=0.5) then
                x=x-1
            else
                x=x+1
            end if
        end do
        xbar=xbar+x
        x2bar=x2bar+x**2
    end do
    xbar=xbar/nwalks
    x2bar=x2bar/nwalks
    write(1,*) nsteps**0.5, (x2bar-xbar**2)**0.5
    nsteps=2*nsteps
end do



end program q2c
