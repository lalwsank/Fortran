program triple_integral
implicit none

real,allocatable,dimension(:) :: A
real::r,pi,s, mean, sqmean, e
integer :: i,j,n,dimen
dimen = 3
allocate(A(3))
n = 10**6
pi = 4*atan(1.0)
s = 0
sqmean = 0; mean = 0
do i=1,n
    do j = 1,dimen
        call random_number(r)
        r = pi*r
        A(j) = r
    enddo
    s = s + sin(product(A))
    mean = mean + (s/r)
    sqmean = sqmean + ((s/r)**2)
enddo
mean = mean/n; sqmean = sqmean/n

e = (sqmean - mean**2)
e = sqrt(e)
s = (s/n)*pi**3


write(*,*)"I = ", s, "epsilon = ", e/sqrt(n*1.0)

end program

!OUTPUT
! I =    7.64889526     epsilon =    61927.3945

!Process returned 0 (0x0)   execution time : 0.250 s
!Press any key to continue.
