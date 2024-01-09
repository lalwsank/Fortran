program inverse_transform
    integer::i,n
	real::r,x
	open(1,file="result.dat",status="unknown")
	write(*,*)"Enter Number of Sampling:"
    read(*,*)n
!                               -
!p(x) = 2*(1-x) ... 0 <= x <= 1  | ... Probability Distribution function
!     = 0       ... otherwise    |
!                               -
!
!
!f(x) = y = Integral[p(x),0,1]  ... Cumulative Distribution function
!
!y = 2*(x - x^2/2)  ... 0 <= x <= 1
!
!
!f^-1(y) = x  ... non uniform random number
!
!x = 1 +/- sqrt(1-y)

    do i = 1,n
        call random_number(r)
        x = 1 - (1-r)**0.5
        write(1,*)x
    end do


end program
