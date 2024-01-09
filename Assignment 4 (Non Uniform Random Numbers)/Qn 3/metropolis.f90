program metropolis_method
implicit none
integer::i,n,m
real::p,del,deln,r,x,w,integral,Norm

n = 110000; m = 10000
del = 0.3; x = 0
integral = 0

open(1,file="result.dat",status="unknown")
do i = 1,n

	call random_number(deln)
	call random_number(r)
	deln = del*(2*deln - 1)    !Random number between [-del,del]
	w = p(x + deln)/p(x)       !Ratio of P( x + deln ) / P(x)
	if(r <= w) then            !probabilioty of acceptance = w
		x = x + deln
	endif

	if(i >= 10000) then        !Accept data above a certain sample
		write(1,*)x
		Norm = 1/(1-exp(-1.0)) !Normalising Constant for p(x)
		integral = integral + (exp(-x**2)/p(x))/Norm
	endif
enddo
write(*,*)"integral value of f(x)=exp(-x**2) from 0 to 1 is:", integral/(n-m)

end program

real function p(x)
	implicit none
	real::x
	if(x > 1) then
		p = 0
	elseif(x < 0) then
		p = 0
	else
		p = exp(-x)
	endif
end function

!Output
!integral value of f(x)=exp(-x**2) from 0 to 1 is:  0.746906459
