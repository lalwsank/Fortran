program ising_model_calculations
	implicit none
	integer::n,nmc,m,i
	real::kb, T, num
    double precision::a, b, c, d, e, mag, e2, mag2
	nmc = 500000;  n = 1000;   m = 200000; kb = 1.0;  T = 0.7
	e = 0;	       e2 = 0;     mag = 0;	   mag2 = 0
	num = real(nmc - m)
	open(1, file = "result.dat", status = "old")
	open(2, file = "Calculation.dat")

    do i = 1,nmc
        read(1,*)a,b,c,d
		if(i > m) then
            e = e + b;      e2 = e2 + b**2.0
			mag = mag + d;  mag2 = mag2 + d**2.0
		endif
		a = 0.0; b = 0.0; c = 0.0; d = 0.0
	enddo
	e = e/num;			mag = mag/num
	e2 = e2/num;		mag2 = mag2/num


	write(2,*)"By Fluctuation Dissipation Theorem"
	write(2,*)"Specific Heat Capacity, Cv = (<E^2> - <E>^2)/(kb*T**2)"
	write(2,*)"Therefore,"
	write(2,*)"Specific Heat Capacity (Cv):", real((e2 - e**2.0)/(kb*T**2.0))
	write(2,*)"Similarly,"
	write(2,*)"Magnetic Susceptibility, χ = (<M^2> - <M>^2)/(kb*T)"
	write(2,*)"Susceptibility (χ) :", real((mag2 - mag**2.0)/(kb*T))

end program


!OUTPUT
! By Fluctuation Dissipation Theorem
! Specific Heat Capacity, Cv = (<E^2> - <E>^2)/(kb*T**2)
! Therefore,
! Specific Heat Capacity (Cv):   4.22062527E-04
! Similarly,
! Magnetic Susceptibility, χ = (<M^2> - <M>^2)/(kb*T)
! Susceptibility (χ) :   2.41711568E-02

