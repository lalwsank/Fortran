module general					!General Module to be used everywhere
	real,allocatable::x(:)		!list position of n particles
	integer::n					!no. of particles
	real::xcut,l				!cut of potential, length of box
end module

program LJ_fluid
	use general
	implicit none
	integer::i,j,k,nmc,m
	real::dx, r, r1, r2, du, del, xtest, w
	real::T, a, u, uold, unew, kb, utot, s

!kb = boltzman constant, T = temperature, a = particle diameter
!nmc = no. of MC cycles, m = cycles to be excluded

	n = 20;		l = 40;		nmc = 5000;	kb = 1;     s = 0
	T = 0.3;	xcut = 3;	a = 0.95;	del = 0.3;  m = 3000
	open(1,file="result.dat")

	allocate(x(n))				!Allocating Latice points
	do i = 1,n					!Initializing Latice
		x(i) = (i-1)*a
	enddo

	do i = 1, nmc
	    do k = 1,n
    		call random_number(r1)
    		call random_number(r2)
    		j = int(n*r1) + 1	!selecting random latice point to change distance
    		dx = del*(2*r2 - 1)	!random change in distance
    		xtest = x(j) + dx
    		if (xtest > l) then	!Boundary condition upper bound
    			xtest = xtest - l
    		elseif (xtest < 0) then	!Boundary condition lower bound
    			xtest = xtest + l
    		endif
    		call energy(j,x(j),u); uold = u		!Calculating energy of x(j)
    		call energy(j,xtest,u); unew = u	!Calculating energy of x(j) + dx
    		if (unew < uold) then				!Accepting condition
    			x(j) = xtest
    		else
    			du = unew - uold				!Metropolis condition
    			w = exp(-du/(kb*T))
    			call random_number(r)
    			if (r <= w) then
    				x(j) = xtest
    			endif
    		endif
        enddo
		call totalenergy(utot)	!Calculating energy with new latice distances
		write(1,*) utot

		if (i > m) then							!Excluding m cycles
    		s = s + utot
	    endif
	enddo
    write(*,*)"Average Value of Energy is:", s/(nmc-m)
end program

! LJ Potential [V = 4*epsilon*((sigma/r)^12 - (sigma/r)^6)] for every particle

! Calculates energy of j^th particle w.r.t. all other particles
subroutine energy(j,xj,u)
	use general
	implicit none
	integer::i,j
	real::d,u,xj
	u = 0.0
	do i = 1,n
		if(i == j) cycle
		d = xj - x(i)
		if(d > l/2.0) then		!Boundary condition upper bound
			d = d - l
		elseif(d < -l/2.0) then	!Boundary condition lower bound
			d = d + l
		endif

		if(abs(d) < xcut) then	!Boundary condition w.r.t. cut off potential
			u = u + 4 * ( (1/d**12) - (1/d**6) )
		endif
	enddo
	return
end subroutine

!Calculate energy of all particle w.r.t. all other particles
subroutine totalenergy(u)
	use general
	implicit none
	integer::i,j
	real::d,u
	u = 0.0
	do i = 1,n-1
		do j = i+1, n
			d = x(i) - x(j)
			if(d > l/2.0) then		!Boundary condition upper bound
				d = d - l
			elseif(d < -l/2.0) then	!Boundary condition lower bound
				d = d + l
			endif

			if(abs(d) < xcut) then	!Boundary condition w.r.t. cut off potential
				u = u + 4 * ( (1/d**12) - (1/d**6) )
			endif
		enddo
	enddo
	return
end subroutine

!OUTPUT
!Average Value of Energy is:  -13.2113447 (a = 0.95)
!Average Value of Energy is:  -14.7483358 (a = 1.12)
