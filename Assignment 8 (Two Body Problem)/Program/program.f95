program two_body_problem
	implicit none
	real::G,Ma,Mb,gravconst,t,tmax,dt	!Constants
	real::xa,xb,ya,yb		            !positions
	real::vxa,vxb,vya,vyb	            !velocities
	real::rx,ry,modr3,fx,fy

	open(1,file="body1.dat");	open(2,file="body2.dat")

	G = 1; Ma = 1; Mb = 1; gravconst = G*Ma*Mb
	tmax = 2.5; dt = 0.001; t = 0
	!particle a's initial position and velocity
	xa = 0.5; ya = 0.0; 	vxa = 0; vya = 0.5

	!particle b's initial position and velocity
	xb = -0.5; yb = 0.0;	vxb = 0.0; vyb = -0.5

	do while(t <= tmax)
		rx = xa - xb;	ry = ya - yb
		modr3 = (rx**2.0 + ry**2.0)**1.5

		fx = -gravconst*rx/modr3;	fy = -gravconst*ry/modr3

		vxa = vxa + fx*dt/Ma;	vya = vya + fy*dt/Ma
		vxb = vxb - fx*dt/Mb;	vyb = vyb - fy*dt/Mb

		xa = xa + vxa*dt;	ya = ya + vya*dt
		xb = xb + vxb*dt;	yb = yb + vyb*dt

		t = t + dt

		write(1,*)xa,ya; 	write(2,*)xb,yb

	enddo

end program
