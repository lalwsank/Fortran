Program box_muller_method
    real::mean,std,r1,r2
    integer::i,n
    write(*,*)"Enter The Number of Samples:"
    read(*,*)n
    write(*,*)"Enter The mean of Distribution:"
    read(*,*)mean
    write(*,*)"Enter The Std. Deviation of Distribution:"
    read(*,*)std
    open(1,file = "result.dat",status="unknown")


    do i = 1,n
        call random_number(r1)
        call random_number(r2)
        R = sqrt(-2*log(r1))
        theta = 2*(4*atan(1.0))*r2
		x = mean + (R*cos(theta))*std
		y = mean + (R*sin(theta))*std
        write(1,*) x
	enddo

end Program
