program sample_mean
    implicit none
    integer::n,i,j
    real::x1,x2,y1,y2,x,y,f,area,er
    open(1,file="result2.dat",status="unknown")

    write(*,*)"Enter lower and Upper x limits respectively:"
    read(*,*)x1,x2
    write(*,*)"Enter lower and Upper y limits respectively:"
    read(*,*)y1,y2
    n = 5
    do j = 1,15
        y = 0
        do i = 1,n
            x = (x2-x1)*rand() + x1
            y = y + f(x)
		end do
        area = (x2-x1)*(y/n)
        er = abs((1.5-area)*100/1.5)
        write(1,*)n,area,er,"%"
        n = n*2
    enddo
end program

real function f(x)
    implicit none
    real::x,y
    if(x <= 1)then
        y = 1
    elseif(x > 1) then
        y = -x + 2
    end if
    f = y
end function

!OUTPUT
! Enter lower and Upper x limits respectively:
!0
!2
! Enter lower and Upper y limits respectively:
!0
!1
!
!Process returned 0 (0x0)   execution time : 6.469 s
!Press any key to continue.
