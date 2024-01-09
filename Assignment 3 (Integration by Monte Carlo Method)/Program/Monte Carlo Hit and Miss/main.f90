program hit_and_miss
    implicit none
    integer::n,ns,i,j
    real::x1,x2,y1,y2,x,y,f,area,er
    open(1,file="result1.dat",status="unknown")

    write(*,*)"Enter lower and Upper x limits respectively:"
    read(*,*)x1,x2
    write(*,*)"Enter lower and Upper y limits respectively:"
    read(*,*)y1,y2
    n = 5
    do j = 1,15
        ns = 0
        do i = 1,n
            x = (x2-x1)*rand() + x1
            y = (y2-y1)*rand() + y1
            if( y <= f(x) ) then
                ns = ns + 1
            end if
        end do
        area=(x2-x1)*(y2-y1)*ns*1.0/n
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
!Process returned 0 (0x0)   execution time : 2.562 s
!Press any key to continue.
