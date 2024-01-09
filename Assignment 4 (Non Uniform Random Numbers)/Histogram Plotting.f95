program histogram

implicit none
Real::xmin,xmax,delx,x,k
Real,allocatable,dimension(:)::A,B
integer::n,i,j,bin

write(*,*)"Enter No. of Observations:"
read(*,*)n
allocate(A(n)); A = 0
write(*,*)"Enter the Value of Delta x: "
read(*,*)delx
write(*,*)"Enter the Minimum Value of x : "
read(*,*)xmin
write(*,*)"Enter the Maximum Value of x : "
read(*,*)xmax
bin = (xmax-xmin)/delx
allocate(B(bin+1)); B = 0

open(1,file="result.dat", status="unknown")
do i = 1,n
    read(1,*)A(i)
enddo

do i = 1,n
	j = (A(i)-xmin)/delx + 1
    if(j > bin+1) then
		B(bin+1) = B(bin+1) + 1
    endif
    B(j) = B(j) + 1
enddo
B = B/n/delx
open(2,file="histogram_data.dat", status="unknown")
do i = 1,bin+1
    write(2,*)xmin + delx/2 + (i-1)*delx, B(i)
enddo
write(*,*)"Total Probability (must be 1) is:",sum(B)
end program histogram
