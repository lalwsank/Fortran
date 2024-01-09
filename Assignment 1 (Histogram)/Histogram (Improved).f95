program histogram
implicit none
Real::xmin,xmax,delx,x				!Real Variables
Real,allocatable,dimension(:)::A,B	!Real Arrays
integer::n,i,j,bin					!Integer Variables

write(*,*)"Enter No. of Observations:"
read(*,*)n
allocate(A(n)); A = 0	!	Allocating Dimension to Input Array
write(*,*)"Enter the Value of Delta x: "
read(*,*)delx
write(*,*)"Enter the Minimum Value of x : "
read(*,*)xmin
write(*,*)"Enter the Maximum Value of x : "
read(*,*)xmax
bin = (xmax-xmin)/delx	!	No. of Class Marks
allocate(B(bin+1)); B=0	!	Allocating Dimension to results
!	Opening and Reading The Dataset
open(1,file="assign01_input.dat", status="unknown")
do i = 1,n
    read(1,*)A(i)
enddo
!	Counting Numbers to their respective Class marks
do i = 1,n
!	Eg. if delx = 2 and xmin = 0 the 3 belongs to [int(3/2)+1]st = 2nd Bin
    j = (A(i)-xmin)/delx + 1
!	Boundary Condition where j > Dimension of Result Array
    if(j > bin+1) then
		B(bin+1) = B(bin+1) + 1
    endif
    B(j) = B(j)+1
enddo
B=B/n	!Normalization of Result Array
!		Opening Result Dataset File
open(2,file="histogram_data.dat", status="unknown")

!	Printing Result in the Result File
write(2,*) xmin-delx/2 , 0
do i = 1,bin+1
    write(2,*) xmin + delx/2 + (i-1)*delx, B(i)
enddo
write(2,*) xmax+delx/2 , 0
write(*,*)"Total Probability (must be 1) is:",sum(B)
end program histogram

!OUTPUTS
! Enter No. of Observations:
!100000
! Enter the Value of Delta x:
!0.1
! Enter the Minimum Value of x :
!-1
! Enter the Maximum Value of x :
!1

! Process returned 0 (0x0) execution time : 12.030 s
! Press any key to continue.
