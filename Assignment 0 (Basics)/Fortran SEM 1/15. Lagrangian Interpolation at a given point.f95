program lagrangian_interpolation
implicit none

real::sum,xg,l
integer::n,i,j,k
real,dimension(:),allocatable::x,y
write(*,*)"Please Enter the Number of dimensions/No. of Data Points:"
read(*,*)n
allocate(x(n))
allocate(y(n))


write(*,*)"Enter the values of x and its corresponding y:"
do i=1,n
	read(*,*)x(i),y(i)
enddo

write(*,*)"Enter the values of xg at which y is to be interpolated:"
read(*,*)xg

sum = 0

do j=1,n
l=1	
do k=1,n
	if(k .ne. j) then
	l=l*(xg-x(k))/(x(j)-x(k))
    endif
enddo
sum = sum + l*y(j)
enddo

write(*,*)"The interpolated value at",xg,"is",sum



end program lagrangian_interpolation