program a_Random_walk
Implicit None
Real :: x,sqx,var,er						!Declaring Real Numbers
Integer, dimension(:,:),allocatable :: A	!Declaring Allocatable Array
Integer :: n,t,iter,dist,i,j				!Declaring Integers

write(*,*)"Enter the Number of Steps (Integer): "
read(*,*)n
write(*,*)"Enter Number of Iterations (Integer): "
read(*,*)iter
write(*,*)"Enter Number of Iterations to be Printed(Integer): "
read(*,*)j
allocate(A(j,n)) ; A = 0  		!Allocating and Presetting Integer Array
open(1,file="Result.dat",status="unknown")	!Creating Result file
open(2,file="assign02_input.dat",status="unknown")	!Result File

do i = 1,iter					!Using Do loop for given number of itterations
    dist = 0					!Preseting position to 0
    do t = 1,n					!Using do loop for n steps
		!Here int(2*Rand()) output Ranges between int(2*(0 to 1)) = 0 or 1
        dist = dist + (-1)**int(2*Rand())
        !Adding +1/-1 to position each having equal probability
        if(i <= j) then
			A(i,t) = dist		!inputing distance in the 2D Array
        endif
	end do
	!Printing Results in file to run through Histogram Program
    write(2,*)dist
    x = x + dist				!Summing the final positions
	sqx = sqx + dist**2			!Summing the squares of final positions
enddo

er = -(x/iter)*100 !Error % = (Expected - Observed)*100/Expected
write(*,*) "(Expected) Mean: ", 0
write(*,*) "(Observed) Mean: ", x/iter, "Error:", er,"%"
var = sqx/iter - (x/iter)**2; er = (var-n)*100/n  !Variance = <x^2> - <x>^2
write(*,*) "(Expected) Variance = n:", n
write(*,*) "(Observed) Variance = <x^2> - <x>^2:", var, "Error:", er,"%"
er = (var**0.5-n**0.5)*100/n**0.5		!Standard Deviation = Sqrt(Variance)
write(*,*) "(Expected) Standard Deviation = Sqrt(n) :", n**0.5
write(*,*) "(Observed) Standard Deviation = Sqrt(Var):", var**0.5, "Error:", er ,"%"
do t = 1,n
    write(1,*)t, (A(i,t),i=1,j)			!Printing Data in Result file
end do

end program a_Random_walk

!OUTPUTS:
! Enter the Number of Steps (Integer):
!1000
! Enter Number of Iterations (Integer):
!1000000
! Enter Number of Iterations to be Printed(Integer):
!4
! (Expected) Mean: 0
! (Observed) Mean: 1.21600004E-02 Error: -1.21599996 %
! (Expected) Variance = n: 1000
! (Observed) Variance = <x^2> - <x>^2: 999.22988062 Error: -7.7011938E-2 %
! (Expected) Standard Deviation = Sqrt(n) : 31.6227760
! (Observed) Standard Deviation = Sqrt(Var): 31.6105976  Error: -3.85263200E-2 %

! Process returned 0 (0x0)   execution time : 75.430 s
! Press any key to continue.
