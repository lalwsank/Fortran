Program Least_sqr_fit
IMPLICIT NONE

real:: x(100),y(100),sumx,sumy,sumxx,sumxy,a,b
integer::n,i

sumx=0
sumy=0
sumxx=0
sumxy=0

WRITE(*,*)"Enter Number of Observations"
read(*,*)n
WRITE(*,*)"Enter the Values of x and corresponding y:"
do i=1,n
    read(*,*)x(i),y(i)
enddo

open(1,file="Least_sqr_fit.dat",status = "unknown")

do i=1,n
    write(1,*)x(i),y(i)
enddo

do i = 1,n
    sumx = sumx + x(i)
    sumy = sumy + y(i)
    sumxx = sumxx + x(i)**2
    sumxy = sumxy + x(i)*y(i)
enddo

a = (sumy*sumxx-sumx*sumxy)/(n*sumxx-sumx**2)
b = (sumx*sumy-n*sumxy)/(sumx**2-n*sumxx)

write(*,*)"Hence, the equation of best fit line is:","y =",b,"x","+ ",a

End Program Least_sqr_fit