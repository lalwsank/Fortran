real function f(x,y)
    real::x,y
    f = y*y*x
	!y = 2/(2-x**2)
end

Program heun
IMPLICIT NONE
real,external::f
real::x,y,h,c
integer::n,i

write(*,*)"Enter Initial condition of x i.e. x0:"
read(*,*)x
write(*,*)"Enter Initial condition of x i.e. y0:"
read(*,*)y
write(*,*)"Please insert the width of the x (i.e. interval) i.e. h:"
read(*,*)h
write(*,*)"Please insert the number of iterations:"
read(*,*)n

open(2,file="heun.dat",status="unknown")

write(*,*)"Answer using Heun's Method is:"
write(*,*)x,y
do i = 1,n
    y = y + (h/2)*(f(x,y)+f(x+h,y+h*f(x,y)))
    x = x + h
    write(*,*)x,y
    write(2,*)x,y 
ENDDO
write(*,*)"The Final value of x and its y is",x,y

End Program heun