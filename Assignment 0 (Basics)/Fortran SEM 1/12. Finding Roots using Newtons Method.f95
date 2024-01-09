Program newton
implicit none
Real::x0,x1,x,error,i,f,fd

f(x) = x**2 - 2*x +1
fd(x) = 2*x - 2
write(*,*)"Enter Initial value of x (NOT 0)"
read(*,*)x0
write(*,*)"Enter acceptable Error "
read(*,*)error
i=0
do 10 while(ABS(x1-x0).GT.error)
x0 = x1
x1 = x0 - f(x0)/fd(x0)
i = i + 1
write(*,*)"Iteration Number:",i,"value of x1=",x1
10 CONTINUE
write(*,*)"Root of function is:",x1
STOP

end Program newton