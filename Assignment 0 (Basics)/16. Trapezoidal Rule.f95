program Trapezoidal_rule

IMPLICIT NONE
real::a,b,x,sum1,sum2,h,s,ans
INTEGER:: n,i

f(x)=Sin(x)
write(*,*)"Enter the Lower Limit:"
read(*,*)a
write(*,*)"Enter the Upper Limit:"
read(*,*)b
write(*,*)"Enter the number of boxes i.e. n:"
read(*,*)n

ans = -Cos(b)+Cos(a)

open(1,file="trapezoid_data.dat",status="unknown")

h=(b-a)/n
s=0
sum2=0

do i = 1,n-1
    s=s+f(a+i*h)
    write(1,*)i,abs((ans-(h*((f(a)+f(b))/2+s)))*100/ans)
enddo

sum1=h*((f(a)+f(b))/2+s)

WRITE(*,*)"Using Trapezoidal Rule, the value of the I(t) =", sum1
write(*,*)"expected value for Integral[f(x)] analytically is:",ans
write(*,*)"Percentage Error between the two is:",abs((ans-sum1)*100/ans),"%"

end program Trapezoidal_rule