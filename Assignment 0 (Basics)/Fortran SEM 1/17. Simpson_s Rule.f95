program Simpsons_rule

IMPLICIT NONE
real::a,b,x,sum1,sum2,h,s,ans,f
INTEGER:: n,i

f(x) = Sin(x)

write(*,*)"Enter the Lower Limit:"
read(*,*)a
write(*,*)"Enter the Upper Limit:"
read(*,*)b
write(*,*)"Enter the number of boxes i.e. n:"
read(*,*)n

ans = -Cos(b)+Cos(a)

open(1,file="simpsons_data.dat",status="unknown")

h=(b-a)/n
s=0
sum2=0

do i = 1,n-1
    if(mod(i,2).ne.0) then
    s = s + 4*f(a+i*h)
    else
    s=s+2*f(a+i*h)
    ENDIF
    write(1,*)i,abs((ans-((h/3)*((f(a)+f(b))+s)))*100/ans)
enddo

sum1=(h/3)*((f(a)+f(b))+s)

WRITE(*,*)"Using Simpson's Rule, the value of the Integral[f(x)] =", sum1
write(*,*)"expected value for Integral[f(x)] analytically is:",ans
write(*,*)"Percentage Error between the two is:",abs((ans-sum1)*100/ans),"%"

end program Simpsons_rule