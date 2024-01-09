Program exp_fit

real::x(100),y(100),Yn(100),sumx,sumy,sumxx,sumxy,w,a,b,An,t0
integer::n

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

t0 = x(1)

open(1,file="Exp_fit_data.dat",status = "unknown")

do i=1,n
    write(1,*)x(i),y(i)
enddo

do i=1,n
    Yn(i)=LOG(y(i))
enddo

do i = 1,n
    sumx=sumx+x(i)
    sumy=sumy+Yn(i)
    sumxx=sumxx+x(i)**2
    sumxy=sumxy+x(i)*Yn(i)
enddo

An = (sumy*sumxx-sumx*sumxy)/(n*sumxx-sumx**2)
Bn = (sumx*sumy-n*sumxy)/(sumx**2-n*sumxx)
a = EXP(An)

write(*,*)"Hence, the equation of Decay is:",a,"Exp(",b,"(t -",t0,"))"

End Program exp_fit
