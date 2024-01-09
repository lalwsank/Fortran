Program Horners_method
implicit none
INTEGER::A(100),x,n,ans,int,i
WRITE(*,*)"Enter the Order of the Polynomial (<100):"
read(*,*)n
WRITE(*,*)"Enter Coefficiants of Polynomial one by one starting with the highest order:"
do i=1,n
    read(*,*)A(i)
    ENDDO

WRITE(*,*)"the function you have entered is:"
do i=1,n
    write(*,*)A(i),"*x^",n-i
    ENDDO
    
WRITE(*,*)"Enter the Value of x:" 
read(*,*)x

ans=1
do i=1,n
    ans=x*ans+A(i)
    ENDDO

write(*,*)"the solution of the function using horners method is:",ans

End Program Horners_method
