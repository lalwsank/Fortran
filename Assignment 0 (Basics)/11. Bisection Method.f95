real function f(x)
    implicit none
    real::x
    f=x**3 - 2*x + 1
end function

program bisection
    real:: a,b,c,error,f
    write(*,*)"Enter acceptable Error (Recommended Error = 0.00000001) :"
    read(*,*)error
    write(*,*)"Enter two numbers a and b between which the root is to be found"
    
    1 read(*,*) a
    read(*,*)b
    
    2 if (f(a)*f(b) .lt. 0) then
    c=(a+b)/2.0
    else
    write(*,*)"Try with another values of a and b"
    goto 1
    end if
    if (f(a)*f(c) .lt. 0) then
    b=c
    else
    a=c
    end if
    if (abs(b-a) .gt. error) goto 2


    write(*,*)"The root is",c

end program

