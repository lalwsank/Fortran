Program Primes_less_then_n
Implicit None
INTEGER :: x, counts, num, i

Write(*,*)"Enter the Number that the Prime numbers should be less than"
read(*,*) x
write(*,*)"The Prime Numbers Less than",x,"are"
do num = 1, x

    counts = 0
    
    IF(num==1)THEN
    write(*,*)1
    ELSE
    do i=2,num
        
        if( MOD(num,i)==0 )THEN
        counts = counts + 1
        ENDIF
    ENDDO

    if(counts == 1)THEN
    write(*,*)num
    
    ENDIF
    
    ENDIF
    
ENDDO

End Program Primes_less_then_n
