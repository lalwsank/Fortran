Program Largest_Among_3_Numbers
implicit none

real::big
real,DIMENSION(3)::A
write(*,*)"Type the 3 Numbers"
Read(*,*)(A(i),i=1,3)

big=A(i)

do i=2,3
    if(A(i).GT.big)THEN
    big = A(i)
    ENDIF
ENDDO
write(*,*)"Largest Number Among given 3 is", big

End Program Largest_Among_3_Numbers
