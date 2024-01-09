Program Largest_Among_n_Numbers
implicit none
real::big
INTEGER::n
real,DIMENSION(1000)::A

write(*,*)"type total number of numbers"
Read(*,*)n
write(*,*)"Type in the",n,"Numbers"
Read(*,*)(A(i),i=1,n)

big=A(1)

do i = 2,n
    if(A(i).GT.big)THEN
    big = A(i)
    ENDIF
ENDDO
write(*,*)"Largest Number Among given",n,"Numbers is", big

End Program Largest_Among_n_Numbers
