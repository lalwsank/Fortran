PROGRAM Prime_Number
implicit none
INTEGER :: num,counts,i
write(*,*)"Enter the Number that is to be tested."
read(*,*)num
IF(num==1)THEN
write(*,*)num,"is a Prime Number."
ELSE
counts=0

do i=2,num
    if(MOD(num,i)==0)THEN
    counts=counts+1
    ENDIF
    ENDDO
if(counts==1)THEN
write(*,*)num,"is a Prime Number."
ELSE
write(*,*)num,"is not a Prime Number"
ENDIF
ENDIF

END PROGRAM Prime_Number