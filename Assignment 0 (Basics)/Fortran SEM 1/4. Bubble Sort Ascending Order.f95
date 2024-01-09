PROGRAM Sort
implicit none
INTEGER i,j,n
REAL T, A(1000)
WRITE(*,*) "Type The Value of N"
READ(*,*)n
WRITE(*,*) "Type The Numbers"
READ(*,*)(A(i),i=1,n)

DO i=1,n-1
   DO j=i+1,n
      IF (A(i) .GT. A(j)) THEN
            T = A(i) 
           A(i) = A(j) 
           A(j) = T   
           END IF
           END DO
           END DO

WRITE(*,*)"The Numbers in Decending Order:"
WRITE(*,*)(A(i),i=1,n)


END PROGRAM Sort