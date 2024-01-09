Program Matrix
IMPLICIT NONE
integer::i,j,k,order
integer::A(100,100),B(100,100),C(100,100)

write(*,*) "Enter Order of Matrices:"
read (*,*) order
write(*,*) "Enter values of Matrix A:"
do i=1,order
    read(*,*)(A(i,j),j=1,order)
enddo
write(*,*) "Enter values of Matrix B:"
do i=1,order
    read(*,*)(B(i,j),j=1,order)
enddo

do i = 1, order 
    do j = 1, order 
        do k = 1, order 
            C (i, j) = C (i, j) + A (i, k) * B (k, j) 
        enddo 
    enddo 
enddo
do i=1,order
    write(*,*)(C(i,j),j=1,order)
enddo



End Program Matrix