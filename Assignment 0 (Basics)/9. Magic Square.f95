program magicsquare
	implicit None
    integer::i,j,order,x,y,n,row,col,nxtrow,nxtcol,m
    integer::A(100,100)
    
    write(*,*) "Enter Odd order of Magic Square:"
    read (*,*) order
    write (*,*) "Enter The First Number of the Magic Square:"
    read (*,*) x
    
    do i=1,order
        do j=1,order
            A(i,j)=0
        end do
    end do
   
    row=1
    col=order/2 +1
    A(row,col)=x
   
    n=(x-1)+order**2
    do y=(x+1),n
        nxtrow=row-1
        nxtcol=col+1
        if (nxtrow.lt.1) then
            nxtrow=order
        end if
        if (nxtcol.gt.order) then
            nxtcol=1
        end if
        m=A(nxtrow,nxtcol)
        if (m/=0) then
            nxtrow=row+1
            nxtcol=col
        end if
        row=nxtrow
        col=nxtcol
           A(row,col)=y
    end do

    write (*,*) "The magic square is displayed below"
        
    do i=1,order
        write(*,*) (A(i,j),j=1,order)
    end do

end program magicsquare