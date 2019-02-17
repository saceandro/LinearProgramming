subroutine simplex(p,c,q)
implicit none

integer i,j,k,l,p
integer,parameter :: m=4,n=4
real(8), intent(in) ::  c(0:m,0:n),p
real(8), intent(out) :: q(0:m,0:n)
real(8) min, d

do i = 0,m
   write(*,*) c(i,0:n)
enddo

l = 1
do while (l <= m)
   l = 1
   do while ((l <= n).and.(c(0,l) >= 0))
      l = l + 1
   enddo
   if (l <= m) then
      k = 1
      do while ((k <= m).and.(c(k,l) >= 0))
         k = k + 1
      enddo
      if (k <= m) then
         min = c(k,0)/abs(c(k,l))
      else
         write(*,*) "unbounded"
         exit
      endif
      do i = (k+1),m
         if (c(i,l) < 0) then
            d = c(i,0)/abs(c(i,l))
            if (d < min) then
               k = i
               min = d
            endif
         endif
      enddo
      call convert_matrix(k,l,c,q)
      c(0:m,0:n) = q(0:m,0:n)
      write(*,*) k,l
      do i = 0,m
         write(*,*) c(i,0:n)
      enddo
   else
      write(*,*) c(0,0)
   endif
enddo
q(0:m,0:n) = c(0:m,0:n)
endsubroutine simplex
