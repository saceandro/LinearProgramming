program 2phase_simplex
implicit none

integer i,j,k,l,p
integer, parameter :: m=3,n=3 
real(8) a(0:m,0:n), b(0:m,0:n), c(0:(m+1),0:(n+1)), q(0:(m+1),0:(n+1)), min, d, min2, f

a(0,0:n) = (/0.,-2.,-1.,-1./)
a(1,0:n) = (/4.,-2.,-2.,1./)
a(2,0:n) = (/4.,-2.,0.,-4./)
a(3,0:n) = (/1.,4.,-3.,1./)

do i = 0,m
   write(*,*) a(i,0:n)
enddo

do i = 0,m
   c(0,i) = 0
enddo
c(0,m+1) = 1
c(1,m+1) = 0
do i = 0,m
   c(i+1,0:n) = a(i,0:n)
enddo
do i = 2,(m+1)
   c(i,n+1) = 1
enddo

p = 1
min2 = a(1,0)
do j = 2,m
   f = a(j,0)
   if (f < min) then
      p = j
      min2 = f
   endif
enddo

if (min2 < 0) then
   call convert_matrix2(p,c,q)
   call simplex(q,r)
   if (r(0,0) == 0) then
      b(0:m,0:n) = r(1:m+1,0:
   

l = 1
do while (l <= m)
   l = 1
   do while ((l <= n).and.(a(0,l) >= 0))
      l = l + 1
   enddo
   if (l <= m) then
      k = 1
      do while ((k <= m).and.(a(k,l) >= 0))
         k = k + 1
      enddo
      if (k <= m) then
         min = a(k,0)/abs(a(k,l))
      else
         write(*,*) "unbounded"
         exit
      endif
      do i = (k+1),m
         if (a(i,l) < 0) then
            d = a(i,0)/abs(a(i,l))
            if (d < min) then
               k = i
               min = d
            endif
         endif
      enddo
      call convert_matrix(k,l,a,b)
      a(0:m,0:n) = b(0:m,0:n)
      write(*,*) k,l
      do i = 0,m
         write(*,*) a(i,0:n)
      enddo
   else
      write(*,*) a(0,0)
   endif
enddo

endprogram 2phase_simplex
