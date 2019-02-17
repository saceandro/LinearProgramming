!このプログラムは、ファイル simplex_sub.f90 に書かれた、
!サブルーチン convert_matrix(k,l,m,n)を呼び出す。

program simplex
implicit none

integer, parameter :: m=2,n=4
integer i,j,k,l,x(n+m),s
real(8) a(0:m,0:n), b(0:m,0:n), min, d, y(n+m)


a(0,0:n) = (/0,-2,2,-3,3/)
a(1,0:n) = (/4,-1,1,-5,5/)
a(2,0:n) = (/6,-3,3,-1,1/)

open(unit=1, file="simplex1.dat")

do i = 1,(n+m)
   x(i) = i
enddo

do i = 0,m
   write(1,*) a(i,0:n)
enddo
write(1,*)

l = 1
do while (l <= n)
   l = 1
   do while ((l <= n).and.(a(0,l) >= 0.))
      l = l + 1
   enddo
   if (l <= n) then
      k = 1
      do while ((k <= m).and.(a(k,l) >= 0.))
         k = k + 1
      enddo
      if (k <= m) then
         min = a(k,0)/abs(a(k,l))
      else
         write(1,*) "unbounded"
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
      s = x(k+n)
      x(k+n) = x(l) 
      x(l) = s
      call convert_matrix(k,l,a,b)
      a(0:m,0:n) = b(0:m,0:n)
      write(1,*) k,l
      write(1,*)
      do i = 0,m
         write(1,*) a(i,0:n)
      enddo
      write(1,*)
   else
      do i = 1,(n+m)
         if (i > n) then
            y(x(i)) = a(i-n,0)
         else
            y(x(i)) = 0.
         endif
      enddo
      write(1,*) y(1:(n+m))
      write(1,*)
      write(1,*) a(0,0)
   endif
enddo

close(1)

endprogram simplex
