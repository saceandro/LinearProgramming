subroutine convert_matrix(k,l,a,b)
implicit none

integer i,j
integer, parameter :: m=2,n=4
integer, intent(in) :: k,l
real(8), intent(in) :: a(0:m,0:n)
real(8), intent(out) :: b(0:m,0:n)

do i = 0,m
   do j = 0,n
      b(i,j) = a(i,j) - a(i,l)*a(k,j)/a(k,l)
   enddo
enddo
do j = 0,n
   b(k,j) = -a(k,j)/a(k,l)
enddo
do i = 0,m
   b(i,l) = a(i,l)/a(k,l)
enddo
b(k,l) = 1./a(k,l)

endsubroutine convert_matrix
