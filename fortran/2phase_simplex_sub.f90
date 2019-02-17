subroutine convert_matrix2(p,c,q)
implicit none

integer i,j
integer, parameter :: m=3,n=3 
integer, intent(in) :: p
real(8), intent(in) :: c(0:(m+1),0:(n+1))
real(8), intent(out) :: q(0:(m+1),0:(n+1))

do i = 0,m+1
   do j = 0,n+1
      q(i,j) = c(i,j) - c(i,n+1)*c(p,j)/c(p,n+1)
   enddo
enddo
do j = 0,n+1
   q(p,j) = -c(p,j)/c(p,n+1)
enddo
do i = 0,m+1
   q(i,n+1) = c(i,n+1)/c(p,n+1)
enddo
q(p,n+1) = 1./c(p,n+1)

endsubroutine convert_matrix2
