program LU_decomposition

implicit none
integer i, j, k, n
real(8),allocatable :: a(:,:), b(:), c(:), x(:)
real(8) dump

n = 4
allocate(a(n,n),b(n),c(n),x(n))

open(300,file="inpt.dat")
do i = 1, n
  read(300,*) a(1:n, i)
end do
read(300,*) b(1:n)
close(300)

do i = 1, n
  dump = 1.d0/a(i,i)
  do j = i+1, n
    a(i,j) = a(i,j) * dump
  end do
  do j = i+1, n
    dump = a(j,i)
    do k = i+1, n
      a(j,k) = a(j,k) - a(i,k)*dump
    end do
  end do
end do

do i = 1, n
  c(i) = b(i)
  do j = 1, i-1
    c(i) = c(i) - a(j,i)*c(j)
  end do   
  !c(i) = 
end do

do i = n, 1, -1
  x(i) = c(i)
  do j = n, i+1, -1
    x(i) = x(i) - a(j,i)*x(j)
  end do
  x(i) = x(i)/a(i,i)
end do

write(*,'(4f)') x(1:n)




end program
