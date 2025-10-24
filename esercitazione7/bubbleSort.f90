
program bubbleSort
  implicit none
  integer, dimension(:), allocatable :: x
  integer :: N, i, j, temp

  ! Input dell'array
  print *, "Inserisci il numero di elementi dell'array:"
  read *, N
  allocate(x(N))
 
  print *, "Inserisci gli elementi dell'array:"

  read *, x

  ! Algoritmo di bubble sort
  do i = 1, N
     do j = 1, N-i
        if (x(j) > x(j+1)) then
           temp = x(j)
           x(j) = x(j+1)
           x(j+1) = temp
        end if
     end do
  end do

  ! Output dell'array ordinato
  print *, "Array ordinato:",x

  deallocate(x)
end program bubbleSort
