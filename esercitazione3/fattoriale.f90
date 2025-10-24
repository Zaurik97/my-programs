program fattoriale
  implicit none
  real :: fattorialeCalcolato
  integer :: i,j,N                                            

  print*," N= ?"
  read*,N
 
  !fattorialeCalcolato = Calcolofattoriale (N)
  fattorialeCalcolato = 1
 
  do  i=1,N
       
        fattorialeCalcolato = 1                        
  do j=1,i
  fattorialeCalcolato = fattorialeCalcolato*j
  end do
 
  print*, "il valore del fattoriale di ", i, "è", fattorialeCalcolato
 
  end do
 
end program fattoriale
