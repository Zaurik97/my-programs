program taylor
  implicit none
  real :: taylorCalcolato,x,Fatt
  integer :: i,Nmax                                          

  print*," Nmax= ?"
  read*,Nmax
 
  print*," quanto vale x ?"
  read*,x
 
  taylorCalcolato = 1
  Fatt = 1
 
  do  i=1,Nmax
       
        taylorCalcolato = taylorCalcolato + ((((-1)*x)**i)/(Fatt*i))
 
  end do
 
  print*, "il valore dello sviluppo in taylor dei primi",Nmax,"termini è di ", taylorCalcolato
  print*, "il valore della funzione esponenziale è di: ",EXP(-x)
 
end program taylor