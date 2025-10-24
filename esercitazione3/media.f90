program media
  implicit none
  real :: aver,varianza,x
  integer :: i,l,N,sommaQuadrati                                                

  print*," N= ?"
  read*,N
  print*," immetti i dati uno alla volta"

  aver=0.0
 
  !aggiungo il calcolo della varianza e della somma dei quadrati nello stesso ciclo
  sommaQuadrati=0 !essendo intero x il suo quadrato è intero
  varianza=0.0
 
  do i=1,N                                            
     
  read*,x
  aver = aver + x
  sommaQuadrati = sommaQuadrati + (x*x)
     
  end do          
   
  aver = aver/N
  varianza = (sommaQuadrati/N) - aver**2 !media dei quadrati meno il quadrato della media

  print*," media di ",N," dati : ",aver
  print*," somma dei quadrati di ",N," dati : ",sommaQuadrati
  print*," varianza dei dati: ",varianza
 
end program media