program sommaQuadrati

  implicit none

  integer :: sommaFormula=0,N,j=1,sommaCiclo=0

  read*,N
  sommaFormula = ((N*(N+1)*(2*N+1))/(6)) 


  do j = 1,N
    sommaCiclo = sommaCiclo + j**2
  end do

print*,"la somma fattta con la formula dei quadrati dei primi N numeri con N= ",N,"vale: ",sommaFormula," e la somma fatta con il ciclo vale: "sommaCiclo

    end program sommaQuadrati