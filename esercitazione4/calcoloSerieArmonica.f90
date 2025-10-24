program calcoloSerieArmonica
  implicit none
  real::valoreSerie,valoreSerieContraria,a
  integer::i,j,Nmax                                        

  Nmax = 1000000 !un milione
  ValoreSerie = 0.0
  ValoreSerieContraria = 0.0
   
  do i=1,Nmax/10000 !salti di 10k
       

  do j=1,10000 !riutilizzo il valore precedentemente calcolato e poi aggiungo i 10000 valori per l'N successivo
  valoreSerie = valoreSerie +  1/(j + 0.0 + ( (i-1)*10000) )
  valoreSerieContraria = valoreSerieContraria +  1/(( i*10000) - (j-1.0) )
  end do
 
  !write(i,*)valoreSerie
  print*, "il valore della serie armonica per         N=",i*10000, "è: ",valoreSerie !stampo il valore in console
  print*, "il valore della serie armonica partendo da N=",i*10000, "è: ",valoreSerieContraria !stampo il valore in console
 
  end do
 
end program calcoloSerieArmonica