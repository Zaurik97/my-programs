module funzioniAggiunte
implicit none
contains
recursive function AlgoritmoDieuclide(a,b) RESULT(mcd)

INTEGER, INTENT(IN) :: a,b
integer::mcd
IF( b == 0  )THEN
mcd = a
ELSE IF( MOD(a,b) == 0  )THEN
mcd = b
ELSE
mcd = AlgoritmoDieuclide(b,MOD(a,b))
END IF


end function AlgoritmoDieuclide
end module funzioniAggiunte

program euclide
use funzioniAggiunte

implicit none
integer::a,b,mcd

print*,"inserire valori per a e b"
read*,a,b

print*,"l'MCD tra a e b vale: ", AlgoritmoDieuclide(a,b)

end program euclide