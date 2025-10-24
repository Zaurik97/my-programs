module funzioniAggiunte
implicit none
contains
function calcoloMediaPesata(x,w) result(mp)
real, dimension(:),intent(in) ::  x,w
real::mp
mp=dot_product(x,w)/sum(w)
end function calcoloMediaPesata

end module funzioniAggiunte

program main
use funzioniAggiunte
implicit none

real, dimension(:),allocatable ::  x,w
integer::n
print*,"inserire numero coordinate vettori:"
read*,n
allocate(x(n),w(n))

read*,x

read*,w
print*,"il vettore w ha coordinate:",w
print*,"il vettore x ha coordinate:",x

print*,"la media pesata vale:",calcoloMediaPesata(x,w)
end program main