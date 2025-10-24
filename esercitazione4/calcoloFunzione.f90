program funzione

implicit none

real::pi,delta,x,y
integer::i,N

N = 100
delta = 0.001/N
pi = acos(-1.0)

do i=1,N
    x = delta*i !x da 0.001 a 0.1
    y = sqrt(2/pi/x)*((3/x**2-1)*sin(x)-3/x*cos(x))
    write(7,*)x,y
end do

print*,"finito di generare il file fort.7"

end program