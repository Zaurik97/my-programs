program graficoFunzione

implicit none

integer :: N,i
real :: a,b,intervallo,valoreFunzione,pi

pi=acos(-1.0)

print*,"inserire N"
read*,N
print*,"inserire a"
read*,a
print*,"inserire b"
read*,b

intervallo = ( abs(b-a) )/N

DO i=1,N
    a= a + intervallo
    valoreFunzione = 1/EXP(4 * a**2 * sin(10*a))
    print*,a,valoreFunzione
    write(unit=10,fmt=*)a,valoreFunzione
END DO
 

end program graficoFunzione