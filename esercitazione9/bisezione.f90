module squareRoot
    implicit none
    interface f
        module procedure ft
    end interface f

 contains

    function ft(x, z) result(res) !se res = 0 x è la radice di z
    
        real, intent(in) :: x, z
        real :: res
        res = x**2 - z
        
    end function ft
    
    subroutine bisezione(f1,z, x_low, x_high, tol, x_mid, f_mid, iterations)
    	procedure(ft) :: f1
        real, intent(in) :: z, tol
        real, intent(inout) :: x_low, x_high, x_mid, f_mid
        integer, intent(out) :: iterations
        real :: f_low

        iterations = 0

        if (f1(x_low, z) * f1(x_high, z) > 0.0) then
            write(*, *) "L'intervallo specificato non contiene la radice."
            return
        end if


        do while (abs(x_high - x_low) > tol)
            x_mid = (x_low + x_high) / 2.0
            f_mid = f1(x_mid, z)

            if (f_mid == 0.0) then
                exit
            else if (f_mid * f1(x_low, z) < 0.0) then
                x_high = x_mid
            else
                x_low = x_mid
            end if

            iterations = iterations + 1
        end do

    end subroutine bisezione

    end module squareRoot

program main
    use squareRoot
    implicit none

    real :: z, x_low, x_high, x_mid, f_mid, tol
    integer :: iterations

    print*, "Inserisci il numero reale z:"
    read*, z
    
    print*, "Inserisci l'estremo inferiore dell'intervallo:"
    read*, x_low
    
    if (x_low<0.0)then
    x_low=0.0
    end if
    
    print*, "Inserisci l'estremo superiore dell'intervallo:"
    read*, x_high
    
    print*, "Inserisci la tolleranza:"
    read*, tol

    call bisezione(ft,z, x_low, x_high, tol, x_mid, f_mid, iterations)

    
    print*, "Radice quadrata approssimata:", x_mid
    print*, "Valore della funzione in x_mid:", abs(f_mid)
    print*,"Semiampiezza dell'intervallo finale:", abs(x_high - x_low)
    print*, "Numero di iterazioni eseguite:", iterations

 
end program main
