module squareRoot
implicit none
interface f
        module procedure df
end interface f

contains 
	
function df(x) result(res)
	real, intent(in) :: x
	real :: res
	res = 2 * x
end function
 
 subroutine newtonAlgorithm(f1,z, x0, threshold, epsilon, sqrt_result)
 	
        implicit none
        procedure(df) :: f1
        real, intent(in) :: z, x0, threshold, epsilon
        real, intent(out) :: sqrt_result
        
        real :: x, x_next, f, f_prime
        
        
        f(x) = x**2 - z
        
       
        x = x0
        
        do while (abs(f(x)) > threshold)
            x_next = x - f(x) / f1(x)
            
            if (abs(x_next - x) < epsilon) then
                exit
            end if
            
            x = x_next
        end do
        
        sqrt_result = x
        
    end subroutine newtonAlgorithm

end module squareRoot

program main
use squareRoot
    implicit none
    
   
    real :: z, x0, threshold, epsilon, sqrt_result
    
   
    print*, "Inserisci il numero reale di cui calcolare la radice quadrata:"
    read*,z
    
    if (z<0.0)then 
    print*,"in R non esitono radici di numeri negativi"
    else 
    print*, "Inserisci il punto di partenza per l'iterazione:"
    read*, x0
    print*, "Inserisci la soglia per il valore assoluto della funzione:"
    read*, threshold
    print*, "Inserisci la soglia per la differenza tra iterazioni successive:"
    read*, epsilon
    
   
    call newtonAlgorithm(df,z, x0, threshold, epsilon, sqrt_result)
    
    
    print*, "Radice quadrata di", z, ":", sqrt_result
    end if
    
end program main
