module prec
  integer, parameter :: rk = selected_real_kind(6)
end module prec

module funzi
  use prec
  implicit none

 contains

  function f_trapezoid(fun, a, b, n) result(r)
    real(kind=rk), intent(in) :: a, b
    real(kind=rk) :: r
    interface
    
      function fun(x) result(res)
        use prec
        real(kind=rk), intent(in) :: x
        real(kind=rk) :: res
      end function fun
      
    end interface

    integer, intent(in) :: n
    real(kind=rk) :: h, x, sum
    integer :: i

    h = (b - a) / n
    sum = 0.0_rk

    do i = 0, n
      x = a + i * h
      if (i == 0 .or. i == n) then
        sum = sum + 0.5_rk * h * fun(x)
      else
        sum = sum + h * fun(x)
      end if
    end do

    r = sum
  end function f_trapezoid

  function f_simpson(fun, a, b, n) result(r)
    real(kind=rk), intent(in) :: a, b
    real(kind=rk) :: r
    interface
      function fun(x) result(res)
        use prec
        real(kind=rk), intent(in) :: x
        real(kind=rk) :: res
      end function fun
    end interface

    integer, intent(in) :: n
    real(kind=rk) :: h, x, sum
    integer :: i

    h = (b - a) / n
    sum = fun(a) + fun(b)

    do i = 1, n - 1, 2
      x = a + i * h
      sum = sum + 4.0_rk * fun(x)
    end do

    do i = 2, n - 2, 2
      x = a + i * h
      sum = sum + 2.0_rk * fun(x)
    end do

    r = sum * h / 3.0_rk
  end function f_simpson

  function fun(x) result(r)
    real(kind=rk), intent(in) :: x
    real(kind=rk) :: r
    r = EXP(-x**2)
  end function fun

end module funzi

program integral_approximation
  use funzi
  implicit none
  
  real(kind=rk) :: q_trap, q_simp, a, b, error, exact_value
  integer :: n, num_points

  print*,"inserire il numero di intervalli:"
  read*,num_points !100

  print*,"inserire l'estremo inferiore:"
  read*,a !-4
  print*,"inserire  l'estremo superiore:"
  read*,b !4
  
  
  



do n = 11, num_points, 10
  q_trap = f_trapezoid(fun, a, b, n)
  q_simp = f_simpson(fun, a, b, n)
 
end do

  

    print*,"l'integrale di simpson vale:", q_simp
    print*,"l'integrale con i trapezi vale:", q_trap
end program integral_approximation