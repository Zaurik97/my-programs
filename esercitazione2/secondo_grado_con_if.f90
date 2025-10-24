program secondo_grado

    implicit none               
  
    ! l' istruzione precedente obbliga a definire esplicitamente il tipo di ogni variabile
  
  
    real    :: a,b,c            ! i coefficienti reali dell' equazione
  
                                  ! a x**2 + b x + c = 0

    real :: discr,xp,xm      ! discriminante e soluzioni corrispondenti
                                  ! alle due scelte del segno davanti la
                                  ! radice quadrata

    read*, a,b,c                ! i coefficienti vengono letti da tastiera

    discr = b**2 - 4*a*c
    
    IF( discr < 0 )THEN

        print*,"non esistono soluzioni reali a tale equazione"
        
    ELSE

        xp = ( -b + sqrt(discr) )/(2*a)
        xm = ( -b - sqrt(discr) )/(2*a)

        print*,xp,xm                ! le due radici vengono scritte sullo schermo come complessi

    END IF
      
    
end program secondo_grado