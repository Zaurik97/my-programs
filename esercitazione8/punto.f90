module geom
    implicit none

    type :: punto
        real :: x, y
    end type punto

    type :: triangolo
        type(punto) :: vertice_A
        type(punto) :: vertice_B
        type(punto) :: vertice_C
    end type triangolo

    interface operator (+)
        module procedure sum_points
    end interface operator (+)

    interface operator (*)
        module procedure scala
    end interface operator (*)

    interface operator (-)
        module procedure invert
    end interface operator (-)

    contains

    function sum_points(p, q) result(r)
        type(punto), intent(in) :: p, q
        type(punto) :: r
        r%x = p%x + q%x
        r%y = p%y + q%y
    end function sum_points

    function scala(p, k) result(r)
        type(punto), intent(in) :: p
        real, intent(in) :: k
        type(punto) :: r
        r%x = p%x * k
        r%y = p%y * k
    end function scala

    function invert(p) result(r)
        type(punto), intent(in) :: p
        type(punto) :: r
        r%x = -p%x
        r%y = -p%y
    end function invert

end module geom


program piano
    use geom
    implicit none
    type(punto) :: A, B, C
    type(triangolo) :: T1, T2, T3
    integer :: i

    open(unit=20, file='fort.20', status='replace')

    print*, 'Coordinate 2D del primo vertice:'
    read*, A
    print*, 'Coordinate 2D del secondo vertice:'
    read*, B
    print*, 'Coordinate 2D del terzo vertice:'
    read*, C
    T1 = triangolo(A, B, C)

    print*, 'Vertice A:', T1%vertice_A
    print*, 'Vertice B:', T1%vertice_B
    print*, 'Vertice C:', T1%vertice_C

    T2%vertice_A = -T1%vertice_A
    T2%vertice_B = -T1%vertice_B
    T2%vertice_C = -T1%vertice_C

    print*, 'Coppie di coordinate dei vertici del triangolo T2:'
    print*, T2%vertice_A
    print*, T2%vertice_B
    print*, T2%vertice_C

    write(20, *) T1%vertice_A
    write(20, *) T1%vertice_B
    write(20, *) T1%vertice_C
   
    write(20, *) T2%vertice_A
    write(20, *) T2%vertice_B
    write(20, *) T2%vertice_C
   

T3%vertice_A = scala(sum_points(T2%vertice_C, T2%vertice_A), 0.5)
T3%vertice_B = scala(sum_points(T2%vertice_A, T2%vertice_B), 0.5)
T3%vertice_C = scala(sum_points(T2%vertice_B, T2%vertice_C), 0.5)


print*, 'Coppie di coordinate dei vertici del triangolo T3:'
print*, T3%vertice_A
print*, T3%vertice_B
print*, T3%vertice_C

write(20, *) T3%vertice_A
write(20, *) T3%vertice_B
write(20, *) T3%vertice_C



 close(20)
end program piano