program esercitazioneKind
implicit none
integer:: j
integer, parameter :: rk=selected_real_kind(33) 
real(kind=rk) ::x,y

do j=1,10000
x=2._rk**(-j)

IF (x==0) THEN

exit
END IF

print*,"il più piccolo numero calcolabile è:", x

end do

y=log10(x)

print*,"la potenza massima è:",y

end program esercitazioneKind
