program pendolo
 implicit none

 real    :: l,g=9.81 ! valori di default per massa e cost. elastica
 real    :: dt,ekin,epot,time
 real    :: angle,velang,velang_parziale,massa
 integer :: nstep,it
 write(unit=*,fmt="(a)",advance="no")"delta t : "   ! il formato (a) chiede che il dato sia trattato come 
 read*,dt                                           ! caratteri e advance="no"  sopprime il  
 write(unit=*,fmt="(a)",advance="no")"n.step: "     ! carattere "a capo"  alla fine della linea per cui 
 read*,nstep                                        ! la prossima operazione di lettura/scrittura inzia
 write(unit=*,fmt="(a)",advance="no")"lunghezza: "      ! sulla stessa riga di schermo di quella corrente
 read*,l
 write(unit=*,fmt="(a)",advance="no")"ang(0): "
 read*,angle
 write(unit=*,fmt="(a)",advance="no")"velang(0): "
 read*,velang
write(unit=*,fmt="(a)",advance="no")"massa(0): "
 read*,massa

 it=0        ! step 0 : valori iniziali
time=2*acos(-1)*sqrt(l/g)
 write(unit=1,fmt=*)it,it*dt,angle,velang,massa
 epot = massa*g*l*sin(angle)
 ekin =  0.5 * massa * vel**2
 write(unit=2,fmt=*)it,dt*it,ekin,epot,ekin+epot

 do it = 1,nstep
    pos = angle + velang * dt + 0.5* g * dt**2
    vel_parziale = velang + 0.5 * dt * g         !  prima parte della formula per le velocita'
    epot =  massa*g*l*sin(angle)
    vel = vel_parziale + 0.5 * dt * f/massa         !  la formula per le velocita' viene completata qui
    write(unit=1,fmt=*)it,it*dt,angle,velang,massa
    ekin = 0.5 * massa * vel**2
    write(unit=2,fmt=*)it,it*dt,ekin,epot,ekin+epot

 end do
 end program pendolo
