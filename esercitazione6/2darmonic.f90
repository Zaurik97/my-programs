program harmonic
 implicit none

 real    :: massa,kappa ! valori di default per massa e cost. elastica
 real    :: dt,ekin,epot,Lz
 real,dimension(2)    :: pos,vel,vel_parziale,f
 integer :: nstep,it
 write(unit=*,fmt="(a)",advance="no")"delta t : "   ! il formato (a) chiede che il dato sia trattato come 
 read*,dt                                           ! caratteri e advance="no"  sopprime il  
 write(unit=*,fmt="(a)",advance="no")"n.step: "     ! carattere "a capo"  alla fine della linea per cui 
 read*,nstep                                        ! la prossima operazione di lettura/scrittura inzia
 write(unit=*,fmt="(a)",advance="no")"massa: "      ! sulla stessa riga di schermo di quella corrente
 read*,massa
 write(unit=*,fmt="(a)",advance="no")"kappa: "
 read*,kappa
 write(unit=*,fmt="(a)",advance="no")"pos(0): "
 read*,pos
 write(unit=*,fmt="(a)",advance="no")"vel(0): "
 read*,vel

 it=0        ! step 0 : valori iniziali

 write(unit=1,fmt=*)it,it*dt,pos,vel
 epot =  0.5 * kappa * dot_product(pos,pos)
 f    = - kappa * pos
 ekin =  0.5 * massa * dot_product(vel,vel)
 write(unit=2,fmt=*)it,dt*it,ekin,epot,ekin+epot

 do it = 1,nstep
    pos = pos + vel * dt + 0.5* f/massa * dt**2
    vel_parziale = vel + 0.5 * dt * f/massa         !  prima parte della formula per le velocita'
    f    = - kappa * pos
    epot =  0.5 * kappa * dot_product(pos,pos)
    vel = vel_parziale + 0.5 * dt * f/massa         !  la formula per le velocita' viene completata qui
    write(unit=1,fmt=*)it,it*dt,pos,vel
    ekin = 0.5 * massa * dot_product(vel,vel)
    write(unit=2,fmt=*)it,it*dt,ekin,epot,ekin+epot

 end do

Lz=massa*(pos(1)*vel(2)-pos(2)*vel(1))
print*,"il momento angolare è:",Lz
 end program harmonic
