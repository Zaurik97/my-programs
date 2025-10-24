program corpi1
use gravitazione
 implicit none
real(kind=kr), dimension(3,nbody) :: pos,vel,f
real(kind=kr), dimension(3)::velcm
real::dt
real(kr) :: epot,ekin
 integer :: nstep,it,k,l
 write(unit=*,fmt="(a)",advance="no")"delta t : "  
 read*,dt                                            
 write(unit=*,fmt="(a)",advance="no")"n.step: "    
 read*,nstep                                      
 write(unit=*,fmt="(a)",advance="no")"massa: "      
 read*,mass
 write(unit=*,fmt="(a)",advance="no")"pos(0): "
 read*,pos
 write(unit=*,fmt="(a)",advance="no")"vel(0): "
 read*,vel
it=0        
  do l=1,3
     velcm(l)=sum(vel(l,:)*mass)/sum(mass)
end do

vel =vel-spread(velcm,2,nbody)

write(unit=1,fmt=*)it,it*dt,pos(:,2)-pos(:,1),vel(:,2)-vel(:,1)
write(unit=3,fmt=*)it,it*dt,pos(:,3)-pos(:,1),vel(:,2)-vel(:,1)

call interazione(pos,f,epot)
 ekin =  sum(spread(mass,1,3)*vel**2 )/2
 write(unit=2,fmt=*)it,dt*it,ekin,epot,ekin+epot

 do it = 1,nstep
    pos = pos + vel * dt + 0.5* f/spread(mass,1,3) * dt**2
   vel = vel + 0.5 * dt * f/spread(mass,1,3)  
   
call interazione(pos,f,epot)
 vel = vel + 0.5 * dt * f/spread(mass,1,3)
     
     
    write(unit=1,fmt=*)it,it*dt,pos(:,2)-pos(:,1),vel(:,2)-vel(:,1)
    write(unit=3,fmt=*)it,it*dt,pos(:,3)-pos(:,1),vel(:,3)-vel(:,1)
   
    ekin = sum(spread(mass,1,3)*vel**2)/2
    write(unit=2,fmt=*)it,it*dt,ekin,epot,ekin+epot
   
 end do
 end program corpi1