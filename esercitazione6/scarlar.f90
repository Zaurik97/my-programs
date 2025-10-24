program scalar
 implicit none
integer,parameter::N=2
integer::I
real::vett=0,vett1=0,vett2=0,y,z,k=0,modul,modul1,angle,angle1
real, dimension(N)    :: a
real, dimension(N)    :: b
real, dimension(N)    :: c
read*,a
print*,"il vettore a ha coordinate:",a
read*,b
print*,"il vettore b ha coordinate:",b

 do I=1,N
vett=vett+a(I)**2
vett1=vett1+b(I)**2
vett2=vett2+a(I)*b(I)
end do
print*,vett,vett1,vett2
y=dot_product(a,b)
print*,"il prodotto scalare è:",y
do I=1,N
c(I)=k+a(I)*b(I)
end do
z=sum(c)
print*,"il prodotto scalare è:",z

modul=sqrt(dot_product(a,a))
print*,"il modulo di a è:",modul
modul1=sqrt(dot_product(b,b))
print*,"il modulo di b è:",modul1

angle=acos(y/(modul*modul1))
print*,"l'angolo in radianti è:",angle
angle1=angle*(180/acos(-1.0))
print*,"l'angolo in gradi è:",angle1
end program scalar
