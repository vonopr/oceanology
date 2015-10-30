	  real function Rdist(fi1,fi2,psi1,psi2)
c     расстояние между двумя точками в сантиметрах
c     fi 1,2 - широты
c     psi 1,2 - долготы
	  include 'par.inc'
      include 'com.inc'
     	alpha=(fi2-fi1)/2.0
    	beta=  (psi2-psi1)/2.0
c   	Pirad=3.14159/180.0/2.0
    	prom=(sind(alpha))**2+(cosd(fi1)*sind(beta))**2
    	Rad=637800000.0
        Rdist=2.0*Rad*sqrt( prom)
    	return
    	end
