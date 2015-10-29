c     интерпол€ци€ разреза по €чейкам сетки
	subroutine interpol(P,R,npoint,Rlatt,Rlong,Slatt,Slong,
     #ip,jp,deltaR,Q)
	include 'par.inc'
	include 'com.inc'
c     INPUT:
c      P поле на регул€рной сетке
c      R  данные на точках разреза
c      Q точки разреза c учетом усвоенных данных - output 
c     deltr - коэфф. по пространству
c     is,js -  ..
c     npoint - число точек разреза
      REAL P(is,js,ks), Rlatt(is,js),Rlong(is,js)  !!регул€рные пол€
      REAL R(301,ks),Slatt(301),Slong(301)!! данные точек разреза
	REAL Q(301,ks),Qsum(ks)!! данные точек разреза c учетом усвоенных данных
	REAL W(15000,ks),betaR(15000,ks),betasum(ks),wsum(ks)!!веса и коэфф. усвоени€ дл€ соседних точек
	REAL betaRW(ks)
	INTEGER ipoint(15000,ks),jpoint(15000,ks) !!номера €чеек, попадающих в усвоение
	INTEGER ip(301),jp(301) !! €чейки, в которые попадают точки разреза
	INTEGER icount(ks) !!счетчик числа усваиваемых точек 
C-------------------------------------------------------
      gamma= 1.0	
	do 1 n=1,npoint  !!проход последовательно по точкам разреза
         icount=0      !!счетчик точек, попадающих в усвоение
	   wsum=0.0      !! сумма весов
	   betasum=0.0
	   do 2 j=1,js
         do 2 i=1,is    
	      dist=Rdist(Rlatt(i,j),Slatt(n),Rlong(i,j),Slong(n))
            if(dist.LE.deltaR) then
	         do k=1,kp(i,j)
			    icount(k)=icount(k)+1   
				if(icount(k).GT.15000) then
	               print *,'np=',n,icount(k)
	               pause
	            endif
	            ipoint(icount(k),k)=i
		        jpoint(icount(k),k)=j
	            W(icount(k),k)=deltaR-dist !!веса: dr-рассто€ние
	            if(w(icount(k),k).LT.0.0) 
     #                  print *,'W(l)=',W(icount(k),k),icount(k)
	            Wsum(k)=Wsum(k)+W(icount(k),k)
	            betaR(icount(k),k)=exp(-(dist /deltaR)**2) !!
	            betasum(k)=betasum(k)+betaR(icount(k),k)
	         enddo
	      endif
  2      continue
	   betaRW=0.0
	   do k=1,ks
	      do l=1,icount(k)
	         betaRW(k)=betaRW(k)+betaR(l,k)*W(l,k)
	      enddo
	      betaRW(k)=betaRW(k)/(icount(k)*Wsum(k))!!средневзвешенный коэфф. по пространству
	   enddo
	   do k=1,ks
	      if(k.LT.kp(ip(n),jp(n))) then  !!проверка -  на дне
	         Qsum=0.0
	         do l=1,icount(k)
	            i=ipoint(l,k)
	            j=jpoint(l,k)
                  if(k.LT.kp(i,j)) then  !!проверка -  на дне
	               Qsum(k)=Qsum(k)+W(l,k)*betaR(l,k)*P(i,j,k)
	            endif
	         enddo
               Q(n,k)=(1-gamma*betaRW(k))*R(n,k)+
     #                      gamma*Qsum(k)/(icount(k)*Wsum(k))
	      else
	         Q(n,k)=R(n,k)  !!-99.9
c	pause
	      endif
	   enddo
	   	   
	   

  1   continue
	return
	end