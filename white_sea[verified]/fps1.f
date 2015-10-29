      subroutine zcoeu(ahz,tay,tax,thy,thx,
     #                 b2,a1,b1,c1,fu,fv,k1,ke,km)

****** coefficient for "progonka", fluxes qa,qh on both boundaries
******  a1(k)*a(k-1)+c1(k)*a(k)+b1(k)*a(k+1)=f1(k)
******  da/dt=d(ah*da/dz)/dz
****** z=0       ah*da/dz=-qa
****** z=H       ah*da/dz=-qh

        include 'par.inc'
        include 'com.inc'

      real ahz(km),b2(1:km),
     *     a1(1:km),b1(1:km),c1(1:km),fu(1:km),fv(1:km)



      a1(k1)=0.

      if(k1.eq.1)then

      b1(k1)=ahz(1)/z(2)
      c1(k1)=-b1(k1)-a1(k1)+b2(k1)

      elseif(k1.eq.2)then

      b1(2)=ahz(2)/(zw(3)-zw(2))/(z(3)-z(2))
      c1(2)=-a1(2)-b1(2)-1./dt+b2(2)

      fu(2)= - 2*tax/(z(3)+z(2))+fu(2)   !!!! was  2*tax/(z(3)+z(2)) ??? 13.10.
      fv(2)= - 2*tay/(z(3)+z(2))+fv(2)   !!!!!   --!!--    --!!--

      endif


      do 1 k=k1+1,ke-1
      a1(k)=ahz(k-1)/(z(k)-z(k-1))/(zw(k)-zw(k-1))
      b1(k)=ahz(k)/(z(k+1)-z(k))/(zw(k)-zw(k-1))
      c1(k)=-a1(k)-b1(k)-1./dt+b2(k)
   1   continue

      a1(ke)=ahz(ke-1)/(z(ke)-z(ke-1))/(zw(ke)-zw(ke-1))
       c1(ke)=-a1(ke)-b1(ke)-1./dt+b2(ke)
       fu(ke)=thx/(zw(ke)-z(ke))+fu(ke)   !!!! was zw(ke)-zw(ke-1) ??? 13.10.
      fv(ke)=thy/(zw(ke)-z(ke))+fv(ke)   !!!!!!!  --!!--    --!!--

      return
      end


      subroutine zcoef(ahz,qa,qh,a2,
     #                 b2,a1,b1,c1,f1,l0,lb,ke,km)

****** coefficient for "progonka", fluxes qa,qh on both boundaries
******  a1(k)*a(k-1)+c1(k)*a(k)+b1(k)*a(k+1)=f1(k)
******  da/dt=d(ah*da/dz)/dz
****** z=0       ah*da/dz=-qa
****** z=H       ah*da/dz=-qh
c       include 'par.inc'
c       include 'com.inc' 
        include 'par.inc'
        include 'com.inc'

      real ahz(1:km),a2(1:km),b2(1:km),
     *     a1(1:km),b1(1:km),c1(1:km),f1(1:km)
      k1=2

      if(l0.eq.1)k1=1

      a1(k1)=0.
      b1(ke)=0.
cccc b.c. on the surface

      if(l0.eq.1)then
c bound. cond - newton flux on surface
      b1(k1)=ahz(1)/z(2)
      c1(k1)=-b1(k1)-a1(k1)+b2(k1)
        f1(k1)=a1(k1)
      elseif(l0.eq.2)then
C     boundary conditions for fluxes
      b1(2)=ahz(2)/zw(2)/(z(3)-z(2))
      c1(2)=-a1(2)-b1(2)-1./dt+b2(2)
      f1(2)= - qa/zw(2)+a2(2)

      elseif(l0.eq.-1)then
c   b.c on surfaces z=0,z=zw(ke)=h
c      here qa==a(i,j,1),qh==a(i,j,ke+1/2)

      b1(2)=ahz(2)/(z(3)-z(2)/2)/(z(3)-z(2))
      f1(2)=ahz(1)/(z(3)-z(2)/2)/z(2)
      c1(2)=-f1(2)-b1(2)-1./dt+b2(2)
      f1(2)=-f1(2)*qa+a2(2)

      elseif(l0.eq.-2)then
c  b.c. in the centres of boundary boxes
      a1(2)=0.
      b1(2)=0.
      c1(2)=-1.
      f1(2)=-qa
      endif

      if(lb.eq.2)then
      a1(ke)=+ahz(ke-1)/(z(ke)-z(ke-1))/(zw(ke)-zw(ke-1))	!  was +
      c1(ke)=-a1(ke)-b1(ke)-1./dt+b2(ke)              	!  was -
      f1(ke)=+qh/(zw(ke)-zw(ke-1))+a2(ke)

        elseif(lb.eq.-1)then
c   b.c on surfaces ,z=zw(ke)=h
c      here qa==a(i,j,1),qh==a(i,j,ke+1/2)

      f1(ke)=ahz(ke)/(zw(ke)-z(ke))/(zw(ke)-zw(ke-1))
      a1(ke)=ahz(ke-1)/(z(ke)-z(ke-1))/(zw(ke)-zw(ke-1))
      c1(ke)=-f1(ke)-b1(ke)-1./dt+b2(ke)
      f1(ke)=-f1(ke)*qh+a2(ke)

      elseif(lb.eq.-2)then
c  b.c. in the centres of boundary boxes
      a1(ke)=0.
      b1(ke)=0.
      c1(ke)=-1.
      f1(ke)=-qh

      endif


      do 1 k=k1+1,ke-1
      a1(k)=ahz(k-1)/(z(k)-z(k-1))/(zw(k)-zw(k-1))
      b1(k)=ahz(k)/(z(k+1)-z(k))/(zw(k)-zw(k-1))
      c1(k)=-a1(k)-b1(k)-1./dt+b2(k)
      f1(k)=   a2(k)
  1   continue
       return
      end


      subroutine abc(a,a1,b1,c1,f1,a2,b2,l0,ke,i,j,im,jm,km)
      real a(im,jm,km),
     *     a1(1:km),b1(1:km),c1(1:km),f1(1:km),
     *     a2(1:km),b2(1:km)

      k1=2

      if(l0.eq.1)k1=1



      a2(k1)=f1(k1)/c1(k1)
      b2(k1)=-b1(k1)/c1(k1)

      do 1 k=k1+1,ke
      det=c1(k)+a1(k)*b2(k-1)
      a2(k)=(f1(k)-a1(k)*a2(k-1))/det
  1   b2(k)=-b1(k)/det

      a(i,j,ke)=a2(ke)

      do 2 k=ke-1,k1,-1
  2   a(i,j,k)=a2(k)+b2(k)*a(i,j,k+1)

      if(l0.gt.1)a(i,j,1)=a(i,j,2)
      

      return
      end

      function d2dz(a,ahz,qa,qh,ke,i,j,k,i1,j1,im,jm,km)
      real a(im,jm,km),ahz(im,jm,km)
        include 'par.inc'
        include 'com.inc'

c      print*,qa,'d2dz <- qa'
c	print*,qh,'d2dz <- qh'
c	pause 
	
      az1=0.5*(ahz(i,j,k-1)+ahz(i1,j1,k-1))
      az2=0.5*(ahz(i,j,k)+ahz(i1,j1,k))

      if(k.eq.2)then

      b1=az2/zw(2)/(z(3)-z(2))
      f1=0                                       - qa/zw(2)

      d2dz=b1*(a(i,j,k+1)-a(i,j,k)) -f1

         elseif(k.lt.ke)then

         a1=az1/(z(k)-z(k-1))/(zw(k)-zw(k-1))
         b1=az2/(z(k+1)-z(k))/(zw(k)-zw(k-1))

         d2dz=b1*(a(i,j,k+1)-a(i,j,k))+a1*(a(i,j,k-1)-a(i,j,k))

             elseif(k.eq.ke)then

              a1=az1/(z(ke)-z(ke-1))/(zw(ke)-zw(ke-1))
              f1=+qh/(zw(ke)-zw(ke-1))
              d2dz=a1*(a(i,j,k-1)-a(i,j,k))-f1

              else
              d2dz=0.

        endif
        return
        end


      function advz(a,wu,wm,i,j,k,im,jm,km,mp,ph)
        include 'par.inc'
        include 'com.inc'
        real a(im,jm,km),ph(im,jm)

	  
      advz=0.

       zw(1)=-Ph(i,j)/980.   !!!!!  Ó×ÅÒ ÊÎËÅÁÀÍÈÉ ÓÐÎÂÍß
       z(1)=zw(1)
       z(2)=(zw(1)+zw(2))/2.                                                                                      



        zd=zw(k+1)-z(k+1)
        z0=zw(k)-z(k)
        zu=zw(k-1)-z(k-1)

	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

c      if(k.eq.kp(i,j).and.mp.eq.1)a(i,j,k+1)=a(i,j,k)       !!!!  stop 25.07.2004

       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  IMP!!   TOO  WARN near  bottom 
        goto 78
        am=fm(a(i,j,k),a(i,j,k+1),zd,z0)*wm     !!!!  modified oct. 02. 
        au=fm(a(i,j,k),a(i,j,k-1),zu,z0)*wu    !!!!   mod.
78      continue

	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Godunov !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	goto 1

	 if(wu.lt.0.)au=a(i,j,k)*wu             !Godunov  classic
	 if(wu.ge.0.)au=a(i,j,k-1)*wu          !Godunov  classic

      if(wm.lt.0.)am=a(i,j,k+1)*wm            !Godunov classic
      if(wm.ge.0.)am=a(i,j,k)*wm              !Godunov classic
 
ccc      if(wm.lt.0.)am=(a(i,j,k)+a(i,j,k+1))/2.*wm         !Godunov  exp.3  +sum/2
	
	 

1     continue




      !!!!!!!!!!!!!!!!!!!!!!!!!!!!   semisum   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ccc      goto 2

       dzu=zw(k-1)-z(k-1)
       dzd=z(k)-zw(k-1)
	 dzsum=z(k)-z(k-1)

      au=(a(i,j,k)*dzd/dzsum + a(i,j,k-1)*dzu/dzsum)*wu !


       dzu=zw(k)-z(k)
       dzd=z(k+1)-zw(k)
	 dzsum=z(k+1)-z(k)

      am=(a(i,j,k+1)*dzd/dzsum+a(i,j,k)*dzu/dzsum )*wm !


2     continue

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          advz=(am-au)/(zw(k)-zw(k-1))     !



       zw(1)=0.   !!!!!!   LEVEL    MOD.
       z(1)=0.
                                                                                      
      
      return
      end










      function advzu(a,w,i,j,k,im,jm,km)
        include 'par.inc'
        include 'com.inc'
        real a(im,jm,km),w(im,jm,km)

      
	wm=w(i,j,k)
	wu=w(i,j,k-1)
	wm1=w(i-1,j,k)
	wu1=w(i-1,j,k-1)
	
c      if(k.eq.2)then
c        am=fm(a(i,j,3),a(i,j,2),z(2),(z(3)-zw(2)))*(wm+wm1)/2  
c        au=a(i,j,2)*(wu+wu1)/2

c          advzu=(am-au)/zw(2)

c             else

c        zd=zw(k+1)-z(k+1)
c        z0=zw(k)-z(k)
c        zu=zw(k-1)-z(k-1)

      
c      am=fm(a(i,j,k),a(i,j,k+1),zd,z0)*(wm+wm1)/2     
c      au=fm(a(i,j,k),a(i,j,k-1),zu,z0)*(wu+wu1)/2



	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

c	goto 1
	am=a(i,j,k)*(wm+wm1)/2 
	if((wm+wm1)/2.lt.0)am=a(i,j,k+1)*(wm+wm1)/2 !  R

	au=a(i,j,k-1)*(wu+wu1)/2
	if((wu+wu1)/2.lt.0)au=a(i,j,k)*(wu+wu1)/2   !  R
1     continue
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          advzu=(am-au)/(zw(k)-zw(k-1))  
      
c           endif
      
      return
      end


      function advzv(a,w,i,j,k,im,jm,km)
        include 'par.inc'
        include 'com.inc'
        real a(im,jm,km),w(im,jm,km)

      
	wm=w(i,j,k)
	wu=w(i,j,k-1)
	wm1=w(i,j-1,k)
	wu1=w(i,j-1,k-1)
	
c      if(k.eq.2)then
c        am=fm(a(i,j,3),a(i,j,2),z(2),(z(3)-zw(2)))*(wm+wm1)/2  
c        au=a(i,j,2)*(wu+wu1)/2
c
c          advzv=(am-au)/zw(2)

c             else

c        zd=zw(k+1)-z(k+1)
c        z0=zw(k)-z(k)
c        zu=zw(k-1)-z(k-1)

      
c      am=fm(a(i,j,k),a(i,j,k+1),zd,z0)*(wm+wm1)/2     
c      au=fm(a(i,j,k),a(i,j,k-1),zu,z0)*(wu+wu1)/2



	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

c	goto 1
	am=a(i,j,k)*(wm+wm1)/2 
	if((wm+wm1)/2.lt.0)am=a(i,j,k+1)*(wm+wm1)/2   ! R
	au=a(i,j,k-1)*(wu+wu1)/2
	if((wu+wu1)/2.lt.0)au=a(i,j,k)*(wu+wu1)/2     ! R
1     continue
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          advzv=(am-au)/(zw(k)-zw(k-1))  
      
c           endif
      
      return
      end



      function advzt(wu,wm,i,j,k,im,jm,km)
        include 'par.inc'
        include 'com.inc'
      advzt=0.
      if(k.eq.2)then
        am=wm
        au=wu
c       au=0.
          advzt=(am-au)/zw(2)

             else
c        zd=zw(k+1)-z(k+1)
c        z0=zw(k)-z(k)
c        zu=zw(k-1)-z(k-1)

        am=wm
        au=wu
       

          advzt=(am-au)/(zw(k)-zw(k-1))

           endif

      return
      end

      function advu1(u,v,i,j,k,im,jm,km,ir)
        include 'par.inc'
        include 'com.inc'

c      integer ku(im,jm)
      dimension
     *          u(im,jm,km),v(im,jm,km)

      advu1=0.

              arad=2./rad/(sn(j)+sn(j+1))
********* step1
              aw=(u(i-1,j,k)+u(i,j,k))**2/4.
              ae=(u(i+1,j,k)+u(i,j,k))**2/4.



c	if(ir.eq.2)ae=u(i,j,k)**2
              dxw=x(i)-x(i-1)
              dxe=x(i+1)-x(i)
              dx=(dxw+dxe)/2.
       if(ku(i-1,j).lt.k)then   !!!! lt before 12.09.
	     aw=0.
       dx=dx+.5*dxw
	end if
       if(ku(i+1,j).lt.k)then    !!!!! lt before 12.09
	 ae=0.
       dx=dx+.5*dxe
	end if
          advu1= advu1+ (ae-aw)/dx*arad

**********  step2


             if(j.eq.1)then
                 vs=(v(i,j,k)+v(i-1,j,k))/2.*sn(j)            !
                 as=u(i,j,k)*vs
                an=(v(i,j+1,k)+v(i-1,j+1,k))/2.*sn(j+1)*
     *                     (u(i,j,k)+u(i,j+1,k))/2.


             elseif(j.eq.jss)then
             vn=(v(i,j+1,k)+v(i-1,j+1,k))/2.*sn(j+1)            !
               an=u(i,j,k)*vn
                 as=(v(i,j,k)+v(i-1,j,k))/2.*sn(j)*
     *             (u(i,j,k)+u(i,j-1,k))/2.

                   else
                 an=(v(i,j+1,k)+v(i-1,j+1,k))/2.*sn(j+1)*
     *               (u(i,j,k)+u(i,j+1,k))/2.

                   as=(v(i,j,k)+v(i-1,j,k))/2.*sn(j)*
     *                   (u(i,j,k)+u(i,j-1,k))/2.
c      if(ir.eq.2.and.j.ne.8)as=v(i-1,j,k)*sn(j)*u(i,j,k)

                    endif
          advu1= advu1+ (an-as)/dy*arad

c     if(j.eq.8.and.i.eq.55.and.k.eq.10)
c    &write(6,*)'aeee',an,-as,ae,-aw
      return
      end

