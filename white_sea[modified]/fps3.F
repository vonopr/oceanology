      function adv(xx,u,v,w,i,j,k,im,jm,km,qt)
        include 'par.inc'
        include 'com.inc'

  
      dimension
     *          xx(im,jm,km)
     *         ,u(im,jm,km),v(im,jm,km),w(im,jm,km)
     *         ,qt(im,jm)


      adv=0.

      ap=xx(i,j,k)
      snn=sn(j+1)/2.+sn(j)/2.

      if(i.eq.1)then
              aw=ap                       !
              dxw=x(i+1)-x(i)
           elseif(k.gt.kp(i-1,j))then
               aw=ap
               dxw=x(i+1)-x(i)
               else
              aw=xx(i-1,j,k)
              dxw=x(i+1)/2-x(i-1)/2

       endif
      if(i.eq.is-1)then
              ae=ap                       !
              dxe=x(i+1)-x(i)
           elseif(k.gt.kp(i+1,j))then
               ae=ap
               dxe=x(i+1)-x(i)
               else
              ae=xx(i+1,j,k)
              dxe=x(i+2)/2-x(i)/2
      endif
      if(j.eq.1)then
              as=ap                       !
          elseif(k.gt.kp(i,j-1))then
               as=ap
               else
           as=xx(i,j-1,k)

      endif

      if(j.eq.js-1)then
              an=ap                       !
           elseif(k.gt.kp(i,j+1))then
               an=ap
               else
            an=xx(i,j+1,k)
      endif

      dx=dxe+dxw

         if(it(i,j).ne.5) then
      ut1=(u(i,j,k)+u(i+1,j,k))/2./rad/snn
      vt1=(v(i,j,k)+v(i,j+1,k))/2./rad
                         else
      ut1=(u(i,j,k)+u(i,j,k))/2./rad/snn
      vt1=(v(i,j,k)+v(i,j,k))/2./rad
                        endif

      adv=   ut1*(dxw**2*(ae-ap)+dxe**2*(ap-aw))/dxe/dxw/dx
     #              -ut1*ut1*dt*((ae-ap)/dxe-(ap-aw)/dxw)/dx
     #      + vt1*(an-as)/dy/2.
     *    -vt1*vt1*dt*(an+as-2.*ap)/2./dy/dy

****** vertical advection
       wt1=w(i,j,k)/2.+w(i,j,k-1)/2.

      if(k.eq.1)then
       adv=adv - wt1*qt(i,j)/ql

      else

       au=xx(i,j,k-1)
        if(k.le. kp(i,j))then
        ad=xx(i,j,k+1)
        else
        ad=xx(i,j,k)     !
        endif
       dzd=z(k+1)-z(k)
       dzu=z(k)-z(k-1)
       dz=dzd+dzu

                if(k.eq.2)then

       adv=adv+wt1*((ad-ap)/dzd-qt(i,j)/ql)/2.
     #   - wt1*wt1*dt*((ad-ap)/dzd+qt(i,j)/ql)/(dzd/2.+dzu)/2.


                  else

       adv= adv
     #   + wt1*((ad-ap)*dzu**2+(ap-au)*dzd**2)/dzd/dzu/dz
     #   - wt1*wt1*dt*((ad-ap)/dzd-(ap-au)/dzu)/dz

       endif
       endif
       return
       end






      
                   
      subroutine Leith(xx,at,u,v,w,i,j,k,im,jm,km,qt)
         include 'par.inc'
        include 'com.inc'

  
      dimension
     *          xx(im,jm,km),at(im,jm,km)
     *         ,u(im,jm,km),v(im,jm,km),w(im,jm,km)
     *         ,qt(im,jm)


      ap=at(i,j,k)
      snn=sn(j+1)/2.+sn(j)/2.
                      
      if(i.eq.1)then
              aw=ap                      
              dxw=x(i+1)-x(i)
           elseif(k.ge.kp(i-1,j))then  
               aw=ap
               dxw=x(i+1)-x(i)
               else
              aw=at(i-1,j,k)
              dxw=x(i+1)/2-x(i-1)/2
       endif
       

      if(i.eq.is-1)then
              ae=ap                      
              dxe=x(i+1)-x(i)
           elseif(k.ge.kp(i+1,j))then      
               ae=ap
               dxe=x(i+1)-x(i)
               else
              ae=at(i+1,j,k)
              dxe=x(i+2)/2-x(i)/2
      endif
         

      if(j.eq.1)then
              as=ap                      
          elseif(k.ge.kp(i,j-1))then      !!!!  was gt 23.07.2004 
               as=ap
               else
           as=at(i,j-1,k)

      endif
         



      if(j.eq.js-1)then
              an=ap                       
           elseif(k.ge.kp(i,j+1))then   !!!! was gt 23.07.2004   
               an=ap
               else
            an=at(i,j+1,k)
      endif
      
         
          
      dx=dxe+dxw
      ut1=(u(i,j,k)+u(i+1,j,k))/2./rad/snn
      vt1=(v(i,j,k)+v(i,j+1,k))/2./rad

      xx(i,j,k)=xx(i,j,k)
     *      -(ut1*(dxw**2*(ae-ap)+dxe**2*(ap-aw))/dxe/dxw/dx
     #      +ut1*ut1*dt*((ae-ap)/dxe-(ap-aw)/dxw)/dx
     *)*dt


      xx(i,j,k)=xx(i,j,k)  - (vt1*(an-as)/dy/2. 
     *      + vt1*vt1*dt*(an+as-2.*ap)/2./dy/dy
     *)*dt 



****** vertical advection

       wt1=w(i,j,k)/2.+w(i,j,k-1)/2.     !!!!! was   w(,,k-1)  ???  5.10.

      if(k.eq.1)then    !!!!!!! k ne 1 all time see HDRL  ?????????
c      xx(i,j,k)=xx(i,j,k) - (wt1*qt(i,j)/ql)*dt

      else

       au=at(i,j,k-1)
        if(k.lt. kp(i,j))then   !!! lt instead le 03.09  flux=0 if k=kp
        ad=at(i,j,k)          !!! if, le, flux .ne. 0 
        else
        ad=at(i,j,k+1)     !!!!  was k  23.07.2004
        endif


       dzd=z(k+1)-z(k)
       dzu=z(k)-z(k-1)
       dz=dzd+dzu


       if(k.eq.2)then

      xx(i,j,k)=xx(i,j,k)  - (wt1*((ad-ap)/dzd  - qt(i,j)/ql)/2.
     #   + wt1*wt1*dt*((ad-ap)/dzd+qt(i,j)/ql)/(dzd/2.+dzu)/2.
     *)*dt


            else

      xx(i,j,k)=xx(i,j,k)
     #   - (wt1*((ad-ap)*dzu**2+(ap-au)*dzd**2)/dzd/dzu/dz
     #   + wt1*wt1*dt*((ad-ap)/dzd-(ap-au)/dzu)/dz
     *)*dt


       endif
       endif

c      wu=w(i,j,k-1)                      
c      wm=w(i,j,k)           

c      diva=(div(u,v,i,j,k,im,jm,km)+(wm-wu)/(zw(k)-zw(k-1)))
c     &*at(i,j,k)*dt
c      xx(i,j,k)=xx(i,j,k)-diva
    
       return
       end
      subroutine LeithC(xx,at,u,v,w,i,j,k,im,jm,km,qt)
         include 'par.inc'
        include 'com.inc'

  
      dimension
     *          xx(im,jm,km),at(im,jm,km)
     *         ,u(im,jm,km),v(im,jm,km),w(im,jm,km)
     *         ,qt(im,jm)


      ap=at(i,j,k)
      snn=sn(j+1)/2.+sn(j)/2.
                      
      if(i.eq.1)then
              aw=ap                      
              dxw=x(i+1)-x(i)
           elseif(k.ge.kp(i-1,j))then  
               aw=ap
               dxw=x(i+1)-x(i)
               else
              aw=at(i-1,j,k)
              dxw=x(i+1)/2-x(i-1)/2
       endif
       

      if(i.eq.is-1)then
              ae=ap                      
              dxe=x(i+1)-x(i)
           elseif(k.ge.kp(i+1,j))then      
               ae=ap
               dxe=x(i+1)-x(i)
               else
              ae=at(i+1,j,k)
              dxe=x(i+2)/2-x(i)/2
      endif
         

      if(j.eq.1)then
              as=ap                      
          elseif(k.ge.kp(i,j-1))then      !!!!  was gt 23.07.2004 
               as=ap
               else
           as=at(i,j-1,k)

      endif
         



      if(j.eq.js-1)then
              an=ap                       
           elseif(k.ge.kp(i,j+1))then   !!!! was gt 23.07.2004   
               an=ap
               else
            an=at(i,j+1,k)
      endif
      
         
          
      dx=dxe+dxw
      ut1=(u(i,j,k)+u(i+1,j,k))/2./rad/snn
      vt1=(v(i,j,k)+v(i,j+1,k))/2./rad
      u1=u(i,j,k)/2./rad/snn
         u2=u(i+1,j,k)/2./rad/snn
         v1=v(i,j,k)/2./rad
         v2=v(i,j+1,k)/2./rad


      xx(i,j,k)=xx(i,j,k)- 
     & ((u2*dxw**2*(ae-ap)+u1*dxe**2*(ap-aw))/dxe/dxw/dx
     # +ut1*dt*(u2*(ae-ap)/dxe-u1*(ap-aw)/dxw)/dx)*dt



      xx(i,j,k)=xx(i,j,k)  - ((v2*an-v1*as)/dy/2.        
     * + vt1*dt*(v2*an+v1*as-(v2+v1)*ap)/2./dy/dy)*dt     




****** vertical advection

       wt1=w(i,j,k)/2.+w(i,j,k-1)/2. 
          w2=w(i,j,k)
          w1=w(i,j,k-1)    

      if(k.eq.1)then    !!!!!!! k ne 1 all time see HDRL  ?????????
c      xx(i,j,k)=xx(i,j,k) - (wt1*qt(i,j)/ql)*dt

      else

       au=at(i,j,k-1)
        if(k.lt. kp(i,j))then   !!! lt instead le 03.09  flux=0 if k=kp
        ad=at(i,j,k)          !!! if, le, flux .ne. 0 
        else
        ad=at(i,j,k+1)     !!!!  was k  23.07.2004  !!! do not modify ->  COOLER 
        endif


       dzd=z(k+1)-z(k)
       dzu=z(k)-z(k-1)
       dz=dzd+dzu


       if(k.eq.2)then

      xx(i,j,k)=xx(i,j,k)  - ((w2*(ad-ap)/dzd  - w1*qt(i,j)/ql)/2.
     #   + dt*(w2**2*(ad-ap)/dzd+w1**2*qt(i,j)/ql)/(dzd/2.+dzu)/2.)*dt


            else

      xx(i,j,k)=xx(i,j,k)
     #   - ((w2*(ad-ap)*dzu**2+w1*(ap-au)*dzd**2)/dzd/dzu/dz
     #   + dt*(w2**2*(ad-ap)/dzd-w1**2*(ap-au)/dzu)/dz)*dt


       endif
       endif

   
       return
       end




      function fm(a1,a2,x1,x2)
      fm=(a1*x2+a2*x1)/(x1+x2)
      return
      end

      subroutine bstr(u,v,thx,thy,hu,hv,im,jm,km,ctah)

******  bottom stresses calculation
******   from bottom Ekmann layer
******    thx= (0.5*ql*f)**1/2 /H*(u-v)
******    thy= (0.5*ql*f)**1/2 /H*(u+v)
******    where qw,qe -  pressure fluxes on west-east boundary v-mesh
******          qn,qs -  pressure fluxes on nord-south boundary u-mesh
******
******
        include 'par.inc'
        include 'com.inc'


      real u(im,jm,km),v(im,jm,km),thx(im,jm),thy(im,jm)
 


      ep1=(ql/4/omega/acs(6))**0.5*ctah
c      ep=(ql*omega*acs(6))**0.5*ctah

      do 1 j=1,js
      do 1 i=1,iss
      thy(i,j)=0.
      ep=(ql*omega*acs(j))**0.5*ctah
      if(kv(i,j).gt.1)thy(i,j)
     #  =ep*zintw(v,i,j,1,ks,im,jm,kv,zw)/zw(kv(i,j))
   
  1   continue
      do 3 i=1,is
      do 3 j=1,jss
      thx(i,j)=0.
      ep=(ql*omega*acs(j))**0.5*ctah
      if(ku(i,j).gt.1)thx(i,j)
     #  =ep*zintw(u,i,j,1,ks,im,jm,ku,zw)/zw(ku(i,j))

  3   continue
      return
      end
c-------------------------------------------------
      subroutine taflux(a,ta,qt,qwa,im,jm,k,wu)
        include 'par.inc'
        include 'com.inc'
      real  a(im,jm,k),ta(im,jm),qt(im,jm)

      do 1 i=1,iss
      do 1 j=1,jss

        qt(i,j)=qwa*(ta(i,j)-a(i,j,k))*wu




c         deltaT=a(i,j,k)-ta(i,j) ! Tатмосферы - Tморя
c            if(deltaT.LT.25) then
c                Aconst=2.0E-03
c                Cteta=9.7E-04
c            endif
c            if(deltaT.GE.25) then 
c                Aconst=1.2E-03
c                Cteta= 1.46E-03
c            endif
c         Aconst=1.2E-03
c         Cteta=1.41E-03
c            wu=wu/100.0           ! перевод скорости из см/сек в м/сек
c         qt(i,j)=Aconst+ Cteta*wu*deltaT
c            qt(i,j)=qt(i,j)*100.0 ! перевод из K*m/sec в K*cm/sec 
  1   continue

      return
      end
c-------------------------------------------------
      subroutine tafluxSOM(a,ta,qt,qwa,im,jm,k,tax,tay)
        include 'par.inc'
        include 'com.inc'
      real  a(im,jm,k),ta(im,jm),qt(im,jm),tax(im,jm),tay(im,jm)

      do 1 i=2,iss
      do 1 j=2,jss

        qt(i,j)=qwa*(ta(i,j)-a(i,j,k))
     *      *sqrt(tax(i,j)**2+tay(i,j)**2)*1000.

  1   continue
      return
      end


        subroutine wind(tax,tay,tx,ty,tim,tim2,im,jm)
        real tax(im,jm),tay(im,jm)
        do i=1,im
        do j=1,jm
        tax(i,j)=tx*(1-exp(-tim/tim2)) 
        tay(i,j)=ty*(1-exp(-tim/tim2)) 
        enddo
        enddo
        return 
        end

        subroutine cotid(a,amax,c,im,jm,km,k,k2,time,am,a0)
        include 'par.inc'
        include 'com.inc'
      real a(im,jm,km),amax(im,jm,km),c(im,jm,k2)

        do 1 i=1,im
        do 1 j=1,jm
         
        if(kp(i,j).le.k.or.it(i,j).ne.0)then
        c(i,j,k2)=-0.1
        amax(i,j,k)=am
        else
        if(a(i,j,k)/a0.le.amax(i,j,k))goto 1
        amax(i,j,k)=a(i,j,k)/a0
        c(i,j,k2)=time
        endif
 1    continue
        return
        end
        subroutine cotidu(u,v,amax,c,im,jm,km,k,k2,time,am,a0)
        include 'par.inc'
        include 'com.inc'
      real u(im,jm,km),v(im,jm,km),amax(im,jm,k2),c(im,jm,k2)

        do 1 i=1,im
        do 1 j=1,jm
         
        if(kp(i,j).le.k.or.it(i,j).ne.0)then
        c(i,j,k2)=-0.1
        amax(i,j,k2)=am
        else
        a=sqrt((u(i,j,k)+u(i+1,j,k))**2+
     #   (v(i,j,k)+v(i,j+1,k))**2)/2
        if(a/a0.le.amax(i,j,k2))goto 1
        amax(i,j,k2)=a/a0
        c(i,j,k2)=time
        endif
 1    continue
        return
        end
        subroutine cotidv(u,v,amax,c,im,jm,km,time,am,a0)
        include 'par.inc'
        include 'com.inc'
      real u(im,jm,km),v(im,jm,km),amax(im,jm),c(im,jm)

        do 1 i=1,im
        do 1 j=1,jm
         
        if(it(i,j).ne.0)then
        c(i,j)=-0.1
        amax(i,j)=am
        else
        u1=0
        u2=0
        v1=0
        v2=0

        if(ku(i,j).gt.1)
     #u1=zintw(u,i,j,1,km,im,jm,ku(i,j),zw)/zw(ku(i,j))
        if(ku(i+1,j).gt.1)
     #u2=zintw(u,i+1,j,1,km,im,jm,ku(i+1,j),zw)/zw(ku(i+1,j))
        if(kv(i,j).gt.1)
     #v1=zintw(v,i,j,1,km,im,jm,kv(i,j),zw)/zw(kv(i,j))
        if(kv(i,j+1).gt.1)
     #v2=zintw(v,i,j+1,1,km,im,jm,kv(i,j+1),zw)/zw(kv(i,j+1))

        a=sqrt((u1+u2)**2+(v1+v2)**2)/2
  
        if(a/a0.le.amax(i,j))goto 1
        amax(i,j)=a/a0
        c(i,j)=time
        endif
 1    continue
        return
        end

         subroutine coef(u,v,ut,vt,w,fu,fv,us,tax,tay,thx,thy,
     &ph,pht,ro,ahz,an,as,aw,ae,ap,af,aff,
     #im,jm,km,
     #al,al1,cph,cu,cadv,cq,ct,cstep,ns)
        include 'par.inc'
        include 'com.inc'
      real
     #   fv(im,jm,km),fu(im,jm,km),us(im,jm,km)
     *     ,thx(im,jm),thy(im,jm) 
     *     ,u(im,jm,km),v(im,jm,km),tax(im,jm),tay(im,jm)
     *     ,ut(im,jm,km),vt(im,jm,km),ahz(im,jm,km)
     *     ,w(im,jm,km),ro(im,jm,km),ph(im,jm),pht(im,jm)
     *     ,ae(im,jm),aw(im,jm),as(im,jm),an(im,jm)
     #     ,ap(im,jm),af(im,jm),aff(im,jm)


      do 100 i=1,im
      do 100 j=1,jm
             as(i,j)=0.
             an(i,j)=0.
             aw(i,j)=0.
             ae(i,j)=0.
             ap(i,j)=0.
 100         af(i,j)=0.

        l=1   !!!!  l=0  -> правая часть (rppw) ->  -dw/dz    !!!! 06.04.03
        

       call rppw(u,v,ut,vt,w,fu,fv,us,tax,tay,thx,thy,
     # ph,ro,ahz,im,jm,km,l,al1,cph,cu,cadv,cq,cstep,pht)

     
      continue
      do 1 i=1,iss
      do 1 j=1,jss


        ke=kp(i,j)
      if(ke.le.1)goto 1
      iit=(it(i,j)-4)*(it(i,j)-5)*(it(i,j)-6)*it(i,j)
c      iit=(it(i,j)-4)*(it(i,j)-6)*it(i,j)
      if(iit.eq.0)then
******************************************************
************** inner points
        snn=sn(j+1)/2+sn(j)/2
        dx=x(i+1)-x(i)
        hh=zw(kp(i,j))/rad/rad/snn

        ij=((iss-i)+(j-1))*((jss-j)+(iss-i))
        hpar=(i-1)*(ku(i,j)-1)
         if(hpar.le.0.)then
              aw(i,j)=0.
               else
                     dxw=x(i+1)/2-x(i-1)/2
                     aw(i,j)=-hh/snn/dxw/dx
                     if(i.eq.iss.and.ku(is,j).gt.1)then
                      aw(i,j)=0.
                      if(ij.eq.0)aw(i,j)=1.
                      endif
             end if

        ij=((i-1)+(jss-j))*((j-1)+(i-1))
c        hpar=(iss-i)*hu(i+1,j)*(it(i+1,j)-2)      !!!!!
        hpar=(iss-i)*(ku(i+1,j)-1)      !!!!!
          if(hpar.le.0.)then
                ae(i,j)=0.
                 else
                     dxe=x(i+2)/2-x(i)/2
                     ae(i,j)=-hh/snn/dxe/dx
                    if(i.eq.1.and.ku(1,j).gt.1)then
                        ae(i,j)=0.
                        if(ij.eq.0)ae(i,j)=1.
                        end if
        end if

        ij=((i-1)+(jss-j))*((jss-j)+(iss-i))
c        hpar=(j-1)*hv(i,j)*(it(i,j-1)-2)           !!!!!!
        hpar=(j-1)*(kv(i,j)-1)           !!!!!!
              if(hpar.le.0.)then
                  as(i,j)=0.
                    else
                       as(i,j)=-hh*sn(j)/dy/dy
                       if(j.eq.jss.and.kv(i,js).gt.1)then
                              as(i,j)=0.
                              if(ij.eq.0)as(i,j)=1.
                            endif
        endif

        ij=((i-1)+(j-1))*((j-1)+(iss-i))
        hpar=(jss-j)*(kv(i,j+1)-1)
            if(hpar.le.0.)then
                  an(i,j)=0.
                    else
                      an(i,j)=-hh*sn(j+1)/dy/dy
                      if(j.eq.1.and.kv(i,1).gt.1)then
                            an(i,j)=0.
                            if(ij.eq.0)an(i,j)=1.
                           endif
        endif

        ap(i,j)=-(an(i,j)+as(i,j)+ae(i,j)+aw(i,j))
     *               +ct/980/dt/dt                        !!!!!! *l  06.04.03
     
      af(i,j)=
     * -fu(i,j,ks)
c     *        +((2.*ph(i,j)-pht(i,j))/dt/dt
     *        + ph(i,j)/dt/dt*ct/980. *l                    !!!!!!  *l  06.04.03

       if(ap(i,j).gt.0.)then
              aw(i,j)=aw(i,j)/ap(i,j)
              ae(i,j)=ae(i,j)/ap(i,j)
              an(i,j)=an(i,j)/ap(i,j)
              as(i,j)=as(i,j)/ap(i,j)
              af(i,j)=af(i,j)/ap(i,j)
              ap(i,j)=1.
              endif

******************************************************
******** outer liquid points
       elseif(it(i,j).eq.2)then
              an(i,j)=0.
              as(i,j)=0.
              aw(i,j)=0.
              ae(i,j)=0.
              ap(i,j)=1.
 

      endif
 1    continue

      return
      end


      subroutine coefTEMP1(u,v,ut,vt,w,fu,fv,us,tax,tay,thx,thy,
     #          ph,pht,ro,ahz,an,as,aw,ae,ap,af, 
     #          im,jm,km,
     #         al,al1,cph,cu,cadv,cq,ct,cstep,ns)   !eq. calc.
        include 'par.inc'
        include 'com.inc'

      
      real    fv(im,jm,km), fu(im,jm,km),us(im,jm,km)
     *     ,thx(im,jm),thy(im,jm) 
     *     ,u(im,jm,km),v(im,jm,km),tax(im,jm),tay(im,jm)
     *     ,ut(im,jm,km),vt(im,jm,km),ahz(im,jm,km)
     *     ,w(im,jm,km),ro(im,jm,km),ph(im,jm),pht(im,jm)
     *     ,ae(im,jm),aw(im,jm),as(im,jm),an(im,jm)
     #     ,ap(im,jm),af(im,jm)
     *     ,aee(im,jm),aww(im,jm),ass(im,jm),ann(im,jm)
     #     ,uf(im,jm),vf(im,jm)


      do 100 i=1,im
      do 100 j=1,jm
             as(i,j)=0.
             an(i,j)=0.
             aw(i,j)=0.
             ae(i,j)=0.
             ap(i,j)=0.
 100         af(i,j)=0.

        l=1    !!!!  l=0  -> правая часть (rppw) ->  -dw/dz
      
       call rppw(u,v,ut,vt,w,fu,fv,us,tax,tay,thx,thy,
     # ph,ro,ahz,im,jm,km,l,al1,cph,cu,cadv,cq,cstep,pht)

     
      continue
      do 1 i=1,iss
      do 1 j=1,jss

                        ddxx=sn(j)*rad*(x(i+1)-x(i))
                        ddyy=dy*rad 
                        ug=ddxx/dt*0.7
                           vg=ddyy/dt*0.7
             if((ph(i+1,j)-ph(i,j))/980.lt.1.e-4) then
                     uf(i,j)=ug 
                          else
       uf(i,j)=-(ph(i,j)-pht(i,j))/dt/((ph(i+1,j)-ph(i,j))/ddxx)
            endif
         
             if((ph(i,j+1)-ph(i,j))/980.lt.1.e-4) then
                     vf(i,j)=vg 
                          else
      vf(i,j)=-(ph(i,j)-pht(i,j))/dt/((ph(i,j+1)-ph(i,j))/ddyy)
            endif

                        cx=1/2/dt/ddxx
                           cy=1/2/dt/ddyy


        ke=kp(i,j)
      if(ke.le.1)goto 1
      iit=(it(i,j)-4)*(it(i,j)-5)*(it(i,j)-6)*it(i,j)
c      iit=(it(i,j)-4)*(it(i,j)-6)*it(i,j)
      if(iit.eq.0)then
******************************************************
************** inner points
        snn=sn(j+1)/2+sn(j)/2
        dx=x(i+1)-x(i)
        hh=zw(kp(i,j))/rad/rad/snn

        ij=((iss-i)+(j-1))*((jss-j)+(iss-i))
        hpar=(i-1)*(ku(i,j)-1)
         if(hpar.le.0.)then
              aw(i,j)=0.
                                                    aww(i,j)=0.
               else
                     dxw=x(i+1)/2-x(i-1)/2
                     aw(i,j)=-hh/snn/dxw/dx
                                                    aww(i,j)=cx*uf(i,j)
                     if(i.eq.iss.and.ku(is,j).gt.1)then
                      aw(i,j)=0.
                                                 aww(i,j)=0.
                      if(ij.eq.0)aw(i,j)=1.
                      endif
             end if

        ij=((i-1)+(jss-j))*((j-1)+(i-1))
c        hpar=(iss-i)*hu(i+1,j)*(it(i+1,j)-2)      !!!!!
        hpar=(iss-i)*(ku(i+1,j)-1)      !!!!!
          if(hpar.le.0.)then
                ae(i,j)=0.
                                                    aee(i,j)=0.
                 else
                     dxe=x(i+2)/2-x(i)/2
                     ae(i,j)=-hh/snn/dxe/dx
                                                    aee(i,j)=-cx*uf(i,j)
                    if(i.eq.1.and.ku(1,j).gt.1)then
                        ae(i,j)=0.
                                                    aee(i,j)=0.
                        if(ij.eq.0)ae(i,j)=1.
                        end if
        end if

        ij=((i-1)+(jss-j))*((jss-j)+(iss-i))
c        hpar=(j-1)*hv(i,j)*(it(i,j-1)-2)           !!!!!!
        hpar=(j-1)*(kv(i,j)-1)           !!!!!!
              if(hpar.le.0.)then
                  as(i,j)=0.
                                                 ass(i,j)=0.  
                    else
                       as(i,j)=-hh*sn(j)/dy/dy
                                                 ass(i,j)=-cx*vf(i,j)
                       if(j.eq.jss.and.kv(i,js).gt.1)then
                              as(i,j)=0.
                                                    ass(i,j)=0.
                              if(ij.eq.0)as(i,j)=1.
                            endif
        endif

        ij=((i-1)+(j-1))*((j-1)+(iss-i))
        hpar=(jss-j)*(kv(i,j+1)-1)
            if(hpar.le.0.)then
                  an(i,j)=0.
                                                    ann(i,j)=0.
                    else
                      an(i,j)=-hh*sn(j+1)/dy/dy
                                                    ann(i,j)=cx*vf(i,j)  
                      if(j.eq.1.and.kv(i,1).gt.1)then
                            an(i,j)=0.
                                                    ann(i,j)=0.
                            if(ij.eq.0)an(i,j)=1.
                           endif
        endif

        ap(i,j)=-(an(i,j)+as(i,j)+ae(i,j)+aw(i,j))
     *               +ct/980/dt/dt
      af(i,j)=
     * -fu(i,j,ks)
c     *        +((2.*ph(i,j)-pht(i,j))/dt/dt
     *        + ph(i,j)/dt/dt*ct/980.

       if(ap(i,j).gt.0.)then
              aw(i,j)=(aw(i,j)+aww(i,j))/ap(i,j)
              ae(i,j)=(ae(i,j)+aee(i,j))/ap(i,j)
              an(i,j)=(an(i,j)+ann(i,j))/ap(i,j)
              as(i,j)=(as(i,j)+ass(i,j))/ap(i,j)
              af(i,j)=af(i,j)/ap(i,j)
              ap(i,j)=1.
              endif

******************************************************
******** outer liquid points
       elseif(it(i,j).eq.2)then
              an(i,j)=0.
              as(i,j)=0.
              aw(i,j)=0.
              ae(i,j)=0.
              ap(i,j)=1.
 

      endif
 1    continue

      return
      end


      subroutine coefTEMP2(u,v,ut,vt,w,fu,fv,us,tax,tay,thx,thy,
     #          ph,pht,ro,ahz,an,as,aw,ae,ap,af,aff, 
     #          im,jm,km,
     #         al,al1,cph,cu,cadv,cq,ct,cstep,ns)   !eq. calc.
        include 'par.inc'
        include 'com.inc'
 
      real  
     *     thx(im,jm),thy(im,jm) 
     *     ,u(im,jm,km),v(im,jm,km),tax(im,jm),tay(im,jm)
     *     ,ut(im,jm,km),vt(im,jm,km),ahz(im,jm,km)
     *     ,w(im,jm,km),ro(im,jm,km),ph(im,jm),pht(im,jm)
     *     ,ae(im,jm),aw(im,jm),as(im,jm),an(im,jm)
     #     ,ap(im,jm),af(im,jm),aff(im,jm)
     *     ,aee(im,jm),aww(im,jm),ass(im,jm),ann(im,jm)
     #     ,uf(im,jm),vf(im,jm),
     #fu(im,jm,km),fv(im,jm,km),us(im,jm,km),utm(im,jm,km),vtm(im,jm,km)

         
      do 100 i=1,im
      do 100 j=1,jm
             as(i,j)=0.
             an(i,j)=0.
             aw(i,j)=0.
             ae(i,j)=0.
             ap(i,j)=0.
                        aff(i,j)=0
                      aww(I,J)=0.
                                               aee(I,J)=0.
                                               ann(I,J)=0.
                                               ass(I,J)=0.  
 100         af(i,j)=0.

        l=1    !!!!  l=0  -> правая часть (rppw) ->  -dw/dz
      
       call rppw(u,v,ut,vt,w,fu,fv,us,tax,tay,thx,thy,
     # ph,ro,ahz,im,jm,km,l,al1,cph,cu,cadv,cq,cstep,pht)

     
      continue
      do 1 i=1,iss
      do 1 j=1,jss

      ke=kp(i,j)
         if(ke.le.1)goto 1
                           if(ns.eq.1)aff(i,j)=0   

                        ddxx=sn(j)*rad*(x(i+1)-x(i))
                        ddyy=dy*rad 
                        
                        cx=1./2./dt/ddxx
                           cy=1./2./dt/ddyy

c                       print*,ddxx,ddyy,cx,cy
c                           pause 44


                        uf(i,j)=0.               
                                                                    

                        vf(i,j)=0.                                    
                                               
                                               

      
      
      iit=(it(i,j)-4)*(it(i,j)-5)*(it(i,j)-6)*it(i,j)
c      iit=(it(i,j)-4)*(it(i,j)-6)*it(i,j)
      if(iit.eq.0)then
******************************************************
************** inner points
        snn=sn(j+1)/2+sn(j)/2
        dx=x(i+1)-x(i)
        hh=zw(kp(i,j))/rad/rad/snn

        ij=((iss-i)+(j-1))*((jss-j)+(iss-i))
        hpar=(i-1)*(ku(i,j)-1)
         if(hpar.le.0.)then
              aw(i,j)=0.
                                                    aww(i,j)=0.
               else
                     dxw=x(i+1)/2-x(i-1)/2
                     aw(i,j)=-hh/snn/dxw/dx
                                                    aww(i,j)=-cx*uf(i,j)
                     if(i.eq.iss.and.ku(is,j).gt.1)then
                      aw(i,j)=0.
                                                 aww(i,j)=0.
                      if(ij.eq.0)aw(i,j)=1.
                                                 if(ij.eq.0)aww(i,j)=1.
 
                      endif
             end if

        ij=((i-1)+(jss-j))*((j-1)+(i-1))
c        hpar=(iss-i)*hu(i+1,j)*(it(i+1,j)-2)      !!!!!
        hpar=(iss-i)*(ku(i+1,j)-1)      !!!!!
          if(hpar.le.0.)then
                ae(i,j)=0.
                                                    aee(i,j)=0.
                 else
                     dxe=x(i+2)/2-x(i)/2
                     ae(i,j)=-hh/snn/dxe/dx
                                                    aee(i,j)=cx*uf(i,j)
                    if(i.eq.1.and.ku(1,j).gt.1)then
                        ae(i,j)=0.
                                                    aee(i,j)=0.
                        if(ij.eq.0)ae(i,j)=1.
                                                 if(ij.eq.0)aee(i,j)=1.

                        end if
        end if

        ij=((i-1)+(jss-j))*((jss-j)+(iss-i))
c        hpar=(j-1)*hv(i,j)*(it(i,j-1)-2)           !!!!!!
        hpar=(j-1)*(kv(i,j)-1)           !!!!!!
              if(hpar.le.0.)then
                  as(i,j)=0.
                                                 ass(i,j)=0.  
                    else
                       as(i,j)=-hh*sn(j)/dy/dy
                                                 ass(i,j)=-cy*vf(i,j)
                       if(j.eq.jss.and.kv(i,js).gt.1)then
                              as(i,j)=0.
                                                    ass(i,j)=0.

                            if(ij.eq.0)as(i,j)=1.
                                                 if(ij.eq.0)ass(i,j)=1.

                            endif
        endif

        ij=((i-1)+(j-1))*((j-1)+(iss-i))
        hpar=(jss-j)*(kv(i,j+1)-1)
            if(hpar.le.0.)then
                  an(i,j)=0.
                                                    ann(i,j)=0.
                    else
                      an(i,j)=-hh*sn(j+1)/dy/dy
                                                    ann(i,j)=cy*vf(i,j)  
                      if(j.eq.1.and.kv(i,1).gt.1)then
                            an(i,j)=0.
                                                    ann(i,j)=0.
                            if(ij.eq.0)an(i,j)=1.
                                                 if(ij.eq.0)ann(i,j)=1.

                           endif
        endif

        ap(i,j)=-(an(i,j)+as(i,j)+ae(i,j)+aw(i,j))
     *               +ct/980/dt/dt
      af(i,j)=
     * -fu(i,j,ks)
c     *        +((2.*ph(i,j)-pht(i,j))/dt/dt
     *        + ph(i,j)/dt/dt*ct/980.
     #        +aff(i,j) 



       if(ap(i,j).gt.0.)then
              aw(i,j)=(aw(i,j)+aww(i,j))/ap(i,j)
              ae(i,j)=(ae(i,j)+aee(i,j))/ap(i,j)
              an(i,j)=(an(i,j)+ann(i,j))/ap(i,j)
              as(i,j)=(as(i,j)+ass(i,j))/ap(i,j)
              af(i,j)=af(i,j)/ap(i,j)
              ap(i,j)=1.
              endif

******************************************************
******** outer liquid points
       elseif(it(i,j).eq.2)then
              an(i,j)=0.
              as(i,j)=0.
              aw(i,j)=0.
              ae(i,j)=0.
              ap(i,j)=1.

                                               aww(I,J)=0.
                                               aee(I,J)=0.
                                               ann(I,J)=0.
                                               ass(I,J)=0.  

       endif
      

      

       if(i.ne.1.and.j.ne.1)then
  
                aff(i,j)=cx*(uf(i,j)*(ph(i+1,j)-ph(i-1,j))/(-980))
     #             +cy*(vf(i,j)*(ph(i,j+1)-ph(i,j-1))/(-980))
       endif

 1    continue


c       print *,aff(100,40)
c         pause 22

      return
      end
       subroutine coefNL(u,v,ut,vt,w,fu,fv,us,tax,tay,thx,thy,
     #ph,pht,ro,ahz,an,as,aw,ae,ap,af,phtt,
     #im,jm,km,
     #al,al1,cph,cu,cadv,cq,ct,cstep,ns)
        include 'par.inc'
        include 'com.inc'
      real
     #   fv(im,jm,km),fu(im,jm,km),us(im,jm,km),ffu(im,jm,km)
     *     ,thx(im,jm),thy(im,jm) 
     *     ,u(im,jm,km),v(im,jm,km),tax(im,jm),tay(im,jm)
     *     ,ut(im,jm,km),vt(im,jm,km),ahz(im,jm,km)
     *     ,w(im,jm,km),ro(im,jm,km),ph(im,jm),pht(im,jm)
     *     ,ae(im,jm),aw(im,jm),as(im,jm),an(im,jm)
     #     ,ap(im,jm),af(im,jm),phtt(im,jm)


      do 100 i=1,im
      do 100 j=1,jm
             as(i,j)=0.
             an(i,j)=0.
             aw(i,j)=0.
             ae(i,j)=0.
             ap(i,j)=0.
 100         af(i,j)=0.

        l=1   !!!!  l=0  -> правая часть (rppw) ->  -dw/dz    !!!! 06.04.03
        

       call rppw(u,v,ut,vt,w,fu,fv,us,tax,tay,thx,thy,
     # ph,ro,ahz,im,jm,km,l,al1,cph,cu,cadv,cq,cstep,pht)

     
      continue
      do 1 i=1,iss
      do 1 j=1,jss


        ke=kp(i,j)
      if(ke.le.1)goto 1
      iit=(it(i,j)-4)*(it(i,j)-5)*(it(i,j)-6)*it(i,j)
c      iit=(it(i,j)-4)*(it(i,j)-6)*it(i,j)
      if(iit.eq.0)then
******************************************************
************** inner points
        snn=sn(j+1)/2+sn(j)/2
        dx=x(i+1)-x(i)
        hh=zw(kp(i,j))/rad/rad/snn

        ij=((iss-i)+(j-1))*((jss-j)+(iss-i))
        hpar=(i-1)*(ku(i,j)-1)
         if(hpar.le.0.)then
              aw(i,j)=0.
               else
                     dxw=x(i+1)/2-x(i-1)/2
                     aw(i,j)=-hh/snn/dxw/dx
                     if(i.eq.iss.and.ku(is,j).gt.1)then
                      aw(i,j)=0.
                      if(ij.eq.0)aw(i,j)=1.
                      endif
             end if

        ij=((i-1)+(jss-j))*((j-1)+(i-1))
c        hpar=(iss-i)*hu(i+1,j)*(it(i+1,j)-2)      !!!!!
        hpar=(iss-i)*(ku(i+1,j)-1)      !!!!!
          if(hpar.le.0.)then
                ae(i,j)=0.
                 else
                     dxe=x(i+2)/2-x(i)/2
                     ae(i,j)=-hh/snn/dxe/dx
                    if(i.eq.1.and.ku(1,j).gt.1)then
                        ae(i,j)=0.
                        if(ij.eq.0)ae(i,j)=1.
                        end if
        end if

        ij=((i-1)+(jss-j))*((jss-j)+(iss-i))
c        hpar=(j-1)*hv(i,j)*(it(i,j-1)-2)           !!!!!!
        hpar=(j-1)*(kv(i,j)-1)           !!!!!!
              if(hpar.le.0.)then
                  as(i,j)=0.
                    else
                       as(i,j)=-hh*sn(j)/dy/dy
                       if(j.eq.jss.and.kv(i,js).gt.1)then
                              as(i,j)=0.
                              if(ij.eq.0)as(i,j)=1.
                            endif
        endif

        ij=((i-1)+(j-1))*((j-1)+(iss-i))
        hpar=(jss-j)*(kv(i,j+1)-1)
            if(hpar.le.0.)then
                  an(i,j)=0.
                    else
                      an(i,j)=-hh*sn(j+1)/dy/dy
                      if(j.eq.1.and.kv(i,1).gt.1)then
                            an(i,j)=0.
                            if(ij.eq.0)an(i,j)=1.
                           endif
        endif

        ap(i,j)=-(an(i,j)+as(i,j)+ae(i,j)+aw(i,j))
     *               +1./980./dt/dt                        !!!!!! *l  06.04.03

       !!!!!!!!!!!!!!!!!!!!!!!!  коррекция из-за колебаний уровня   !!!!!!!
      
      ww=w(i,j,1)
c      dzw=zw(2)-zw(1)
c         dzi=ph(i,j)/980.
c         d=abs(dzi)
c      if(d.gt.dzw.and.dzi.lt.0.)ww=w(i,j,3)
c      if(d.lt.dzw.and.dzi.lt.0.)ww=w(i,j,2)
     
      af(i,j)=

     * -fu(i,j,ks)

c     *-(-ffu(i,j,ks)-(pht(i,j)-phtt(i,j))/dt**2/980.) !NL->uf(dz/dx)+vf(dz/dy)явно!

c     *-(-ww/dt-(pht(i,j)-phtt(i,j))/dt**2/980.)*.2 !NL->uf(dz/dx)+vf(dz/dy)явно!

     *        + ph(i,j)/dt/dt/980. 

       if(ap(i,j).gt.0.)then
              aw(i,j)=aw(i,j)/ap(i,j)
              ae(i,j)=ae(i,j)/ap(i,j)
              an(i,j)=an(i,j)/ap(i,j)
              as(i,j)=as(i,j)/ap(i,j)
              af(i,j)=af(i,j)/ap(i,j)
              ap(i,j)=1.
              endif

******************************************************
******** outer liquid points
       elseif(it(i,j).eq.2)then
              an(i,j)=0.
              as(i,j)=0.
              aw(i,j)=0.
              ae(i,j)=0.
              ap(i,j)=1.
 

      endif
 1    continue

      return
      end

