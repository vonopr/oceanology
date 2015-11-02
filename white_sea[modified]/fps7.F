      function vt1(v,vt,im,jm,km,i,j,k,li)
         
        include 'par.inc'
        include 'com.inc'
     
           

      real v(im,jm,km),vt(im,jm,km)

        c1=1.*li+acs(j)*(1-li)
        c2=1.*li+acs(j+1)*(1-li)
       iv=(it(i,j)-5)*it(i,j)
       iv1=(it(i,j)-2)*(it(i,j)-9)
       iv2=(it(i,j)-4)*(it(i,j)-6)
       if(iv1.eq.0.and.it(i-1,j).eq.1)then
         iv1=-1
        iv=0
        end if
      if(iv.eq.0)then

      vt1=(v(i,j,k)+v(i-1,j,k)+vt(i,j,k)+vt(i-1,j,k))*c1/8
     *  +(v(i,j+1,k)+v(i-1,j+1,k)
     *  +vt(i,j+1,k)+vt(i-1,j+1,k))*c2/8

        elseif(iv1.eq.0)then

c          vt1=(v(i-1,j,k)+vt(i-1,j,k))*c1/4
c     *     +(v(i-1,j+1,k)+vt(i-1,j+1,k))*c2/4
         ci1=1.
c      if(v(i-1,j,k).eq.0.)ci1=0.
         ci2=1.
c      if(v(i,j+1,k).eq.0.)ci2=0.
       if(it(i,j+1).eq.0)ci2=0.
c      vt1=0.
       vt1=                  !!!!(ci1*(v(i-1,j,k)+vt(i-1,j,k))*c1/2.  !!!m.9.10
     *     +(v(i,j+1,k)+vt(i,j+1,k))*c2/2.           !!!(ci1+ci2)
c         if((ci1+ci2).eq.0.)write(6,*)'cii1112',i,j,k

         elseif(iv2.eq.0)then

      vt1=(v(i,j,k)+vt(i,j,k))*c1/4.
     *  +(v(i,j+1,k)+vt(i,j+1,k))*c2/4.
           endif

      return
      end

 
      function ut1(u,ut,im,jm,km,i,j,k,li)
        include 'par.inc'
        include 'com.inc'
      

       real u(im,jm,km),ut(im,jm,km)


      iv=(it(i,j)-4)*it(i,j)
      iv1=(it(i,j)-5)*(it(i,j)-6)
      iv2=(it(i,j)-7)*(it(i,j)-9)
      acsn=(acs(j)+acs(j+1))/2.*(1-li)+li
      acss=(acs(j)+acs(j-1))/2.*(1-li)+li

      if(iv.eq.0)then
      ut1=(u(i,j,k)+u(i+1,j,k)+ut(i,j,k)+ut(i+1,j,k))/8.*acsn
     # +(u(i,j-1,k)+u(i+1,j-1,k)
     #+ut(i,j-1,k)+ut(i+1,j-1,k))/8.*acss
c        if(j.eq.41)then
c       print *,'u(i,j,k),u(i+1,j,k),u(i,j-1,k),u(i+1,j-1,k)',
c     &i,j,k,u(i,j,k),u(i+1,j,k),u(i,j-1,k),u(i+1,j-1,k) 
c       print *,'ut(i,j,k),ut(i+1,j,k),ut(i,j-1,k),ut(i+1,j-1,k)',
c     &i,j,k,ut(i,j,k),ut(i+1,j,k),ut(i,j-1,k),ut(i+1,j-1,k)
c         end if 
           elseif(iv1.eq.0)then
      ut1=(u(i,j,k)+u(i+1,j,k)+ut(i,j,k)+ut(i+1,j,k))/4.*acsn
c      ut1=((u(i,j-1,k)+ut(i,j-1,k))*acss+(u(i+1,j,k)+ut(i+1,j,k))
c     &*acsn)/4.
c
c       if(it(i-1,j-1).eq.1)ut1=.5*(u(i+1,j,k)+ut(i+1,j,k))*acsn
c         elseif(iv2.eq.0)then

c      ut1=(u(i,j-1,k)+u(i+1,j-1,k)+ut(i,j-1,k)+ut(i+1,j-1,k))/4.*acss

       endif
        return
        end

      subroutine ww(u,v,ut,vt,w,fu,fv,us,tax,tay,thx,thy,
     #ph,ro,ahz,im,jm,km,
     #al1,cph,cu,cadv,cq,cstep,pht)
        include 'par.inc'
        include 'com.inc'
      real
     *      fu(im,jm,km), fv(im,jm,km),us(im,jm,km)
     *     ,thx(im,jm),thy(im,jm)
     *     ,u(im,jm,km),v(im,jm,km),tax(im,jm),tay(im,jm)
     *     ,ut(im,jm,km),vt(im,jm,km),ahz(im,jm,km)
     *   ,w(im,jm,km),ro(im,jm,km),ph(im,jm),pht(im,jm)

*******    w -calculation:
*******
*******      dw/dt +aga*w =int_h_z (div(rpu,rpv))+al*delta w
*******      here  du/dt=rpu
*******            dv/dt=rpv
*******
****** w - calculation *************************


       GOTO 24   !!!!!!!!   W  ->  »« ”–¿¬Õ≈Õ»ﬂ Õ≈–¿«–€¬ÕŒ—“»


       l=1  !!!!! l=1  Ô‡‚‡ˇ ˜‡ÒÚ¸ ->  +div(ut,vt,,,,,)

      call rppw(u,v,ut,vt,w,fu,fv,us,tax,tay,thx,thy,
     #ph,ro,ahz,im,jm,km
     #,l,al1,cph,cu,cadv,cq,cstep)

      do 1 i=2,iss
      do 1 j=2,jss
        ke=kp(i,j)
      iit=(it(i,j)-5)*it(i,j)

      if(iit.eq.0)then
       do  k=1,ke
           
      us(i,j,k)=fu(i,j,ke)- fu(i,j,k)   
                        !!!! in rppw '+w(i,j,k)/dt' have been  included
                        !!!! w=div(ut,vt,,,, 
       enddo


      do k=kp(i,j),ks 
      us(i,j,k)=0.
      enddo

      endif
 1    continue



      do 2 i=2,iss                             !!!!!!!!!!!!!  WR   !!!!!!!!!
      do 2 j=2,jss                      
      do 2 k=1,kp(i,j)                        


      w(i,j,k)=us(i,j,k)*dt                    !!!!!!!!!!!!!! WR !!!!!!!!!!!   
            
c      if(k.eq.1)ph(i,j)=ph(i,j)+w(i,j,1)*dt


 2    continue




24     continue


      

      do 22 i=2,iss
        do 22 j=2,jss

        ke=kp(i,j)
        if(ke.le.2.or.it(i,j).eq.1)goto 22

        w(i,j,ke)=0.
      
       zw(1)=-Ph(i,j)/980.   !!!!!  ”◊≈“  ŒÀ≈¡¿Õ»… ”–Œ¬Õﬂ
       z(1)=zw(1)
       z(2)=(zw(1)+zw(2))/2.
                
        do 122 k=ke,2,-1

      dzw=zw(k)-zw(k-1)

      diva=div(u,v,i,j,k,im,jm,km)
      w(i,j,k-1)=w(i,j,k)+diva*(dzw)   !!!!!!!!!!!!!  R
122   continue

       zw(1)=0.    !!!!!!   LEVEL   MOD.
       z(1)=zw(1)
       z(2)=(zw(1)+zw(2))/2.
       z(2)=(zw(1)+zw(2))/2.
22    continue

      continue


      RETURN
      END


       function zint(a,i,j,k1,k2,im,jm,ka)
        include 'par.inc'
        include 'com.inc'
      dimension a(im,jm,k2),ka(im,jm)
      zint=0.
      do 1 k=k1+1,k2
      if(k.le.ka(i,j))then
      dz=z(k)-z(k-1)
c      elseif(z(k-1).lt.ha(i,j))then
c      dz=ha(i,j)-z(k-1)
      else
      dz=0.
      endif
      zint=zint+(a(i,j,k)+a(i,j,k-1))/2.*dz
  1   continue
      return
      end

       function zintw(a,i,j,k1,k2,im,jm,kt,za)
      
          include 'par.inc'
        include 'com.inc'
      dimension a(im,jm,k2),za(k2),kt(im,jm)

c         if(i.eq.120.and.j.eq.15)print*,'',kt(i,j)
c        if(i.eq.120.and.j.eq.15)pause
        ke=kt(I,J)
      

      zintw=0.
      do 1 k=k1+1,k2
      if(k.lt.ke)then
      dz=za(k)-za(k-1)
c      elseif(za(k-1).lt.ha(i,j))then
c      dz=ha(i,j)-za(k-1)
      else
      dz=0.
      endif
      zintw=zintw+a(i,j,k)*dz
c        if(i.eq.120.and.j.eq.15.and.k.eq.2)print*,'zintw in zintw',zintw
c        if(i.eq.120.and.j.eq.15.and.k.eq.2)pause
  1   continue
    
      return
      end




       subroutine intw(a,b,i,j,k1,k2,im,jm,km,ph)
        include 'par.inc'
        include 'com.inc'
      dimension a(im,jm,km),b(im,jm,km),ph(im,jm)



           !!!!!!  HAVE TO BE:    k1=1    and   dzeta  <  or   = zw(3)

       zw(1)=-ph(i,j)/980.   !!!!!  ”◊≈“  ŒÀ≈¡¿Õ»… ”–Œ¬Õﬂ
       z(1)=zw(1)
       z(2)=(zw(1)+zw(2))/2.

           
      do 2 k=1,k1                                 ! WR  -> everythihg

 2    b(i,j,k)=0.     !!!! U,V begin from k=2     ! WR 
      
      do 1 k=k1+1,k2                              ! WR 
            
      dz=zw(k)-zw(k-1)                            ! WR 
        
      b(i,j,k)=b(i,j,k-1)+a(i,j,k)*dz             ! WR

  1   continue

       zw(1)=0.      !!!!!!   LEVEL    MOD.
       z(1)=zw(1)
       z(2)=(zw(1)+zw(2))/2.
 
      return
      end


       subroutine intr1(a,b,i,j,k2,im,jm,km)
****** for vel.calculation, all T-S variables are in the box's centres
        include 'par.inc'
        include 'com.inc'

      dimension a(im,jm,km),b(im,jm,km)

      b(i,j,1)=0.
      
      do 1 k=2,k2

      dz=zw(k)-zw(k-1)           
            
      b(i,j,k)=b(i,j,k-1)+a(i,j,k)*dz
      
  1   continue
      return
      end


       subroutine intrA(a,b,i,j,k2,im,jm,km,dzi)                        ! Proved: called  in 'fps4.f' once and in 'fps6.f' twice only.
        include 'par.inc'
        include 'com.inc'

      dimension a(im,jm,km),b(im,jm,km)

      b(i,j,1)=dzi
                                
      do 1 k=2,k2

      dz=zw(k)-zw(k-1)           
            
      b(i,j,k)=b(i,j,k-1)  +(1.+a(i,j,k))*dz*980.  !+dz*par
      
  1   continue
      return
      end

         subroutine udzmax(u,v,ph,timestep)
          include 'par.inc'
           include 'com.inc'
            real u(in,jn,ks),v(in,jn,ks),ph(in,jn)
           DZMAX=-100000000.
          DZMIN=100000000.
         sdz=0.
        ps=0.
       DO 25 J=1,jn
      DO 25 I=1,in
c       print *,'i,j,dz',i,j,ph(i,j)
        IF(ph(I,J).EQ.0.)GO TO 25                                         
         SDZ=SDZ+ph(I,J)                                                   
          PS=PS+1.
         IF(ph(I,J).le.DZMAX)go to 701
        DZMAX=ph(I,J)                  
       imx=i
       jmx=j
701     IF(ph(I,J).ge.DZMIN)go to 25
         DZMIN=ph(I,J)
        imn=i
       jmn=j
   25 CONTINUE                               
         sdz=sdz/ps                           
        umm=0.
       do j=1,jn
        do i=1,in
c       print *,'i,j,uv',i,j,k,u(i,j,2),v(i,j,2)
       do k=1,ks
      UV=SQRT(u(i,j,K)*u(i,j,K)+v(i,j,K)*v(i,j,K))
      IF(ABS(UV).LE.UMM)GO TO 22
       KM=K                                                
        IM=I                                                
         JM=J                                                   
        UMX=u(i,j,k)                                                     
       VMX=v(i,j,k)                                                     
      UMM=UV                    
22     continue
        end do
        end do
       end do
      day=timestep/(24.*60.)
      hr=timestep/60.-int(day)*24.
        minu=int(timestep-60.*aint(timestep/60.))
      RETURN
      END        
        
                                                                                                        
      SUBROUTINE HDRL(a,at,ta,qt,u,v,w,us,ahz,
     *                 aq1,pr,wt,im,jm,km,
     #                 a1,b1,c1,f1,a2,b2,cstep,azz,l0,ph,lp)


******************************************************************
comment:  t  - s    calculation
*******   1.  adv  -  advection terms
*******   2.  diff  -  diffusion terms
*******  here aq,qq - diffusion coefficients
******************************************************************

        include 'par.inc'
        include 'com.inc'
        common ns
      dimension ph(im,jm),
     *          a(im,jm,km),at(im,jm,km),ta(im,jm),qt(im,jm)
     *         ,u(im,jm,km),v(im,jm,km),w(im,jm,km)
     #         ,ahz(im,jm,km),us(im,jm,km),a2(km),pr(im,jm,km)
     #         ,a1(km),b1(km),c1(km),f1(km),azz(km),b2(km)
     !
     !         ,tw(kn),tw1(kn),adz(kn)

c      call taflux(a,ta,qt,qwa,im,jm,1)


      cstep1=cstep
      us=0.
      resm=0.
     
c         do i=2,is
c         do j=2,js
c         do k=2,ks

c        ua(i,j,k)=u(i,j,k)*(a(i,j,k)+a(i-1,j,k))/2.
c        va(i,j,k)=v(i,j,k)*(a(i,j,k)+a(i,j-1,k))/2.
c        wa(i,j,k-1)=w(i,j,k-1)*(a(i,j,k)+a(i,j,k-1))/2.

c      enddo
c      enddo
c      enddo


ccc        goto 352  

      do  1 i=2,iss
      do  1 j=jss,2,-1


       zw(1)=-Ph(i,j)/980.   !!!!!  ”◊≈“  ŒÀ≈¡¿Õ»… ”–Œ¬Õﬂ
         z(1)=zw(1)
       z(2)=(zw(1)+zw(2))/2.
        
      ke=kp(i,j)

      iit=it(i,j)*(it(i,j)-5)


      if(iit.eq.0)then


!!!!!!!!!!!!!!!!!!!!!!!!!!  smoothing w*t adv z !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        goto 79
       do k=2,ke
        tw1(k)= a(i,j,k)*(w(i,j,k-1)+w(i,j,k))/2.
       enddo
        tw1(1)=a(i,j,1)*w(i,j,1)

       do k=2,ke
        tw(k)=tw1(k)*(zw(k)-zw(k-1))+tw1(k+1)*(zw(k+1)-zw(k))
       enddo
        tw(1)=a(i,j,1)*w(i,j,1)*zw(2)

       do k=2,ke
        dz=zw(k+1)-zw(k)
        dz1=zw(k+1)-zw(k-1)

        adz(k)= (tw(k+1)/dz1 - tw(k)/dz1)/dz
       enddo

        adz(1)=adz(2)
79    continue
!!!!!!!!!!!!!!!!!!!!!!!!!!  smoothing w*t !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      do  2 k=2,ke

      if(it(i,j).eq.2)a(i,j,k)= outf(a(i,j,k),a(i-1,j+1,k),at(i-1,j+1,k)
     #,a(i-2,j+2,k),a(i+1,j-1,k),1.,i,j,k,ns)
     
           
      wu=w(i,j,k-1)                      
      wm=w(i,j,k)           




       
      difuu= -delta(at,k,i,j,im,jm,km,1)*aq1      
      a2(k)=difuu




      aa= adv1(a,u,v,i,j,k,im,jm,km)
      a2(k)=a2(k)+aa



      aa= advz(a,wu,wm,i,j,k,im,jm,km,ph)
      a2(k)=a2(k)+aa



      continue


 
      continue


      continue

      continue  !!!!!!!!!!!!!!!LEITH!!!!!!!!!!!!!!!!!!!!!!!!!!!! 11.11.03.
       
      a2(k)=a2(k)-at(i,j,k)/dt 
        
      azz(k)=ahz(i,j,k)*pr(i,j,k)

      b2(k)=0.




      if(lp.eq.1)   a(i,j,k)=-a2(k)*dt    !ˇ‚Ì‡ˇ ÒıÂÏ‡!     new

 2    continue

      if(lp.eq.1)      goto 10        !ˇ‚Ì‡ˇ ÒıÂÏ‡!   !    new    
    
                                         

      a2(1)=-Ta(i,j)*wt

      b2(1)=-wt

      lb=2 ! (b.c.- flux to the bottom (=0.))
ccc        if(it(i,j).ne.5)lb=2       

      call zcoef(azz,qt(i,j),0.,a2,b2,a1,b1,c1,f1,l0,lb,ke,km)
           

      call abc(a,a1,b1,c1,f1,a2,b2,l0,ke,i,j,im,jm,km)

c       qt(i,j)=(Ta(i,j)-a(i,j,1))*wt
c        if(l0.eq.2)a(i,j,1)=a(i,j,2)



 10    continue

      a(i,j,ke+1)=a(i,j,ke) 


      endif

       zw(1)=0.   !!!!!!   LEVEL    MOD.
       z(1)=zw(1)
       z(2)=(zw(1)+zw(2))/2.


  1   continue

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      continue





             goto 913  !!!!  Leith  off

       c=0.99
                          
        do  i=2,im-1
        do  j=2,jm-1

      if(it(i,j).ne.0.or.it(i,j).ne.5)goto 1000

       zw(1)=-Ph(i,j)/980.   !!!!!  ”◊≈“  ŒÀ≈¡¿Õ»… ”–Œ¬Õﬂ
       z(1)=zw(1)
       z(2)=(zw(1)+zw(2))/2.

      

        do  k=2,kp(i,j)

        call LeithC(a,at,u,v,w,i,j,k,im,jm,km,0.) !  at  normaly  

      a(i,j,k)=(a(i,j,k)*c +at(i,j,k)*(1-c)) 

      enddo



       zw(1)=0.   !!!!!!   LEVEL    MOD.
       z(1)=0.




1000   continue
        enddo
        enddo

913   continue


c       print*,'a1,a2,at1',a(60,60,1),a(60,60,2),at(60,60,1)
c         pause 000


!!!!!!!!!!!!!!!!!!!!!   RAD    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      goto 349

        do  i=2,iss
        do  j=2,jss

c        if(it(i,j).ne.1)a(i,j,1)=a(i,j,2)

      if(it(i,j).eq.2)then

        do  k=1,kp(i,j)

       if(k.gt.kp(i,j))goto 347

      a(i,j,k)=(a(i-1,j+1,k)+a(i,j-1,k))/2.

        if(v(i-1,j+1,k).gt.0.)a(i,j,k)=a(i,j-1,k)




     

        if(k.eq.kp(i,j)) a(i,j,k+1)=a(i,j,k)
      
             enddo

        endif

347   continue

  
        enddo
        enddo
349   continue

      return
        end




      function umax1(u,v,im,jm,km,k)
        include 'par.inc'
        include 'com.inc'
      real u(im,jm,km),v(im,jm,km)
      umax1=0.
      do 1 i=1,im
      do 1 j=1,jm
      uuu=u(i,j,k)*u(i,j,k)+v(i,j,k)*v(i,j,k)
      uu=sqrt(uuu)
      if(umax1.le.uu)umax1=uu
 1    continue
      return
      end

      function umm1(u,v,im,jm,km,k)
        include 'par.inc'
        include 'com.inc'
      real u(im,jm,km),v(im,jm,km)
      aaa=0.
      umm1=0.
      do 1 i=1,im
      do 1 j=1,jm
      uuu=u(i,j,k)*u(i,j,k)+v(i,j,k)*v(i,j,k)
      uu=sqrt(uuu)
      umm1=umm1+uu
      if(kp(i,j).gt.1)aaa=aaa+1.
 1    continue
      if(aaa.ne.0.)umm1=umm1/aaa
      return
      end


      subroutine av(p,pt,im,jm,km,k1,k2,avp)                        ! Proved: 'av' is called in 'fps4.f' in subroutine 'Ravtime' only. It's absence in fps4.f does not affect first 24 days of calculations.
        include 'par.inc'
        include 'com.inc'

      real pt(im,jm,km),avp(km),p(in,jn,kn)


      do 3 k=k1,k2

        avp(k)=0.
      n1=0

      do 1 i=1,im
      do 1 j=1,jm

      pt(i,j,k)=0

      ke=kp(i,j)
      

      if(k.gt.ke)goto 1                            

      avp(k)=avp(k)+p(i,j,k)
      n1=n1+1

 1    continue

        if(n1.ge.1)avp(k)=avp(k)/n1

3     continue

      
      do 2 i=1,iss
      do 2 j=1,jss
      do 2 k=k1,k2

      if(k.gt.kp(i,j))then                                
      pt(i,j,k)=0.
      else
      pt(i,j,k)=p(i,j,k)-avp(k)
      endif

 2    continue
      return
      end

