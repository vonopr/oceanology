      subroutine frfo(frf,pr,fu,fv,alz,rfcr,i,j,ke,im,jm,km)

****  pr(i,j,k)=1/prt , prt -Prandtl-Shmidt parametre

        include 'par.inc'
        include 'com.inc'
      real frf(im,jm,km),fu(im,jm,km),fv(im,jm,km),pr(im,jm,km)
        frf(i,j,1)=0.09
        pr(i,j,1)=1.
      do 1  k=2,ke
      pr(i,j,k)=1.
      frf(i,j,k)=alz

      if(fu(i,j,k).gt.1.e-16)then

      ri=fv(i,j,k)/fu(i,j,k)

      if(ri.gt.0.)pr(i,j,k)=(1.+5*ri)/(1.+5.77*ri)**2
      if(pr(i,j,k).lt.0.1)pr(i,j,k)=0.1

      rf=ri*pr(i,j,k)
        if(rf.lt.rfcr)then
         frf(i,j,k)=alz*(1.-rf/rfcr)/(1.-rf)/pr(i,j,k)
          else
          frf(i,j,k)=0.
         endif

      endif

  1   continue




      return
      end
 
      subroutine ture(u,v,fu,fv,frf,tax,tay,pr,ef,
     # ahz,azz,a3,b3,c3,ffu,d2,e2,
     # tb,te,ro,qt,qb,tbt,tet,im,jm,km,z0,tb0,te0,
     # pare,alz,prb,rfcr,qbh,qeh,drdt,
     # l0b,lbb,l0e,lbe,
     # alb)

        include 'par.inc'
        include 'com.inc'

      real tb(im,jm,km),te(im,jm,km), ro(im,jm,km)
     *      ,ahz(im,jm,km),u(im,jm,km),v(im,jm,km)
     *      ,tbt(im,jm,km),tet(im,jm,km)
     *      ,fu(im,jm,km),fv(im,jm,km) 
     *      ,frf(im,jm,km),pr(im,jm,km)

     #       ,a3(km),b3(km),c3(km),ef(km)
     *      ,ffu(km),d2(km),e2(km),azz(km)
       
     *      ,pare(5),qb(im,jm),qt(im,jm)
     *      ,tax(im,jm),tay(im,jm)

      qe=0.

      do 1 i=1,im
      do 1 j=1,jm
        iv=(it(i,j)-2)*(it(i,j)-5)*(it(i,j)-4)*  !!!!!  delete it=2 ??
     #   (it(i,j)-6)*it(i,j)
       ke=kp(i,j)

      if(ke.lt.2.or.iv.ne.0)goto 1

        ef(1)=1.
       do 11 k=2,ke


       ef(k)=(zw(ke)-z(k))*z(k) 
  11  continue
 
       call s2(u,v,tax,tay,fu,i,j,i-1,j-1,
     #               ke,im,jm,km,z0)
 
      call n2(ro,qt(i,j),pr,fu(i,j,1),fv,i,j,ke,im,jm,km,drdt)     
 
      call frfo(frf,pr,fu,fv,alz,rfcr,i,j,ke,im,jm,km)
 
ccc      call frfc(frf,pr,fu,fv,i,j,ke,im,jm,km)  !!!!  was  'c'

      do k=1,ke
      fv(i,j,k)=fv(i,j,k)*pr(i,j,k)
      tbt(i,j,k)=tb(i,j,k)
      tet(i,j,k)=te(i,j,k)
      azz(k)=ahz(i,j,k)
      enddo

 
          k1=2

      ud2=udin2(u,v,z0,i,j,im,jm,km,ke)
        udatm=tax(i,j)**2+tay(i,j)**2
      if(l0b.lt.0) qb(i,j)=udatm/alz**0.5

      if(lbb.lt.0) qbh=ud2/0.3
 
      do k=1,ke
      azz(k)=ahz(i,j,k)/prb
      enddo

       call tbs(tbt,tb,te,azz,fu,fv,frf,ef,
     #  a3,b3,c3,ffu,d2,e2,tb0,qb(i,j),qbh,i,j,
     #  im,jm,km,l0b,lbb,ke,alb)

 
ccc      if(l0e.lt.0)qe=(alz**0.75*tb(i,j,2)**1.5*(zw(ke)-z(2)))/0.435  !!!  OLD
      if(l0e.lt.0)qe=(alz**0.75*tb(i,j,2)**1.5*(zw(5)-zw(1)))/0.435    !!!! NEW 


      if(lbe.lt.0)  qeh=(ud2**1.5*zw(ke))/0.435
 

      do k=1,ke
      azz(k)=ahz(i,j,k)/pare(4)
      enddo
      call tes(te,tet,tb,azz,fu,fv,frf,ef,
     #  a3,b3,c3,ffu,d2,e2,te0,qe,qeh,i,j,im,jm,km,l0e,
     #  lbe,ke,pare,alb)

      call turb(tb,te,ef,frf,ahz,i,j,im,jm,km,ke)
         
 1    continue



      return
      end


      subroutine turb(tb,te,ef,frf,ahz,i,j,im,jm,km,ke)
        include 'par.inc'
        include 'com.inc'
      real tb(im,jm,1:km),ahz(im,jm,km),frf(im,jm,km),
     *     te(im,jm,km),ef(km)

      at1=tb(i,j,2)**2/te(i,j,2)*ef(2)*frf(i,j,1)

      ahz(i,j,1)=at1/2.+0.1

      do 2 k=2,ke-1

        frf2=(frf(i,j,k-1)+frf(i,j,k))/2.

      at2=tb(i,j,k+1)**2/te(i,j,k+1)*ef(k+1)*frf2

      dz2=zw(k+1)-zw(k)
      dz1=zw(k)-zw(k-1)



      ahz(i,j,k)=(at1*dz2+at2*dz1)/(dz1+dz2)
     #   +0.1

      at1=at2

  2   continue



      ahz(i,j,ke)=tb(i,j,ke)**2/te(i,j,ke)*ef(ke)
     # *frf(i,j,ke)


      return
      end




      subroutine incbe(tb,te,frf,pr,prt,ahz,ef                          ! Proved: it is called in fps.for only
     #           ,tb0,te0,im,jm,km
     #           ,wt,drdt,pare,alz,z0,rfcr,qb !initial
     #           ,l0t,lbt,l0b,lbb,l0e,lbe,qa,prb,alb)

        include 'par.inc'
        include 'com.inc'

      real
     # tb(im,jm,km),te(im,jm,km),frf(im,jm,km),pr(im,jm,km)
     # ,ahz(im,jm,km),pare(5),ef(km)


1000  format(a11)


      open(5,file='pare.dat')

* pr(i,j,k)=1/pr  pr - Prandtl-Schmidt parameter

         

       rewind 5
       read(5,*) wt,tb0,prt,te0,drdt,alb
       read(5,*)pare,alz,prb,rfcr,z0,qb
       read(5,*) l0t,lbt,l0b,lbb,l0e,lbe
       close(5)

      do 1 i=1,im
      do 1 j=1,jm

      ke=kp(i,j)

      te1=te0*zw(ke)**2/4.
         
      do 11 k=1,km
      ahz(i,j,k)=qa !
        te(i,j,k)=0.
      frf(i,j,k)=0.09
      pr(i,j,k)=1./prt
 11   tb(i,j,k)=0.
         
      if(ke.gt.1)then

      do 2 k=1,km
      te(i,j,k)=te1
  2   tb(i,j,k)=tb0


         endif

 1     continue

        

       format(20e12.6)


        do 4 i=1,im
        do 4 j=1,jm
        ke=kp(i,j)
        do 3 k=1,ke
        ef(k)=(zw(ke)-z(k))*z(k)
3     continue
        if(ke.gt.1)call turb(tb,te,ef,frf,ahz,i,j,im,jm,km,ke)
4     continue
        return
      end

      subroutine tbs(tbt,tb,te,azz,fu,fv,frf,ef,
     #        a1,b1,c1,f1,a2,b2,tb0,
     #        qb,qh,i,j,im,jm,km,l0,lb,ke,alb)

        include 'par.inc'
        include 'com.inc'

      real tbt(im,jm,km),tb(im,jm,km),azz(1:km)
     *    ,a2(1:km),b2(1:km),
     *     a1(1:km),b1(1:km),c1(1:km),f1(1:km)
     *     ,te(im,jm,km),frf(im,jm,km),ef(km)
     *     ,fu(im,jm,km),fv(im,jm,km)


      k1=2
      do 1 k=2,ke

      if(k.eq.ke)then
      fu1=fu(i,j,ke)
      fv1=fv(i,j,ke)
      elseif(k.eq.2)then
      fu1=fu(i,j,1)
      fv1=fv(i,j,1)
      else
      dz1=z(k)-z(k-1)
      dz2=z(k+1)-z(k)
      dz=z(k+1)-z(k-1)

      frf1=(frf(i,j,k)*dz1+frf(i,j,k-1)*dz2)/dz
      fu1=(fu(i,j,k)*dz1+fu(i,j,k-1)*dz2)/dz
      fv1=(fv(i,j,k)*dz1+fv(i,j,k-1)*dz2)/dz
      endif
c      print *,k
c        pause 111
      a2(k)=-(fu1-fv1)
     #    *tb(i,j,k)**2/te(i,j,k)*frf1*ef(k)
     #    -tbt(i,j,k)/dt - alb*DELTA(tbt,k,i,j,im,jm,km,-1) 
c       !ÌÂÔÓÌˇÚÌ‡ˇ ‚ˇÁÍÓÒÚ¸ ‚ ÚÛ·ÛÎÂÌÚÌÓÈ ˜‡ÒÚË - Û·ËÚÓ 1 Ë˛Îˇ 2002
c      print *,'in tbs',a2(k)
c        pause 111

      b2(k)=-te(i,j,k)/tb(i,j,k)/ef(k)

  1   continue


      call zcoef(azz,qb,qh,a2,b2,a1,b1,c1,f1,l0,lb,ke,km)
      call abc(tb,a1,b1,c1,f1,a2,b2,k1,ke,i,j,im,jm,km)

      do 2 k=2,ke
      if(tb(i,j,k).lt.tb0)tb(i,j,k)=tb0


  2   continue

      tb(i,j,1)=tb(i,j,2)
*********************************
      return
      end

      subroutine tes(te,tet,tb,azz,fu,fv,frf,ef,
     #        a1,b1,c1,f1,a2,b2,te0,
     #        qe,qeh,i,j,im,jm,km,l0,lb,ke,pare
     #        ,alb)
  

        include 'par.inc'
        include 'com.inc'

      real te(im,jm,km),tb(im,jm,km),azz(1:km),
     *  a2(1:km),b2(1:km),
     *  a1(1:km),b1(1:km),c1(1:km),f1(1:km),pare(5)
     *  ,tet(im,jm,km),frf(im,jm,km),ef(km)
     *  , fu(im,jm,km),fv(im,jm,km)

       k1=2

      do 1 k=2,ke

      b2(k)=0.
     # -te(i,j,k)/tb(i,j,k)*pare(1)/ef(k)


      if(k.eq.ke)then
      fu1=fu(i,j,ke)
      fv1=fv(i,j,ke)
      elseif(k.eq.2)then
      fu1=fu(i,j,1)
      fv1=fv(i,j,1)
      else
      dz1=z(k)-z(k-1)
      dz2=z(k+1)-z(k)
      dz=z(k+1)-z(k-1)

      frf1=(frf(i,j,k)*dz1+frf(i,j,k-1)*dz2)/dz
      fu1=(fu(i,j,k)*dz1+fu(i,j,k-1)*dz2)/dz
      fv1=(fv(i,j,k)*dz1+fv(i,j,k-1)*dz2)/dz
      endif

      c1ro=pare(2)                   !stable stratification
      if(fv1.lt.0.) c1ro=pare(3)     !unstable

      a2(k)=- (fu1*pare(5) -fv1*c1ro)
     #    *tb(i,j,k)*frf1*ef(k)
     #    -tet(i,j,k)/dt
     #   -frf1/pare(4)*tb(i,j,k)**2*(zw(ke)*zw(ke)/ef(k)-2.)
     #    - alb*DELTA(tet,k,i,j,im,jm,km,-1)
!   Û·ËÚÓ, Í‡Í ‚ tbs


    1   continue


      call zcoef(azz,qe,qeh,
     #         a2,b2,a1,b1,c1,f1,l0,lb,ke,km)



      call abc(te,a1,b1,c1,f1,a2,b2,k1,ke,i,j,im,jm,km)
      te1=te0*zw(ke)**2/4.

      do 2 k=2,ke
      if(te(i,j,k).lt.te1)te(i,j,k)=te1


  2   continue

      te(i,j,1)=te(i,j,2)
*********************************
      return
      end


      function udin2(u,v,z0,i,j,im,jm,km,ke)
        include 'par.inc'
        include 'com.inc'
      real u(im,jm,km),v(im,jm,km)
      u1=(u(i,j,ke)+u(i-1,j,ke))**2/4.
      v1=(v(i,j,ke)+v(i,j-1,ke))**2/4.
      dz=(zw(ke)-z(ke))/z0
      alo=alog(dz)**2
      udin2=(u1+v1)/alo*0.435**2
      return
      end

      subroutine taubot(u1,v1,tb1,thx,thy,z0,ke)

        include 'par.inc'
        include 'com.inc'


      thx=0.
      thy=0.

      dz=zw(ke)-z(ke)
      az=alog(dz/z0)/0.435
      uu=u1**2+v1**2
      uu=sqrt(uu)
      uu=sqrt(tb1*0.3)
c      thx=u1*uu/az/az
c      thy=v1*uu/az/az
      thx=u1*uu/az
      thy=v1*uu/az
c      print * ,thx,thy

      return
      end

      SUBROUTINE HRVL(u,v,ut,vt,w,fu1,fv1,us,tax,tay,thx,thy,           ! Proved: it is called in fps.for once only.
     * ph,ro,im,jm,km,cq,al1,cu,cadv,cph,
     * ahz,fuu,fvv,azz,e2,d2,r2,g2,a3,b3,c3,ua,va,z0,cstep)
 
************************************************************************
comments     bound.cond.   from geostr
********   horisontal velocities u - v  calculation ***********
********   HRVL(ddx,ddy,adv,zint,delta)             ***********
******  1. u =  ut +(diff + diff(horis) - coriolis*(.55vt+.45v)+ dpx+dpro)*dt
******  2. u -  ut = - alf* dxu  + alf**2* d(du),    where   alf= u*dt/dx
************************************************************************

        include 'par.inc'
        include 'com.inc'

      real  v(is,js,ks),u(is,js,ks),
     *      fu1(im,jm,km),fv1(im,jm,km),us(im,jm,km)
     *     ,thx(im,jm),thy(im,jm)
     *     ,tax(im,jm),tay(im,jm)
     *     ,vt(is,js,ks),ut(is,js,ks)
     *     ,w(im,jm,km),ro(im,jm,km),ph(im,jm)
     *     ,e2(km),d2(km),r2(km),g2(km),fuu(km),fvv(km)
     *     ,a3(km),b3(km),c3(km),ua(km),va(km),azz(km)
     *     ,u00(im,jm,km),ahz(im,jm,km)
c          print *,'vel00',rad 


      


 
      dt1=dt
      a1=1/dt
      a2=1/dt


      ut=u
      vt=v




                            
      

      call rpin(u,v,ut,vt,w,fu1,fv1,us,tax,tay,thx,thy,
     # ph,ro,ahz,im,jm,km,al,al1,
     # cph,cu,cadv,cq,cstep)

      

      do 11 j=jss,2,-1
      do 11 i=2,iss

       zw(1)=-ph(i,j)/980.   !!!!!  ”◊≈“  ŒÀ≈¡¿Õ»… ”–Œ¬Õﬂ
       z(1)=zw(1)
       z(2)=(zw(1)+zw(2))/2.

        ke=ku(i,j)

        iv=(it(i,j)-5)*it(i,j)  

      if(iv.eq.0.and.ke.gt.1)then    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         
         tayy=vt1(tay,tay,im,jm,1,i,j,1,1)
         taxx=tax(i,j)

       thyy=vt1(thy,thy,im,jm,1,i,j,1,1)  !!!!!!!!  was  ir,  ?????
       thxx=thx(i,j)

 

         f1=2*omega*(acs(j)+acs(j+1))/2.*(1.-cu)
   
         do 10 k=1,km
             
            fuu(k)=-ut(i,j,k)/dt-fu1(i,j,k)

            fvv(k)=-vt1(vt,vt,im,jm,km,i,j,k,1)/dt
     #      -vt1(fv1,fv1,im,jm,km,i,j,k,1)

            azz(k)=0.5*(ahz(i-1,j,k)+ahz(i,j,k))
            e2(k)=0.

            ua(k)=0.
            va(k)=0.
  10     continue

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  TURB   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!        
      dz=(zw(ke)-z(ke))     !!!  was z(ke)
ccc      dz=200.  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
      alo=alog(dz/z0)**2

      v2=vt1(vt,vt,im,jm,km,i,j,ke,1)
      udin=sqrt(u(i,j,ke)**2+v2**2)/alo*0.435**2

      e2(ke)=-udin/ (zw(ke)-z(ke))   !    !!!was -zw(ke-1) ???
          

      call zcoeu(azz,tayy,taxx,thyy,thxx,
     #    e2,a3,b3,c3,fvv,fuu,2,ke,km)
      

      call abcu(va,ua,a3,b3,c3,fvv,fuu,e2,d2,r2,g2,f1,
     * 2,ke,km) 

c        ua(1)=ua(2)+taxx*z(2)/azz(2)  !!!!!!!!!!!!!  !!!!! v z=0 

      u(i,j,1)=ua(2)  !!!!!!!
         
      do 12 k=2,ke
        if(it(i,j).ne.2.)u(i,j,k)=ua(k)

        if(it(i,j).eq.2.and.u(i,j,k).eq.u00(i,j,k))u(i,j,k)=ua(k)
        if(it(i,j).eq.2.and.it(i-1,j).eq.1)u(i,j,k)=ua(k)
 12   continue

       elseif(ke.gt.1)then
       endif

      thx(i,j)=udin*(u(i,j,ke))                     !!!!!!!!!!!!udin !for tur mod


       zw(1)=0.      !!!!!!   LEVEL    MOD.
       z(1)=zw(1)
       z(2)=(zw(1)+zw(2))/2.

 11   continue
                                                    

      do 21 j=jss,2,-1
      do 21 i=2,iss

       zw(1)=-ph(i,j)/980.   !!!!!  ”◊≈“  ŒÀ≈¡¿Õ»… ”–Œ¬Õﬂ    
       z(1)=zw(1)
       z(2)=(zw(1)+zw(2))/2. 
    
          iv=it(i,j)*(it(i,j)-5)

        ke=kv(i,j)

      if(iv.eq.0.and.ke.gt.1)then   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
***********************inner  points

      taxx=ut1(tax,tax,im,jm,1,i,j,1,1)
      tayy=tay(i,j)

      thxx=ut1(thx,thx,im,jm,1,i,j,1,1)
      thyy=thy(i,j)

      f1=2*omega*acs(j)*(1.-cu)

                                                 

      do 20 k=1,km
      fvv(k)=-vt(i,j,k)/dt-fv1(i,j,k)

      fuu(k)=-ut1(ut,ut,im,jm,km,i,j,k,1)/dt
     #     -ut1(fu1,fu1,im,jm,km,i,j,k,1)

       azz(k)=0.5*(ahz(i,j-1,k)+ahz(i,j,k))
c       if(k.lt.3.and.azz(k).lt.10)azz(k)= 10. 

       e2(k)=0.
       ua(k)=0.
       va(k)=0.
        
 20   continue


                                                        
          ! for turbulent model!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      dz=(zw(ke)-z(ke))   !!! was z(ke)
ccc      dz=200.  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      alo=alog(dz/z0)**2
                                              

      u2=ut1(ut,ut,im,jm,km,i,j,ke,1)
                                      
      udin=sqrt(u2*u2+v(i,j,ke)**2)/alo*0.435**2

      e2(ke)=-udin/ (zw(ke)-z(ke))      !    !!!! was  -zw(ke-1)  ???

                                                                                             
      call zcoeu(azz,tayy,taxx,thyy,thxx,
     #    e2,a3,b3,c3,fvv,fuu,2,ke,km)
                                                     
      call abcu(va,ua,a3,b3,c3,fvv,fuu,e2,d2,r2,g2,f1,
     * 2,ke,km) 

c                              va(1)=va(2)+tayy*z(2)/azz(2)


      v(i,j,1)=va(2)  !!!!!!!

      do 22 k=2,ke

      if(it(i,j).ne.2)v(i,j,k)=va(k)      !!!!! was ne 5 


  22    continue

        elseif(ke.gt.1.and.it(i,j).ne.7)then

      endif

      thy(i,j)=udin*(v(i,j,ke))                                !!!! !udin for tur mod

 
        zw(1)=0.     !!!!!!!   LEVEL  MOD. 
       z(1)=zw(1)
       z(2)=(zw(1)+zw(2))/2.
 
 
 21   continue









         goto 5634      
      do j=1,js
      do i=1,is
      do k=1,ks

      if(it(i,j).eq.5)then
        if(ku(i+1,j).ge.k) u(i+1,j,k)=u(i,j,k)      !

      end if
      end do
      end do
      end do


      do j=1,js
      do i=1,is
      do k=1,ks

      if(it(i,j).eq.2)then

        if(ku(i+1,j).gt.k) u(i+1,j,k)=u(i,j,k)      !
        if(ku(i+1,j).le.k) u(i+1,j,k)=0.

      if(kv(i,j).gt.k) v(i,j,k)=v(i,j+1,k)      !
      if(kv(i,j).le.k) v(i,j,k)=0.    !

      end if
      end do
      end do
      end do
5634  continue



      do k=1,ks
      do j=2,jss
      do i=2,iss
      if(it(i,j).eq.2)then

      u(i,j,k)=(u(i-1,j,k)+u(i,j+1,k))/2.  !!! w-point of U
      u(i+1,j,k)=u(i,j,k)                  !!! E-point of U
             
      v(i,j,k)=v(i-1,j+1,k)                  !!! N-point of U
             
      u(i+1,j-1,k)=u(i,j,k)                  !  it=3
      v(i+1,j,k)=v(i,j,k)                    !  it=3

      end if

      end do
      end do
      end do
        continue






      return
      end
c--------------------------------------------
      subroutine rpin(u,v,ut,vt,w,fu,fv,us,tax,tay,thx,thy,
     # ph,ro,ahz,im,jm,km, al,al1,
     # cph,cu,cadv,cq,cstep)
        include 'par.inc'
        include 'com.inc'

      real LUm,LVm,
     *      fu(im,jm,km), fv(im,jm,km),us(im,jm,km)
     *     ,thx(im,jm),thy(im,jm)
     *     ,u(im,jm,km),v(im,jm,km),tax(im,jm),tay(im,jm)
     *     ,ut(im,jm,km),vt(im,jm,km),ahz(im,jm,km)
     *     ,w(im,jm,km),ro(im,jm,km),ph(im,jm)

c        print *,'thx,thy in rpin',thx(120,15),thy(120,15)
c        pause
         
       lbu=- int(1.0)


      fu=0.
      fv=0.
      us=0.


      do 11 i=2,im-1
      do 11 j=2,jm-1


       zw(1)=-ph(i,j)/980.   !!!!!  ”◊≈“  ŒÀ≈¡¿Õ»… ”–Œ¬Õﬂ
       z(1)=zw(1)
       z(2)=(zw(1)+zw(2))/2.

      if(kp(i,j).le.1.or.it(i,j).eq.3)goto 11  !!!!    04.07.  it=3 

      do 1 k=2,ku(i,j)
 1    us(i,j,k)=ddx(ro,i,j,k,im,jm,km)*980.


c        dzi=0.                                 !!!!!!!!
      call intr1(us,fu,i,j,ku(i,j),im,jm,km)

       zw(1)=0.    !!!!!!   LEVEL   MOD.
       z(1)=zw(1)
       z(2)=(zw(1)+zw(2))/2.

 11   continue



      
      do 4 j=2,jm-1
      do 4 i=2,im-1
*********

       zw(1)=-ph(i,j)/980.   !!!!!  ”◊≈“  ŒÀ≈¡¿Õ»… ”–Œ¬Õﬂ
       z(1)=zw(1)
       z(2)=(zw(1)+zw(2))/2.

      iv=(it(i,j)-5)*it(i,j)
  

        iad=1

        if(it(i,j).eq.2) iad=0
      if(it(i,j).eq.5)iad=0
      if(it(i+1,j).eq.5.or.it(i,j-1).eq.5)iad=0 
       

      if(iv.eq.0.and.ku(i,j).gt.1)then  !!!!! p1

      acsn=acs(j)/2.+acs(j+1)/2.

      ddp=ddx(ph,i,j,1,im,jm,1)*cph

      ctn=(cs(j)/sn(j)+cs(j+1)/sn(j+1))/2.

      


       do 41 k=2,ku(i,j)
                  
      ut0=ut(i,j,k)

      wu=(w(i,j,k-1)+w(i-1,j,k-1))/2. 
        wm=(w(i,j,k)+w(i-1,j,k))/2.     
      


      fu(I,J,k)= -ddp

     #+iad*( - advzu(ut,w,i,j,k,im,jm,km))*cstep*cadv

ccc     # -advu(ut,vt,w,i,j,k,im,jm,km,tax,lbu,it(i,j))*iad

     #  - LUm(ut,vt,i,j,k,im,jm,km)*cadv*cstep*iad

     #     -ut0*vt1(v,vt,im,jm,km,i,j,k,1)*ctn/rad*cadv*iad

     #        -cu*2*omega*vt1(v,vt,im,jm,km,i,j,k,0)

c     #         -et*ut0*cu

     #            +AL1*DELTAu(ut,i,j,k,im,jm,km,lbu)*iad  !!!!! ut 1.11..02 *iad

     #              -fu(i,j,k)                             !!!!!     fu=grad P

     #  +d2dz(u,ahz,tax(i,j),thx(i,j),ku(i,j),i,j,k,i-1,j,im,jm,km)*cq

  41   continue

      if(ku(i,j).le.km-1)then    
      do 42 k=ku(i,j)+1,km 
         u(i,j,k)=0.     
  42  fu(i,j,k)=0.
            
      endif   

      endif                  !!!!!!!!!!!!!!!!  end p1


       zw(1)=0.   !!!!!    LEVEL  MOD.
       z(1)=zw(1)
       z(2)=(zw(1)+zw(2))/2.

  4   continue




*****************************************************************************


      do 22 i=1,im-1
      do 22 j=2,jm-1

       zw(1)=-ph(i,j)/980.   !!!!!  ”◊≈“  ŒÀ≈¡¿Õ»… ”–Œ¬Õﬂ
       z(1)=zw(1)
       z(2)=(zw(1)+zw(2))/2.

      if(kp(i,j).le.1.OR.it(i,j).EQ.3)goto 22 !!04.07.02 it=2,3
                                                 !!!!! impos.to calculate grad p  
      do 2 k=2,kv(i,j)
 2     us(i,j,k)=ddy(ro,i,j,k,im,jm,km)*980.

       if(kv(i,j).lt.km)then
      do 201 k=kv(i,j)+1,km
 201   us(i,j,k)=0.
        endif


c       dzi=0.                                  !!!!!!!!!
       call intr1(us,fv,i,j,kv(i,j),im,jm,km,kv(i,j),dzi)

       zw(1)=0.   !!!!!    LEVEL  MOD.
       z(1)=zw(1)
       z(2)=(zw(1)+zw(2))/2.

 22   continue








      do 23 i=1,im-1
      do 23 j=2,jm-1

      if(kv(i,j).le.1)goto 23



        if(kv(i,j).lt.km)then
          do 25 k=kv(i,j)+1,km
 25       us(i,j,k)=0.
        endif
 23   continue

      do 5 i=1,im-1
      do 5 j=jm-1,2,-1

       zw(1)=-ph(i,j)/980.   !!!!!  ”◊≈“  ŒÀ≈¡¿Õ»… ”–Œ¬Õﬂ
       z(1)=zw(1)
       z(2)=(zw(1)+zw(2))/2.

      iv=it(i,j)*(it(i,j)-5)

        iad=1
        if(it(i,j).eq.5)iad=0                  !!! 04.07 .or.it(i,j-1).eq.2)iad=0
      if(it(i+1,j).eq.5)iad=0 
        if(it(i,j).eq.2)iad=0
      if(it(i+1,j).eq.5.or.it(i,j-1).eq.5)iad=0

      if(iv.eq.0.and.kv(i,j).gt.1)then

      ddp= ddy(ph,i,j,1,im,jm,1)*cph

      al2=al

      iv1=(it(i,j)-5)*(it(i,j)-6)




      do 51 k=2,kv(i,j)

      wu=(w(i,j,k-1)+w(i,j-1,k-1))/2.        
        wm=(w(i,j,k)+w(i,j-1,k))/2.           

      vt0=vt(i,j,k)

      u2= ut1(u,ut,im,jm,km,i,j,k,1)                                    

      fv(I,J,k)=    - ddp

     #+iad*(-advzv(vt,w,i,j,k,im,jm,km))*cstep*cadv


     #  - LVm(ut,vt,i,j,k,im,jm,km)*cadv*cstep*iad

     #      +u2**2*CS(J)/SN(J)/RAD*cadv*iad  
                              
     #        +cu*2*omega*ut1(u,ut,im,jm,km,i,j,k,0)


     #                  +AL1*DELTAv(vt,i,j,k,im,jm,km,lbu)*iad !!vt 1,11.02 *iad

     #                    -fv(i,j,k)
  
     #  + d2dz(v,ahz,tay(i,j),thy(i,j),kv(i,j),i,j,k,i,j-1,im,jm,km)*cq

 51   continue

      if(kv(i,j).le.km-1)then  
      do 52 k=kv(i,j)+1,km 
        v(i,j,k)=0.      
 52   fv(i,j,k)=0.
      
        endif
*******************
       endif


       zw(1)=0.   !!!!   LEVEL  MOD.
       z(1)=zw(1)
       z(2)=(zw(1)+zw(2))/2.

 5    continue


      continue
      return
      end


 

  

