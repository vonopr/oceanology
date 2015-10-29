      subroutine save1(a,b,c,d,im,jm,k1,k,a1) 
      include 'par.inc'
      include 'com.inc'
      Character   file1*10
      Character   file2*10
      real a(im,jm,k1),b(im),c(jm),d(im,jm)

      do 1 i=1,im
      do 1 j=1,jm
         if(k.gt.kp(i,j).or.kp(i,j).eq.1)then
            d(i,j)=a1
         else
            d(i,j)=a(i,j,k)
         endif
1     continue
200   format(a10)
      print *,'filename for picture, for bot'
      read(*,200)  file1 
c         ,file2          
      nm=15
      open(15,file=file1)
c       open(5,file=file2)
      rewind 15
c     rewind 5
      do i=1,im
      do j=jm,1,-1
         if(k.gt.kp(i,jm+1-j).or.kp(i,jm+1-j).eq.1)then
            write(nm,*)b(i),c(j)
c       write(5,*)b(i),c(j) 
         else
           write(nm,*)b(i),c(j),d(i,jm+1-j)
         endif
      enddo
      enddo
      close(15)
c       close(5)      
      return
        end
        subroutine save2(a,b,d,im,jm,km,j1,a1) 

        include 'par.inc'
        include 'com.inc'

      Character   file1*10
      Character   file2*10
      real a(im,jm,km),b(im),d(im,km),z1(20)

          do 1 i=1,im
          do 1 k=1,km
        if(k.gt.kp(i,j1).or.kp(i,j1).eq.1)then
        d(i,k)=a1
        else
        d(i,k)=a(i,j1,k)
        endif
        z1(k)=-z(k)/10000.
1       continue
           
      
200       format(a10)

      print *,'filename for picture, bottom'
          read(*,200)  file1,file2
                 
      nm=15
        open(15,file=file1)
        open(5,file=file2)
      rewind 15
 
      rewind 5
          do i=1,im
          do k=1,15
        if(k.gt.kp(i,j1).or.kp(i,j1).eq.1)then
      write(nm,*)b(i),z1(k) 
      write(5,*)b(i),z1(k) 
        else
      write(nm,*)b(i),z1(k),d(i,k)
        endif
          enddo
          enddo
          close(15)
        close(5)

      return
        end


        subroutine save3(a,b,d,im,jm,km,j1,a1) 

        include 'par.inc'
        include 'com.inc'
      Character   file1*10
 
      real a(im,jm,km),b(im),d(im,km),z1(20)
          do 1 i=1,im-j1

          do 1 k=1,km
        if(k.gt.kp(i,i+j1).or.kp(i,i+j1).eq.1)then
        d(i,k)=a1
        else
        d(i,k)=a(i,i+j1,k)
        endif
        z1(k)=-z(k)/10000.
1       continue
           
      
200       format(a10)

      print *,'filename for picture'
          read(*,200)  file1
                 
      nm=15
      open(15,file=file1)
      rewind 15
          do i=1,im-j1
          do k=1,16
      write(nm,*)b(i),z1(k),d(i,k)
          enddo
          enddo
          close(15)
       

      return
        end


      subroutine savebe(te,tb,im,jm,km,ln)
      real tb(im,jm,km),te(im,jm,km)
      Character*5   file1(11)

     	do i1=1,11
	   file1(i1)='be'//char(i1+46) ! ôàéëû îò be0 äî be9
	enddo  
c      if(ln-1.eq.1)file1='be1'
c      if(ln-1.eq.2)file1='be2'
c      if(ln-1.eq.3)file1='be3'
c      if(ln-1.eq.4)file1='be4'
      nm=12
      open(12,file=file1(ln))
      write(12,2012)tb
      write(12,2012)te
      close(12)
2012  format(20e12.6)
      return
      end




      subroutine picuv(u1,v1,u,v,xx,yy,im,jm,km,it)
      dimension u1(im-1,jm-1),v1(im-1,jm-1),
     & U(im,jm,km),v(im,jm,km)
     & ,xx(im),yy(jm),it(im,jm)

C xx,yy - user coordinates
c       include 'par.inc'
c       include 'com.inc'

c
        character file1*11
1100  format(a11)

        x00=1.
        y00=1.
        facx=1.
        facy=1.
        nfi=8

        lx=im-1
        ly=jm-1
        am=100
        av=0.1
        yx=1.0

 555  continue
  

        print *,'velocity lavel for picture? k='
        read *,kk


        do i=1,lx
        do j=1,ly
        if(it(i,j).eq.0)then
         v1(i,ly+1-j)=-(v(i,j,kk)+v(i,j+1,kk))/2
         u1(i,ly+1-j)=(u(i,j,kk)+u(i+1,j,kk))/2
        else
        v1(i,ly+1-j)=0.
        u1(i,ly+1-j)=0.
        endif
          enddo
        enddo

        print *,'filename bln?'
        read(*,1100)file1
        print *,'am,av,yx?'
        read *,am,av,yx 
        OPEN(8,FILE=file1)
         
           do 3 j=1,ly
           i1=1
        au=float(j)/2. -j/2
           if(au.gt.1.e-3)i1=2
           do 3 i=i1,lx,2
           x=xx(i)
           y=yy(j)
         uc=u1(i,j)         
         vc=v1(i,j)   
         call ARROW(X,Y,Uc,Vc,AM,av,yx)
3        CONTINUE
        uc=am
        vc=0.
        x=40.
        y=64.25
        call arrow(x,y,uc,vc,am,av,yx)


        close(8)
        print *,'again? 1=yes'
        read *,nn
        if(nn.eq.1)goto 555

        return
        end

      subroutine picuw(u1,w1,u,w,xx,im,jm,km)
      dimension u1(im-1,km-1),w1(im-1,km-1),
     & U(im,jm,km),w(im,jm,km)
     & ,xx(im)

C xx,yy - user coordinates
        include 'par.inc'
        include 'com.inc'

c
        character file1*11
1100  format(a11)

        x00=1.
        y00=1.
        facx=1.
        facy=1.
        nfi=8

        lx=im-1
        lz=km-1
        am=100
        av=0.1
        yx=1.0

 555  continue
  

        print *,'velocity lavel for picture? j1='
        read *,j1
        print *,y(js-j1+1)

        print *,'am,av,c1?'
        read *,am,av,c1
        u0=110*5*1.e4/(x(im)-x(1))/rad/sn(10)*980
        am=am*u0
        yx=(xx(im)-xx(1))/c1 
        ww=0
        w0=1/rad/sn(10)/(x(im)-x(1))*zw(km)
        do i=1,lx
        do k=2,lz
      w1(i,k)=-(w(i,j1,k)+w(i,j1,k-1))/2/w0
      u1(i,k)=(u(i,j1,k)+u(i+1,j1,k))/2
        ww=w1(i,k)+ww

          enddo
        enddo

        if(w0.eq.0)print *,'sos'
        print *,'filename bln?'
        read(*,1100)file1



        OPEN(8,FILE=file1)
         
           do 3 j=2,lz
           i1=1
c       au=float(j)/2. -j/2
c          if(au.gt.1.e-3)i1=2
           do 3 i=i1,lx,1
           x0=(xx(i)+xx(i+1))/2
           y0=-z(j)/zw(16)
         uc=u1(i,j)         
         vc=w1(i,j)   
         call ARROW(X0,Y0,Uc,Vc,AM,av,yx)
3        CONTINUE
        close(8)
        print *,'again? 1=yes'
        read *,nn
        if(nn.eq.1)goto 555

        return
        end



      subroutine picuwm(u1,w1,u,w,xx,im,jm,km)
      dimension u1(im-1,km-1),w1(im-1,km-1),
     & U(im,jm,km),w(im,jm,km)
     & ,xx(im)

C xx,yy - user coordinates
        include 'par.inc'
        include 'com.inc'

c
        character file1*11
1100  format(a11)

        x00=1.
        y00=1.
        facx=1.
        facy=1.
        nfi=8

        lx=im-1
        lz=km-1
        am=100
        av=0.1
        yx=1.0

 555  continue
  

        print *,'velocity lavel for picture? j1='
        read *,j1
        print *,'am,av,c1?'
        read *,am,av,c1
        u0=110*5*1.e4/(x(im)-x(1))/rad/sn(10)*980
        am=am*u0
        yx=(xx(im)-xx(1))/c1 
        ww=0
        w0=1/rad/sn(10)/(x(im)-x(1))*zw(km)
 
        do i=1,lx
        uu=(zintw(u,i,j1,1,km,im,jm,ku(i,j1),zw)+
     *zintw(u,i+1,j1,1,km,im,jm,ku(i+1,j1),zw))/2
        if(kp(i,j1).gt.1)then
        uu=uu/zw(kp(i,j1))
        else
        uu=0
        endif
        do k=2,lz
      w1(i,k)=-(w(i,j1,k)+w(i,j1,k-1))/2/w0
      u1(i,k)=(u(i,j1,k)+u(i+1,j1,k))/2
        if(k.le.kp(i,j1))u1(i,k)=u1(i,k)-uu
        ww=w1(i,k)+ww

          enddo
        enddo

        if(w0.eq.0)print *,'sos'
        print *,'filename bln?'
        read(*,1100)file1



        OPEN(8,FILE=file1)
         
           do 3 j=2,lz
           i1=1
c       au=float(j)/2. -j/2
c          if(au.gt.1.e-3)i1=2
           do 3 i=i1,lx,1
           x0=(xx(i)+xx(i+1))/2
           y0=-z(j)/zw(16)
         uc=u1(i,j)         
         vc=w1(i,j)   
         call ARROW(X0,Y0,Uc,Vc,AM,av,yx)
3        CONTINUE
        close(8)
        print *,'again? 1=yes'
        read *,nn
        if(nn.eq.1)goto 555

        return
        end



      SUBROUTINE ARROW(X,Y,U,V,AM,AV,yy)
c      x,y - position of the arrow in map units
c      u,v - velocity
c      am - scale factor ( in map units)
c      av - size of the pointer 
c      yy -   x y scale factor
c      if map is scaled like x per page unit =2,y ... =1 then yy=2
       PI=3.1415927
       v=v/yy
      gyp=sqrt(u*u+v*v) 
      R=gyp/am 
      if(gyp.eq.0) GO TO 80
  
      csa=u/gyp
      sna=v/gyp
      gyp1=sqrt(u*u+v*yy*v*yy)
      if(gyp1.eq.0) write(*,*) 'sos'

      csb=u/gyp1
      snb=v*yy/gyp1
      
     
      if(snb.ge.0) then
      B=ACOS(csb) 
      else 
      B=2*pi-ACOS(csb)
      end if

      X1=X+R*csa
      Y1=Y+R*sna
      write(8,'(i1,2x,i1)') 2,1 
      write(8,'(f10.4,1x,f10.4)') x,y
      write(8,'(f10.4,1x,f10.4)') x1,y1


      X2=X1-av*COS(B+PI/12.)
      Y2=Y1-av/yy*SIN(B+PI/12.)
      X3=X1-av*COS(B-PI/12.)
      Y3=Y1-av/yy*SIN(B-PI/12.)

      write(8,'(i1,2x,i1)') 4,1 
      write(8,'(f10.4,1x,f10.4)') x1,y1
      write(8,'(f10.4,1x,f10.4)') x2,y2
      write(8,'(f10.4,1x,f10.4)') x3,y3
      write(8,'(f10.4,1x,f10.4)') x1,y1


   80 RETURN
      END

        subroutine savealog(u,v,t,tb,x1,im,jm,km,z0)
      dimension u(im,jm,km),v(im,jm,km),
     & t(im,jm,km),tb(im,jm,km),x1(im)
  

C xx,yy - user coordinates
        include 'par.inc'
        include 'com.inc'


        open(1, file='pr1.dat')
        open(2, file='pr2.dat')
        open(3, file='pr3.dat')
        open(4,file='pr4.dat')
        print *,'i1?, j1?'
        read *,i1,j1
        print *,x1(i1),kp(i1,j1+1)
        ke=kp(i1,j1+1)
        do k=1,ke
        z1=alog((zw(ke)-z(k))/z0)
        uuu=u(i1,j1,k)/z1
        vvv=v(i1,j1,k)/z1
        z2=-z(k)
        write(3,*)z2,uuu,vvv,t(i1,j1,k)
        write(4,*)z2,tb(i1,j1,k)
        enddo
        close(1)
        close(2)
        close(3)
        close(4)
        return
        end

        subroutine savetime(iiv,im,jm,km,v,nn,n3)
        include 'par.inc'
        include 'com.inc'
        integer iiv(im,jm,km,*)
        real v(im,jm,km)
        do 1 i=1,im
        do 1 j=1,jm
        do 1 k=1,km
        iiv(im,jm,km,nn/2)=v(im,jm,km)*n3
  1     continue
        return
        end


      subroutine sipp(eps,t,r,e,a,b,d,c,q,wrksp1,wrksp2,im,jm,niter)
*           A(I,J) = as
*           E(I,J) = an
*           B(I,J) = aw
*           D(I,J) = ae
*           C(I,J) =  ap
*           q(i,j)= af
* 15.11.1992 sip involving

      dimension  R(im,jm),t(im,jm),WRKSP1(im,jm),WRKSP2(im,jm),
     *  a(im,jm),b(im,jm),c(im,jm),d(im,jm),e(im,jm),q(im,jm)
      ni=niter
      n1=im-1
      n2=jm
*     print *,'sipp in'
      eps1=eps*10.
      N1M = im
      APARAM = 1.0
C     ITERATIVE LOOP
      DO 200 IT=1,ni
C     CALCULATE THE RESIDUALS
         RESMAX = 0.0
         RESMN = 0.0
         DO 140 J=1,N2
            DO 120 I=1,N1
C     FIVE POINT MOLECULE FORMULA
* 12.11.1992
      tij=t(i,j)
      tijp=0.
      tijm=0.
      tipj=0.
      timj=0.
      if(j.lt.n2)tijp=t(i,j+1)
      if(j.gt.1)tijm=t(i,j-1)
      if(i.lt.n1)tipj=t(i+1,j)
      if(i.gt.1)timj=t(i-1,j)
        r(i,j) = q(i,j) - a(i,j)*Tijm  - b(i,j)*Timj  - c(i,j)*tij
     >    - d(i,j)*Tipj  - e(i,j)*Tijp
c         print *,'sppp',i,j,r(i,j)
c     &, q(i,j),a(i,j),Tijm,b(i,j),Timj,c(i,j),tij
c     >, d(i,j),Tipj,e(i,j),Tijp
        if(abs(q(i,j)).lt.eps1)then
        anorm=1.
        else
          anorm=abs(q(i,j))
        end if
               ARES = ABS(R(I,J)/anorm)
               RESMAX =aMAX1(RESMAX,ARES)
*               RESMN = RESMN + ARES
  120       CONTINUE
  140    CONTINUE
*     print *,'sipp 140'
*        RESMN = RESMN/(FLOAT(N1*N2))
         IFAIL = 1
c      print *,'d03u',i,j,it,
c     &N1, N2, N1M, A(32,41),B(32,41),C(32,41),
c     &D(32,41),E(32,41), APARAM,IT,R(32,40),WRKSP1(32,41),
c     &WRKSP2(32,41), IFAIL
         CALL D03UAFr(N1, N2, N1M, A, B, C, D, E, APARAM, IT, R,WRKSP1,
     *    WRKSP2, IFAIL)
*     print *,'d03 ifail=',ifail
C     CHECK ERROR FLAG
C     UPDATE THE DEPENDENT VARIABLE
*        DELMAX = 0.0
*        DELMN = 0.0
         DO 180 J=1,N2
            DO 160 I=1,N1
               T(I,J) = T(I,J) + R(I,J)
  160       CONTINUE
  180    CONTINUE
C     CONVERGENCE TESTS HERE IF REQUIRED
      if(abs(resmax).lt.eps)goto 201
  200 CONTINUE
  201 CONTINUE
c      print *,'sipp it,res =',it,resmax
c      if(it.gt.ni)
c     >print *,'sipp warning : no sufficient accuracy res=',resmax,anorm
      return
      END
      FUNCTION DDX(a,i,j,k,im,jm,km)
        include 'par.inc'
        include 'com.inc'

      DIMENSION a(im,jm,km)
      snn=sn(j+1)/2+sn(j)/2
      ddx=0.
      if(i.eq.1)then
      dx=x(2)
      ddx=(a(i+1,j,k)-a(i,j,k))/dx/rad/snn
      else 
	if(kp(i-1,j).ge.k.and.kp(i,j).ge.k)then
      dx=x(i+1)/2-x(i-1)/2
      ddx=(a(i,j,k)-a(i-1,j,k))/dx/rad/snn
	else
	ddx=0.
      endif
      endif
      RETURN
      END


      FUNCTION DDy(a,i,j,k,im,jm,km)
        include 'par.inc'
        include 'com.inc'
      DIMENSION a(im,jm,km)
	ddy=0.
      if(j.eq.1)then
      ddy=(a(i,j+1,k)-a(i,j,k))/dy/rad
      else
      if(kp(i,j-1).ge.k.and.kp(i,j).ge.k)then
       ddy=(a(i,j,k)-a(i,j-1,k))/dy/rad
      else
	ddy=0.
       endif
       endif
      RETURN
      END



      FUNCTION DELTA(xx,k,i,j,im,jm,km,ll)
        include 'par.inc'
        include 'com.inc'

      DIMENSION xx(im,jm,km)

***** delta for scalar values, defined in the center of the mesh
*****  ll=1 for T,S,ro - zero solid boundary fluxes
****** ll=-1 for turb values
c      print *,'in DELTA',k
      delta=0.
      ap=xx(i,j,k)
      snn=sn(j+1)/2.+sn(j)/2.
c       ll=1.

      if(i.eq.1)then
              aw=ap*ll                       !
              dxw=x(i+1)-x(i)
           else


		 if(k.ge.kp(i-1,j))then
               aw=ap*ll
               dxw=x(i+1)-x(i)
           else
              aw=xx(i-1,j,k)
              dxw=x(i+1)/2-x(i-1)/2
           endif
      endif




      if(i.eq.is-1)then
              ae=ap*ll                       !
              dxe=x(i+1)-x(i)
           else
		 if(k.ge.kp(i+1,j))then
               ae=ap*ll
               dxe=x(i+1)-x(i)
           else
              ae=xx(i+1,j,k)
              dxe=x(i+2)/2-x(i)/2
      
           endif
      endif



      if(j.eq.1)then
              as=ap*ll                       !
           else
		 if(k.ge.kp(i,j-1))then
              as=ap*ll
           else
              as=xx(i,j-1,k)

           endif
	endif



      if(j.eq.js-1)then
              an=ap*ll                       !
           else
		 if(k.ge.kp(i,j+1))then
               an=ap*ll
           else
            an=xx(i,j+1,k)
      
           endif
      endif

             DELTA=1./RAD**2/SNn*(
     #              (sn(j+1)*(an-ap)+sn(j)*(as-ap))/dy/dy
     #              +2./SNn*((ae-ap)/dxe+(aw-ap)/dxw)/(dxe+dxw))

      RETURN
      END




      FUNCTION DELTAu(xx,i,j,k,im,jm,km,ll)
****** for u - component,irregular C-mesh
        include 'par.inc'
        include 'com.inc'
      DIMENSION xx(im,jm,km)
      deltau=0.
              ap=xx(i,j,k)


       

      snn=sn(j+1)/2.+sn(j)/2.

      if(i.ne.1)then
              aw=xx(i-1,j,k)
              dxw=x(i)-x(i-1)
        if(ku(i-1,j).lt.k)aw=ap*ll
              else
              aw=ap*ll
              dxw=x(i+1)-x(i)
         endif


      if(i.ne.is)then
              ae=xx(i+1,j,k)
              dxe=x(i+1)-x(i)
         if(it(i,j).eq.2.and.it(i+1,j).eq.3)ae=ap
	           else
              ae=ap*ll
              dxe=x(i)-x(i-1)
         endif


      if(j.ne.1)then
           as=xx(i,j-1,k)
	if(it(i,j).eq.2)as=ap
           if(ku(i,j-1).lt.k) as=ap*ll
         else
****** if    u=0 on s.b.
         as=ap*ll
****** if   du/dy=0 on s.b.
       if(ku(i,j).lt.k) as=ap*ll

      endif



      if(j.ne.js-1) then
            an=xx(i,j+1,k)
           if(ku(i,j+1).lt.k) an=ap*ll
           else
****** if    u=0 on s.b.
         an=ap*ll
****** if   du/dy=0 on s.b.
      
      endif
        DELTAu=1./rad**2/snn*(
     #   (sn(j+1)*(an-ap)+sn(j)*(as-ap))/dy/dy
     #      +2./snn*((ae-ap)/dxe+(aw-ap)/dxw)/(dxe+dxw))
c      if(j.eq.3.and.i.eq.5)print *,'del',i,k,j,deltau
c     &,an,ap,as,ae,aw,dy,dxe,dxw,snn,rad


       
      RETURN
      END

      FUNCTION DELTAv(xx,i,j,k,im,jm,km,ll)
**** irregular C-mesh for v
        include 'par.inc'
        include 'com.inc'
      DIMENSION xx(im,jm,km)

	

      deltav=0.
      ap=xx(i,j,k)
      snn=sn(j+1)/2.+sn(j)/2.
      sns=sn(j)/2+sn(j-1)/2

             if(i.ne.1)then
              aw=xx(i-1,j,k)
              if(kv(i-1,j).lt.k) aw=ap*ll
            dxw=x(i+1)/2-x(i-1)/2
           else
          dxw=x(i+1)-x(i)
         aw=ap*ll
       
       endif


      if(i.ne.is-1)then
       ae=xx(i+1,j,k)
	
        if(kv(i+1,j).lt.k) ae=ap*ll
         dxe=x(i+1)/2-x(i)/2
          else
           dxe=x(i)-x(i-1)
           ae=ap*ll
              endif


       if(j.ne.1)then
      as=xx(i,j-1,k)
	if(kv(i,j-1).lt.k)as=ap*ll
      if(it(i-1,j-1).eq.5.or.it(i-1,j-1).eq.2)as=ap
	       else
        as=ap*ll
         endif


          if(j.ne.js)then
           an=xx(i,j+1,k)
       if(kv(i,j+1).lt.k)an=ap*ll
           else
          an=ap*ll
         endif


        DELTAv=1./RAD**2/SN(j)*(
     # (snn*(an-ap)+sns*(as-ap))/dy/dy
     #+2./SN(J)*((ae-ap)/dxe+(aw-ap)/dxw)/(dxe+dxw))

      RETURN
      END

       function div(u,v,i,j,k,im,jm,km)
	  include 'par.inc'
        include 'com.inc'
      dimension
     *  u(im,jm,km),v(im,jm,km)

      up=u(i,j,k)
      vp=v(i,j,k)
      ue=u(i+1,j,k)
      vn=v(i,j+1,k)

	 if(ku(i,j).lt.k)up=0.
	  if(ku(i+1,j).lt.k)ue=0.
	   if(kv(i,j).lt.k)vp=0.
	    if(kv(i,j+1).lt.k)vn=0.

c	if(it(i,j).eq.2)then
c	if(it(i+1,j).eq.3)ue=u(i,j,k)
c	if(it(i,j-1).ne.1)vp=vn
c	end if

       dx=x(i+1)-x(i)
       snn=sn(j+1)/2+sn(j)/2



      div=
     *      1/rad/snn*(vn*sn(j+1)-vp*sn(j))/dy+
     *      1/rad/snn*(ue-up)/dx

      return
      end

      subroutine n2(tt,qt,pr,afu,fv,i,j,ke,im,jm,km,drdt) !¢ £à ­¨ç­ëå ãá«®¢¨ïå!

c     afu==fu(i,j,1)
      real tt(im,jm,km),fv(im,jm,km),pr(im,jm,km)

        include 'par.inc'
        include 'com.inc'

      fv(i,j,1)=-980.*drdt
     #   *qt/afu**0.5/(z(2)*0.41)**2/pr(i,j,1)
c      fv(i,j,1)=0.0 ! âðåìåííî
      do k=2,ke-1
      fv(i,j,k)=980.*
     #   (tt(i,j,k+1)-tt(i,j,k))/(z(k+1)-z(k))
c	if((tt(i,j,k+1)-tt(i,j,k)).LT.0.) then
c	   print *,(tt(i,j,k+1)-tt(i,j,k)),fv(i,j,k),i,j,k
c	   pause
c	endif
      enddo
c       fv(i,j,2)=0.
      fv(i,j,ke)=0.

      return
      end

      subroutine s2(u,v,tax,tay,fu,i,j,i1,j1,
     #               ke,im,jm,km,z0)             !ãç¥áâì «®£ à¨ä¬¨ªã
c   shear calculation

      real u(im,jm,km),v(im,jm,km),fu(im,jm,km)
     #     ,tax(im,jm),tay(im,jm)


        include 'par.inc'
        include 'com.inc'

      udin2=(tax(i,j)+tax(i1,j))**2/4
     # +(tay(i,j)+tay(i,j1))**2/4
      udin=sqrt(udin2)+1.e-6

      fu(i,j,1)=udin/0.435/0.435/z(2)/z(2)
      up1=(u(i,j,2)+u(i1,j,2))/2.
      vp1=(v(i,j,2)+v(i,j1,2))/2.
      do 1 k=2,ke-1
      up2=(u(i,j,k+1)+u(i1,j,k+1))/2.
      vp2=(v(i,j,k+1)+v(i,j1,k+1))/2.

      fu(i,j,k)=
     # ((up2-up1)**2+(vp2-vp1)**2) /(z(k+1)-z(k))**2

      up1=up2
      vp1=vp2
  1   continue
      az=(zw(ke)-z(ke))/z0
      alo=alog(az)

      fu(i,j,ke)=(up2**2+vp2**2)/((zw(ke)-z(ke))*alo)**2

      return
      end

      subroutine frfc(frf,pr,fu,fv,i,j,ke,im,jm,km)
***** from stress-equation model
**** pr(i,j,k)=1/prt , prt -Prandtl-Shmidt parametre
        include 'par.inc'
        include 'com.inc'
      common /const/ c2,c4,c6,c7,bet1,bet2,rfcr
      real frf(im,jm,km),fu(im,jm,km),fv(im,jm,km),pr(im,jm,km)

      bet1=(0.5+c4*(1.+3.*c7/c2))/(c4-1.)     !  to incond subr
      bet2=(2.+c4*(1.+1.5*c7/c6))/(c4-1.)     !  to incond subr
      crf=2./3.*(1-1./c4)
      rfcr=crf/(crf+2.*c7/c2+2./c4)          !    to incond subr

      crf6=2./3.*(1-1./c4)/c6
      al0=c4/c6

      do 1 k=1,ke
      if(fu(i,j,k).lt.1.e-18)then
      pr(i,j,k)=bet1/bet2*al0
      frf(i,j,k)=crf6/rfcr/pr(i,j,k)
        elseif(abs(fv(i,j,k)).lt.1.e-7)then
         pr(i,j,k)=al0
         frf(i,j,k)=crf6/pr(i,j,k)
          else

      ri=fv(i,j,k)/fu(i,j,k)
      al1=ri*bet1
      al2=ri*bet2
      det=(1.+al0*al1)**2-4.*al0*al2
      det1=0.
      if(det.gt.0.)det1=sqrt(det)
      if(fv(i,j,k).lt.0.)det1=-det1
      pr(i,j,k)=(1.+al0*al1+det1)/al2/2.

      rf=ri*pr(i,j,k)

      if(rf.lt.1.)then
      frf(i,j,k)=crf6*(1.-rf/rfcr)/(1.-rf)/pr(i,j,k)
      else
      frf(i,j,k)=1.e-3
      endif

      endif
 1    continue
      return
      end
  
  

