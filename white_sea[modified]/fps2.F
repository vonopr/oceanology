      function advv1(u,v,i,j,k,im,jm,km,ir)
        include 'par.inc'
        include 'com.inc'

      dimension 
     *          u(im,jm,km),v(im,jm,km)

      advv1=0.

              arad=1./rad/sn(j)

********* step1

          an=(v(i,j,k)+v(i,j+1,k))**2*(sn(j+1)+sn(j))/8.
          as=(v(i,j,k)+v(i,j-1,k))**2*(sn(j-1)+sn(j))/8.


        if(it(i,j-1).eq.2)as=v(i,j,k)*(sn(j-1)+sn(j))/2.
       dy1=dy
	   if(kv(i,j+1).lt.k)then
    	an=0.
        dy1=1.5*dy
	      end if
	    if(kv(i,j-1).lt.k) then
        as=0.
        dy1=1.5*dy
          end if

       advv1=advv1+(an-as)/dy1*arad


c********  step 2

             if(i.eq.1)then
          aw=v(i,j,k)*(u(i,j,k)+u(i,j-1,k))/2.
          ae=(v(i,j,k)+v(i+1,j,k))*(u(i+1,j,k)+u(i+1,j-1,k))/4.


                 elseif(i.eq.iss)then
          aw=v(i,j,k)*(u(i+1,j,k)+u(i+1,j-1,k))/2.
          aw=(v(i,j,k)+v(i-1,j,k))*(u(i,j,k)+u(i,j-1,k))/4.

                   else
        ae=(v(i,j,k)+v(i+1,j,k))*(u(i+1,j,k)+u(i+1,j-1,k))/4.
       aw=(v(i,j,k)+v(i-1,j,k))*(u(i,j,k)+u(i,j-1,k))/4.
      if(ir.eq.5)ae=v(i,j,k)*u(i+1,j,k)
        endif

          dx=x(i+1)-x(i)


          advv1= advv1+ (ae-aw)/dx*arad


      return
      end





      function adv1(a,u,v,i,j,k,im,jm,km)

        include 'par.inc'
        include 'com.inc'
      dimension
     *          u(im,jm,km),v(im,jm,km),a(im,jm,km)

      adv1=0.


      up=u(i,j,k)
      vp=v(i,j,k)
      ue=u(i+1,j,k)
      vn=v(i,j+1,k)

	 if(ku(i,j).lt.k)up=0.
	  if(ku(i+1,j).lt.k)ue=0.
	   if(kv(i,j).lt.k)vp=0.
	    if(kv(i,j+1).lt.k)vn=0.


              arad=1./rad /((sn(j)+sn(j+1))/2.)  


			 
********* step1  X
!                                    |
!!!!!!!!!!!!!!!!!!!!!!!!!   semisum  V  central point  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         goto 2   ! 

       ii=1  ! sum/2
c	 ii=0  ! central point

              aw=(a(i-ii,j,k)+a(i,j,k))/2.*up
              ae=(a(i+ii,j,k)+a(i,j,k))/2.*ue

      if(it(i,j).eq.5.or.kp(i,j).eq.k)  aw=a(i,j,k)*up   !  NEW  3.7.2007
      if(it(i,j).eq.5.or.kp(i,j).eq.k)  ae=a(i,j,k)*ue   !  NEW  3.7.2007

                  
              dxw=x(i)-x(i-1)
              dxe=x(i+1)-x(i)
              dx=(dxw+dxe)/2.
2     continue
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!                                           |
       !!!!!!!!!!!!!!!!  перенос по потоку  V  Godunov  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

ccc      goto 1

       aa=1.
       if(up.lt.0)aa=-1.
	 if(kp(i-1,j).lt.k)a(i-1,j,k)=a(i,j,k)
              aw=(a(i-1,j,k)*(1+aa)+a(i,j,k)*(1-aa))/2.*up

       aa=1.
       if(ue.lt.0)aa=-1.
	 if(kp(i+1,j).lt.k)a(i+1,j,k)=a(i,j,k)
              ae=(a(i+1,j,k)*(1-aa)+a(i,j,k)*(1+aa))/2.*ue

            
              dxw=x(i)-x(i-1)
              dxe=x(i+1)-x(i)
              dx=(dxw+dxe)/2.
      continue

       !!!!!!!!!!!!!!   end  of  перенос по потоку    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


          adv1= adv1+ (ae-aw)/dx*arad   ! arad= 1/....




**********  step2  Y


!                                    | 
!!!!!!!!!!!!!!!!!!!!!!!!!!  semisum  V  central point  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       goto 3   ! 

       jj=1  ! sum/2
c	 jj=0  ! central point

            as=(a(i,j-jj,k)+a(i,j,k))/2.*vp*sn(j)
            an=(a(i,j+jj,k)+a(i,j,k))/2.*vn*sn(j+1)

      if(it(i,j).eq.5.or.kp(i,j).eq.k)   as=a(i,j,k)*vp*sn(j)   !  NEW  3.7.2007
      if(it(i,j).eq.5.or.kp(i,j).eq.k)   an=a(i,j,k)*vn*sn(j+1) !  NEW  3.7.2007

3     continue


!                                                 |
        !!!!!!!!!!!!!!!!!!!!!   перенос по потоку V  Godunov   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
ccc       goto 4

      aa=1
      if(vp.lt.0)aa=-1
      if(kp(i,j-1).lt.k)a(i,j-1,k)=a(i,j,k)
           as=(a(i,j-1,k)*(1+aa)+a(i,j,k)*(1-aa))/2.*vp*sn(j)


      aa=1
      if(vn.lt.0)aa=-1
      if(kp(i,j+1).lt.k)a(i,j+1,k)=a(i,j,k) 
            an=(a(i,j+1,k)*(1-aa)+a(i,j,k)*(1+aa))/2.*vn*sn(j+1)

      continue
                            
        !!!!!!!!!!!!!!!!!!! end`of   перенос по потоку  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


          adv1= adv1+ (an-as)/dy*arad   ! arad= 1/....


      return
      end

      function LUm(u,v,i,j,k,im,jm,km)
        include 'par.inc'
        include 'com.inc'
      dimension
     *          u(im,jm,km),v(im,jm,km)
      real LUm
      LUm=0.

*************************************************************** step1
      up=u(i,j,k)
      uw=u(i-1,j,k)
	ue=u(i+1,j,k)

      uee=u(i+1,j,k)
      if(i.lt.iss)uee=u(i+2,j,k)

      uww=u(i-1,j,k)
      if(i.gt.2)uww=u(i-2,j,k)

        arad=2./rad/(sn(j)+sn(j+1))   

                
              dxw=x(i)-x(i-1)
              dxe=x(i+1)-x(i)
              dx=(dxw+dxe)/2.

      
        a=1.
	  if(uw.le.0.)a=0.
        b=1.
	  if(ue.le.0.)b=0.

      LUm= LUm +(ue*(b*up+(1.-b)*uee) - uw*(a*uww+(1.-a)*up))/dx/2.*arad  !  R



*****************************************************************  step2

       vd=(v(i,j+1,k)+v(i-1,j+1,k))/2.
       vu=(v(i,j,k)+v(i-1,j,k))/2.

       ud=u(i,j+1,k)
	 um=u(i,j,k)
       uu=u(i,j-1,k)
	 
        a=1.
	  if(vd.le.0.)a=0.
        b=1.
	  if(vu.le.0.)b=0.

      LUm=LUm+(vd*(a*um+(1.-a)*ud)*sn(j+1)-vu*(b*uu+(1.-b)*um)*sn(j))

     !/dy*arad                                                   ! R

      return
      end

	function LVm(u,v,i,j,k,im,jm,km)
	  include 'par.inc'
        include 'com.inc'
      dimension
     *          u(im,jm,km),v(im,jm,km)
      real LVm
      LVm=0.

*************************************************************** step1
      
      vm=v(i,j,k)
      vw=v(i-1,j,k)
      ve=v(i+1,j,k)

      uw=(u(i,j,k)+u(i,j-1,k))/2.
      ue=(u(i+1,j,k)+u(i+1,j-1,k))/2.

        a=1.
	  if(ue.le.0.)a=0.
        b=1.
	  if(uw.le.0.)b=0.



        arad=2./rad/(sn(j)+sn(j+1))   

                
              dxw=x(i)-x(i-1)
              dxe=x(i+1)-x(i)
              dx=(dxw+dxe)/2.


          LVm= LVm +(ue*(a*vm +(1.-a)*ve) - uw*(b*vw +(1.-b)*vm))

     !/dx*arad                             !  R


*****************************************************************  step2
       vu=v(i,j-1,k)      
       vm=v(i,j,k)
       vd=v(i,j+1,k)

       vdd=v(i,j+1,k)
      if(j.lt.jss)vdd=v(i,j+2,k)
       vuu=v(i,j-1,k)
      if(j.gt.2)vuu=v(i,j-2,k)


        a=1.
	  if(vd.le.0.)a=0.
        b=1.
	  if(vu.le.0.)b=0.



         LVm= LVm + (vd*(a*vm+(1.-a)*vdd)*sn(j+1) 
     !       - vu*(b*vuu+(1.-b)*vm) *sn(j)) /dy/2.*arad   ! R

      return
      end



      function advt(u,v,i,j,k,im,jm,km)
        include 'par.inc'
        include 'com.inc'

c      integer ku(im,jm)
      dimension
     *          u(im,jm,km),v(im,jm,km)

      advt=0.

              arad= 2./rad/(sn(j)+sn(j+1))      
********* step1
              aw=u(i,j,k)
              ae=u(i+1,j,k)
              dxw=x(i)-x(i-1)
              dxe=x(i+1)-x(i)
              dx=(dxw+dxe)/2.
              if(k.ge.kp(i+1,j))ae=0.    !!!!  12.09.

          advt= advt+ (ae-aw)/dx*arad


**********  step2

              as=v(i,j,k)*sn(j)
              an=v(i,j+1,k)*sn(j+1)
              if(k.ge.kp(i,j+1))an=0.    !!!!!!  12.09.

          advt= advt+ (an-as)/dy*arad

      return
      end

  
      function advu(u,v,w,i,j,k,im,jm,km,tax,ll,ir)
        include 'par.inc'
        include 'com.inc'


      dimension
     *          u(im,jm,km),v(im,jm,km),w(im,jm,km)
     *          ,tax(im,jm)

      advu=0.
              ap=u(i,j,k)
              ut1=2*u(i,j,k)/rad/(sn(j)+sn(j+1))

********* step1

       ii=(i-1)*(is-i)
       if(ii.ne.0)then
              aw=u(i-1,j,k)
              ae=u(i+1,j,k)
	if(ir.eq.2)ae=u(i,j,k)
              dxw=x(i)-x(i-1)
              dxe=x(i+1)-x(i)
              dx=dxw+dxe

          advu= advu+ ut1*(dxw**2*(ae-ap)+dxe**2*(ap-aw))/dxe/dxw/dx
     #              -ut1*ut1*dt*((ae-ap)/dxe-(ap-aw)/dxw)/dx
              vt1=((v(i,j,k)+v(i,j+1,k))*dxw
     #          +(v(i-1,j+1,k)+v(i-1,j,k))*dxe)/dx/rad/2.
      if(ir.eq.2)vt1=(v(i,j+1,k)*dxw*2.
     #          +(v(i-1,j+1,k)+v(i-1,j,k))*dxe)/dx/rad/2.
 
 
              wt1=(dxw*w(i,j,k)+dxe*w(i-1,j,k))/dx         !! norma
              if(k.ne.1)wt1=wt1/2.
     #          +(dxw*w(i,j,k-1)+dxe*w(i-1,j,k-1))/dx/2.

  
             elseif(i.eq.1)then    !!!!!   if i=1 or i=is -> ! 
                 vt1=(v(i,j,k)+v(i,j+1,k))/rad/2.            !
                 wt1=w(i,j,k)                                !
                 if(k.ne.1)wt1=wt1/2.+w(i,j,k-1)/2.          !
               else                                          !
                  vt1=(v(i-1,j+1,k)+v(i-1,j,k))/rad/2.       !
                  wt1=w(i-1,j,k)                             !
                  if(k.ne.1)wt1=wt1/2.+w(i-1,j,k-1)/2.       !
      endif

********* step2

      if(j.ne.1)then
           as=u(i,j-1,k)
	if(ir.eq.2)as=ap
           if(ku(i,j-1).lt.k) as=ap*ll
         else
         as=ap
         if(ku(i,j).lt.k) as=ap*ll

      endif

      if(j.ne.js-1) then
            an=u(i,j+1,k)
           if(ku(i,j+1).lt.k) an=ap*ll
           else
         an=ap
      if(ku(i,j+1).lt.k) an=ap*ll
      endif

         advu=advu+vt1*(an-as)/2./dy
     *    -vt1*vt1*dt*(an+as-2.*ap)/2./dy/dy
   


****** vertical advection


      if(k.eq.1)then
       advu=advu - wt1*tax(i,j)/ql

      else

       au=u(i,j,k-1)
       ad=u(i,j,k+1)     !!!  here  uh=vh=0

       dzd=z(k+1)-z(k)
       dzu=z(k)-z(k-1)
       dz=dzd+dzu

                if(k.eq.2)then

       advu=advu+wt1*((ad-ap)/dzd-tax(i,j)/ql)/2.
     #   - wt1*wt1*dt*((ad-ap)/dzd+tax(i,j)/ql)/(dzd/2.+dzu)/2.

  

                  else

       advu= advu
     #   + wt1*((ad-ap)*dzu**2+(ap-au)*dzd**2)/dzd/dzu/dz
     #   - wt1*wt1*dt*((ad-ap)/dzd-(ap-au)/dzu)/dz

  
       endif
       endif

      continue
      return
      end

      function advv(u,v,w,i,j,k,im,jm,km,tay,ll,ir)
        include 'par.inc'
        include 'com.inc'

 
      dimension
     *          u(im,jm,km),v(im,jm,km),w(im,jm,km),
     *          tay(im,jm)

      advv=0.
              ap=v(i,j,k)
              vt1=v(i,j,k)/rad

********* step1

       jj=(j-1)*(js-j)
       if(jj.ne.0)then
              as=v(i,j-1,k)
              an=v(i,j+1,k)
       if(ir.eq.5)as=ap
          advv= advv+ vt1*(an-as)/dy/2.
     *    -vt1*vt1*dt*(an+as-2.*ap)/2./dy/dy

              ut1=(u(i,j,k)+u(i+1,j,k)
     #          +u(i+1,j-1,k)+u(i,j-1,k))/rad/4./sn(j)
      if(ir.eq.5)ut1=(u(i,j,k)+2.*u(i+1,j,k)
     #       +u(i,j-1,k))/rad/4./sn(j)

              wt1=(w(i,j,k)+w(i,j-1,k))/2
              if(k.ne.1)wt1=wt1/2.
     #                 +(w(i,j,k-1)+w(i,j-1,k-1))/4.

             elseif(j.eq.1)then
                 ut1=(u(i,j,k)+u(i+1,j,k))/rad/2./sn(j)            !
                 wt1=w(i,j,k)                                !
                 if(k.ne.1)wt1=wt1/2.+w(i,j,k-1)/2.
               else
                  ut1=(u(i+1,j-1,k)+u(i,j-1,k))/rad/2./sn(j)
                  wt1=w(i,j-1,k)                             !
                  if(k.ne.1)wt1=wt1/2.+w(i,j-1,k-1)/2.
      endif

********* step2

      if(i.ne.1)then
              aw=v(i-1,j,k)
              if(kv(i-1,j).lt.k) aw=ap*ll
              dxw=x(i+1)/2-x(i-1)/2
              else
              dxw=x(i+1)-x(i)

************ if v=0 on boundary

             aw=ap*ll

************ if dv/dn=0

             if(kv(i,j).ge.k) aw=ap
        endif
      if(i.ne.is-1)then
              ae=v(i+1,j,k)
	if(ir.eq.5)ae=ap
              if(kv(i+1,j).lt.k) ae=ap*ll
              dxe=x(i+2)/2-x(i)/2
              else
              dxe=x(i+1)-x(i)

************ if v=0 on boundary

             ae=ap*ll

************ if dv/dn=0

             if(kv(i+1,j).ge.k) ae=ap
         endif
           dx=dxw+dxe

         advv= advv+ut1*(dxw**2*(ae-ap)+dxe**2*(ap-aw))/dxe/dxw/dx
     #              -ut1*ut1*dt*((ae-ap)/dxe-(ap-aw)/dxw)/dx



****** vertical advection


      if(k.eq.1)then
       advv=advv - wt1*tay(i,j)/ql

      else

       au=v(i,j,k-1)   
       ad=v(i,j,k+1)!!! here uh=vh=0

       dzd=z(k+1)-z(k)
       dzu=z(k)-z(k-1)
       dz=dzd+dzu

                if(k.eq.2)then

       advv=advv+wt1*((ad-ap)/dzd-tay(i,j)/ql)/2.
     #   - wt1*wt1*dt*((ad-ap)/dzd+tay(i,j)/ql)/(dzd/2.+dzu)/2.


                  else

       advv= advv
     #   + wt1*((ad-ap)*dzu**2+(ap-au)*dzd**2)/dzd/dzu/dz
     #   - wt1*wt1*dt*((ad-ap)/dzd-(ap-au)/dzu)/dz

       endif
       endif

      continue
      return
      end
