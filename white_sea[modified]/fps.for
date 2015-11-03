c      use numerical_libraries
c      use DFPORT
      include 'par.inc'                        ! include file, gives parameter values
      include 'com.inc'                        ! defines variables, common for all program blocks and subroutines
      
      common ns                                  ! ns - number of steps -  defined as a common variable 
      
      real*4 timestep(120000)
        
      real  sa(in,jn)
     *     ,thx(in,jn),thy(in,jn)                !bottom stress
     *     ,tax(in,jn),tay(in,jn)                !surface stress
     *     ,u(in,jn,kn),v(in,jn,kn),w(in,jn,kn)  !velocity components
     *     ,tav(in,jn,kn),sav(in,jn,kn),pav(in,jn)
     *     ,ut(in,jn,kn),vt(in,jn,kn)            !ones from prev.time step
     *  ,s(in,jn,kn),t(in,jn,kn),at(in,jn,kn),st(in,jn,kn) !temperature,salinity
     *,ro(in,jn,kn),ph(in,jn),pht(in,jn),phtt(in,jn) !dencity,surface pressure
     #     ,ta(in,jn),qt(in,jn),qb(in,jn)        !atmosph.temp,surface t-flux

     #     ,str(in,jn,kn),str0(in,jn),strt(in,jn,kn)     
      
     *     ,ae(in,jn),aw(in,jn),as(in,jn),an(in,jn),ap(in,jn)
     *     ,af(in,jn),x1(in),uh(in,jn),vh(in,jn)
     *     ,wr1(in,jn),wr(in,jn)
     *     ,fu(in,jn,kn), fv(in,jn,kn),us(in,jn,kn)      !work arrays
     *     ,energ(120000),hp(in,jn)
c       #     ,ene(3900),tim(3900)
     *     ,umt(in,jn,kn),wmt(in,jn,kn)

c****************************************************************
     *     ,smt(in,jn,kn)
     *     ,bmt(in,jn,kn),wav(in,jn,kn)
     #     ,ahz(in,jn,kn),pr(in,jn,kn),frf(in,jn,kn)
     #     ,tb(in,jn,kn),te(in,jn,kn),ahzt(in,jn,kn)
     #     ,tbt(in,jn,kn),tet(in,jn,kn)
     #     ,a3(kn),b3(kn),c3(kn),fuu(kn),fvv(kn)
     #     ,e2(kn),d2(kn),r2(kn),g2(kn),ef(kn),azz(kn)
     #   ,ua(kn),va(kn),pare(5)

     #     ,rc(in,jn)
     &  ,tam(5),txm(5),tym(5),sg(5),tg(5),vd(5),vo(5),tr(5),
     & dzg(12),taa(24),saa(24)
     # ,uav(in,jn,kn),vav(in,jn,kn)



       real pi(in,jn)  ! initial pressure and temperature Pa,Ta 
       real tam1(in,jn),tam2(in,jn) !   ?????
       real pam1(in,jn),pam2(in,jn) !   ?????


      REAL Ts(in,jn,kn),Ss(in,jn,kn),Usum(in,jn,kn),Rosum(in,jn,kn),  ! Temperature, Salinity, X-Velocity, Density, Y-Velocity, Z-Velocity, Height over average level for (X_i, Y_j, Z_k) points
     #Vsum(in,jn,kn),Wsum(in,jn,kn),Zs(in,jn)  ! для записи осреднения
      integer kh(inn,jnn),inx(12),iny(12)         
     #       ,kpp(in,jn)
c     #     ,jju(in,jn,kn,72),jjv(in,jn,kn,72),ii


!!!!!!!!!!!!!!!!!!!!!!!!!!! for  POLY_WS_2007  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      REAL Uf(8,7,kn),Vf(8,7,kn),Wf(8,7,kn)
      Character*8   file11(8)
      Character*8   file12(8)
      Character*8   file13(8)
      data file11/'U1','U2','U3','U4','U5','U6','U7','U8'/
      data file12/'V1','V2','V3','V4','V5','V6','V7','V8'/
      data file13/'W1','W2','W3','W4','W5','W6','W7','W8'/
!!!!!!!!!!!!!!!!!!!!!!!!!!! for POLY_WS_2007  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      parameter (fpi=3.141592)
      character*2 fnum(25)

      data taa/7.61,6.91,5.16,4.32,3.63,2.39,1.18,.25,-.46,-.41,-.41,
     &-.48,-.74,-.88,-1.02,-1.12,-1.19,-1.22,-1.37,-1.44,-1.45,-1.49,
     &-1.53,-1.56/

      data saa/2.85,3.3,4.42,5.49,6.12,6.72,7.15,7.4,7.64,7.84,7.95,
     &7.96,8.24,8.34,8.45,8.75,8.95,9.01,9.22,9.39,9.53,9.59,9.65,9.7/   





      data tam/2.7,8.2,11.6,11.5,8./         ! t° атмосферы по месяцам

      data txm/5.,-80.,-120.,-70.,-70./      ! ветер по месяцам X-составляющая
      data tym/70.,-10.,120.,-10.,30./       ! ветер по месяцам Y-составляющая

      data sg/28.81,29.22,29.35,29.46,29.80/ ! S в Горле по месяцам  climate
      data tg/3.0,3.01,6.5,8.22,7.7/         !-.49 T в Горле по месяцам climate

c     data tg/3.0,3.01,7.,9.,8/         ! NEW  T в Горле по месяцам    WARM

      data vd/18.06,10.38,4.92,3.46,3.79/ ! скорость течения Онеги ? по мес.
      data vo/12.04,6.92,3.28,2.30,2.52/  ! скорость течения Двины ? по мес.

c      data vd/.18,.10,.05,.03,.04/    ! скорость течения  NO RAINS !
c      data vo/.09,.04,.01,.01,.02/

      data tr/5.,12.,18.,16.,10./






c координаты уровня при втоке *********************

      data inx/120,121,122,123,124,125,126,127,128,129,130,131/  
      data iny/12,13,14,15,16,17,18,19,20,21,22,23/  
      data fnum/'01','02','03','04','05','06','07',
     $'08','09','10','11','12','13','14','15','16',     
     $'17','18','19','20','21','22','23','24','25'/



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  MODIFIED 9.12.03.

c        print *,'ntype=? 0= 1 day  nnnnn'
c        read *,ntype 
         ntype=1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        ln=1 
c--------------------------------------------------
      
      im=133
      jm=110
      km=24       
      kmm=24          
      n2=10

c     open(40,file='out')   !!!!!!!!!!!!   c -  30.11.03
       


      str=0.
      str0=0.
      strt=0.

      open(41,file='tmay')
      open(42,file='smay')
      read(41,*)t  !!!!                    95 year i.c.  -> nonhomo..
      read(42,*)s   !!!


!!!!!!!!!!!!!!!!!!!!!   STR    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c       str(105,77,2)=100.     ! ARKHANGELSK
c       str(105,77,1)=100.
!!!!!!!!!!!!!!!!!!!!!   STR    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!   STR    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c       str(95,57,2)=100.       ! front
c       str(95,57,1)=100.
!!!!!!!!!!!!!!!!!!!!!   STR    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       

      call cond(nstep,nprin,nk,niter,al,al1,ctah,ct,cadv,eps,
     #          aaa,qwa,cu1,x1,energ,timestep,ns,taa,saa,
     #          kh,s,t,ta,u,v,w,tx,ty,thx,thy,ph,pht,phtt,im,jm,km,smt
     *          ,hp,kmm,aday,p0d,p0md,cst,str)




       ahzt=9.

      call kk(hp,im,jm)

      kp(120,11)=5   !  IMP ! GORLO
      it(120,11)=3   !  IMP !


       kp(48,74)=3   !!  СПП - съемка,   гора





        

!!!!!!!!!!!!!!!!!!!!!!  определение ПРОМЕЖУТОЧНОГО ТЕПЛОГО СЛОЯ  !!!!!!!!!!!!!!
      goto 493
493   continue
      alb=1.e5          !!!! test for turbulence / temp

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




      call incbe(tb,te,frf,pr,prt,ahz,ef,
     #           tb0,te0,im,jm,km,
     #           wt,drdt,pare,alz,z0,rfcr,qb, !initial
     #           l0t,lbt,l0b,lbb,l0e,lbe,ql,prb,alb)

      close(11)



      nf=int( 60*60/dt )

      if(ntype.eq.0)nstep=nprin
      m=0


              
      time1=0. 
      do j=1,jss
      do i=1,iss
         iit=(it(i,js-j)-2)*(it(i,js-j)-1)*(it(i,js-j)-3)
         if(iit.eq.0)kh(i,j)=99
      enddo
      enddo

      alt=al
      qlt=ql/prt/2.

      call DENSw(t,s,ro,im,jm,km,ns)          !density calculation

        tim2=60*60*24
        tim=ns*dt
        tim=1.e6
 
      call ture(u,v,fu,fv,frf,tax,tay,pr,ef,
     # ahz,azz,a3,b3,c3,fuu,d2,e2,
     # tb,te,ro,qt,qb,tbt,tet,im,jm,km,z0,tb0,te0,
     # pare,alz,prb,rfcr,qbh,qeh,drdt,
     # l0b,lbb,l0e,lbe,alb)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1


      lmm=1
      if(lmm.eq.2)then
         call Ravtime(us,fu,fv,umt,im,jm,km,kmm,1,0,1)
         do i=1,im
         do j=1,jm
           fu(i,j,1)=fu(i,j,1)-wmt(i,j,1)
         enddo
         enddo
      endif

      do k=1,km
         azz(k)=9.
      enddo
      do j=1,jm
      do i=1,im
         uh(i,j)=0.
         vh(i,j)=0. 
         do k=1,km
            ahz(i,j,k)=9.
            tav(i,j,k)=0.
            sav(i,j,k)=0. 
            uav(i,j,k)=0.

         enddo
      enddo
      enddo

            vav=0.


      time0=0.
      if(ns.gt.0)time0=timestep(ns)
      if(ns.eq.0)aday=0.
      nsave=1

      print *,'time steps'
      secm=3600.*24.*30.
      pnv=0.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

c_____   сдвиг по времени для уровня =0 на 15 мая 1995 г 


c     dt0=60.*60.*3.


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c____________________________

      do k=1,ks
         saa(k)=saa(k)+20.   !???????????????????????????
      enddo

c     lnf=0
      nfd=330
      ic=1 ! счетчик числа записей в по месяцам
      open(758,file='stmp.dat') ! Запись S>=30

      Ts=0.0  !!сумматор T, S и пр. 
      Ss=0.0
      Rosum=0.0
      Usum=0.0
      Vsum=0.0
      Wsum=0.0
      Zs=0.0
      Nsum=0  ! счетчик для осреднения за 10 суток



       !!!!  рассогласовано: кр-массив температура и  соленость
       

      kp(120,14)=4      
      kp(120,13)=4
      kp(115,18)=4



      kp(35,86)=1       !!!!!!!  задание ББК при   ->   it(i,j)=3


      !!!!  построение рельефа для смещенных скоростей (ku(i,j)и kv(i,j))
      !!!!  по рельефу 
      !!!!  центров боксов ( массив kp(i,j)) 



c     goto 54

        do 112 i=1,im
        do 112 j=1,jm
         if(kp(i,j).eq.2)kp(i,j)=3
        ke=kp(i,j)
        hp(i,j)=zw(ke)
  112    continue

         do 2212 j=1,jm
        ku(1,j)=kp(1,j)
        do 2212 i=2,im
        ku(i,j)=min0(kp(i-1,j),kp(i,j))
 2212    continue

        do 312 i=1,im
        kv(i,1)=kp(i,1)
        do 312 j=2,jm
        kv(i,j)=min0(kp(i,j-1),kp(i,j))
  312    continue









      al11=al1



!!!!!!!!   параметры записи разрезов  АКУСТИКА   !!!!!!!!!!!!!!!! 

       goto 402
      nACO=1 !480
      nA=0
      num=1 
      nstep=1 !480*3
402   continue

!!!!!!!!   end  параметры записи разрезов  АКУСТИКА   !!!!!!!!!!!!!!!! 

!!!!!!!!!!!!!!!!!!! for dim4 WS 2007   !!!!!!!!!!!!!!!!!!!!!!!!!

       ND=1     ! recording file UVW for DIM4
     
       open(11,file='file11(1)',access='direct',recl=8*7*24)
       open(12,file='file12(1)',access='direct',recl=8*7*24)
       open(13,file='file13(1)',access='direct',recl=8*7*24)



!       ns=34065       !     absolute start DIM4_WS_2007     
!       nsback=34204   !        absolute global back DIM4_WS_2007

!     ns=1    !         real start DIM4_WS_2007   
!     nsback=139  !       real global back DIM4_WS_2007

      i1=51
      i2=58
      j1=35
      j2=41

!!!!!!!!!!!!!!!!!!!   for  dim4 WS 2007   !!!!!!!!!!!!!!!!!!!!!!!!!






       open(82,file='ta.dat',access='direct',recl=4*133*110)
       open(81,file='pa.dat',access='direct',recl=4*133*110)
       open(540,file='levelsolo.dat')



         id=0
          ir=0
           idd=0
            irr=0



       uav=0.
       vav=0.
       wav=0.
       tav=0.
       sav=0.
       pav=0.




       nr=1   ! need to save by DUMP, if to continue (atm. assimilation)
       nrr=2
       n6=0


c
c _______________ ОСНОВНОЙ ЦИКЛ ПО ВРЕМЕНИ  _____________


      do 9 nn=1,nstep



       go to 779  !!!!    ACOUSTIC     don't work

      !!!!!!!!!!!!!!    ACOUSTIC   !!!!!!!!!!!!!!!!!  

      nA=nA+1

c     if(nA.eq.nACO) call ACO1(t,s,ph,pz,ro,num)
c     if(nA.eq.nACO) call ACO2(t,s,ph,pz,ro,num)  ! don't modified

      if(nA.eq.nACO) num=num+1
      if(nA.eq.nACO) nA=0

779   continue


      !!!!!!!!!!!!!!    ACOUSTIC   !!!!!!!!!!!!!!!!!  






      n3=1000
      nsave=nsave+1



      cstep=1    !!!!!  старт с дивергентной схемы адвекции

      
      ct1=ct
      ns=ns+1

      timestep(ns)=dt
      if(ns.ne.1) timestep(ns)=timestep(ns-1)+dt

      tim2=60*60*24
      tim0=nn*dt/180.
      tim=1.e6

      amon=timestep(ns)/secm
      mon0=int(amon)
      mon=mon0+1

      
      
C     ЗАПИСЬ ДАННЫХ ЧЕРЕЗ 15, 30... суток
      time15=24*3600*15 !sec in 15 days

      if(mod(timestep(ns),time15).EQ.0) then    
         ic=int( timestep(ns)/time15 )
            
         call savemonth(ic,ns,s,t,u,v,w,ph
     #                  ,pht,im,jm,km)
         print  *,ic

      endif




        timhd=timestep(ns)/3600.



      if(mon.eq.5)goto 9       !!!! mon=5 -> mon+1=6 -> overflow


      
c Большая начальная горизонтальная вязкость при запуске
c      if(ns.LE.480) al1=1.0E6
c      if((ns.GT.480).AND.(ns.LT.960)) al1=2.0E5
c      if(ns.GE.960) al1=al11 



      rns=1.0   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 


      sgc=sg(mon)+((timestep(ns)-secm*mon0)*(sg(mon+1)-sg(mon)))/secm       
      tgc=tg(mon)+((timestep(ns)-secm*mon0)*(tg(mon+1)-tg(mon)))/secm
      trc=tr(mon)+((timestep(ns)-secm*mon0)*(tr(mon+1)-tr(mon)))/secm
      vdc=vd(mon)+((timestep(ns)-secm*mon0)*(vd(mon+1)-vd(mon)))/secm       
      voc=vo(mon)+((timestep(ns)-secm*mon0)*(vo(mon+1)-vo(mon)))/secm
           
      uwx=  txm(mon)+((timestep(ns)-secm*mon0)*
     &(txm(mon+1)-txm(mon)))/secm
      uwy=  tym(mon)+((timestep(ns)-secm*mon0)*
     &(tym(mon+1)-tym(mon)))/secm
      uw=sqrt(uwx*uwx+uwy*uwy) !модуль скорости

!суточный ход температуры воздуха MAIN  was .8/.4

      tac=tam(mon)+((timestep(ns)-secm*mon0)*(tam(mon+1)-tam(mon)))/secm

      tacV=-.3*tac*sin(da1*20.*timhd) !min ta 5-00  max ta near  13-30
      tac=tac+tacV

c     print *,' ta, taV   ',  tac,'   ',tacV

!суточный ход температуры воздуха MAIN was .8/.4




      do j=1,jm
      do i=1,im

         ta(i,j)=tac

         tax(i,j)=rns*.000006*uw*uwx  +RAND(0)*0.05     !!!! was  .000006*  MAIN
         tay(i,j)=rns*.000006*uw*uwy  +RAND(0)*0.05     !!!!    



         if(j.le.23.and.it(i,j).eq.3)then

           do k=1,12 ! соленость и температура в Горле
               t(i,j,k)=tgc     +(-1.0-tgc)*z(k)/z(12)  *0.8  
               s(i,j,k)=sgc     +(29.9-sgc)*z(k)/z(12)  *0.8   

       if(s(i,j,k).GT.30.) print *,'Gorlo s>30',s(i,j,k),i,j,k
       if(t(i,j,k).lT.-1.6) print *,'Gorlo t<-1.6',t(i,j,k),i,j,k

ccc      print*,t(i,j,k),s(i,j,k)

           enddo

           do k=13, kn
              t(i,j,k)=-0.5
              s(i,j,k)=29.8
           enddo
           
         endif    
      enddo
      enddo

c      print*,tay(5,5)
c              pause 111


      time1=time1+dt/60./60.
      
      
       
       goto 500

       anu=.002       !  NORMA UMBA 0.008 , но 0.05 дает   разумные результаты  

       if(ns.lt.30725) goto  500
c      интерполяция по времени первичных полей Ра и Та
        nsr=ns-30719   ! 30719       
       cnm= (timestep(nsr-1)-dt)/(6.*3600.)
        iterm=int(cnm)
         acnm=aint(cnm)
        nterm1=iterm+1
       nterm2=nterm1+1
      if((nn.eq.1).or.(cnm.eq.acnm))then
      if(nn.eq.1)then
       write(6,*)'pam1',nterm1,cnm

      read(81,rec=nterm1)pam1
c     write(6,*)'pam1'
      read(82,rec=nterm1)tam1
       end if
c       write(6,*)'tam01',nterm1,timestep(ns)/(3600.*24.),
c     &time0p/(3600.*24)
      if(nn.gt.1)then
      do j=1,jn
      do i=1,in
      tam1(i,j)=tam2(i,j)
      end do
      end do
      end if
c     write(6,*)'tam2',nterm2,timestep(ns)/(3600.*24.)
      read(81,rec=nterm2)pam2
      read(82,rec=nterm2)tam2
      tm0= timestep(nsr-1)-dt
c     close (12)
       print*, 'taa1',timestep(nsr-1)-dt,tm0
     &,nterm1,nterm2
      end if
      do j=1,110
       do i=1,133
      ta(i,j)=   tam1(i,j)+((timestep(nsr-1)-dt-tm0)*(tam2(i,j)-
     &tam1(i,j)))/21600.
      pi(i,j)=  pam1(i,j)+((timestep(nsr-1)-dt-tm0)*(pam2(i,j)-
     &pam1(i,j)))/21600.
       end do
        end do

c      ******************************************************************************
c     tx,ty and интерполяция по пространству  tx,ty
      call txty(pi,tax,tay,rad,omega,anu)

500   continue

c     nr=6    !test Akkerblom
c     nrr=7
c     n6=120
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       if(ns.lt.31200.or.ns.ge.33980) goto  5000      ! start SOM at 00:00 19.07.2007     local time

       if(ns.eq.31200)  read(81,rec=nr)pam1       
       if(ns.eq.31200)  read(82,rec=nr)tam1

       if(ns.eq.31200)  read(81,rec=nrr)pam2
       if(ns.eq.31200)  read(82,rec=nrr)tam2
        
       n66=120    ! number of the steps from time of the mesurement to next one

       if(n6.ge.n66)nr=nr+1
       if(n6.ge.n66)nrr=nr+1


       if(n6.ge.n66)    read(81,rec=nr)pam1       
       if(n6.ge.n66)    read(82,rec=nr)tam1

       if(n6.ge.n66)    read(81,rec=nrr)pam2
       if(n6.ge.n66)    read(82,rec=nrr)tam2

       n6=n6+1     ! index - from time of the mesurement to next one

       if(n6.gt.n66)n6=0

      do i=1,is
      do j=1,js
      pi(i,j)=pam1(i,j)+(real(n6)/real(n66))*(pam2(i,j)-pam1(i,j)) 
      ta(i,j)=tam1(i,j)+(real(n6)/real(n66))*(tam2(i,j)-tam1(i,j))
      enddo
      enddo

      pi=-pi        !   Sarkisian/Marchuk book 

      do j=2,jss
      alfa=sqrt(omega*cs(j)/4.e9)
      do i=2,iss
      tax(i,j)=1./2./alfa*(+ddx(pi,i,j,1,im,jm,1)             !sign     Sarkisian/Marchuk book 
     !      -ddy(pi,i,j,k,im,jm,km))

      tay(i,j)=1./2./alfa*(-ddx(pi,i,j,1,im,jm,1)             !sign     Sarkisian/Marchuk book 
     !      -ddy(pi,i,j,k,im,jm,km))
      enddo
      enddo

       if(ns.eq.31200)print*,'o"key assimilation, rec between  =',nr,nrr
       if(n6.ge.n66)    print*,'o"key assimilation, rec between  =',nr,nrr

             
5000  continue

cc     if(ns.gt.33240)  call prarray(tax,1)
cc     if(ns.gt.33240)  print*,' '
cc     if(ns.gt.33240)  call prarray(tay,1)
cc     if(ns.gt.33240)  pause 77



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!  LEVEL    LEVEL   LEVEL  LEVEL EVEL !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

c            dt0=60.*60.*3.
c          timh=timestep(ns)/3600.-dt0 !время=часы,со сдвигом на настройку начала


           da1=3.1415926/180.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

ccc         3216  -   основной сдвиг (в часах) от начала года до 00 часов 15 мая  ns=64320


           timh= 3216. + timestep(ns)/3600. ! Greenwhich strongly


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         
c          timh=4752. + timestep(ns)/3600. !уравнение сделано по Гринвичу       ( анализ на Соловках 2006)
                                           !4752 (часы) - прямой сдвиг от начала года на
                                           !00 часов 18 июля т.е. на 198 дней
                                                             
                                                       
c          timh=3216. + timestep(ns)/3600. !уравнение сделано по Гринвичу 
                                           !3216 (часы) - прямой сдвиг от начала года на
                                           !00 часов 15 мая т.е. на 134 дня
                                                             !
                                                             !
                                           !До 00 часов 18 июля имеем 17+30*17=64 дня
                                                             !т.е. ns=30720 это старт СОМ БМ СОЛО 2006                                                                                         ! ВЕРНО  ДЛЯ КЛИМАТА
                                                             !т.е. ns=64320+30720+15108=110148 это старт 1 съемки СПП                                                                      
      dzg(1)=.8088154793*5.1*cos(da1*((13.9430356*timh)+
     &274.2344961-216.8))+         
     &.8834757209*14.8*cos(da1*((15.0410686*timh)+
     &187.4798548-330.1))+         
     &1.037439823*209.7*cos(da1*((28.9841042*timh)+
     &100.8260815-334.0))+         
     &1.*58.9*cos(da1*((30.0410686*timh)-28.6))
       dzg(1)=rns*dzg(1)*980.         
      dzg(12)=.8088154793*1.4*cos(da1*((13.9430356*timh)+
     &274.2344961-272.7))+         
     &.8834757209*9.7*cos(da1*((15.0410686*timh)+
     &187.4798548-344.1))+         
     &1.037439823*237.9*cos(da1*((28.9841042*timh)+
     &100.8260815-358.9))+         
     & 1.*45.3*cos(da1*((30.0410686*timh)-62.0))
       dzg(12)=rns*dzg(12)*980.
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ccc   dzg(1)=0.0            !!  TIDE   OUT !!
ccc   dzg(12)=0.0
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      do ni=2,11
         dzg(ni)=dzg(1)+(ni-1)*(dzg(12)-dzg(1))/11.
      enddo


c       RAD    RAD     RAD
      do ni=1,12
         i=inx(ni)
         j=iny(ni)

      ph(i,j)=outf(ph(i,j),ph(i-1,j+1),pht(i-1,j+1),ph(i-2,j+2),
     #dzg(ni),0.,i,j,1,ns)

      enddo
c       RAD    RAD     RAD

c       print*,dzg(3)/980.
c       pause 555
 
      if(ns.eq.0)ct1=0.

c  переопределение давления Онеги и Двины    *****   **********
      do i=77,108
      do j=77,106
         if(kp(i,j+1).ne.1.and.it(i,j+1).eq.3.AND.it(i,j).EQ.0)
     #ph(i,j+1)=ph(i,j) 
         if(kp(i+1,j).ne.1.and.it(i+1,j).eq.3.AND.it(i,j).EQ.0)
     #ph(i+1,j)=ph(i,j)
      enddo  
      enddo


      cq=1.
      cu=1.
      cph=0.
      call coefNL(u,v,u,v,w,fu,fv,us,tax,tay,thx,thy
     #      ,ph,     ro,ahz,an,as,aw,ae,ap,af
     #      ,im,jm,km
     #      ,al1,cph,cu,cadv,cq, cstep)        !eq. calc.

      do 1 i=1,is
      do 1 j=1,js
         if(it(i,j).eq.2) af(i,j)=ph(i,j)
         if(ns.gt.1)then
            phtt(i,j)=pht(i,j)
            pht(i,j)=ph(i,j)
         endif
 1    continue


      
      call sipp(eps,ph,qt,an,as,aw,ae,ap,af,wr1,wr,im,jm,200)






       ut=u
       vt=v



      cq=0.
      cu=.45
      cph=1.
      continue

      call HRVL(u,v,ut,vt,w,fu,fv,us,tax,tay,thx,thy,
     * ph,ro,im,jm,km,cq,al1,cu,cadv,cph,
     * ahz,fuu,fvv,azz,e2,d2,r2,g2,a3,b3,c3,ua,va,z0,cstep)



      cq=1.
      cu=1.
      cph=1.
      cnn=nn/2.

c      print*,w(99,40,1),(ph(99,40)-pht(99,40))/dt/(-980.), ' <- w,dz/dt'
c      print*,w(96,52,1),(ph(96,52)-pht(96,52))/dt/(-980.), ' <- w,dz/dt'

       call ww(u,v,u,v,w,fu,fv,us,tax,tay,thx,thy,
     #ph,ro,ahz,im,jm,km,al,al1,cph,
     #cu,cadv,cq,ct,cstep)


ccc        uu=sqrt(u(80,60,1)**2+v(80,60,1)**2)
ccc        print*,uu


         
      call energy(energ(ns),u,v,im,jm,km,l1)



      tmp=24.*3600./dt !число шагов в сутках
      tempN=float(ns)/tmp

      print *,'energy = ',energ(ns),   ' ns=',ns,  ' days =',tempN



      goto 795
!!!!!!!!!!!!!!!!!!!!!!!!!!!  POLY_WS_2007 - recording barotropic components  !!!!!!!!!!
     
      if(ns.lt.33565)goto 795

       do i=i1,i2
       do j=j1,j2
       do k=1,kn
        Uf(i+1-i1,j+1-j1,k)=u(i,j,k)     
        Vf(i+1-i1,j+1-j1,k)=v(i,j,k)  
        Wf(i+1-i1,j+1-j1,k)=w(i,j,k)  
       enddo
       enddo
       enddo


      write(11,rec=(ns-33564))Uf  !
      write(12,rec=(ns-33564))Vf
      write(13,rec=(ns-33564))Wf


cc      print*,u(55,38,2)
cc      print*,'O KEY recording UVW, rec=,ns= ',rec,ns

cc    pause 555

795    continue


!!!!!!!!!!!!!!!!!!!!!!!!!!!  POLY_WS_2007  - recording barotropic components  !!!!!!!!!!





        l0s=2 !(flux qt=0 on surface)
        l0t=2


       do nri=1,2
c      ****************** Onega ********    
      i00=77
      i11=77
      j00=105
      j11=105
      vc=voc
      if(nri.eq.2)then
c      ***************** Dvina *****
        i00=101
        i11=107
        j00=77
        j11=83
        vc=vdc

      end if

      do j=j00,j11
      do i=i00,i11

         if(it(i,j+1).eq.3.and.it(i,j).eq.0) then

           do k=1,kp(i,j)
              v(i,j+1,k)=0.
              if(ph(i,j).le.6000.)v(i,j+1,k)=-rns*vc
           end do

         do k=1,kp(i,j)
            s(i,j+1,k)=s(i,j,k)
            t(i,j+1,k)=t(i,j,k)
            if(ph(i,j).le.6000.)then
              s(i,j+1,k)=11.   !!-4.+s(i,j,k) ! соленость в устье Двины
              t(i,j+1,k)=trc  !+t(i,j,k)/20.

c                 print*,'k,trc,t',k,trc,t(i,j+1,k)
c                 print*,'ke',kp(i,j+1)
c                 pause 11 
                   
                                end if
         end do
                                             end if


      if(it(i+1,j).eq.3.and.it(i,j).eq.0)then
       do k=1,ks
        u(i+1,j,k)=0.
        if(ph(i,j).le.6000.)u(i+1,j,k)=-rns*vc
       end do
        do k=1,ks
       s(i+1,j,k)=s(i,j,k)
      t(i+1,j,k)=t(i,j,k)
        if(ph(i,j).le.6000.)then
           s(i+1,j,k)=11.                !  соленость в устье Двины
           t(i+1,j,k)=trc !+t(i,j,k)/20.   ! +t(i,j,k)-5.
c                 print*,'t',t(i+1,j,k)
c                 print*,'ke',kp(i+1,j)
c                 pause 11  
                            end if
       end do

                                         end if 


        end do
        end do 
        end do
c     GOTO 879  !!!!   BELOMOR  DON"T  WORK


c       ***************************
c       Belomor canal, течёт всегда*
c       ***************************
      i=36
      j=86
       do k=1,kp(i,j)
         u(i-1,j,k)=1.     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       end do
       do k=1,kp(i,j)
          s(i-1,j,k)=14.   !!-4.+s(i,j,k)
          t(i-1,j,k)=trc   !+t(i,j,k)/17.

       enddo

       do j=1,jn
       do i=1,in
            sa(i,j)=s(i,j,1)
       enddo
       enddo
       
      at=t
      st=s
                                    
      call taflux(s,sa,qt,qwa,im,jm,2,uw)

      ii=0
      call HDRL(s,st,sa,qt,u,v,w,us,ahz,
     *          al1,pr,wt,im,jm,km,
     #          a3,b3,c3,fuu,d2,r2,cstep,azz,l0s,ph,0)  ! 1 - not abc,  0 - abc 

      call taflux(t,ta,qt,qwa,im,jm,2,uw)
      ii=0
      call HDRL(t,at,ta,qt,u,v,w,us,ahz,
     *          al1,pr,wt,im,jm,km,
     #          a3,b3,c3,fuu,d2,r2,cstep,azz,l0t,ph,0)  ! 1 - not abc,  0 - abc 


       do j=1,jn
       do i=1,in
          s(i,j,1)=s(i,j,2)
          t(i,j,1)=t(i,j,2)
       enddo
       enddo



c   qt - temperature flux is defined- for ture

       

        !!!!     контроль выполнения законов сохранения тепла и соли. 
c -------------- проверка и запись в STMP t < -2.0  или  s > 30
                   
           do j=1,jn
           do i=1,in
           do k=1,kn
      if(k.gt.kp(i,j))goto 123
       
      if(it(i,j).ne.3.AND.t(i,j,k).lt.-1.6)then
      print *,'t<-1.6 ',t(i,j,k),i,j,k,kp(i,j),it(i,j)
      write(758,777)t(i,j,k),i,j,k,kp(i,j),it(i,j),
     #ns
      endif 
      if(it(i,j).ne.3.AND.s(i,j,k).gt.30.)then 
      write(758,777)s(i,j,k),i,j,k,kp(i,j),it(i,j),
     #ns
      print *,'s> 30 ',s(i,j,k),i,j,k,kp(i,j),it(i,j)
      endif
 123  continue 

           enddo
           enddo
           enddo
 777  format(F10.4,6I6)
c--------------------------------------


       ahzt=ahz

      alb=3.e5
      call ture(u,v,fu,fv,frf,tax,tay,pr,ef,
     # ahz,azz,a3,b3,c3,fuu,d2,e2,
     # tb,te,ro,qt,qb,tbt,tet,im,jm,km,z0,tb0,te0,
     # pare,alz,prb,rfcr,qbh,qeh,drdt,
     # l0b,lbb,l0e,lbe,alb)



      ahz=(ahz+ahzt)/2.



       goto 980
c________________________________________________
c        коррекция плохой обусловленности системы по вертикали

      do i=1,in
      do j=1,jn
      do k=1,kn  

      if(k.gt.kp(i,j))goto 234
      ahz(i,j,k)=ahz(i,j,k)+RAND(0)*10 !коэф.верт. турб.вязкости
234   continue
      enddo 
      enddo
      enddo
c_________________________________________________
 
980   continue 
 

        call DENSw(t,s,ro,im,jm,km,ns)



       !###################################################################


ccc       goto 674

        !ЗАПИСЬ DUMP  ЧЕРЕЗ ПРОИЗВОЛЬНЫЙ ИНТЕРВАЛ = ri (в шагах)
       if(ns.gt.31200)  then
ccc       if(ns.gt.1)  then

      if(ir.eq.20)then

       id=id+1
       write(540,*)ph(46,74)/980.,1.*id

          print  *,'id=',id
          ir=0

                  endif
      ir=ir+1
                        endif


c ******************************************************



      if(ns.ge.31200.and.ns.lt.33704)       then  !  COM 2006 SOLO

      if(ns.eq.33565)call DUMP(53,ns,s,t,u,v,w,ph,pht,im,jm,km)

      nswr=480
      if(ns.ge.33565)nswr=20
      if(ns.eq.33565)irr=0

      if(irr.eq.nswr) then

       idd=idd+1

      call DUMP(idd,ns,s,t,u,v,w,ph,pht,im,jm,km)



          print  *,'idd =',idd
          irr=0


                     endif

          irr=irr+1
ccc       print  *,'ns,irr =',ns,irr

                           endif


       !################################################################


      if(ns.eq.33725) print *,  ' finish POLY  - 33704,
     *                            finish meteo (now) - 33980 '                ! finish POLY  - 33704, finish meteo (now) - 33980


  9   continue   !  GLOBAL  MAIN  ns



      call layer(bmt,rc,im,jm,km,tb0)

       close (11)
       close (12)
       close (13)


      stop
      end

*************************

      subroutine cond(nstep,nprin,nk,iter,al,al1,ctah,ct,cadv,
     #  eps,aaa,qwa,cu1,x1,energ,timestep,ns,taa,saa,
     #  kh,s,t,ta,u,v,w,tx,ty,thx,thy,ph,pht,phtt,im,jm,km
     *  ,smt ,hp
     *  ,kmm,aday,p0d,p0md,cstep,str)
        include 'par.inc'
        include 'com.inc'
       
       integer  kh(im-1,jm-1)
      real s(in,jn,kn),t(in,jn,kn),fi(jm),ta(im,jm)
     *     ,thx(im,jm),thy(im,jm),x1(im),taa(kn),saa(kn)
     *     ,u(im,jm,km),v(im,jm,km),tax(in,jn),tay(in,jn)
     *     ,w(im,jm,km),ph(im,jm),pht(im,jm),phtt(im,jm)
     *     ,energ(120000)
     *     ,str(im,jm,km)
     *     ,smt(im,jm,kmm),hp(in,jn),aff(im,jm)

c      DOUBLE PRECISION s(in,jn,kn),t(in,jn,kn)

      real*8 timestep(120000)


      parameter (fpi=3.1415926)

      open(8,file='par0.dat')
      open(3,file='hbot.dat')
c      pause 333
      open(2,file='rrrr.dat')
c     pause 444

c        write(40,*)'8 begin'

c     pause 111
        read(8,*)init,aaa
c      print *, 'after read par0'
c     pause 222
      
      read(8,*)dt,vis,p0d,p0md
c     print *,'  dt=',dt
c     pause 111
      read(8,*)rad,ql,al,al1,grad,omega,ddd,ddw
      read(8,*)nstep,nprin,nk,eps,iter
      read(8,*)u0,v0,t0,tx,ty,ctah,ct,cadv
      read(8,*)tan,tas,qwa,cu1
      read(8,*)cstep
      read(8,*)(zw(k),k=1,18)
      read(8,*)(zw(k),k=19,km)

      read(3,*)alamb0,almax,teta0,tetmax
c     print*,'l0,ll,f0,fl',alamb0,almax,teta0,tetmax
c     pause 2

      read(3,*)hp

c      print *,'tx,ty',tx,ty
c     pause 111 

      do j=1,jm
      read(2,1000)(it(i,j),i=1,im)
      enddo
1000  format(133i1)
        do k=1,km
c                                 print*,'zw',zw(k)
        zw(k)=zw(k)*100.
        enddo
c                                  pause


      z(1)=zw(1)       !!!!! was deleted       
      do 1 k=2,kn
         z(k)=(zw(k)+zw(k-1))/2.
  1   continue

        do i=1,is
        do j=1,js
        hp(i,j)=hp(i,j)*100.
        if(hp(i,j).ne.0..and.hp(i,j).lt.(z(3)+0.4)) hp(i,j)=z(3)+0.4
         enddo
         enddo

      dt=dt*60.
      aga=ddd/dt
      ag=ddw/dt
      et=vis/dt
      dy=(tetmax-teta0)/(js-1.)*grad
      dx=(almax-alamb0)/(is-1.)*grad
      
 
    
  
      
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




      do 2 i=1,is
         x(i)=dx*(i-1)
         x1(i)=(almax-alamb0)/(is-1)*(1.*i-0.5)+alamb0
 2    continue
      do 4 j=1,js
         y(j)=90.-tetmax+(tetmax-teta0)/(js-1)*(1.*j-0.5)
         TETA=teta0*GRAD+(J-1.)*Dy
         SN(J)=SIN(TETA)
         CS(J)=COS(TETA)
  4   continue
*******************************************************
c      print*,dx*rad*sn(109),dy*rad,rad,grad
c     pause 1



      do 5 j=1,js
c      acs(j)=cs(1)+(cs(js)-cs(1))/(js-1)*(j-1)
c      asn(j)=(cs(js)-cs(1))
         acs(j)=cs(j)
         asn(j)=sn(j)
c      yy=2*(j-0.5)/(js-1.)*fpi
         yy=(j-0.5)/(js-1.)*fpi
         fi(j)=cos(yy)
c      fi(j)=-cos(yy)
  5   continue
*c      do 13 j=1,js
*c      sn(j)=1.
*c      cs(j)=1.
*c  13  continue
      et=(ql*omega*acs(6))**0.5*ctah/z(ks)!!!!!! 04.08.02 delete c  



      if(init.eq.0)then  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



            ns=1  !  


            u=0.
            v=0.
            w=0.
            ph=0.

       goto 58
        do i=1,im
        do k=1,km
        do j=1,jm
           s(i,j,k)=saa(k)+20.
           t(i,j,k)=taa(k)
        enddo
        enddo
        enddo  
58    continue
              


      print *,t(101,72,3),t(110,30,3)



         else     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!  init = 2    -> reading

       nm=11
       rewind 11



      open(11,file='c:\somws_2007\mon')  !!


      read(nm,*)ns   !,aday     !,wq,wq,wq,wq,wq,wq,wg  ! for a0-climate  file

      read(nm,2012)u
      read(nm,2012)v
      read(nm,2012)w
      read(nm,2012)ph
      read(nm,2012)pht
      read(11,2012)t
      read(11,2012)s

      endif


      do 16 i=1,is
      do 16 j=1,js
      tax(i,j)=tx
      tay(i,j)=ty
 16    continue



      close(8)
      close(2)
      close(3)
      close(4)

      close(11)
      close(41)
      close(42)  

2012  format(20e12.6)
      do j=1,jss
      do i=1,iss
      iit=(it(i,js-j)-2)*(it(i,js-j)-1)*(it(i,js-j)-3)
      if(iit.eq.0) kh(i,j)=99
      enddo
      enddo


ccc        ns=1  !!!!  TEMP !


      return
      end
c---------------------------------------------
        subroutine point7(u,v,t,s,im,jm,km)
        real u(im,jm,km),v(im,jm,km),t(im,jm,km)
     *  ,s(im,jm,km)
        include 'par.inc'
        include 'com.inc'


        open(5,file='river.dat')
        read(5,*)t0,th,s0,sh,v0 
        close(5)
        dth=th-t0
        dsh=sh-s0
        do 1 i=1,im
        do 1 j=1,jm
        if(it(i,j).ne.7)goto 1
        do 2 k=1,kp(i,j)
        v(i,j,k)=v0
        s(i,j,k)=s0+dsh*z(k)/z(kp(i,j))
        t(i,j,k)=t0 +dth*z(k)/z(kp(i,j)) 
        u(i,j,k)=0.
2     continue
1     continue
      return
         end
*************************
      function cstadv(nn,cst) 
      icst=int( cst )
        au=float(nn)/cst-nn/icst
      if(au.lt.1.e-3)then
      cstadv=0.
      else
      cstadv=1.
      endif
      return
      end
        subroutine tur0(ahz,im,jm,km,ql)
        real ahz(im,jm,km)
        do 1 i=1,im
        do 1 j=1,jm
        do 1 k=1,km
1       ahz(i,j,k)=ql
        return
        end 

        
      
*####################################################################
            function outs(aout,ap,apt,ain)
********
********
********
********
      outs=0.
      da1=ain-ap
      if(abs(da1).lt.1.e-8)then
      outs=ap
      else
      dta=ap-apt
      c1=dta/da1
      c0=1.
      c=amin1(c1,c0)
      if(c.lt.0.)then
      outs=ap
      else
      outs=(aout*c0+c*ap)/(c0+c)
      endif
      endif
      return
      end
c----------------------------------------
      function outf(aout,ap,apt,ain,a0,u,i1,j1,k1,ns)
        include 'par.inc'
        include 'com.inc'
      cns=float(ns)
c     print *,'ns=  ',ns,' u= ',u,' u> .1 - t/s',' aout= ',aout 
      
c      outf=0.
c     if(kp(i1,j1).lt.k1) then
c        outf=aout !(aout+a0)*0.5    !!! for T,S only,   ph d'nt be 
c        goto 1  !go to exit
c     endif
c     if(kp(i1+1,j1-1).lt.k1) a0=aout
c     if(kp(i1-1,j1+1).lt.k1) ap=aout
c     if(kp(i1-1,j1+1).lt.k1) apt=ap
c     if(kp(i1-2,j1+2).lt.k1) ain=ap
      c0=360000/(3*60)*sqrt(2.)  !!! заменить на DX/DT  
        sns=480. !!!!!! parametr determ's relaxetion speed init. cond.
      da1=ain-ap
      if(abs(da1).lt.1.e-10)then
        outf=a0   !*(1.-sns/(cns+sns))+aout*(sns/(sns+cns))
c        outf=(a0+aout)/2.
c       outff=outf-aout
c       if(u.gt.0.1)print *,'divide by small value, delta',outff   
ccc        if(u.lt.0.1) outf=a0     !!! for ph    u=0
      else
        dta=ap-apt
        c1=c0*dta/da1
        if(abs(c1).GT.abs(c0)) then
           signC=c1/abs(c1)
       
           c1=c0*signC
c          print *,'c1 changed=',c1
        endif
        if(c1.LT.0.) then
           
           outf=a0    !*(1.-sns/(cns+sns))+aout*(sns/(cns+sns))
c            outf=(a0+aout)/2.
                  outff=outf-aout
c     if(u.gt.0.1.and.abs(outff).gt..5)print*,'d,a0,aout',outff,a0,aout
c                 pause
             if(u.lt.0.1)  outf=a0
        else

      c10=c1/c0
      if(abs(c10).gt..01)c10=.01
      

         outf=(aout+c10*ap)/(c10+1)   !!!!   RADIATION

c     outff=aout-outf
c     if(u.gt.0.1)print *,'DELTA  radiation, c1/c0   ','  ',outff,c1/c0
c     pause 


             if (u.lt.0.1)   outf= (outf-aout)+a0   !!! free wave out and in



c          if (kp(i1+1,j1-1).LE.k1)  outf=aout
        endif
      endif
c1     continue
c      if(abs(outf-aout).gt..2)print *,'C =  ',c1,' a0=',a0,' k=',k1
      return
      end



c----------------------------------------
      function out11(aout,ap,apt,ain,cf)
        include 'par.inc'
        include 'com.inc'

        c0=dy*rad/abs(dt)  !!! grid speed 
 
        da1=ain-ap

        if(abs(da1).lt.1.e-8)then
        out11=aout
        else

        dta=ap-apt
        c1=-c0*dta/da1

        if(c1.gt.0.) then  !  WR gt
           
        out11=aout

        else

        c10=c1/c0

        if(abs(c10).gt.abs(cf))c10=cf*abs(c1)/c1
      
c        out11=aout*(c10+1)-c10*ap  !!!!   RADIATION явная схема

         out11=(-c10*ap+aout)/(1-c10) !!!! неявная схема

                   
        endif 
        endif

      return
      end


      function outSS(aout,ap,apt,ain,cf)
        include 'par.inc'
        include 'com.inc'

        c0=dy*rad/abs(dt)  !!! grid speed 
 
        da1=ap-ain

        if(abs(da1).lt.1.e-8)then
        outss=aout
        else

        dta=ap-apt
        c1=-c0*dta/da1

        if(c1.lt.0.) then   !  WR lt
           
        outss=aout

        else

        c10=c1/c0

        if(abs(c10).gt.abs(cf))c10=cf*abs(c1)/c1
      
c        outss=aout*(1-c10)+ap*c10   !!!!   RADIATION явная схема

         outss=(c10*ap+aout)/(1+c10) !!!!  неявная схема
           

        endif
        endif

      return
      end




















c        ******************************************
      function ares(a,at,it,i,j,k,im,jm,km,k1m,k1,a0,a1,hp,u,v)
      integer it(im,jm)
      real a(im,jm,km),at(im,jm,km),hp(im,jm)
     *     ,u(im,jm,k1m),v(im,jm,k1m)

*     bound.cond on liquid boundary for p,t,s
*     a0=0.for p, a0=at for T,S ,
*     a1=p0.for p, a1=0. for T,S ,
      ares=0.
         ainw=gg(a(i-2,j,k),a(i-1,j,k),hp(i-2,j))
         aine=gg(a(i+2,j,k),a(i+1,j,k),hp(i+2,j))
         ainn=gg(a(i,j+2,k),a(i,j+1,k),hp(i,j+2))
         ains=gg(a(i,j-2,k),a(i,j-1,k),hp(i,j-2))
         ue=u(i,j,k1)
         vs=-v(i,j+1,k1)
         uw=-u(i+1,j,k1)
         vn=v(i,j,k1)
      j56=(it(i,j+1)-5)*(it(i,j+1)-6)
      i46=(it(i+1,j)-4)*(it(i+1,j)-6)
      if(it(i,j).eq.2)then
         ares=outf(a(i,j,k),a(i-1,j,k),at(i-1,j,k),ainw,a0,ue,i,j,k,ns)
         if(it(i,j+1).eq.5)ares=ares/2.+
     #      outf(a(i,j,k),a(i,j+1,k),at(i,j+1,k),ainn,a0,vs,i,j,k,ns)/2.
          ares=ares+a1
*####################################################################

      elseif(it(i,j).eq.9)then
       ares=outf(a(i,j,k),a(i-1,j,k),at(i-1,j,k),ainw,a0,ue,i,j,k,ns)/2.
     #    + outf(a(i,j,k),a(i,j-1,k),at(i,j-1,k),ains,a0,vn,i,j,k,ns)/2.
         ares=ares+a1

*####################################################################
      elseif(it(i,j).eq.7)then
         ares=outf(a(i,j,k),a(i,j-1,k),at(i,j-1,k),ains,a0,vn,i,j,k,ns)
         if(it(i+1,j).eq.4)ares=ares/2.+
     #      outf(a(i,j,k),a(i+1,j,k),at(i+1,j,k),aine,a0,uw,i,j,k,ns)/2.
          ares=ares+a1

*####################################################################
      elseif(j56.eq.0)then
         ares=outf(a(i,j,k),a(i,j+1,k),at(i,j+1,k),ainn,a0,vs,i,j,k,ns)
         if(it(i,j+1).eq.6)ares=ares/2.+
     #      outf(a(i,j,k),a(i+1,j,k),at(i+1,j,k),aine,a0,uw,i,j,k,ns)/2.
          ares=ares+a1
*######################################################################
      elseif(i46.eq.0)then
       ares=outf(a(i,j,k),a(i+1,j,k),at(i+1,j,k),aine,a0,uw,i,j,k,ns)/2.
          ares=ares+a1
      else
          ares=0.

       endif
       return
       end
       function gg(a,b,hr)
       if(hr.gt.0.)then
       gg=a
       else
       gg=b
       endif
       return
       end

*************
      
        subroutine abcu(ua,va,a3,b3,c3,fuu,fvv,
     #  e2,d2,r2,g2,f1,k1,ke,km) 

*  du/dt +fv = d(ahz*du/dz)/dz -u2
*  dv/dt -fu = d(ahz*dv/dz)/dz -v2
*
      real ua(km),va(km),
     *     fuu(km),fvv(km),
     *     a3(km),b3(km),c3(km),
     *     e2(km),d2(km),r2(km),g2(km)
 
  
        det=c3(k1)**2+f1*f1
      e2(k1)=-b3(k1)*c3(k1)/det
      r2(k1)=-b3(k1)*f1/det
        d2(k1)=(fuu(k1)*c3(k1)-f1*fvv(k1))/det
        g2(k1)=(fvv(k1)*c3(k1)+f1*fuu(k1))/det

      do 1 k=k1+1,ke
      detr=c3(k)+a3(k)*e2(k-1)
      deti=f1-a3(k)*r2(k-1)
 
      detz=detr*detr+deti*deti

      e2(k)=-b3(k)*detr/detz
      r2(k)=-b3(k)*deti/detz
      a2r=fuu(k)-a3(k)*d2(k-1)
      a2i=fvv(k)-a3(k)*g2(k-1)

      d2(k)=(detr*a2r-deti*a2i)/detz
      g2(k)=(detr*a2i+deti*a2r)/detz

  1   continue

      ua(ke)=d2(ke)
      va(ke)=g2(ke)

      do 2 k=ke-1,k1,-1
      ua(k)=d2(k)       +e2(k)*ua(k+1)-r2(k)*va(k+1)
      va(k)=g2(k)       +r2(k)*ua(k+1)+e2(k)*va(k+1)
  2   continue

      continue
      return
      end

   
      subroutine prARRAY(p,kkk)
        include 'par.inc'
        include 'com.inc'
      real p(in,jn,kkk)   !!!   


c      open(19,file='prARRAY')

       do i=1,is
      do j=1,js
c     do k=1,ks
      if(it(i,j).eq.1)p(i,j,1)=0.
      enddo
      enddo
c     enddo



      i1=51
      i2=58

       do k=1,1  ! 5
       do j=35,41


       write(6,5) (p(i,j,kkk),i=i1,i2)    !!    
c       write(19,5) (p(i,j,kkk),i=i1,i2)    !!   

       enddo

       write(6,7)k
c      write(19,7)k

       i1=i1+10
       i2=i2+10

       enddo

5     format(8F7.1)
7     format(i2)

      return
      end
       
         subroutine txty(p,tx,ty,rad,omega,anu)
       real tx(133,110),ty(133,110),p(133,110)
      fpi=3.1415926
      da1=fpi/180.
      an0=23.
c     dax=2.
c     day=1.
       ds=300000.
      day=ds/rad
          do j=1,109
         an=an0*da1   +(j-1)*day
      alf=sqrt((omega*cos(an))/anu)
       cf=2.8e+5/(2.*alf)


        do i=1,132

c       print*,cf,alf,an,anu
c      pause 5

         dpx=(p(i+1,j)+p(i+1,j+1)-p(i,j)-p(i,j+1))/(2.*ds)  
          dpy=(p(i,j+1)+p(i+1,j+1)-p(i,j)-p(i+1,j))/(2.*ds)
         tx(i,j)=cf*(-dpx+dpy)  !
        ty(i,j)=-cf*(dpx+dpy)  !
       end do
      end do 
          do j=1,110
       tx(133,j)=tx(132,j)
       ty(133,j)=ty(132,j)
        end do
          do i=1,133
       tx(i,110)=tx(i,109)
       ty(i,110)=ty(i,109)
        end do


c       print*,cf,dpx,dpy



       return
         end

