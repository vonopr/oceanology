      subroutine pout(p0,p0t,p0md,p0d,timestep,ns)
        real*4 timestep(120000)

      parameter (fpi=3.1415926)

      p0=p0md*sin(2*fpi*timestep(ns)/12./60./60.)*980.
     # +p0d*sin(2*fpi*timestep(ns)/24./60./60.)*980.
      p0t=0.
      if(ns.ne.1)then
      p0t=p0md*sin(2*fpi*timestep(ns-1)/12./60./60.)*980.
     # +p0d*sin(2*fpi*timestep(ns-1)/24./60./60.)*980.
      endif
      return
      end
****************************
      SUBROUTINE D03UAFr(N1,N2,N1M,A,B,C,D,E,APARAM,IT,R,WRKSP1,WRKSP2,
     *                  IFAIL)
* all varibles are real
C     MARK 7 RELEASE. NAG COPYRIGHT 1978.
C     MARK 10A REVISED. IER-386 (OCT 1982).
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C
C     **************************************************************
C     D03UAF PERFORMS 1 ITERATION OF THE STRONGLY IMPLICIT PROCEDURE
C     AT EACH CALL TO CALCULATE THE SUCCESSIVE APPROXIMATE
C     CORRECTIONS TO THE SOLUTION OF A SYSTEM OF SIMULTANEOUS
C     ALGEBRAIC EQUATIONS FOR WHICH THE ITERATIVE UPDATE MATRIX IS
C     OF THE FIVE POINT MOLECULE FORM ON A TOPOLOGICALLY TWO-
C     DIMENSIONAL RECTANGULAR MESH.
C
C     STRONGLY IMPLICIT PROCEDURE ROUTINE FOR 2 DIMENSIONAL 05
C     POINT MOLECULES.
C
C     INPUTS
C
C     N1      NUMBER OF NODES IN THE FIRST COORDINATE DIRECTION.
C     N2      NUMBER OF NODES IN THE SECOND COORDINATE DIRECTION.
C     N1M     FIRST DIMENSION OF ALL THE TWO-DIMENSIONAL ARRAYS.
C     A       ARRAY OF DIMENSION (N1M,N2) STORING THE COEFFICIENT
C             OF THE ITERATIVE UPDATE EQUATIONS AS SHOWN BELOW -
C     B       ... DIMENSION (N1M,N2) ...
C     C       ... DIMENSION (N1M,N2) ...
C     D       ... DIMENSION (N1M,N2) ...
C     E       ... DIMENSION (N1M,N2) ...
C
C     A(I,J)*S(I,J-1)+B(I,J)*S(I-1,J)+C(I,J)*S(I,J)+D(I,J)*
C      S(I+1,J)+E(I,J)*S(I,J+1)=R(I,J)
C
C             WITH I=1,2,...,N1 AND J=1,2,...,N2 AND WHERE S(I,J)
C             IS THE ARRAY WHOSE APPROXIMATE VALUES ARE SOUGHT
C             (AND WHICH OVERWRITE THE RESIDUALS R(I,J) STORED ON
C             INPUT IN THE ARRAY R). ANY VALUES OF S OUTSIDE THE
C             (1-N1,1-N2) ARRAY WHICH DEFINES THE PROBLEM REGION
C             ARE TAKEN AS ZERO.
C     R       IS INPUT AS THE ARRAY OF RESIDUALS, CORRESPONDING
C             TO R ABOVE, CHANGED ON OUTPUT TO THE VALUES OF THE
C             UPDATE ARRAY CORRESPONDING TO S ABOVE, DIMENSIONED
C             (N1M,N2).
C     APARAM  IS AN ITERATION ACCELERATION PARAMETER FACTOR
C             TYPICALLY SET TO 1.0. IF CONVERGENCE IS SLOW, IT
C             CAN BE DECREASED. IF DIVERGENCE IS OBTAINED, IT
C             SHOULD BE INCREASED. IN EITHER CASE IT MUST NOT GO
C             OUTSIDE THE BOUNDS PRESCRIBED (SEE IFAIL PARAMETER
C             FOR D03UAF).
C     IT      IS THE ITERATION COUNTER SET AND INCREMENTED BY
C             THE CALLING ROUTINE, IT IS USED TO DETERMINE THE
C             APPROPRIATE ITERATION PARAMETERS.
C     WRKSP1  IS A WORKSPACE ARRAY OF DIMENSION (N1M,N2).
C     WRKSP2  ... DITTO ...
C     IFAIL   IS AN ERROR PARAMETER INDICATOR SET BY THE USER TO
C             INDICATE THE TYPE OF FAILURE IF AN ERROR IS
C             ENCOUNTERED.
C
C     PROCESS
C
C     SET ERROR PARAMETER
C     CHECK INPUT INTEGERS
C     SET FREQUENTLY REQUIRED INTEGER VARIABLES
C     COMMENCEMENT OF CALCULATIONAL PROCEDURE
C     SET ODD/EVEN COUNTERS  KS=1 FOR ODD ITERATIONS
C                            KS=2 FOR EVEN ITERATIONS
C     DETERMINE THE NUMBER OF THE ACCELERATION PARAMETER TO BE USED
C     (THE SAME PARAMETER IS USED FOR 2 ITERATIONS, THERE ARE 9
C     PARAMETERS IN ALL).
C     FIRST CALCULATE THE TERM, ALM, IN THE LARGEST PARAMETER
C     CHECK THE VALUES OF ALM
C     DETERMINE THE ITERATION PARAMETER
C     START THE APPROXIMATE LU FACTORIZATION DETERMINING THE VALUES
C     OF THE ELEMENTS SB,SC IN THE LOWER TRIANGULAR MATRIX L AND
C     WRKSP1 AND WRKSP2 IN THE UPPER TRIANGULAR MATRIX U. PROGRESS
C     FORWARDS FIRST AND INVERT THE LOWER TRIANGULAR MATRIX L AS
C     ONE PROCEEDS SO THAT THE ELEMENTS SB,SC DO NOT HAVE TO BE
C     STORED IN ARRAYS. THE ELEMENTS WRKSP1 AND WRKSP2 HAVE TO BE
C     STORED FOR THE SUBSEQUENT INVERSION OF THE MATRIX U BY BACK
C     SUBSTITUTION.
C     DO JL = 1,N2
C        (JL IS ONLY A COUNTER IN THE SECOND COORDINATE DIRECTION
C         J IS THE INDEX OF THE SECOND COORDINATE AND ON
C         ALTERNATE ITERATIONS SCANS FIRST INCREASING AND THEN
C         DECREASING).
C         DO I = 1,N1
C            (I IS THE INDEX IN THE FIRST COORDINATE DIRECTION)
C             STORE TH VALUE OF THE CENTRAL COEFFICIENT
C             DETERMINE THE TYPE OF THE DIFFERENCE EQUATION
C               FOR A FIVE POINT MOLECULE EQUATION -
C                  SET VALUES OF ADJACENT ARRAY ELEMENTS DEPENDENT
C                  ON NODAL POSITION
C                  CALCULATE THE OFF-DIAGONAL ELEMENTS OF L,
C                  NAMELY SB AND SC
C                  CALCULATE THE OFTEN USED EXPRESSIONS
C                  CALCULATE THE VALUE OF THE DIAGONAL ELEMENT OF L
C                  CALCULATE AND STORE THE ELEMENTS OF U, NAMELY
C                  WRKSP1 AND WRKSP2
C                  CALCULATE THE ELEMENTS OF L**(-1) * RESIDUAL
C                  ARRAY
C               FOR THE EXPLICIT EQUATION DO THE SAME
C     END OF SCAN FOR FORWARD ELIMINATION
C     BACK SUBSTITUTION TO MULTIPLY BY U**(-1)
C     DO JL = 1,N2
C        J RUNS BACKWARDS AND FORWARDS ALTERNATELY
C        DO I = N1,N1-1,....,2,1
C           R IS INITIALLY L**(-1) * RESIDUAL, IT BECOMES
C           (U**(-1) * (L**(-1) *RESIDUAL) = CHANGE
C     END OF SCAN FOR BACK SUBSTITUTION
C     RETURN
C
C     OUTPUTS
C
C     R       ARRAY STORING THE APPROXIMATE SOLUTION TO THE SYSTEM
C             OF EQUATIONS PROVIDED, AFTER ONE ITERATION. NOTE
C             THAT IT HAS OVERWRITTEN THE INPUT RESIDUAL.
C     IFAIL   ERROR INDICATOR
C             =0 CORRECT RETURN
C             =1 EITHER N1.LE.1 OR N2.LE.1
C             =2 N1M IS LESS THAN N1
C             =3 APARAM.LE.0.0
C             =4 APARAM.GT.((N1-1)**2+N2-1)**2)/2.
C
C     ROUTINES USED
C
C     P01AAF  ERROR HANDLING ROUTINE
C
C     **************************************************************
C
C     .. Parameters ..
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='D03UAF')
C     .. Scalar Arguments ..
      INTEGER           IFAIL, IT, N1, N1M, N2
C     .. Array Arguments ..
      dimension         A(N1M,N2), B(N1M,N2), C(N1M,N2), D(N1M,N2),
     *                  E(N1M,N2), R(N1M,N2), WRKSP1(N1M,N2),
     *                  WRKSP2(N1M,N2)
C     .. Local Scalars ..
      DOUBLE PRECISION  ALM, ALPHA, CS, RIBJL, RIJB, SB, SBSEIJ, SC,
     *                  SCSFIJ, SD, SEIBJL, SEIJB, SFIBJL, SFIJB, XKS1,
     *                  XKS2
      INTEGER           I, IB, IERROR, IL, IS, J, JB, JL, KS, KS1, KS2,
     *                  N1M1, N1P1, N2M1, N2P1
C     .. Local Arrays ..
      dimension         ALP(9)
cccc      CHARACTER*1       P01REC(1)                                   ! My own comment. To avoid compiler's warning about unused character.
C     .. External Functions ..
ccc      INTEGER           P01ABF
ccc      EXTERNAL          P01ABF
C     .. Intrinsic Functions ..
      INTRINSIC         MOD, DBLE
C     .. Data statements ..
      DATA              ALP(1), ALP(2), ALP(3), ALP(4), ALP(5), ALP(6),
     *                  ALP(7), ALP(8), ALP(9)/1.0D0, 0.625D0, 0.25D0,
     *                  0.875D0, 0.5D0, 0.125D0, 0.75D0, 0.375D0, 0.0D0/
C     .. Executable Statements ..
      IERROR = 0
C
C     CHECK INPUT INTEGERS
C
c       print *,'r(32,40)',R(32,40)
      IF (N1.LE.1) GO TO 220
      IF (N2.LE.1) GO TO 220
C
      IF (N1M.LT.N1) GO TO 240
C
C     SET FREQUENTLY REQUIRED INTEGER VARIABLES
C
      N1M1 = N1 - 1
      N2M1 = N2 - 1
      N1P1 = N1 + 1
      N2P1 = N2 + 1
C
C     COMMENCEMENT OF CALCULATIONAL PROCEDURE
C     ---------------------------------------
C
C
C     SET ODD/EVEN COUNTER    KS=1  FOR ODD ITERATIONS
C     KS=2  FOR EVEN ITERATIONS
      KS = MOD(IT-1,2) + 1
      IF (KS.EQ.0) KS = 2
C
C
C     SET FREQUENTLY USED ODD/EVEN PARAMETER DERIVATIVES
C
      KS1 = 2 - KS
      XKS1 = DBLE(KS1)
      KS2 = KS - 1
      XKS2 = DBLE(KS2)
C
C     DETERMINE THE NUMBER OF THE ACCELERATION PARAMETER TO BE USED,
C     THE SAME PARAMETER IS USED FOR 2 ITERATIONS, THERE ARE 9
C     PARAMETERS IN ALL
C
      IS = MOD(IT-1,18)
      IF (IS.LT.0) IS = IS + 18
      IS = IS/2 + 1
C
C     CALCULATION OF THE ITERATION ACCELERATION PARAMETER
C
C     (1) CALCULATE THE TERM IN THE LARGEST PARAMETER
C
      ALM = 2.0*APARAM/(DBLE(N1M1*N1M1+N2M1*N2M1))
C
C     (2) CHECK THE VALUES OF ALM
C
      IF (APARAM.LE.0.0) GO TO 260
C
      IF (ALM.GT.1.0) GO TO 280
C
C     (3) THEN DETERMINE THE ITERATION PARAMETER
C
      ALPHA = 1.0 - ALM**ALP(IS)
c       print *, 'alph',ALPHA
C
C     START OF APPROXIMATE LU FACTORIZATION DETERMINING THE VALUES
C     OF THE ELEMENTS SB,SC IN THE LOWER TRIANGULAR MATRIX L AND
C     WRKSP1 AND WRKSP2 IN THE UPPER TRIANGULAR MATRIX U. PROGRESS
C     FORWARDS FIRST AND INVERT THE LOWER TRIANGULAR MATRIX L AS ONE
C     PROCEEDS SO THAT THE ELEMENTS SB,SC DO NOT HAVE TO BE STORED
C     IN ARRAYS. THE ELEMENTS WRKSP1 AND WRKSP2 HAVE TO BE STORED
C     FOR THE SUBSEQUENT INVERSION OF THE MATRIX U BY BACK
C     SUBSTITUTION.
C
c       print *,'d030000b',R(32,40)
      DO 160 JL = 1, N2
C
C        JL IS ONLY A COUNTER IN THE SECOND COORDINATE DIRECTION
C        J IS THE INDEX OF THE SECOND COORDINATE AND ON ALTERNATE
C        ITERATIONS SCANS FIRST INCREASING AND THEN DECREASING
C
         J = KS1*JL + KS2*(N2P1-JL)
         JB = J - KS1 + KS2
C
         DO 140 I = 1, N1
C
C           I IS THE INDEX IN THE FIRST COORDINATE DIRECTION
C
            IB = I - 1
C
C           STORE THE VALUE OF THE CENTRAL COEFFICIENT
C
            CS = C(I,J)
C
C           DETERMINE THE TYPE OF THE DIFFERENCE EQUATION
C
            IF (CS.EQ.0.0) GO TO 100
C
C           FIVE POINT MOLECULE EQUATION
C
C           SET VALUES OF ADJACENT ARRAY ELEMENTS DEPENDENT ON NODAL
C           POSITION
C
            IF ((JB.EQ.0) .OR. (JB.EQ.N2P1)) GO TO 20
C
C           (1A) IF NOT ON  BOUNDARY  SO THAT JB.NE.0 OR N2P1
C
            SEIJB = WRKSP1(I,JB)
            SFIJB = WRKSP2(I,JB)
            RIJB = R(I,JB)
c       if(i.eq.32)print *,'d03s0b',jl,R(I,JB),JB
            GO TO 40
C
   20       CONTINUE
C
C           (1B) THE VALUES EXTERIOR TO THE ARRAYS ARE SET TO ZERO
C
            SEIJB = 0.0
            SFIJB = 0.0
            RIJB = 0.0
   40       CONTINUE
C
            IF (I.EQ.1) GO TO 60
C
C           (2A) IF NOT ON I=1  I-1.NE.0  SO THAT
C
            SEIBJL = WRKSP1(IB,J)
            SFIBJL = WRKSP2(IB,J)
            RIBJL = R(IB,J)
            GO TO 80
C
   60       CONTINUE
C
C           (2B) ON  I=1  THE VALUES EXTERIOR TO THE ARRAY ARE SET TO 0
C
            SEIBJL = 0.0
            SFIBJL = 0.0
            RIBJL = 0.0
   80       CONTINUE
C
C           CALCULATE THE ELEMENTS OF THE LOWER TRIANGULAR MATRIX
C
            SB = (XKS1*A(I,J)+XKS2*E(I,J))/(1.0+ALPHA*SEIJB)
            SC = B(I,J)/(1.0+ALPHA*SFIBJL)
C
C           CALCULATE OFTEN USED EXPRESIONS
C
            SBSEIJ = SB*SEIJB
            SCSFIJ = SC*SFIBJL
C
C           CALCULATE THE VALUE OF THE DIAGONAL ELEMENT OF L
C
            SD = 1.0/(-SB*SFIJB-SC*SEIBJL+CS+ALPHA*(SBSEIJ+SCSFIJ))
c       print *,i,jl,'d03sb',r(i,j),SB,RIJB,SC,RIBJL,SD,
c     & -SB*SFIJB-SC*SEIBJL+CS+ALPHA*(SBSEIJ+SCSFIJ)
C
C           CALCULATE AND STORE THE ELEMENTS OF THE UPPER TRIANGULAR
C           MATRIX
C
         WRKSP1(I,J) = real( (D(I,J)-ALPHA*SBSEIJ)*SD)
         WRKSP2(I,J) = real( (XKS1*E(I,J)+XKS2*A(I,J)-ALPHA*SCSFIJ)*SD )
C
C           CALCULATION OF THE ELEMENTS OF THE INVERSE OF L * RES VECTOR
C
            R(I,J) =   real( (R(I,J)-SB*RIJB-SC*RIBJL)*SD )
C
            GO TO 120
C
  100       CONTINUE
C
C           CALCULATION FOR THE EXPLICIT EQUATION

C
            WRKSP1(I,J) = 0.0
            WRKSP2(I,J) = 0.0
C
  120       CONTINUE
C
  140    CONTINUE
  160 CONTINUE
C
C     END OF SCAN IN THE TWO COORDINATE DIRECTIONS FOR THE FORWARD
C     ELIMINATION
C
C     PROGRESS BACKWARDS TO MULTIPLY BY THE INVERSE OF THE MATRIX U
C
      DO 200 JL = 1, N2
C
C        J RUNS BACKWARDS AND FORWARDS ALTERNATELY
C
         J = KS1*(N2P1-JL) + KS2*JL
         JB = J + KS1 - KS2
C
         DO 180 IL = 1, N1
C
C           THE INDEX FOR THE FIRST COORDINATE ALWAYS RUNS BACKWARDS
C           SINCE IN THE FIRST LOOP IT ALWAYS RAN FORWARDS
C
            I = N1P1 - IL
C
C           R IS INITIALLY THE INVERSE OF L * RESIDUAL
C           IT BECOMES THE INVERSE OF U * INVERSE OF L * RESIDUAL
C           NAMELY THE CHANGE VECTOR
C
            IF ((JB.NE.0) .AND. (JB.NE.N2P1)) R(I,J) = R(I,J) -
     *          WRKSP2(I,J)*R(I,JB)
C
            IF (I.NE.N1) R(I,J) = R(I,J) - WRKSP1(I,J)*R(I+1,J)
C
  180    CONTINUE
  200 CONTINUE
C
C     CORRECT RETURN
C
      IFAIL = 0
      RETURN
C
C     ERROR RETURNS
C
  220 CONTINUE
      IERROR = 1
      GO TO 300
C
  240 CONTINUE
      IERROR = 2
      GO TO 300
C
  260 CONTINUE
      IERROR = 3
      GO TO 300
C
  280 CONTINUE
      IERROR = 4
C
C     ERROR CONDITION
C
  300 CONTINUE
C
ccc      IFAIL = P01ABF(IFAIL,IERROR,SRNAME,0,P01REC)
C
      RETURN
C
      END

      SUBROUTINE DENSw(t,s,ro,im,jm,km,ns)
        include 'par.inc'
        include 'com.inc'
      dimension
     *          t(im,jm,km),s(im,jm,km),ro(im,jm,km) 
 
   
      DO 1 J=1,Js
      DO 1 I=1,Is
        ke=kp(i,j)
        do 11 k=1,ke
       ro(I,J,k)=1.+1.e-3*(
     #          800.97*1.e-4
     #        +588.19*1.e-4*t(i,j,k)
     #        +797.02*1.e-3*s(i,j,k)
     #        -811.46*1.e-5*t(i,j,k)**2
     #        -325.31*1.e-5*t(i,j,k)*s(i,j,k)
     #        +131.71*1.e-6*s(i,j,k)**2
     #        +476.60*1.e-7*t(i,j,k)**3
     #        +389.18*1.e-7*t(i,j,k)**2*s(i,j,k)
     #        +287.97*1.e-8*t(i,j,k)*s(i,j,k)**2
     #        -611.83*1.e-10*s(i,j,k)**3)
11     continue
        if(ke+1.le.ks)then
        s(i,j,ke+1)=s(i,j,ke)
        t(i,j,ke+1)=t(i,j,ke)
        ro(i,j,ke+1)=ro(i,j,ke)
        endif
1     continue

c      if(ns.gt.0)goto 16


****** excluding of unstable stratification ***********

      
      DO 15 J=1,Js
      DO 15 I=1,Is

        ke=kp(i,j)
      do 25 k=1,ke
      zh=zw(k+1)-zw(kp(i,j))  ! zh<0  - liquid point
      rh=ro(i,j,k+1)-ro(i,j,k) ! +.3e-5

      IF( (rh.gt.0.) ) goto  25     !!!!! (zh.gt.0.).OR. was deleted 6.12.

        rm=(ro(i,j,k)+ro(i,j,k+1))/2.
        tm=(t(i,j,k)+t(i,j,k+1))/2.
      sm=(s(i,j,k)+s(i,j,k+1))/2.
                                     
      ro(i,j,k+1)=rm
        ro(i,j,k)=rm                                         
      s(i,j,k+1)=sm
      s(i,j,k)=sm
      t(i,j,k+1)=tm
      t(i,j,k)=tm

 25   CONTINUE

 15   CONTINUE

      if(ns.gt.0)goto 16

      
      DO 35 J=2,Js-1
      DO 35 I=2,Is-1
      do 35 k=1,ks-1
c        if(it(i,j).ne.3)goto 25

      zh=zw(k+1)-zw(kp(i,j))  ! zh<0  - liquid point
      IF(zh.gt.0.) goto  35
      rh=ro(i,j,k+1)-ro(i,j,k)
      if(rh.lt.0)then
        print*,'i,j,k,it,tt,ss,rr',i,j,k,it(i,j),
     !t(i,j,k),t(i,j,k+1),s(i,j,k),s(i,j,k+1),ro(i,j,k),ro(i,j,k+1)
c        pause 222
        endif
35    continue



16    continue

      RETURN
      END
      subroutine energy(energ1,u,v,im,jm,km,l1)
        include 'par.inc'
        include 'com.inc'
      real u(im,jm,km),v(im,jm,km)
        l1=0
      energ1=0.
      do 2 i=2,im-1
      do 2 j=2,jm-1
      if(it(i,j).ne.0)goto 2
      do 1 k=1,kp(i,j)



      uu=(u(i,j,k)+u(i+1,j,k))*(u(i,j,k)+u(i+1,j,k))/4.
     # +(v(i,j,k)+v(i,j+1,k))*(v(i,j,k)+v(i,j+1,k))/4.

      energ1=energ1+sqrt(uu)
        l1=l1+1
  1   continue
  2   continue
        if(l1.ne.0)energ1=energ1/l1
      return
      end

        subroutine rppw(u,v,ut,vt,w,fu,fv,us,tax,tay,thx,thy,
     # ph,ro,ahz,im,jm,km,l,al1,cph,cu,cadv,cq,cstep)


        include 'par.inc'
        include 'com.inc'

      real
     *      fu(im,jm,km), fv(im,jm,km),us(im,jm,km)
     *     ,thx(im,jm),thy(im,jm)
     *     ,u(im,jm,km),v(im,jm,km),tax(im,jm),tay(im,jm)
     *     ,ut(im,jm,km),vt(im,jm,km),ahz(im,jm,km)
     *     ,w(im,jm,km),ro(im,jm,km),ph(im,jm)


      call rpin(u,v,ut,vt,w,fu,fv,us,tax,tay,thx,thy,
     # ph,ro,ahz,im,jm,km,al,al1,
     # cph,cu,cadv,cq,cstep)


       do 1 i=2,iss
       do 1 j=2,jss

        do k=1,km
        us(i,j,k)=0.
        enddo

      ip=(it(i,j)-4)*(it(i,j)-5)*(it(i,j)-6)*it(i,j)*(it(i,j)-2)

      if(ip.eq.0)then
      do  k=2,kp(i,j)

       

      us(i,j,k)=div(fu,fv,i,j,k,im,jm,km)



       if(l.eq.1)us(i,j,k)=us(i,j,k)+div(ut,vt,i,j,k,im,jm,km)/dt

      if(l.eq.0)us(i,j,k)=
     # -div(u,v,i,j,k,im,jm,km)/dt + div(ut,vt,i,j,k,im,jm,km)/dt
c     #        us(i,j,k)+(w(i,j,k)-w(i,j,k-1))/(zw(k)-zw(k-1))/dt   
      enddo
        else
         do k=1,ks
         us(i,j,k)=0.
         enddo
       endif
 1    continue



      do 2 i=1,is-1
      do 2 j=1,js-1
        
       zw(1)=-ph(i,j)/980.   !!!!!  Ó×ÅÒ ÊÎËÅÁÀÍÈÉ ÓÐÎÂÍß
       z(1)=zw(1)
       z(2)=(zw(1)+zw(2))/2.

        k1=1      
      if(it(i,j).ne.1.and.it(i,j).ne.3)
     #call intw(us,fu,i,j,k1,km,im,jm,km,ph)

       zw(1)=0.   !!!!!!!!    LEVEL   MOD.
       z(1)=zw(1)
       z(2)=(zw(1)+zw(2))/2.

  2   continue
 
      return
      end


             
      subroutine avts(t,im,jm,km)
        include 'par.inc'
        include 'com.inc'
      real t(im,jm,km)
      do 2 k=1,km
      tk=0.
      aa=0.
      do 1 i=1,im
      do 1 j=1,jm
      if(kp(i,j).gt.k)then
      tk=t(i,j,k)+tk
      aa=aa+1.
      endif
  1   continue
      if(aa.gt.0.5)then
      do 3 i=1,im
      do 3 j=1,jm
      t(i,j,k)=tk/aa
  3   continue
      endif

  2   continue
      return
      end

      subroutine uavtime(u,ut,umt,ummt,im,jm,km,kmm,kn1,k0)
      real u(im,jm,km),umt(im,jm,kmm),ummt(im,jm),ut(im,jm,km)
        include 'par.inc'
        include 'com.inc'
      do 1 i=1,im
      do 1 j=1,jm
      do 2 k=1,kmm
      k1=k*kn1+k0
      umt(i,j,k)=umt(i,j,k)+u(i,j,k1)
 2    continue
        ke=kp(i,j)
      if(ke.gt.1)then
      ummt(i,j)=ummt(i,j)+zintw(ut,i,j,1,km,im,jm,kp,zw)/zw(kp(i,j))

        else
        ummt(i,j)=0.
        endif


1     continue
      return
      end

      subroutine Ravtime(ro,fu,fv,umt,im,jm,km,kmm,kn1,k0,l)
      real ro(im,jm,km),umt(im,jm,kmm),fu(im,jm,km)
     #     ,fv(im,jm,km)

        include 'par.inc'
        include 'com.inc'

      do 11 k=1,ks
      call av(ro,fv,im,jm,km,k,k,ppp)
        
  11   continue
      do i=1,im
      do j=1,jm
        ke=kp(i,j)
      call intr1(fv,fu,i,j,ke,im,jm,km)

      enddo
      enddo
      do 12 i=1,im
      do 12 j=1,jm
      do 12 k=1,kp(i,j)

      fu(i,j,k)=(fu(i,j,kp(i,j))-fu(i,j,k))*10
  12   continue

        if(l.ne.0)then

      do 1 i=1,im
      do 1 j=1,jm
      do 2 k=1,kmm
      k1=k*kn1+k0
      umt(i,j,k)=umt(i,j,k)+fu(i,j,k1)
  2    continue
        
1     continue
      endif

        return
      end

      subroutine Ravt(fu,umt,im,jm,km,kmm,kn1,k0)
      real umt(im,jm,kmm),fu(im,jm,km)
 
        include 'par.inc'
        include 'com.inc'

      do 1 i=1,im
      do 1 j=1,jm
      do 2 k=1,kmm
      k1=k*kn1+k0
      umt(i,j,k)=umt(i,j,k)+fu(i,j,k1)
  2    continue
        
1     continue
 
        return
      end

      subroutine rr(rrt,t,im,jm,km,ns,kmm)
      real rrt(kmm,km,ns), t(im,jm,km)
        include 'par.inc'
        include 'com.inc'

      do k=1,ks
      rrt(1,k,ns)=t(13,13,k)
      rrt(2,k,ns)=t(30,19,k)
      rrt(3,k,ns)=t(22,18,k)
      rrt(4,k,ns)=t(15,22,k)
      rrt(5,k,ns)=t(18,18,k)
      enddo

      return
      end

        subroutine layer(tb,b,im,jm,km,be0)


        include 'par.inc'
        include 'com.inc'

        real tb(im,jm,km),b(im,jm)

        do 1 i=1,im
        do 1 j=1,jm
        b(i,j)=0.
        ke=kp(i,j)
        do 2 k=2,ke
        if(tb(i,j,k).lt.be0*3)goto 1
        b(i,j)=zw(k)/100.
2       continue
1     continue
        return
        end



      subroutine kk(hp,im,jm)                                        ! Proved: it is used in 'fps.for' only
        include 'par.inc'
        include 'com.inc'
        real hp(im,jm)
60    format(133i3)
        open(12,file='kp1')
        do j=1,jm 
        read(12,60)(kp(i,j),i=1,im)
        enddo
        close(12) 


        do 1 i=1,im
        do 1 j=1,jm

        if(it(i,j).eq.1)goto 1

         if(kp(i,j).le.3)kp(i,j)=3
         if(kp(i,j).ge.23)kp(i,j)=23


        ke=kp(i,j)

        hp(i,j)=zw(ke)
   1    continue


         do 2 j=1,jm
        ku(1,j)=kp(1,j)
        do 2 i=2,im
        ku(i,j)=min0(kp(i-1,j),kp(i,j))
   2    continue

        do 3 i=1,im
        kv(i,1)=kp(i,1)
        do 3 j=2,jm
        kv(i,j)=min0(kp(i,j-1),kp(i,j))
   3    continue



        continue

        return
        end

 
      subroutine savemonth(imonth,ns,s,t,u,v,w,ph                     ! Proved: it is called in fps.for only
     #                     ,pht,im,jm,km)

        include 'par.inc'
        include 'com.inc'

      real s(im,jm,km),t(im,jm,km)
     *     ,u(im,jm,km),v(im,jm,km)
     *     ,w(im,jm,km),ph(im,jm),pht(im,jm)


      Character*4   file1(10)
      data file1/'mon1','mon2','mon3','mon4','mon5','mon6','mon7','mon8'
     #,'mon9','mon0'/
      if(imonth.gt.9)imonth=0
      nm=12
      open(12,file=file1(imonth))


      write(nm,*)ns  !,aday


      write(nm,2012)u
      write(nm,2012)v
      write(nm,2012)w
      write(nm,2012)ph
      write(nm,2012)pht
      write(nm,2012)t
      write(nm,2012)s




2012  format(20e12.6)   
      close(12)

      return
        end


      subroutine DUMP(iD,ns,s,t,u,v,w,ph,pht,im,jm,km)                                    ! Proved: it is called in 'fps.for' only

        include 'par.inc'
        include 'com.inc'

      real s(im,jm,km),t(im,jm,km)
     *     ,u(im,jm,km),v(im,jm,km)
     *     ,w(im,jm,km),ph(im,jm),pht(im,jm)


      Character*8   file1(53)
      data file1/'D1','D2','D3','D4','D5','D6','D7','D8'
     #,'D9','D10','D11','D12','D13','D14','D15','D16','D17'
     #,'D18','D19','D20','D21','D22','D23','D24','D25','D26','D27','D28'
     #,'D29','D30','D31','D32','D33','D34','D35','D36','D37','D38','D39'
     #,'D40','D41','D42','D43','D44','D45'
     #,'D46','D47','D48','D49','D50','D51','D52','D53'/


      nm=22
      open(22,file=file1(iD))

      write(nm,*)ns   !,aday


      write(nm,2012)u
      write(nm,2012)v
      write(nm,2012)w
      write(nm,2012)ph
      write(nm,2012)pht
      write(nm,2012)t
      write(nm,2012)s


2012  format(20e12.6)   
      close(22)

       print*,' save dump => ',file1(iD)



      return
        end
