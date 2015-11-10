        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 13:09:57 2015
        MODULE COEFNL__genmod
          INTERFACE 
            SUBROUTINE COEFNL(U,V,UT,VT,W,FU,FV,US,TAX,TAY,THX,THY,PH,RO&
     &,AHZ,AN,AS,AW,AE,AP,AF,IM,JM,KM,AL1,CPH,CU,CADV,CQ,CSTEP)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: U(IM,JM,KM)
              REAL(KIND=4) :: V(IM,JM,KM)
              REAL(KIND=4) :: UT(IM,JM,KM)
              REAL(KIND=4) :: VT(IM,JM,KM)
              REAL(KIND=4) :: W(IM,JM,KM)
              REAL(KIND=4) :: FU(IM,JM,KM)
              REAL(KIND=4) :: FV(IM,JM,KM)
              REAL(KIND=4) :: US(IM,JM,KM)
              REAL(KIND=4) :: TAX(IM,JM)
              REAL(KIND=4) :: TAY(IM,JM)
              REAL(KIND=4) :: THX(IM,JM)
              REAL(KIND=4) :: THY(IM,JM)
              REAL(KIND=4) :: PH(IM,JM)
              REAL(KIND=4) :: RO(IM,JM,KM)
              REAL(KIND=4) :: AHZ(IM,JM,KM)
              REAL(KIND=4) :: AN(IM,JM)
              REAL(KIND=4) :: AS(IM,JM)
              REAL(KIND=4) :: AW(IM,JM)
              REAL(KIND=4) :: AE(IM,JM)
              REAL(KIND=4) :: AP(IM,JM)
              REAL(KIND=4) :: AF(IM,JM)
              REAL(KIND=4) :: AL1
              REAL(KIND=4) :: CPH
              REAL(KIND=4) :: CU
              REAL(KIND=4) :: CADV
              REAL(KIND=4) :: CQ
              REAL(KIND=4) :: CSTEP
            END SUBROUTINE COEFNL
          END INTERFACE 
        END MODULE COEFNL__genmod
