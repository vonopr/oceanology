        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:33:25 2015
        MODULE HRVL__genmod
          INTERFACE 
            SUBROUTINE HRVL(U,V,UT,VT,W,FU1,FV1,US,TAX,TAY,THX,THY,PH,RO&
     &,IM,JM,KM,CQ,AL1,CU,CADV,CPH,AHZ,FUU,FVV,AZZ,E2,D2,R2,G2,A3,B3,C3,&
     &UA,VA,Z0,CSTEP)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: U(133,110,24)
              REAL(KIND=4) :: V(133,110,24)
              REAL(KIND=4) :: UT(133,110,24)
              REAL(KIND=4) :: VT(133,110,24)
              REAL(KIND=4) :: W(IM,JM,KM)
              REAL(KIND=4) :: FU1(IM,JM,KM)
              REAL(KIND=4) :: FV1(IM,JM,KM)
              REAL(KIND=4) :: US(IM,JM,KM)
              REAL(KIND=4) :: TAX(IM,JM)
              REAL(KIND=4) :: TAY(IM,JM)
              REAL(KIND=4) :: THX(IM,JM)
              REAL(KIND=4) :: THY(IM,JM)
              REAL(KIND=4) :: PH(IM,JM)
              REAL(KIND=4) :: RO(IM,JM,KM)
              REAL(KIND=4) :: CQ
              REAL(KIND=4) :: AL1
              REAL(KIND=4) :: CU
              REAL(KIND=4) :: CADV
              REAL(KIND=4) :: CPH
              REAL(KIND=4) :: AHZ(IM,JM,KM)
              REAL(KIND=4) :: FUU(KM)
              REAL(KIND=4) :: FVV(KM)
              REAL(KIND=4) :: AZZ(KM)
              REAL(KIND=4) :: E2(KM)
              REAL(KIND=4) :: D2(KM)
              REAL(KIND=4) :: R2(KM)
              REAL(KIND=4) :: G2(KM)
              REAL(KIND=4) :: A3(KM)
              REAL(KIND=4) :: B3(KM)
              REAL(KIND=4) :: C3(KM)
              REAL(KIND=4) :: UA(KM)
              REAL(KIND=4) :: VA(KM)
              REAL(KIND=4) :: Z0
              REAL(KIND=4) :: CSTEP
            END SUBROUTINE HRVL
          END INTERFACE 
        END MODULE HRVL__genmod
