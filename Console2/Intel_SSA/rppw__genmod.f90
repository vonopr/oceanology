        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:17:28 2015
        MODULE RPPW__genmod
          INTERFACE 
            SUBROUTINE RPPW(U,V,UT,VT,W,FU,FV,US,TAX,TAY,THX,THY,PH,RO, &
     &AHZ,IM,JM,KM,L,AL1,CPH,CU,CADV,CQ,CSTEP)
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
              INTEGER(KIND=4) :: L
              REAL(KIND=4) :: AL1
              REAL(KIND=4) :: CPH
              REAL(KIND=4) :: CU
              REAL(KIND=4) :: CADV
              REAL(KIND=4) :: CQ
              REAL(KIND=4) :: CSTEP
            END SUBROUTINE RPPW
          END INTERFACE 
        END MODULE RPPW__genmod
