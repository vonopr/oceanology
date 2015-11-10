        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:17:28 2015
        MODULE TES__genmod
          INTERFACE 
            SUBROUTINE TES(TE,TET,TB,AZZ,FU,FV,FRF,EF,A1,B1,C1,F1,A2,B2,&
     &TE0,QE,QEH,I,J,IM,JM,KM,L0,LB,KE,PARE,ALB)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: TE(IM,JM,KM)
              REAL(KIND=4) :: TET(IM,JM,KM)
              REAL(KIND=4) :: TB(IM,JM,KM)
              REAL(KIND=4) :: AZZ(1:KM)
              REAL(KIND=4) :: FU(IM,JM,KM)
              REAL(KIND=4) :: FV(IM,JM,KM)
              REAL(KIND=4) :: FRF(IM,JM,KM)
              REAL(KIND=4) :: EF(KM)
              REAL(KIND=4) :: A1(1:KM)
              REAL(KIND=4) :: B1(1:KM)
              REAL(KIND=4) :: C1(1:KM)
              REAL(KIND=4) :: F1(1:KM)
              REAL(KIND=4) :: A2(1:KM)
              REAL(KIND=4) :: B2(1:KM)
              REAL(KIND=4) :: TE0
              REAL(KIND=4) :: QE
              REAL(KIND=4) :: QEH
              INTEGER(KIND=4) :: I
              INTEGER(KIND=4) :: J
              INTEGER(KIND=4) :: L0
              INTEGER(KIND=4) :: LB
              INTEGER(KIND=4) :: KE
              REAL(KIND=4) :: PARE(5)
              REAL(KIND=4) :: ALB
            END SUBROUTINE TES
          END INTERFACE 
        END MODULE TES__genmod
