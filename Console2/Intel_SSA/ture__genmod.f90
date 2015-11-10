        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 13:02:04 2015
        MODULE TURE__genmod
          INTERFACE 
            SUBROUTINE TURE(U,V,FU,FV,FRF,TAX,TAY,PR,EF,AHZ,AZZ,A3,B3,C3&
     &,FFU,D2,E2,TB,TE,RO,QT,QB,TBT,TET,IM,JM,KM,Z0,TB0,TE0,PARE,ALZ,PRB&
     &,RFCR,QBH,QEH,DRDT,L0B,LBB,L0E,LBE,ALB)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: U(IM,JM,KM)
              REAL(KIND=4) :: V(IM,JM,KM)
              REAL(KIND=4) :: FU(IM,JM,KM)
              REAL(KIND=4) :: FV(IM,JM,KM)
              REAL(KIND=4) :: FRF(IM,JM,KM)
              REAL(KIND=4) :: TAX(IM,JM)
              REAL(KIND=4) :: TAY(IM,JM)
              REAL(KIND=4) :: PR(IM,JM,KM)
              REAL(KIND=4) :: EF(KM)
              REAL(KIND=4) :: AHZ(IM,JM,KM)
              REAL(KIND=4) :: AZZ(KM)
              REAL(KIND=4) :: A3(KM)
              REAL(KIND=4) :: B3(KM)
              REAL(KIND=4) :: C3(KM)
              REAL(KIND=4) :: FFU(KM)
              REAL(KIND=4) :: D2(KM)
              REAL(KIND=4) :: E2(KM)
              REAL(KIND=4) :: TB(IM,JM,KM)
              REAL(KIND=4) :: TE(IM,JM,KM)
              REAL(KIND=4) :: RO(IM,JM,KM)
              REAL(KIND=4) :: QT(IM,JM)
              REAL(KIND=4) :: QB(IM,JM)
              REAL(KIND=4) :: TBT(IM,JM,KM)
              REAL(KIND=4) :: TET(IM,JM,KM)
              REAL(KIND=4) :: Z0
              REAL(KIND=4) :: TB0
              REAL(KIND=4) :: TE0
              REAL(KIND=4) :: PARE(5)
              REAL(KIND=4) :: ALZ
              REAL(KIND=4) :: PRB
              REAL(KIND=4) :: RFCR
              REAL(KIND=4) :: QBH
              REAL(KIND=4) :: QEH
              REAL(KIND=4) :: DRDT
              INTEGER(KIND=4) :: L0B
              INTEGER(KIND=4) :: LBB
              INTEGER(KIND=4) :: L0E
              INTEGER(KIND=4) :: LBE
              REAL(KIND=4) :: ALB
            END SUBROUTINE TURE
          END INTERFACE 
        END MODULE TURE__genmod
