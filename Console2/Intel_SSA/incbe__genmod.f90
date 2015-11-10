        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:33:25 2015
        MODULE INCBE__genmod
          INTERFACE 
            SUBROUTINE INCBE(TB,TE,FRF,PR,PRT,AHZ,EF,TB0,TE0,IM,JM,KM,WT&
     &,DRDT,PARE,ALZ,Z0,RFCR,L0T,LBT,L0B,LBB,L0E,LBE,QA,PRB,ALB)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: TB(IM,JM,KM)
              REAL(KIND=4) :: TE(IM,JM,KM)
              REAL(KIND=4) :: FRF(IM,JM,KM)
              REAL(KIND=4) :: PR(IM,JM,KM)
              REAL(KIND=4) :: PRT
              REAL(KIND=4) :: AHZ(IM,JM,KM)
              REAL(KIND=4) :: EF(KM)
              REAL(KIND=4) :: TB0
              REAL(KIND=4) :: TE0
              REAL(KIND=4) :: WT
              REAL(KIND=4) :: DRDT
              REAL(KIND=4) :: PARE(5)
              REAL(KIND=4) :: ALZ
              REAL(KIND=4) :: Z0
              REAL(KIND=4) :: RFCR
              INTEGER(KIND=4) :: L0T
              INTEGER(KIND=4) :: LBT
              INTEGER(KIND=4) :: L0B
              INTEGER(KIND=4) :: LBB
              INTEGER(KIND=4) :: L0E
              INTEGER(KIND=4) :: LBE
              REAL(KIND=4) :: QA
              REAL(KIND=4) :: PRB
              REAL(KIND=4) :: ALB
            END SUBROUTINE INCBE
          END INTERFACE 
        END MODULE INCBE__genmod
