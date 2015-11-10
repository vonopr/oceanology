        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:33:25 2015
        MODULE FRFO__genmod
          INTERFACE 
            SUBROUTINE FRFO(FRF,PR,FU,FV,ALZ,RFCR,I,J,KE,IM,JM,KM)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: FRF(IM,JM,KM)
              REAL(KIND=4) :: PR(IM,JM,KM)
              REAL(KIND=4) :: FU(IM,JM,KM)
              REAL(KIND=4) :: FV(IM,JM,KM)
              REAL(KIND=4) :: ALZ
              REAL(KIND=4) :: RFCR
              INTEGER(KIND=4) :: I
              INTEGER(KIND=4) :: J
              INTEGER(KIND=4) :: KE
            END SUBROUTINE FRFO
          END INTERFACE 
        END MODULE FRFO__genmod
