        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 13:25:05 2015
        MODULE TURB__genmod
          INTERFACE 
            SUBROUTINE TURB(TB,TE,EF,FRF,AHZ,I,J,IM,JM,KM,KE)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: TB(IM,JM,1:KM)
              REAL(KIND=4) :: TE(IM,JM,KM)
              REAL(KIND=4) :: EF(KM)
              REAL(KIND=4) :: FRF(IM,JM,KM)
              REAL(KIND=4) :: AHZ(IM,JM,KM)
              INTEGER(KIND=4) :: I
              INTEGER(KIND=4) :: J
              INTEGER(KIND=4) :: KE
            END SUBROUTINE TURB
          END INTERFACE 
        END MODULE TURB__genmod
