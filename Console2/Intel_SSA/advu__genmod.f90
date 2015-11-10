        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:17:27 2015
        MODULE ADVU__genmod
          INTERFACE 
            FUNCTION ADVU(U,V,W,I,J,K,IM,JM,KM,TAX,LL,IR)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: U(IM,JM,KM)
              REAL(KIND=4) :: V(IM,JM,KM)
              REAL(KIND=4) :: W(IM,JM,KM)
              INTEGER(KIND=4) :: I
              INTEGER(KIND=4) :: J
              INTEGER(KIND=4) :: K
              REAL(KIND=4) :: TAX(IM,JM)
              INTEGER(KIND=4) :: LL
              INTEGER(KIND=4) :: IR
              REAL(KIND=4) :: ADVU
            END FUNCTION ADVU
          END INTERFACE 
        END MODULE ADVU__genmod
