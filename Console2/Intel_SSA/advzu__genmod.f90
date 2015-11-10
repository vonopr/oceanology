        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:03:35 2015
        MODULE ADVZU__genmod
          INTERFACE 
            FUNCTION ADVZU(A,W,I,J,K,IM,JM,KM)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: A(IM,JM,KM)
              REAL(KIND=4) :: W(IM,JM,KM)
              INTEGER(KIND=4) :: I
              INTEGER(KIND=4) :: J
              INTEGER(KIND=4) :: K
              REAL(KIND=4) :: ADVZU
            END FUNCTION ADVZU
          END INTERFACE 
        END MODULE ADVZU__genmod
