        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 12:50:02 2015
        MODULE ADVZV__genmod
          INTERFACE 
            FUNCTION ADVZV(A,W,I,J,K,IM,JM,KM)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: A(IM,JM,KM)
              REAL(KIND=4) :: W(IM,JM,KM)
              INTEGER(KIND=4) :: I
              INTEGER(KIND=4) :: J
              INTEGER(KIND=4) :: K
              REAL(KIND=4) :: ADVZV
            END FUNCTION ADVZV
          END INTERFACE 
        END MODULE ADVZV__genmod
