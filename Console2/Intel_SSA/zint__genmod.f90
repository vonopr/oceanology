        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:03:37 2015
        MODULE ZINT__genmod
          INTERFACE 
            FUNCTION ZINT(A,I,J,K1,K2,IM,JM,KA)
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              INTEGER(KIND=4) :: K2
              REAL(KIND=4) :: A(IM,JM,K2)
              INTEGER(KIND=4) :: I
              INTEGER(KIND=4) :: J
              INTEGER(KIND=4) :: K1
              INTEGER(KIND=4) :: KA(IM,JM)
              REAL(KIND=4) :: ZINT
            END FUNCTION ZINT
          END INTERFACE 
        END MODULE ZINT__genmod
