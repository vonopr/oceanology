        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:33:25 2015
        MODULE DIV__genmod
          INTERFACE 
            FUNCTION DIV(U,V,I,J,K,IM,JM,KM)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: U(IM,JM,KM)
              REAL(KIND=4) :: V(IM,JM,KM)
              INTEGER(KIND=4) :: I
              INTEGER(KIND=4) :: J
              INTEGER(KIND=4) :: K
              REAL(KIND=4) :: DIV
            END FUNCTION DIV
          END INTERFACE 
        END MODULE DIV__genmod
