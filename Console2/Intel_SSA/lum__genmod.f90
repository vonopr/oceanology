        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 12:50:02 2015
        MODULE LUM__genmod
          INTERFACE 
            FUNCTION LUM(U,V,I,J,K,IM,JM,KM)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: U(IM,JM,KM)
              REAL(KIND=4) :: V(IM,JM,KM)
              INTEGER(KIND=4) :: I
              INTEGER(KIND=4) :: J
              INTEGER(KIND=4) :: K
              REAL(KIND=4) :: LUM
            END FUNCTION LUM
          END INTERFACE 
        END MODULE LUM__genmod
