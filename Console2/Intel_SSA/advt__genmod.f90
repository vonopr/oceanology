        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 12:50:02 2015
        MODULE ADVT__genmod
          INTERFACE 
            FUNCTION ADVT(U,V,I,J,K,IM,JM,KM)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: U(IM,JM,KM)
              REAL(KIND=4) :: V(IM,JM,KM)
              INTEGER(KIND=4) :: I
              INTEGER(KIND=4) :: J
              INTEGER(KIND=4) :: K
              REAL(KIND=4) :: ADVT
            END FUNCTION ADVT
          END INTERFACE 
        END MODULE ADVT__genmod
