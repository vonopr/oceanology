        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:17:27 2015
        MODULE ADVV__genmod
          INTERFACE 
            FUNCTION ADVV(U,V,W,I,J,K,IM,JM,KM,TAY,LL,IR)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: U(IM,JM,KM)
              REAL(KIND=4) :: V(IM,JM,KM)
              REAL(KIND=4) :: W(IM,JM,KM)
              INTEGER(KIND=4) :: I
              INTEGER(KIND=4) :: J
              INTEGER(KIND=4) :: K
              REAL(KIND=4) :: TAY(IM,JM)
              INTEGER(KIND=4) :: LL
              INTEGER(KIND=4) :: IR
              REAL(KIND=4) :: ADVV
            END FUNCTION ADVV
          END INTERFACE 
        END MODULE ADVV__genmod
