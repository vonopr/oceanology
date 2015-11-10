        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 14:50:19 2015
        MODULE UMAX1__genmod
          INTERFACE 
            FUNCTION UMAX1(U,V,IM,JM,KM,K)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: U(IM,JM,KM)
              REAL(KIND=4) :: V(IM,JM,KM)
              INTEGER(KIND=4) :: K
              REAL(KIND=4) :: UMAX1
            END FUNCTION UMAX1
          END INTERFACE 
        END MODULE UMAX1__genmod
