        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:03:36 2015
        MODULE SAVEALOG__genmod
          INTERFACE 
            SUBROUTINE SAVEALOG(U,V,T,TB,X1,IM,JM,KM,Z0)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: U(IM,JM,KM)
              REAL(KIND=4) :: V(IM,JM,KM)
              REAL(KIND=4) :: T(IM,JM,KM)
              REAL(KIND=4) :: TB(IM,JM,KM)
              REAL(KIND=4) :: X1(IM)
              REAL(KIND=4) :: Z0
            END SUBROUTINE SAVEALOG
          END INTERFACE 
        END MODULE SAVEALOG__genmod
