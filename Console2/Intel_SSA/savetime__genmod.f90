        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:03:36 2015
        MODULE SAVETIME__genmod
          INTERFACE 
            SUBROUTINE SAVETIME(IIV,IM,JM,KM,V,NN,N3)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              INTEGER(KIND=4) :: IIV(IM,JM,KM,*)
              REAL(KIND=4) :: V(IM,JM,KM)
              INTEGER(KIND=4) :: NN
              INTEGER(KIND=4) :: N3
            END SUBROUTINE SAVETIME
          END INTERFACE 
        END MODULE SAVETIME__genmod
