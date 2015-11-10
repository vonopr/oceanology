        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:33:25 2015
        MODULE LAYER__genmod
          INTERFACE 
            SUBROUTINE LAYER(TB,B,IM,JM,KM,BE0)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: TB(IM,JM,KM)
              REAL(KIND=4) :: B(IM,JM)
              REAL(KIND=4) :: BE0
            END SUBROUTINE LAYER
          END INTERFACE 
        END MODULE LAYER__genmod
