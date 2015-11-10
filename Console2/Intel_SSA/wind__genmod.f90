        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:03:37 2015
        MODULE WIND__genmod
          INTERFACE 
            SUBROUTINE WIND(TAX,TAY,TX,TY,TIM,TIM2,IM,JM)
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: TAX(IM,JM)
              REAL(KIND=4) :: TAY(IM,JM)
              REAL(KIND=4) :: TX
              REAL(KIND=4) :: TY
              REAL(KIND=4) :: TIM
              REAL(KIND=4) :: TIM2
            END SUBROUTINE WIND
          END INTERFACE 
        END MODULE WIND__genmod
