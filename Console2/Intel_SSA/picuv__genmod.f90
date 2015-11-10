        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:33:25 2015
        MODULE PICUV__genmod
          INTERFACE 
            SUBROUTINE PICUV(U1,V1,U,V,XX,YY,IM,JM,KM,IT)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: U1(IM-1,JM-1)
              REAL(KIND=4) :: V1(IM-1,JM-1)
              REAL(KIND=4) :: U(IM,JM,KM)
              REAL(KIND=4) :: V(IM,JM,KM)
              REAL(KIND=4) :: XX(IM)
              REAL(KIND=4) :: YY(JM)
              INTEGER(KIND=4) :: IT(IM,JM)
            END SUBROUTINE PICUV
          END INTERFACE 
        END MODULE PICUV__genmod
