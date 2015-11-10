        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:03:36 2015
        MODULE PICUWM__genmod
          INTERFACE 
            SUBROUTINE PICUWM(U1,W1,U,W,XX,IM,JM,KM)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: U1(IM-1,KM-1)
              REAL(KIND=4) :: W1(IM-1,KM-1)
              REAL(KIND=4) :: U(IM,JM,KM)
              REAL(KIND=4) :: W(IM,JM,KM)
              REAL(KIND=4) :: XX(IM)
            END SUBROUTINE PICUWM
          END INTERFACE 
        END MODULE PICUWM__genmod
