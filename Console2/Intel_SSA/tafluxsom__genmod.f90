        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 13:02:04 2015
        MODULE TAFLUXSOM__genmod
          INTERFACE 
            SUBROUTINE TAFLUXSOM(A,TA,QT,QWA,IM,JM,K,TAX,TAY)
              INTEGER(KIND=4) :: K
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: A(IM,JM,K)
              REAL(KIND=4) :: TA(IM,JM)
              REAL(KIND=4) :: QT(IM,JM)
              REAL(KIND=4) :: QWA
              REAL(KIND=4) :: TAX(IM,JM)
              REAL(KIND=4) :: TAY(IM,JM)
            END SUBROUTINE TAFLUXSOM
          END INTERFACE 
        END MODULE TAFLUXSOM__genmod
