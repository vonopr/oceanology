        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:17:28 2015
        MODULE TAFLUX__genmod
          INTERFACE 
            SUBROUTINE TAFLUX(A,TA,QT,QWA,IM,JM,K,WU)
              INTEGER(KIND=4) :: K
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: A(IM,JM,K)
              REAL(KIND=4) :: TA(IM,JM)
              REAL(KIND=4) :: QT(IM,JM)
              REAL(KIND=4) :: QWA
              REAL(KIND=4) :: WU
            END SUBROUTINE TAFLUX
          END INTERFACE 
        END MODULE TAFLUX__genmod
