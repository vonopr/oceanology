        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:33:25 2015
        MODULE LEITH__genmod
          INTERFACE 
            SUBROUTINE LEITH(XX,AT,U,V,W,I,J,K,IM,JM,KM,QT)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: XX(IM,JM,KM)
              REAL(KIND=4) :: AT(IM,JM,KM)
              REAL(KIND=4) :: U(IM,JM,KM)
              REAL(KIND=4) :: V(IM,JM,KM)
              REAL(KIND=4) :: W(IM,JM,KM)
              INTEGER(KIND=4) :: I
              INTEGER(KIND=4) :: J
              INTEGER(KIND=4) :: K
              REAL(KIND=4) :: QT(IM,JM)
            END SUBROUTINE LEITH
          END INTERFACE 
        END MODULE LEITH__genmod
