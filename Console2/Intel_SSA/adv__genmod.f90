        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 13:02:04 2015
        MODULE ADV__genmod
          INTERFACE 
            FUNCTION ADV(XX,U,V,W,I,J,K,IM,JM,KM,QT)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: XX(IM,JM,KM)
              REAL(KIND=4) :: U(IM,JM,KM)
              REAL(KIND=4) :: V(IM,JM,KM)
              REAL(KIND=4) :: W(IM,JM,KM)
              INTEGER(KIND=4) :: I
              INTEGER(KIND=4) :: J
              INTEGER(KIND=4) :: K
              REAL(KIND=4) :: QT(IM,JM)
              REAL(KIND=4) :: ADV
            END FUNCTION ADV
          END INTERFACE 
        END MODULE ADV__genmod
