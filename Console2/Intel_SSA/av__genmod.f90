        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 14:50:20 2015
        MODULE AV__genmod
          INTERFACE 
            SUBROUTINE AV(P,PT,IM,JM,KM,K1,K2,AVP)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: P(133,110,24)
              REAL(KIND=4) :: PT(IM,JM,KM)
              INTEGER(KIND=4) :: K1
              INTEGER(KIND=4) :: K2
              REAL(KIND=4) :: AVP(KM)
            END SUBROUTINE AV
          END INTERFACE 
        END MODULE AV__genmod
