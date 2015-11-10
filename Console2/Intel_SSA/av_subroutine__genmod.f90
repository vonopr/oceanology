        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:17:27 2015
        MODULE AV_SUBROUTINE__genmod
          INTERFACE 
            SUBROUTINE AV_SUBROUTINE(P,PT,IM,JM,KM,K1,K2,AVP)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: P(133,110,24)
              REAL(KIND=4) :: PT(IM,JM,KM)
              INTEGER(KIND=4) :: K1
              INTEGER(KIND=4) :: K2
              REAL(KIND=4) :: AVP(KM)
            END SUBROUTINE AV_SUBROUTINE
          END INTERFACE 
        END MODULE AV_SUBROUTINE__genmod
