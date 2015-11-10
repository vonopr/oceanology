        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:33:25 2015
        MODULE DENSW__genmod
          INTERFACE 
            SUBROUTINE DENSW(T,S,RO,IM,JM,KM,NS)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: T(IM,JM,KM)
              REAL(KIND=4) :: S(IM,JM,KM)
              REAL(KIND=4) :: RO(IM,JM,KM)
              INTEGER(KIND=4) :: NS
            END SUBROUTINE DENSW
          END INTERFACE 
        END MODULE DENSW__genmod
