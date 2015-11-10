        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:17:28 2015
        MODULE DUMP__genmod
          INTERFACE 
            SUBROUTINE DUMP(ID,NS,S,T,U,V,W,PH,PHT,IM,JM,KM)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              INTEGER(KIND=4) :: ID
              INTEGER(KIND=4) :: NS
              REAL(KIND=4) :: S(IM,JM,KM)
              REAL(KIND=4) :: T(IM,JM,KM)
              REAL(KIND=4) :: U(IM,JM,KM)
              REAL(KIND=4) :: V(IM,JM,KM)
              REAL(KIND=4) :: W(IM,JM,KM)
              REAL(KIND=4) :: PH(IM,JM)
              REAL(KIND=4) :: PHT(IM,JM)
            END SUBROUTINE DUMP
          END INTERFACE 
        END MODULE DUMP__genmod
