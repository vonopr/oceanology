        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 14:50:57 2015
        MODULE SAVEMONTH__genmod
          INTERFACE 
            SUBROUTINE SAVEMONTH(IMONTH,NS,S,T,U,V,W,PH,PHT,IM,JM,KM)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              INTEGER(KIND=4) :: IMONTH
              INTEGER(KIND=4) :: NS
              REAL(KIND=4) :: S(IM,JM,KM)
              REAL(KIND=4) :: T(IM,JM,KM)
              REAL(KIND=4) :: U(IM,JM,KM)
              REAL(KIND=4) :: V(IM,JM,KM)
              REAL(KIND=4) :: W(IM,JM,KM)
              REAL(KIND=4) :: PH(IM,JM)
              REAL(KIND=4) :: PHT(IM,JM)
            END SUBROUTINE SAVEMONTH
          END INTERFACE 
        END MODULE SAVEMONTH__genmod
