        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 13:09:57 2015
        MODULE COTIDV__genmod
          INTERFACE 
            SUBROUTINE COTIDV(U,V,AMAX,C,IM,JM,KM,TIME,AM,A0)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: U(IM,JM,KM)
              REAL(KIND=4) :: V(IM,JM,KM)
              REAL(KIND=4) :: AMAX(IM,JM)
              REAL(KIND=4) :: C(IM,JM)
              REAL(KIND=4) :: TIME
              REAL(KIND=4) :: AM
              REAL(KIND=4) :: A0
            END SUBROUTINE COTIDV
          END INTERFACE 
        END MODULE COTIDV__genmod
