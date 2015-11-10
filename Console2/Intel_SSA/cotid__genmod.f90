        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 13:02:04 2015
        MODULE COTID__genmod
          INTERFACE 
            SUBROUTINE COTID(A,AMAX,C,IM,JM,KM,K,K2,TIME,AM,A0)
              INTEGER(KIND=4) :: K2
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: A(IM,JM,KM)
              REAL(KIND=4) :: AMAX(IM,JM,KM)
              REAL(KIND=4) :: C(IM,JM,K2)
              INTEGER(KIND=4) :: K
              REAL(KIND=4) :: TIME
              REAL(KIND=4) :: AM
              REAL(KIND=4) :: A0
            END SUBROUTINE COTID
          END INTERFACE 
        END MODULE COTID__genmod
