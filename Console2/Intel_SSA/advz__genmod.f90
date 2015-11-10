        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:33:25 2015
        MODULE ADVZ__genmod
          INTERFACE 
            FUNCTION ADVZ(A,WU,WM,I,J,K,IM,JM,KM,PH)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: A(IM,JM,KM)
              REAL(KIND=4) :: WU
              REAL(KIND=4) :: WM
              INTEGER(KIND=4) :: I
              INTEGER(KIND=4) :: J
              INTEGER(KIND=4) :: K
              REAL(KIND=4) :: PH(IM,JM)
              REAL(KIND=4) :: ADVZ
            END FUNCTION ADVZ
          END INTERFACE 
        END MODULE ADVZ__genmod
