        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 13:02:04 2015
        MODULE ZINTW__genmod
          INTERFACE 
            FUNCTION ZINTW(A,I,J,K1,K2,IM,JM,KT,ZA)
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              INTEGER(KIND=4) :: K2
              REAL(KIND=4) :: A(IM,JM,K2)
              INTEGER(KIND=4) :: I
              INTEGER(KIND=4) :: J
              INTEGER(KIND=4) :: K1
              INTEGER(KIND=4) :: KT(IM,JM)
              REAL(KIND=4) :: ZA(K2)
              REAL(KIND=4) :: ZINTW
            END FUNCTION ZINTW
          END INTERFACE 
        END MODULE ZINTW__genmod
