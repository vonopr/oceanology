        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 13:25:04 2015
        MODULE INTW__genmod
          INTERFACE 
            SUBROUTINE INTW(A,B,I,J,K1,K2,IM,JM,KM,PH)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: A(IM,JM,KM)
              REAL(KIND=4) :: B(IM,JM,KM)
              INTEGER(KIND=4) :: I
              INTEGER(KIND=4) :: J
              INTEGER(KIND=4) :: K1
              INTEGER(KIND=4) :: K2
              REAL(KIND=4) :: PH(IM,JM)
            END SUBROUTINE INTW
          END INTERFACE 
        END MODULE INTW__genmod
