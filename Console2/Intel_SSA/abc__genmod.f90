        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:17:27 2015
        MODULE ABC__genmod
          INTERFACE 
            SUBROUTINE ABC(A,A1,B1,C1,F1,A2,B2,L0,KE,I,J,IM,JM,KM)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: A(IM,JM,KM)
              REAL(KIND=4) :: A1(1:KM)
              REAL(KIND=4) :: B1(1:KM)
              REAL(KIND=4) :: C1(1:KM)
              REAL(KIND=4) :: F1(1:KM)
              REAL(KIND=4) :: A2(1:KM)
              REAL(KIND=4) :: B2(1:KM)
              INTEGER(KIND=4) :: L0
              INTEGER(KIND=4) :: KE
              INTEGER(KIND=4) :: I
              INTEGER(KIND=4) :: J
            END SUBROUTINE ABC
          END INTERFACE 
        END MODULE ABC__genmod
