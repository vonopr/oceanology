        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:17:27 2015
        MODULE ARES__genmod
          INTERFACE 
            FUNCTION ARES(A,AT,IT,I,J,K,IM,JM,KM,K1M,K1,A0,A1,HP,U,V)
              INTEGER(KIND=4) :: K1M
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: A(IM,JM,KM)
              REAL(KIND=4) :: AT(IM,JM,KM)
              INTEGER(KIND=4) :: IT(IM,JM)
              INTEGER(KIND=4) :: I
              INTEGER(KIND=4) :: J
              INTEGER(KIND=4) :: K
              INTEGER(KIND=4) :: K1
              REAL(KIND=4) :: A0
              REAL(KIND=4) :: A1
              REAL(KIND=4) :: HP(IM,JM)
              REAL(KIND=4) :: U(IM,JM,K1M)
              REAL(KIND=4) :: V(IM,JM,K1M)
              REAL(KIND=4) :: ARES
            END FUNCTION ARES
          END INTERFACE 
        END MODULE ARES__genmod
