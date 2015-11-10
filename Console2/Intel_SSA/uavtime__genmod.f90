        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:33:25 2015
        MODULE UAVTIME__genmod
          INTERFACE 
            SUBROUTINE UAVTIME(U,UT,UMT,UMMT,IM,JM,KM,KMM,KN1,K0)
              INTEGER(KIND=4) :: KMM
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: U(IM,JM,KM)
              REAL(KIND=4) :: UT(IM,JM,KM)
              REAL(KIND=4) :: UMT(IM,JM,KMM)
              REAL(KIND=4) :: UMMT(IM,JM)
              INTEGER(KIND=4) :: KN1
              INTEGER(KIND=4) :: K0
            END SUBROUTINE UAVTIME
          END INTERFACE 
        END MODULE UAVTIME__genmod
