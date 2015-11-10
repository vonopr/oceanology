        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 14:50:57 2015
        MODULE RAVTIME__genmod
          INTERFACE 
            SUBROUTINE RAVTIME(RO,FU,FV,UMT,IM,JM,KM,KMM,KN1,K0,L)
              INTEGER(KIND=4) :: KMM
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: RO(IM,JM,KM)
              REAL(KIND=4) :: FU(IM,JM,KM)
              REAL(KIND=4) :: FV(IM,JM,KM)
              REAL(KIND=4) :: UMT(IM,JM,KMM)
              INTEGER(KIND=4) :: KN1
              INTEGER(KIND=4) :: K0
              INTEGER(KIND=4) :: L
            END SUBROUTINE RAVTIME
          END INTERFACE 
        END MODULE RAVTIME__genmod
