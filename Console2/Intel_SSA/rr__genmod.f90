        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 14:50:57 2015
        MODULE RR__genmod
          INTERFACE 
            SUBROUTINE RR(RRT,T,IM,JM,KM,NS,KMM)
              INTEGER(KIND=4) :: KMM
              INTEGER(KIND=4) :: NS
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: RRT(KMM,KM,NS)
              REAL(KIND=4) :: T(IM,JM,KM)
            END SUBROUTINE RR
          END INTERFACE 
        END MODULE RR__genmod
