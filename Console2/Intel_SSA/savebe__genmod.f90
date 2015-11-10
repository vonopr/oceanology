        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:33:25 2015
        MODULE SAVEBE__genmod
          INTERFACE 
            SUBROUTINE SAVEBE(TE,TB,IM,JM,KM,LN)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: TE(IM,JM,KM)
              REAL(KIND=4) :: TB(IM,JM,KM)
              INTEGER(KIND=4) :: LN
            END SUBROUTINE SAVEBE
          END INTERFACE 
        END MODULE SAVEBE__genmod
