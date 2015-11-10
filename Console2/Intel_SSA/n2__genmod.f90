        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 14:24:34 2015
        MODULE N2__genmod
          INTERFACE 
            SUBROUTINE N2(TT,QT,PR,AFU,FV,I,J,KE,IM,JM,KM,DRDT)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: TT(IM,JM,KM)
              REAL(KIND=4) :: QT
              REAL(KIND=4) :: PR(IM,JM,KM)
              REAL(KIND=4) :: AFU
              REAL(KIND=4) :: FV(IM,JM,KM)
              INTEGER(KIND=4) :: I
              INTEGER(KIND=4) :: J
              INTEGER(KIND=4) :: KE
              REAL(KIND=4) :: DRDT
            END SUBROUTINE N2
          END INTERFACE 
        END MODULE N2__genmod
