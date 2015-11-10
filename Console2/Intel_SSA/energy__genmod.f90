        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 12:50:01 2015
        MODULE ENERGY__genmod
          INTERFACE 
            SUBROUTINE ENERGY(ENERG1,U,V,IM,JM,KM,L1)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: ENERG1
              REAL(KIND=4) :: U(IM,JM,KM)
              REAL(KIND=4) :: V(IM,JM,KM)
              INTEGER(KIND=4) :: L1
            END SUBROUTINE ENERGY
          END INTERFACE 
        END MODULE ENERGY__genmod
