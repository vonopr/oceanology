        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:33:24 2015
        MODULE LVM__genmod
          INTERFACE 
            FUNCTION LVM(U,V,I,J,K,IM,JM,KM)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: U(IM,JM,KM)
              REAL(KIND=4) :: V(IM,JM,KM)
              INTEGER(KIND=4) :: I
              INTEGER(KIND=4) :: J
              INTEGER(KIND=4) :: K
              REAL(KIND=4) :: LVM
            END FUNCTION LVM
          END INTERFACE 
        END MODULE LVM__genmod
