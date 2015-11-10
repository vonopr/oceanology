        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:33:24 2015
        MODULE HDRL__genmod
          INTERFACE 
            SUBROUTINE HDRL(A,AT,TA,QT,U,V,W,US,AHZ,AQ1,PR,WT,IM,JM,KM, &
     &A1,B1,C1,F1,A2,B2,CSTEP,AZZ,L0,PH,LP)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: A(IM,JM,KM)
              REAL(KIND=4) :: AT(IM,JM,KM)
              REAL(KIND=4) :: TA(IM,JM)
              REAL(KIND=4) :: QT(IM,JM)
              REAL(KIND=4) :: U(IM,JM,KM)
              REAL(KIND=4) :: V(IM,JM,KM)
              REAL(KIND=4) :: W(IM,JM,KM)
              REAL(KIND=4) :: US(IM,JM,KM)
              REAL(KIND=4) :: AHZ(IM,JM,KM)
              REAL(KIND=4) :: AQ1
              REAL(KIND=4) :: PR(IM,JM,KM)
              REAL(KIND=4) :: WT
              REAL(KIND=4) :: A1(KM)
              REAL(KIND=4) :: B1(KM)
              REAL(KIND=4) :: C1(KM)
              REAL(KIND=4) :: F1(KM)
              REAL(KIND=4) :: A2(KM)
              REAL(KIND=4) :: B2(KM)
              REAL(KIND=4) :: CSTEP
              REAL(KIND=4) :: AZZ(KM)
              INTEGER(KIND=4) :: L0
              REAL(KIND=4) :: PH(IM,JM)
              INTEGER(KIND=4) :: LP
            END SUBROUTINE HDRL
          END INTERFACE 
        END MODULE HDRL__genmod
