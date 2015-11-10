        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:17:28 2015
        MODULE SIPP__genmod
          INTERFACE 
            SUBROUTINE SIPP(EPS,T,R,E,A,B,D,C,Q,WRKSP1,WRKSP2,IM,JM,    &
     &NITER)
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              REAL(KIND=4) :: EPS
              REAL(KIND=4) :: T(IM,JM)
              REAL(KIND=4) :: R(IM,JM)
              REAL(KIND=4) :: E(IM,JM)
              REAL(KIND=4) :: A(IM,JM)
              REAL(KIND=4) :: B(IM,JM)
              REAL(KIND=4) :: D(IM,JM)
              REAL(KIND=4) :: C(IM,JM)
              REAL(KIND=4) :: Q(IM,JM)
              REAL(KIND=4) :: WRKSP1(IM,JM)
              REAL(KIND=4) :: WRKSP2(IM,JM)
              INTEGER(KIND=4) :: NITER
            END SUBROUTINE SIPP
          END INTERFACE 
        END MODULE SIPP__genmod
