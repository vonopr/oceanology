        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 15:33:53 2015
        MODULE COND__genmod
          INTERFACE 
            SUBROUTINE COND(NSTEP,NPRIN,NK,ITER,AL,AL1,CTAH,CT,CADV,EPS,&
     &AAA,QWA,CU1,X1,NS,TAA,SAA,KH,S,T,U,V,W,TX,TY,PH,PHT,IM,JM,KM,HP,  &
     &P0D,P0MD,CSTEP)
              INTEGER(KIND=4) :: KM
              INTEGER(KIND=4) :: JM
              INTEGER(KIND=4) :: IM
              INTEGER(KIND=4) :: NSTEP
              INTEGER(KIND=4) :: NPRIN
              INTEGER(KIND=4) :: NK
              INTEGER(KIND=4) :: ITER
              REAL(KIND=4) :: AL
              REAL(KIND=4) :: AL1
              REAL(KIND=4) :: CTAH
              REAL(KIND=4) :: CT
              REAL(KIND=4) :: CADV
              REAL(KIND=4) :: EPS
              REAL(KIND=4) :: AAA
              REAL(KIND=4) :: QWA
              REAL(KIND=4) :: CU1
              REAL(KIND=4) :: X1(IM)
              INTEGER(KIND=4) :: NS
              REAL(KIND=4) :: TAA(24)
              REAL(KIND=4) :: SAA(24)
              INTEGER(KIND=4) :: KH(IM-1,JM-1)
              REAL(KIND=4) :: S(133,110,24)
              REAL(KIND=4) :: T(133,110,24)
              REAL(KIND=4) :: U(IM,JM,KM)
              REAL(KIND=4) :: V(IM,JM,KM)
              REAL(KIND=4) :: W(IM,JM,KM)
              REAL(KIND=4) :: TX
              REAL(KIND=4) :: TY
              REAL(KIND=4) :: PH(IM,JM)
              REAL(KIND=4) :: PHT(IM,JM)
              REAL(KIND=4) :: HP(133,110)
              REAL(KIND=4) :: P0D
              REAL(KIND=4) :: P0MD
              REAL(KIND=4) :: CSTEP
            END SUBROUTINE COND
          END INTERFACE 
        END MODULE COND__genmod
