        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 10 12:50:02 2015
        MODULE INTERPOL__genmod
          INTERFACE 
            SUBROUTINE INTERPOL(P,R,NPOINT,RLATT,RLONG,SLATT,SLONG,IP,JP&
     &,DELTAR,Q)
              REAL(KIND=4) :: P(133,110,24)
              REAL(KIND=4) :: R(301,24)
              INTEGER(KIND=4) :: NPOINT
              REAL(KIND=4) :: RLATT(133,110)
              REAL(KIND=4) :: RLONG(133,110)
              REAL(KIND=4) :: SLATT(301)
              REAL(KIND=4) :: SLONG(301)
              INTEGER(KIND=4) :: IP(301)
              INTEGER(KIND=4) :: JP(301)
              REAL(KIND=4) :: DELTAR
              REAL(KIND=4) :: Q(301,24)
            END SUBROUTINE INTERPOL
          END INTERFACE 
        END MODULE INTERPOL__genmod
