## CP2KによるMD計算
* 分子動力学(molecular dynamics: MD)計算をCP2Kで行う
* MD計算にはNVEやNVTなど、計算条件により保存される物理量が異なる
    + NVE: 原子/分子数(N)、体積(V)、エネルギー(E)が保存される。micro canonical ensembleともいう
    + NVT: エネルギーではなく温度(T)が保存される。canonical ensembleという
    + NVmu: エネルギーではなく化学ポテンシャル(mu)が保存される。grand canonical ensembleという
* NVEとNVTがよく用いられるので、以下はその２つを解説する

### NVE
```
&GLOBAL
  PROJECT c8_001
  RUN_TYPE MD
  PRINT_LEVEL MEDIUM
&END GLOBAL

&FORCE_EVAL
  METHOD Quickstep
  &DFT
    BASIS_SET_FILE_NAME /lustre0/home/n22240/cp2k/cp2k-10/cp2k/data/BASIS_MOLOPT
    POTENTIAL_FILE_NAME /lustre0/home/n22240/cp2k/cp2k-10/cp2k/data/POTENTIAL

    ! UKS
    MULTIPLICITY 1

    &MGRID
      NGRIDS 4
      CUTOFF 280
      REL_CUTOFF 40
    &END MGRID
    &QS
      METHOD GPW
      EPS_DEFAULT 1.0E-10
    &END QS

    &SCF
      &OUTER_SCF
        EPS_SCF 1.0E-5
        MAX_SCF 6
      &END OUTER_SCF

      SCF_GUESS ATOMIC
      EPS_SCF 1.0E-5
      MAX_SCF 50

    # &MIXING
    #   METHOD BROYDEN_MIXING
    #   ALPHA 0.5
    # &END MIXING

      &OT
        MINIMIZER DIIS
        PRECONDITIONER FULL_KINETIC
        ENERGY_GAP 0.05
      &END OT

    &END SCF

    &XC
      &XC_FUNCTIONAL PBE
      &END XC_FUNCTIONAL
      &VDW_POTENTIAL
        POTENTIAL_TYPE PAIR_POTENTIAL
        &PAIR_POTENTIAL
          TYPE DFTD3 # DFTD3(BJ) for Becke-Jonshon dampling
          CALCULATE_C9_TERM .TRUE. # optional
          REFERENCE_FUNCTIONAL PBE
          PARAMETER_FILE_NAME /lustre0/home/n22240/cp2k/cp2k-10/cp2k/data/dftd3.dat
          R_CUTOFF 15
        &END PAIR_POTENTIAL
      &END VDW_POTENTIAL
    &END XC

  &END DFT

  &SUBSYS
    &KIND Cu
      ELEMENT Cu
      BASIS_SET DZVP-MOLOPT-SR-GTH
      POTENTIAL GTH-PBE
    &END KIND
    &KIND Zn
      ELEMENT Zn
      BASIS_SET DZVP-MOLOPT-SR-GTH
      POTENTIAL GTH-PBE
    &END KIND
    &KIND C
      ELEMENT C
      BASIS_SET DZVP-MOLOPT-SR-GTH
      POTENTIAL GTH-PBE
    &END KIND
    &KIND N
      ELEMENT N
      BASIS_SET DZVP-MOLOPT-SR-GTH
      POTENTIAL GTH-PBE
    &END KIND
    &KIND H
      ELEMENT H
      BASIS_SET DZVP-MOLOPT-SR-GTH
      POTENTIAL GTH-PBE
    &END KIND

    &CELL
      SYMMETRY ORTHORHOMBIC
      A 66.36904248216935  0.000000000000000  0.000000000000000
      B  0.00000000000000 13.263986957170907  0.000000000000000
      C  0.00000000000000  0.000000000000000 26.252703415323644
      PERIODIC XYZ
    &END CELL

    &TOPOLOGY
      COORD_FILE_NAME opt.xyz
      COORDINATE XYZ
      CONNECTIVITY OFF
    &END TOPOLOGY

    &COLVAR
      &DISTANCE
        ATOMS 391 395 ! C-F
      &END DISTANCE
    &END COLVAR

  &END SUBSYS

  &PRINT
    &TOTAL_NUMBERS ON
    &END TOTAL_NUMBERS
  &END PRINT

&END FORCE_EVAL

&MOTION
  &GEO_OPT
    MAX_DR    1.0E-04
    MAX_FORCE 1.0E-04
    RMS_DR    1.0E-04
    RMS_FORCE 1.0E-04
    MAX_ITER  500
    OPTIMIZER LBFGS
  &END GEO_OPT

  &MD
    ENSEMBLE NVT
    STEPS    1000
    TIMESTEP [fs] 1.0
    TEMPERATURE [K] 300.0
    TEMP_TOL [K] 30

    &THERMOSTAT
      &NOSE
        LENGTH 3
        YOSHIDA 3
        TIMECON 100.0
        MTS 2
      &END NOSE
    &END
  &END MD

  &CONSTRAINT
    &FIXED_ATOMS
      COMPONENTS_TO_FIX XYZ
        LIST 73..384
    &END FIXED_ATOMS
  &END CONSTRAINT

  &PRINT
    &TRAJECTORY
      LOG_PRINT_KEY .TRUE.
      FORMAT XYZ
      &EACH
        GEO_OPT 1
      &END EACH
      ADD_LAST NUMERIC
    &END TRAJECTORY
  &END PRINT

&END MOTION
```

### NVE -> NVT
* 変更点は以下
```
to be written
```
