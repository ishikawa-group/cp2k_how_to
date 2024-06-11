* `FILE_NAME`系はcp2k/data系から自動的に取得される
* いくつかの汎関数は&DFTセクションで`@XCTYPE vdW-DF1`のようにすることでインクルードできる。正確な定義は`cp2k/data/xc_section/`にある。ただし、パラメーターの調節などできないので、インプットに貼り付けたほうがいいかもしれない。

# DFT-D3
```
&XC
    &XC_FUNCTIONAL PBE
    &END XC_FUNCTIONAL
    &vdW_POTENTIAL
        DISPERSION_FUNCTIONAL PAIR_POTENTIAL
        &PAIR_POTENTIAL
            TYPE DFTD3 ### or DFTD3(BJ)
            CALCULATE_C9_TERM .TRUE.
            PARAMETER_FILE_NAME dftd3.dat
            REFERENCE_FUNCTIONAL PBE
            D3_SCALING XXX
            R_CUTOFF XXX
        &END PAIR_POTENTIAL
    &END vdW_POTENTIAL
&END XC
```

# van der Waals DFT系
* 基底関数は最低でもdouble zeta polarization (DZVP)にしたほうがよい
* CUTOFFを場合によっては変更する必要がある

## VDW-DF1
```
&XC
    &XC_FUNCTIONAL
        &PBE
            PARAMETRIZATION revPBE
            SCALE_C 0.0
        &END PBE
        &PW92
        &END PW92
    &END XC_FUNCTIONAL
    &vdW_POTENTIAL
        DISPERSION_FUNCTIONAL NON_LOCAL
        &NON_LOCAL
            TYPE DRSLL
            KERNEL_FILE_NAME vdW_kernel_table.dat
            CUTOFF 50
        &END NON_LOCAL
    &END vdW_POTENTIAL
&END XC
```
## VDW-DF2
```
&XC
    &XC_FUNCTIONAL
        &GGA_X_RPW86
        &END GGA_X_RPW86
        &PW92
        &END PW92
    &END XC_FUNCTIONAL
    &vdW_POTENTIAL
        DISPERSION_FUNCTIONAL NON_LOCAL
        &NON_LOCAL
            TYPE LMKLL
            KERNEL_FILE_NAME vdW_kernel_table.dat
            CUTOFF 50
        &END NON_LOCAL
    &END vdW_POTENTIAL
&END XC
```

# BEEF
```
&XC
    &XC_FUNCTIONAL BEEFVDW
    &END XC_FUNCTIONAL
    &VDW_POTENTIAL
        &NON_LOCAL
            TYPE LMKLL
            KERNEL_FILE_NAME vdW_kernel_table.dat
            CUTOFF 50
        &END NON_LOCAL
    &END VDW_POTENTIAL
&END XC
```

# rVV10
```
&XC
    &VDW_POTENTIAL
        DISPERSION_FUNCTIONAL NON_LOCAL
        &NON_LOCAL
            CUTOFF 40
            KERNEL_FILE_NAME rVV10_kernel_table.dat
            PARAMETERS 6.3 0.0093
            TYPE RVV10
        &END NON_LOCAL
    &END VDW_POTENTIAL
    &XC_FUNCTIONAL
        &GGA_C_PBE
        &END GGA_C_PBE
        &GGA_X_RPW86
        &END GGA_X_RPW86
    &END XC_FUNCTIONAL
    &XC_GRID
        USE_FINER_GRID
    &END XC_GRID
&END XC
```