## DFT法によりエネルギーを計算する
* cp2kのプログラムの実行は過去の記事を参考
* 以下はインプットファイル(`test.inp`)の例
* ここでは、DFT(密度汎関数理論)によるエネルギー計算(構造固定)と構造最適化の例を順に紹介する

### インプットの構成
```
! structure of cp2k input file
&SECTION
  KEYWORD VALUE
  &SUBSECTION
    KEYWORD VALUE
  &END SUBSECTION
&END SECTION
```
* cp2kのインプットファイルは、様々なセクションから構成されている
* 各セクションは`&XXX`,`&END XXX`(もしくは単に`&END`)で囲まれる
* セクションは階層的になっており、サブセクションがあるものも多い
* セクション内ではキーワードとパラメーターをペアで指定する。パラメーターは数値、文字列、論理値(`.TRUE.`, `.FALSE.`)などがある
* インデントは必須ではない(見やすいから入れた方がよい)
* コメントアウトは`!`で可能

### 主なセクション
* `GLOBAL`: 計算の種類など、全体の設定を行うため必須となる
* `FORCE_EVAL`: エネルギーやフォース(力)の計算方法を設定。ほぼ必須
* `MOTION`: 原子核の動きに対する設定。構造最適化やMDでは必須

### DFTエネルギー計算に必要な設定
* DFT計算においてはさらに
    * `FORCE_EVAL`中での`DFT`と`SUBSYS`(あるいはそのサブセクション)での設定が必要
* `DFT`: DFT計算条件の設定
    * `QS`: cp2kのDFT計算コード(Quickstep)の設定
    * `SCF`: SCF(自己無撞着場)の計算条件
    * `XC`: 交換-相関汎関数の設定
* `SUBSYS`: 分子構造やユニットセルの設定
    * `KIND`: 原子種(元素)やそれに対する基底関数の設定

### インプットファイルの例
* それでは以上の情報を踏まえてインプット(`test.inp`)を作成してみる

```
&GLOBAL
  PROJECT c8_001
  RUN_TYPE MD
  PRINT_LEVEL MEDIUM
&END GLOBAL

&FORCE_EVAL
  METHOD Quickstep
  &DFT
    BASIS_SET_FILE_NAME BASIS_MOLOPT
    POTENTIAL_FILE_NAME POTENTIAL

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
          PARAMETER_FILE_NAME dftd3.dat
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
```
* 実行: `cp2k -i test.inp -o test.out`

#### その他
* 構造は外部ファイルから読み込むことも可能。その場合はxyz形式にし、`&TOPOLOGY`を以下のように変更する
```
&TOPOLOGY
    COORD_FILE_NAME opt.xyz
    COORDINATE XYZ
    CONNECTIVITY OFF
&END TOPOLOGY
```


## 構造最適化
* `&MOTION`セクションを追加する
```
&MOTION
  &GEO_OPT
    MAX_DR    1.0E-04
    MAX_FORCE 1.0E-04
    RMS_DR    1.0E-04
    RMS_FORCE 1.0E-04
    MAX_ITER  500
    OPTIMIZER LBFGS
  &END GEO_OPT

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
* 原子位置を固定する場合には`&MOTION`セクション内に`&CONSTRAINT`を以下のように追加する
```
&CONSTRAINT
    &FIXED_ATOMS
        COMPONENTS_TO_FIX XYZ
        LIST 73..384
    &END FIXED_ATOMS
&END CONSTRAINT
```

