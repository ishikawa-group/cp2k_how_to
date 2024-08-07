# DFT法によりエネルギーを計算する
* ここでは、cp2kのコンパイルおよび実行の方法についてはできているものとする。これらは[過去の記事](./install.md)を参照のこと
* ここでは、DFT(密度汎関数理論)によるエネルギー計算(構造固定)の例を紹介する
* まず孤立分子の計算を解説し、その後に表面系の計算を解説する

## インプットの構成
```
! an example of CP2K section
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
* インデントは必須ではないが、見やすいから入れた方がよい
* コメントアウトは`!`で可能

### 主なセクションの例
* `GLOBAL`: 計算の種類など、全体の設定を行うため必須となる
* `FORCE_EVAL`: エネルギーやフォース(力)の計算方法を設定。ほぼ必須
* `MOTION`: 原子核の動きに対する設定。構造最適化やMDでは必須

### DFTエネルギー計算に必要な設定
* DFT計算においてはさらに
  + `FORCE_EVAL`中での`DFT`と`SUBSYS`(あるいはそのサブセクション)での設定が必要
* `DFT`: DFT計算条件の設定
  + `QS`: cp2kのDFT計算コード(Quickstep)の設定
  + `SCF`: SCF(自己無撞着場)の計算条件
  + `XC`: 交換-相関汎関数の設定
* `SUBSYS`: 分子構造やユニットセルの設定
  + `KIND`: 原子種(元素)やそれに対する基底関数の設定
  + `CELL`: ユニットセルと周期境界条件(PBC)の設定

## インプットファイルの例
* それでは以上の情報を踏まえて分子のエネルギー計算のインプット(`test_energy_mol.inp`)を作成してみる

```
&GLOBAL
  PROJECT test
  RUN_TYPE ENERGY
  PRINT_LEVEL MEDIUM
&END GLOBAL

&FORCE_EVAL
  METHOD Quickstep

  &DFT
    BASIS_SET_FILE_NAME BASIS_MOLOPT
    POTENTIAL_FILE_NAME POTENTIAL

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
      SCF_GUESS ATOMIC
      EPS_SCF 1.0E-6
      ADDED_MOS 50
      MAX_SCF 50

      &OUTER_SCF
        EPS_SCF 1.0E-6
        MAX_SCF 10
      &END OUTER_SCF

      &MIXING
        METHOD BROYDEN_MIXING
        ALPHA 0.3
        NBUFFER 15
      &END MIXING
      &SMEAR
        ELECTRONIC_TEMPERATURE 300.0
        METHOD FERMI_DIRAC
      &END SMEAR

    &END SCF

    &XC
      &XC_FUNCTIONAL PBE
      &END XC_FUNCTIONAL
    &END XC

  &END DFT

  &SUBSYS
    &KIND C
      ELEMENT C
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
      A 15.00000  0.00000  0.00000
      B  0.00000 15.00000  0.00000
      C  0.00000  0.00000 15.00000
      PERIODIC NONE
    &END CELL

    &TOPOLOGY
      COORD_FILE_NAME benzene.xyz
      COORDINATE XYZ
      CONNECTIVITY OFF
    &END TOPOLOGY
  &END SUBSYS

&END FORCE_EVAL
```
* cp2kの実行: `cp2k -i test.inp -o test.out`

### タグの説明
* 以下、上記`test.inp`についていくつか説明

#### 計算タイプ(&GLOBAL)
* `&GLOBAL`の`RUN_TYPE`で計算のタイプを設定する。構造固定でのエネルギー計算は`ENERGY`

#### 基底関数(&KIND etc.)
* 基底関数と擬ポテンシャルはそれぞれ`BASIS_SET_FILE`と`POTENTIAL_FILE`から読み込む。これらのファイルは`{cp2k_install_dir}/data`から読まれる
* 基底関数は`&KIND`タグで元素ごとに指定する。次の2種をよく使う
    + `SZV-MOLOPT-SR-GTH`: single-zeta, 軽い計算
    + `DZVP-MOLOPT-SR-GTH`: double-zeta plus polarization, 実際の計算

#### DFT計算条件(&DFT, &SCF)
* SCFのアルゴリズムはいろいろなチョイスがあるが、上記で多くの場合カバーできる
* SCFはinner SCFとouter SCFに分けて二重のループを用いることができ、outer SCFを設定したほうが安全。この場合`&OUTER_SCF`セクションを指定する
* SCFの収束条件は`EPS_SCF`で設定する。デフォルトは1.0E-5だが、1.0E-6のほうが良いように思う

#### 構造(&SUBSYS)
* 単位セルの構造と周期境界条件は`&CELL`で指定する
* 分子系の計算では`PERIODIC NONE`でよい
* 構造は外部ファイルから読み込むことも可能。その場合はxyz形式にし、`&TOPOLOGY`を以下のように変更する
```
&TOPOLOGY
    COORD_FILE_NAME opt.xyz
    COORDINATE XYZ
    CONNECTIVITY OFF
&END TOPOLOGY
```

## 計算の実行
`cp2k -i test_energy_mol.inp -o test_energy_mol.out`

## 結果の解析
* 非常に多くの情報が出力されるのでどこに着目するかは課題によるが、確認しなければならないのはSCFがちゃんと収束したかどうか
```
Step     Update method      Time    Convergence         Total energy    Change
------------------------------------------------------------------------------
    1 NoMix/Diag. 0.30E+00    1.2     0.31682869     -8669.6383507452 -8.67E+03
    2 Broy./Diag. 0.30E+00    1.2     0.27347233     -8727.4264675350 -5.78E+01
    3 Broy./Diag. 0.30E+00    1.2     0.26247831     -8671.5981195702  5.58E+01
   ...
   38 Broy./Diag. 0.30E+00    1.1     0.00000236     -8652.4938114086  1.60E-05
   39 Broy./Diag. 0.30E+00    1.1     0.00000121     -8652.4938008172  1.06E-05
   40 Broy./Diag. 0.30E+00    1.1     0.00000073     -8652.4938012689 -4.52E-07

  *** SCF run converged in    40 steps ***
```
* `SCF run converged`が出ていればOK
* Mulliken populatioin(原子の電荷に相当)なども出力されるので必要であれば使う
* 上記のエネルギーは原子単位(atomic unit, Hatree)なので必要に応じて変換する(eVにするには27.2114, kcal/molにするには627.51, kJ/molにするには2625.5をかける)

## 表面系の計算
* 次に、表面系の計算例を解説する
* 以下が表面系のインプットファイル(`test_energy_surf.inp`)

```
&GLOBAL
  PROJECT test
  RUN_TYPE ENERGY
  PRINT_LEVEL MEDIUM
&END GLOBAL

&FORCE_EVAL
  METHOD Quickstep
  &DFT
    BASIS_SET_FILE_NAME BASIS_MOLOPT
    POTENTIAL_FILE_NAME POTENTIAL

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
      SCF_GUESS ATOMIC
      EPS_SCF 1.0E-6
      ADDED_MOS 50
      MAX_SCF 50

      &OUTER_SCF
        EPS_SCF 1.0E-6
        MAX_SCF 10
      &END OUTER_SCF

      &MIXING
        METHOD BROYDEN_MIXING
        ALPHA 0.3
        NBUFFER 15
      &END MIXING

      &SMEAR
        ELECTRONIC_TEMPERATURE 300.0
        METHOD FERMI_DIRAC
      &END SMEAR

    &END SCF

    &XC
      &XC_FUNCTIONAL PBE
      &END XC_FUNCTIONAL
    &END XC

    &POISSON
      PERIODIC XY
      POISSON_SOLVER ANALYTIC
    &END POISSON

    &KPOINTS
      SCHEME MONKHORST-PACK 2 2 1
    &END KPOINTS

  &END DFT

  &SUBSYS
    &KIND Pt
      ELEMENT Pt
      BASIS_SET DZVP-MOLOPT-SR-GTH
      POTENTIAL GTH-PBE
    &END KIND

    &CELL
      SYMMETRY ORTHORHOMBIC
      A 11.76948  0.00000  0.00000
      B  0.00000 11.76948  0.00000
      C  0.00000  0.00000 27.84632
      PERIODIC XY
    &END CELL

    &TOPOLOGY
      COORD_FILE_NAME pt_surf.xyz
      COORDINATE XYZ
      CONNECTIVITY OFF
    &END TOPOLOGY

  &END SUBSYS

&END FORCE_EVAL
```
* 分子系との計算の違い
1. 周期境界条件の設定
    * `PERIODIC XY`とする(真空領域がz方向の場合)
    * 周期境界条件がXYの場合は`&DFT`セクションでPoisson solverの設定を以下のように記述する
    ```
    &POISSON
      PERIODIC XY
      POISSON_SOLVER ANALYTIC
    &END POISSON
    ```
2. k点の設定
    * 逆格子空間の数値積分条件を設定する。`&DFT`内で以下のセクションを与える
    ```
    &KPOINTS
      SCHEME MONKHORST-PACK 2 2 1
    &END KPOINTS
    ```
    * `2 2 1`としているが、数値が大きいほど精度は高い(ただし、周期性がない軸に対するk点の数は1でよい)

## まとめ
* 本記事ではcp2kの基本的な使い方としてエネルギーの計算方法を解説した
* 分子系と表面系の計算例を紹介した(バルク(結晶)系の計算は表面系の計算に近いので割愛)
* 他の計算(構造最適化、MD計算)はのちに触れるが、基本的には上記のインプットファイルにセクションを追加していく形なので、エネルギーの計算が理解できればそれほど困難ではない
* この後のステップとして、[構造最適化](./optimization.md)の計算について解説する
