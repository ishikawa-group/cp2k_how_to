# CP2Kとは
* 原子・分子レベルでの第一原理計算ソフトウェア
* 密度汎関数理論(density functional theory: DFT)、Hatree-Fock、Moller-Presset摂動法(MP2)などの計算ができる
* 周期境界条件(periodic boundary condition: PBC)のある場合とない場合の両方が計算可能
* 基底関数は局在基底(Gauss関数)と平面波規定の**両方**を使う(mixed basis set)
* 全てフリー・オープンソースで利用可能 -> メリット
* 更新が頻繁(年2,3回程度?)に行われ、新しい手法が順次追加される -> メリット
* マニュアル等が少なく、特に日本語のリソースが少ない -> デメリット

# コンパイル
* 公式サイトの指示通りにすればほぼ問題なくコンパイルできる
    * https://github.com/cp2k/cp2k/blob/master/INSTALL.md
* dockerを用いるのが最も簡単だが、ここではソースコードからコンパイルする方法を記述する

## 北大計算機センター(Grand Chariot)でのコンパイル
* PRIMERGY CX2550, Intel Xeon Gold
* cp2k-2023.1のコンパイル例
* 手順
    1. intelコンパイラをロード: `module load intel; module load impi`
    2. Githubからソースをダウンロード: `git clone -b support/v2023.1 --recursive https://github.com/cp2k/cp2k.git cp2k-2023.1`
    3. `cd cp2k-2023.1`
    4. `git submodule update --init --recursive`
    5. `cd tool/toolchain`
    6. 補助ライブラリ等をコンパイル: `./install_cp2k_toolchain.sh --mpi-mode=intelmpi --math-mode=mkl --with-elpa=install --with-sirius=no`
        * `--with-PKG=install`で各パッケージがインストールされる。入れない場合は`=no`。
        * elpaは入れたほうが計算が早いような印象
        * siriusはコンパイルできないことが多いのでパス
    7. `cd ../../`
    8. makefileを編集
        * `./arch`に対応するmakefileがあるので編集する
        * この場合は`Linux-intel-x86_64.psmp`
        * `CXX=icpc`を追加
        * SIRIUS, SCOTCHなど、使っていないライブラリの`USE...`ををコメントアウト
    9. `make ARCH=Linux-intel-x86_64 VERSION=psmp`

* cp2kのバージョンやコンパイル環境で補助ライブラリがコンパイルできる・できないが変わるので注意。基本的にはBLAS, LAPACK, XSMMがあればよい。LIBXC, LIBINTも可能であればコンパイルしたい。

## MacOSでのコンパイル
1. `brew uninstall cp2k`
2. `brew unlink open-mpi scalapack`
3. `ln -f -s /opt/homebrew/bin/gcc-13 /opt/homebrew/bin/gcc`
4. `ln -f -s /opt/homebrew/bin/g++-13 /opt/homebrew/bin/g++`
5. `ln -f -s /opt/homebrew/bin/gfortran-13 /opt/homebrew/bin/gfortran`
6. `git clone --recursive https://github.com/cp2k/cp2k.git cp2k`
7. `cd cp2k`
8. `source arch/Darwin-gnu-arm64.ssmp`
9. `make -j ARCH=Darwin-gnu-arm64 VERSION=ssmp`

## バイナリの種類
* 並列計算に用いるライブラリ(OpenMP, MPI)によってcp2kバイナリファイルの種類が異なる

|種類| 並列計算の設定 |
|:-----|:----------|
| sdbg | OpenMP + debug settings |
| sopt | OpenMP + OMP_NUM_THREADS=1 |
| ssmp | OpenMP |
| pdbg | MPI + OpenMP + debug settings |
| popt | MPI + OpenMP + OMP_NUM_THREADS=1 |
| psmp | MPI + OpenMP |

* 実際の計算にも通るのであればpoptかpsmpがよい。psmpのみでほぼ大丈夫。

# プログラムの実行
### serial計算
* コンパイルしたcp2kは`${HOME}/cp2k/cp2k-version/cp2k/exe/Linux-.../cp2k.sopt`とする
* 実行コマンド: `cp2k.sopt -i input.inp -o output.out`

### parallel計算
* プロセス数をNPROCSとする
* 実行コマンド: `mpiexec -n $NPROCS cp2k.popt -i input.inp -o output.out`
    * `mpiexec.hydra -n ${PJM_MPI_PROC}`を使う場合もある
