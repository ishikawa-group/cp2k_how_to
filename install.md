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

### バイナリの種類
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

### Linux
1. `https://github.com/cp2k/cp2k/releases/`から.tar.bz2ファイルのリンクを取得
2. ソースをダウンロード: `wget https://github.com/cp2k/cp2k/releases/download/v2024.1/cp2k-2024.1-Linux-gnu-x86_64.tar.bz2` (for example)
3. 解凍: `tar jxvf cp2k-2024.1.tar.bz2`
4. `cd cp2k-2024.1; cd tools/toolchain`
5. ライブラリー等の依存パッケージをインストール: `./install_cp2k_toolchain.sh`
6. 環境変数等の設定: `source ./arch/Linux-intel-x86_64.psmp` (in case of intel compiler)
7. Make: `make -j ARCH=Linux-intel-x86_64 VERSION=psmp`

### MacOS
1. `brew uninstall cp2k`
2. `brew unlink open-mpi scalapack`
3. `ln -f -s /opt/homebrew/bin/gcc-13 /opt/homebrew/bin/gcc`
4. `ln -f -s /opt/homebrew/bin/g++-13 /opt/homebrew/bin/g++`
5. `ln -f -s /opt/homebrew/bin/gfortran-13 /opt/homebrew/bin/gfortran`
6. `git clone --recursive https://github.com/cp2k/cp2k.git cp2k`
7. `cd cp2k`
8. `source arch/Darwin-gnu-arm64.ssmp`
9. `make -j ARCH=Darwin-gnu-arm64 VERSION=ssmp`

# プログラムの実行
### serial
* コンパイルしたcp2kは`${HOME}/cp2k/cp2k-version/cp2k/exe/Linux-.../cp2k.sopt`とする
* 実行コマンド: `cp2k.sopt -i input.inp -o output.out`

### parallel
* プロセス数をNPROCSとする
* 実行コマンド: `mpiexec -n $NPROCS cp2k.popt -i input.inp -o output.out`

