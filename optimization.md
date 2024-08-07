# 構造最適化
* 原子核の位置を移動してエネルギー最小化を行う

## インプット
* `&GLOBAL`の`RUN_TYPE`を`GEO_OPT`にすること
* `&MOTION`セクションを追加する

### 構造最適化に必要な設定
* `GEO_OPT`: 最適化条件の設定
  + `MAX_DR`: 閾値となる原子の移動距離の最大値
  + `MAX_FORCE`: 閾値となるforceの最大値
  + `RMS_DR`: 閾値となる原子の移動距離の平均値
  + `RMS_FORCE`: 閾値となるforceの平均値
  + `MAX_ITER`: 最大ステップ数
  + `OPTIMIZER`: 最適化アルゴリズム。BFGSがよい。
* `PRINT`: 出力の設定

### インプットの例
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
