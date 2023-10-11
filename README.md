![image](https://github.com/tatsuruikeda/databox/assets/85558579/e67f55b7-55ff-4a1c-8d76-ccc6cf4bac65)
## 北海道大学 医療AI開発者養成プログラム（CLAP） ##
### ゲノム医療特論 ###
### ミスセンス変異に対するin silico アルゴリズムスコアを素材としたRによる機械学習 〜2023年〜 ###

### Docker imageについて
講義内容を再現できるDocker imageを用意しています．

Docker imageは、アプリケーションをコンテナと呼ばれる軽量な仮想環境にパッケージングしたものです．アプリケーションの実行に必要なすべての環境をまとめて管理でき，OS間のずれが生じません．


まず，以下のサイトからDocker Desktopをインストールします．

[Docker Desktop](https://docs.docker.jp/desktop/index.html)

具体的な手順は以下のサイトが参考になるかもしれません．

[for Mac](https://qiita.com/R_R/items/a09fab09ce9fa9e905c5)

[for Windows](https://qiita.com/Yoshihito-Fukushima/items/0bcd629095218dc81ede)

Docker が使えるようになったらDocker imageを取得します．

```
docker pull buildandshipany/hokudai-rstudio-databox
```
イメージが取得出来たらコンテナを起動します．<YOUR_PASS>は任意のパスワードと置き換えます．

```
docker run  -e PASSWORD=<YOUR_PASS> -d -p 8787:8787 buildandshipany/hokudai-rstudio-databox:ver1 
```
ウェブブラウザで http://localhost:8787 にアクセスします．上で設定したパスワードを使います．

### 講義で使用したRパッケージのライセンス ###

1 pROC パッケージ

著作権者: Xavier Robin

ライセンス: GPL (>= 3)

ライセンス詳細: http://expasy.org/tools/pROC/

2 ggplot2 パッケージ

著作権者: Hadley Wickham

ライセンス: MIT + file LICENSE

ライセンス詳細: https://github.com/tidyverse/ggplot2

3 rpart パッケージ

著作権者: Terry Therneau

ライセンス: GPL-2 | GPL-3

ライセンス詳細: https://cran.r-project.org/package=rpart

4 rpart.plot パッケージ

著作権者: Stephen Milborrow

ライセンス: GPL-3

ライセンス詳細: http://www.milbo.org/rpart-plot/index.html

5 randomForest パッケージ

著作権者: Andy Liaw and Matthew Wiener

ライセンス: GPL (>= 2)

ライセンス詳細: https://www.stat.berkeley.edu/~breiman/RandomForests/

6 neuralnet パッケージ

著作権者: Stefan Fritsch

ライセンス: GPL (>= 2)

ライセンス詳細: https://github.com/bips-hb/neuralnet

7 ape パッケージ

著作権者: Emmanuel Paradis

ライセンス: GPL-2 | GPL-3

ライセンス詳細: https://github.com/emmanuelparadis/ape

8 ggrepel パッケージ

著作権者: Kamil Slowikowski

ライセンス: GPL-3 | file LICENSE

ライセンス詳細:https://github.com/slowkow/ggrepel

9 lattice パッケージ

著作権者: Deepayan Sarkar

ライセンス: GPL (>= 2)

ライセンス詳細: http://lattice.r-forge.r-project.org/

10 caret パッケージ

著作権者: Max Kuhn

ライセンス: GPL (>= 2)

ライセンス詳細: https://github.com/topepo/caret/
