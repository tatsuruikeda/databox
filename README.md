![image](https://github.com/tatsuruikeda/databox/assets/85558579/e67f55b7-55ff-4a1c-8d76-ccc6cf4bac65)
# 北海道大学 医療AI開発者養成プログラム（CLAP） #
### ゲノム医療特論 ###
### ミスセンス変異に対するin silico アルゴリズムスコアを素材としたRによる機械学習 〜2023年〜 ###

Docker imageが使えます．以下，Dockerの使い方．
```
docker pull buildandshipany/hokudai-rstudio-databox
```
イメージが取得出来たらコンテナを起動します．<YOUR_PASS>は任意のパスワードと置き換えます．

```
docker run  -e PASSWORD=<YOUR_PASS> -d -p 8787:8787 buildandshipany/hokudai-rstudio-databox:ver1 
```
ウェブブラウザでhttp://localhost:8787にアクセスします．上で設定したパスワードを使います．