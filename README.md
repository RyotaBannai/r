# Rdev
- This repository includs following codes:
  - 競プロコード in R
  - R 勉強のためのランダムコード
  - 統計的な作業の確認、書き捨てコード
  
# レポート作成手順、開発フロー
- 最終的にNotebookと`{script_name}.nb.html`ファイルを出力する手順について
  - `{script_name}.nb.html`
    - Nootebook がレンダリングされたものがHTML で表現されたファイル
- `.r` でR スクリプトを作成
  - `.r` での記法は、`knitr` の記法に従う
    - コメントは`#-`, R コードは`#+` とする
    - 各コードブロックがNotebokのブロックとなる. pythonをvscode で使うときの `# %%`に相当
  - `.r` が作成できたら、`.r` -> `.Rmd` ファイルに変換
    - 事前に`knitr` パッケージをinstall しておく `pakcage.install(knitr)`
    - `knitr::spin('{script_name}.r', knit = FALSE)` を実行
      - これによって、`{script_name}.r`と同じ階層に、`{script_name}.Rmd` が作成される
      - `Rmd` はNotebook のセルに対応したコードブロックを持ったファイル
        - そのコードブロック内にR スクリプトを記述
        - `Rmd` は一から作成もできる（```r ``` をセルごとに作成）が、`.r` から knitr （`#-` と`#+` を使って）で自動生成する
      - 作成した `Rmd` の先頭に以下のような `output` メタデータを入れる.
        - そうすると Rstudio 上で`Rmd` を更新するたびに、自動で`{script_name}.nb.html`が作成される
  - `Rmd` に対して、`jupytext --to notebook {script_name}.Rmd` を実行してNotebook を作成
  - `jupytext` はpip で事前にinstall
      
```yaml
---
title: "My Notebook"
output: html_notebook
---
```


- Jupyter notebookでR を使う
  - `install.packages("devtools")`
  - `devtools::install_github("IRkernel/IRkernel")`
  - `system.file('kernelspec', package = 'IRkernel')`
  - `jupyter kernelspec list` (run this to be sure that jupyter is in your path, you should see information about the current available kernels.)
  - `R_PAHT = system.file('kernelspec', package = 'IRkernel')`
  - `jupyter kernelspec install {R_PAHT} --name 'R' --user` (you will use path that you received while working in R which could be different)
  - `jupyter kernelspec list` (this list should now include R)
  - [参考](https://stackoverflow.com/a/65042753/5590919)
