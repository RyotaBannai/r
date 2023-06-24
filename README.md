# Rdev
- This repository includs following codes:
  - 競プロコード in R
  - R 勉強のためのランダムコード
  - 統計的な作業の確認、書き捨てコード
  
# レポート作成手順、開発フロー
- 最終的にNotebookと`{script_name}.nb.html`ファイルを出力する手順について
  - `{script_name}.nb.html`はNootebook がレンダリングされたものがHTML で表現されたファイル.
- `.r` でR スクリプトを作成してする.
  - `.r` での記法は、`knitr` の記法に従う.
    - コメントは`#-`, R コードは`#+` とする.
    - 各コードブロックがNotebokのブロックとなる. pythonをvscode で使うときの `# %%`に相当.
  - `.r` が作成できたら、`.r` -> `.Rmd` ファイルに変換する
    - 事前に`knitr` パッケージをinstall しておく `pakcage.install(knitr)`
    - `knitr::spin('script_name}.r', knit = FALSE)` を実行する. 
      - これによって、`{script_name}.r`と同じ階層に、`{script_name}.Rmd` が作成される.
      - `Rmd` はNotebook 形式のためにコードブロックを作ってその中にR のスクリプトを記述したもの.
        - 一から作成できるが、`.r` から生成することを目的としているため、このような手順を踏んでいる.
      - 作成した `Rmd` の先頭に次のようなメタデータを入れる. これを入れると Rstudio が自動で`{script_name}.nb.html`を作成する.
  - `Rmd` に対して、`jupytext --to notebook {script_name}.Rmd` を実行すると、Notebook が作成される.
  - `jupytext` はpip で事前にinstall しておく.
  
      
```yaml
---
title: "My Notebook"
output: html_notebook
---
```