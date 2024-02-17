# miniz

Minimal scripting language in Zig.

2023 年度筑波大学情報科学類で開講された [GB27001 ソフトウェアサイエンス特別講義 A](https://kdb.tsukuba.ac.jp/syllabi/2023/GB27001/jpn/0) の講義内で紹介された [minis](https://github.com/kmizu/minis) の Zig による実装です。

## 実行

### repl

```sh
zig build run

```

### file

```text
hello = 2023;
world = 2024;
hello + world;
```

```sh
zig build run -- file.mnz # 4047
```

## 文法

miniz は以下の文法をサポートしています。

- 関数定義
- 関数呼び出し
- if 式
- while 式
- 連結
- 代入式
- 比較演算子
- 四則演算・モジュロ演算
