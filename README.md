# これは何？

事情の経緯は以下を参照。

- 求む！PCエンジン「妖怪道中記」のパスワード3種
  - https://i486.mods.jp/ichild/yokaipw
- discord 部屋開設のお知らせ
  -  https://twitter.com/Imaha486/status/1474943554430865410
- Twitter: #隠しパスワード解析選手権
  - https://twitter.com/hashtag/%E9%9A%A0%E3%81%97%E3%83%91%E3%82%B9%E3%83%AF%E3%83%BC%E3%83%89%E8%A7%A3%E6%9E%90%E9%81%B8%E6%89%8B%E6%A8%A9

２つのツールを同梱しています

- yokai
  - パスワードからのハッシュ計算＆ハッシュ値からのパスワード検索ツール
- tokenize_filter
  - パスワード一覧と辞書ファイルから、パスワードを人間が読める形に書き下す（読めないものを弾く）ツール

# つかいかた

## yokai

パスワードからハッシュ値を計算

```
$ yokai checksum SPEED-UP
checksum: ED 26 08 EE 3D 23 1D 12
```

ハッシュ値からパスワードを検索

```
$ yokai search  ED 26 08 EE 3D 23 1D 12
target checksum: ED 26 08 EE 3D 23 1D 12
fixed prefix: 
num of threads: 12
[3, 1, 0, 0, 4]   [1, 6, 1, 1, 126] -> 756
[3, 0, 1, 1, 3]   [1, 1, 14, 15, 56] -> 11760
[2, 2, 0, 1, 3]   [1, 21, 1, 15, 56] -> 17640
   (中略)
[0, 5, 3, 0, 0]   [1, 252, 560, 1, 1] -> 141120
---------------------------------------------------
FOUND!!  passcode is...
    SPEED-UP
```

先頭の文字を固定して検索

```
$ yokai search 65 94 0E AC E9 07 33 25  --prefix=KILLNU
target checksum: 65 94 0E AC E9 07 33 25
fixed prefix: KILLNU
num of threads: 12
[2, 0, 1, 0, 5]   [1, 1, 14, 1, 252] -> 3528
   (中略)
---------------------------------------------------
FOUND!!  passcode is...
    KILLNU095TICKS
```
## tokenize_filter

- `-d` で辞書ファイルを指定(複数可)
- パスワードファイルは引数で指定。指定がなければ標準入力から読み取る
  - １行１パスワード
- すべてのアルファベットが辞書の単語で表現できたパスワードのみを出力する
  - 各行は、「パスワード」「タブ文字」「パスワードを辞書単語で分かち書きしたもの」の形式

例：

```
$ echo "KILLNU095TICKS" | tokenize_filter -d dict/american-english -d dict/roman_jp.txt
KILLNU095TICKS	kill ぬ 095 ticks
```
