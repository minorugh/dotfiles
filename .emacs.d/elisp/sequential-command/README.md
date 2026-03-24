このREADMEは、
[https://hke7.wordpress.com/](https://hke7.wordpress.com/2012/04/08/sequential-command-%E3%82%92%E3%82%82%E3%81%86%E5%B0%91%E3%81%97%E8%B3%A2%E3%81%8F) 
のブログ記事を転載したものです。

## sequential-command をもう少し賢く
2012/04/08

僕としては emacs にはこちらの意図を感じとって、
僕を無用なキーの連打などから開放してくれる相棒のような存在であって欲しいと思っています。

そんなことはさておき、今回は sequential-command を少しいじって見ました。
コードはこちらにあります。


[HKey/sequential-command](https://github.com/HKey/sequential-command)

## 追加した機能
追加した機能は `define-sequential-command’ で登録したコマンド側から
自分の次に呼ばれるコマンドを呼び出すことができる、というものです。

例えば次のように `seq-home’ が定義されていたとします。

```emacs-lisp
(define-sequential-command seq-home
  seq-beginning-of-line
  seq-beginning-of-buffer
  seq-return)
```

この時 `seq-home’ を実行していくと次のように動作します。

1. 行頭へ移動 seq-beginning-of-line
2. バッファの先頭へ移動 seq-beginning-of-buffer
3. コマンド開始位置に復帰 seq-return

さて、`seq-beginning-of-line’ が実行されて行頭に移動するときに
すでにカーソルが行頭にあった場合、この操作は無駄です。
という訳で、`seq-home’ によって `seq-beginning-of-line’ が実行された時に
カーソルが行頭にあった場合、自身の動作をパスして
次のコマンドである `seq-beginning-of-buffer’ を呼び出せるようにしました。


## 使い方
`define-sequential-command’ で登録されたコマンドから次のコマンドを呼び出すには
`seq-next’ 関数を使用します。

簡単な例として、先ほどの `seq-beginning-of-line’ を定義してみましょう。
カーソルが行頭にあるかは `bolp’ を使うことで調べられるので、
以下のようにすればいいでしょう。

```emacs-lisp
(defun seq-beginning-of-line ()
  (interactive)
  (if (bolp)
      (seq-next)
    (call-interactively 'beginning-of-line)))
```

## カーソル移動専用のコマンド定義用マクロ
カーソル移動系のコマンドを先ほどの `seq-beginning-of-line’ のように定義すると、
大抵どれも同じような形になるので定義しやすいようにマクロ
`seq-define-cursor-command’ を用意しました。
sequential-command-config.el で定義されています。

sequential-command-config.el の中で先程の `seq-beginning-of-line’ は
次のように定義されています。

```emacs-lisp
(seq-define-cursor-command beginning-of-line)
```

こうすることで、コマンド名の頭に “seq-” がついた `seq-beginning-of-line’ が定義されます。
`seq-define-cursor-command’ は、与えられたコマンドをとりあえず一度実行して
実行の前後でカーソルの位置が変化すればそれまで、
カーソルの位置に変化がなければ `seq-next’ を呼び出す、というものです。


## `seq-next’ を呼び出す条件の変更
`seq-define-cursor-command’ には
`seq-next’ を呼び出す条件を変更するオプション引数が用意されています。
オプション引数である第2引数には、
`seq-next’ が呼び出されるべき時に真を返すフォームを指定します。

百聞は一見に如かず、という事で色々説明されてもわかりにくいと思うので
まずは簡単な実例を見てみましょう。

`back-to-indentation’ は行の先頭文字へ移動するコマンドですが、
コマンドを実行した時にカーソルの位置が変わらないか、後ろに下がってしまう場合は
次のコマンドを呼び出すような sequential-command 用のコマンドを考えてみます。
結果から言えば次のようになります。

```emacs-lisp
(seq-define-cursor-command back-to-indentation
  (<= seq-old-point seq-new-point))
`seq-old-point’ と `seq-new-point’ は
```

`seq-define-cursor-command’ 内で補足される変数で、
与えられたコマンドの実行前と後のカーソルの位置です。

これによって、コマンドの実行前後のカーソルの位置の関係で
`seq-next’ を呼び出す条件を指定することができます。
別に、`seq-old-point’ と `seq-new-point’ を必ず使う必要はありません。
あくまで、`seq-next’ を呼び出すべき時に真になればいいのですから。


## 注意点
最後に、現状の `seq-next’ は
`define-sequential-command’ に使われるコマンドを前提にしています。
したがって `seq-upcase-backward-word’ のような
単体で呼び出されるためのコマンドで使用するためには、
再帰的な呼び出しに対して自衛する必要があります。

