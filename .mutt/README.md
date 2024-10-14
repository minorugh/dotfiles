# Neomutt Configuration

## .muttrc 

### Open URL using w3m

``` dotenv
macro index,pager \cb ": unset wait_key; set pipe_decode\n|w3m\n: \
set wait_key; unset pipe_decode\n" "call w3m to extract URLs out of a message"
``` 
* 参考サイト
 [MuttのURLをw3mを使って開く](https://hanagurotanuki.blogspot.com/2015/04/mutturlw3m.html?utm_source=pocket_shared#google_vignette)
 
- Ctrl-Bを入力してw3mを起動した後、:(コロン)を入力してw3mにURL文字列をリンクに変換するように指示する。
- w3mのオプション設定パネル（oと入力）で、「URLのような文字列をすべてのページでリンクとして扱う」をYESに設定する。
- そうすれば、メッセージのページをめくるたびに「:」を入力し続ける必要がなくなる。

### Open w3m url in an external plaza

- w3mを開き、oと入力してオプション設定パネルを開く
- 「外部プログラム」までスクロールダウンし、「外部ブラウザ」または「第2の外部ブラウザ」を設定する。
- ブラウザを firefox または google-chrome に変更して [OK] を選択して設定を保存します。
- 外部ブラウザを使用してリンクをたどるには<Esc>Mとタイプする。
