# keyd 移行作業 引き継ぎ書

作成日: 2026-03-31  
環境: ThinkPad P1 / Debian 12 (bookworm) / Emacs / fcitx-mozc

---

## 現在の状態（作業終了時点）

**xmodmap で元の状態に戻済み。keyd はインストール済みだが無効化。**

```
keyd: インストール済み (v2.6.0) / systemctl disable 済み
xmodmap: 有効（crontab 毎分実行 + .xprofile から起動）
```

---

## 移行の目的

xmodmap は以下のタイミングでリセットされるため不安定：
- fcitx 再起動時
- スリープ復帰後

特に **PrtSc（keycode 108）→ Alt_R** の設定が外れやすい。  
keyd はカーネルレベルで動作するため、これを解決できるはずだった。

---

## 今日判明したこと

### ✅ 成功した部分

- keyd で **CapsLock → Ctrl** は正常動作
- keyd で **PrtSc → Alt_R**（および Ctrl_R+PrtSc → Print）は正常動作
- keyd の `ro` キー認識は正常（`keyd list-keys` に `ro` あり、`keyd monitor -r` でも確認）

### ❌ 未解決の問題

**「ろ」キー（KEY_RO / keycode 89）→ `_`（アンダースコア）が keyd でできない**

#### 原因の構造

```
物理「ろ」キー (KEY_RO)
  → keyd が横取り
  → X11(fcitx が jp レイアウトを強制適用)
  → 意図しない文字に変換される
```

試した方法と結果：

| 設定 | 結果 |
|------|------|
| `ro = _` | `=` が出力される |
| `ro = ro` | `\` が出力される |
| `ro = noop` | キー無効になる |
| `key89 = _` | `\` が出力される |
| `ro = S-minus` | `=`（jp配列では Shift+minus = `=`）|
| `[global] layout = jp` + `ro = _` | `=` のまま |
| `unicode(005f)` | 動作せず |
| `macro(_)` | `=` のまま |

#### 根本原因

1. **fcitx が X11 に jp レイアウトを強制適用する**
   - `setxkbmap -query` → `layout: jp`
   - `~/.config/fcitx/conf/fcitx-xkb.config` の `OverrideSystemXKBSettings=False` を試みたが fcitx が上書き
   - `fcitx profile` の `EnabledIMList` に `fcitx-keyboard-jp:True` が登録されている

2. **keyd の Unicode 機能は「X11 が US レイアウト」であることが前提**
   - man keyd: "you will need to be using the default US layout on your display server"
   - jp レイアウト環境では keyd の unicode 機能が使えない

3. **keyd の jp レイアウトファイルに `_` が定義されていない**
   - `/usr/local/share/keyd/layouts/jp` に `_` のエントリなし

---

## 次回の再挑戦に向けた手がかり

### 有望なアプローチ

**1. fcitx の jp キーボードを無効化して us に変更する**

`~/.config/fcitx/profile` の `EnabledIMList` を編集：
```
fcitx-keyboard-jp:True → fcitx-keyboard-jp:False
fcitx-keyboard-us:False → fcitx-keyboard-us:True
```
その上で keyd に `[global] layout = jp` を設定すれば、
X11=US / keyd=JP という正しい構成になるはず。

**2. keyd の jp レイアウトファイルに `_` を追加する**

```bash
sudo nano /usr/local/share/keyd/layouts/jp
```

`[jp:layout]` セクションに追加：
```
ro = _
```

`[jp_shift]` セクションにも追加：
```
ro = backslash
```

**3. keyd GitHub Issues / IRC で質問する**

- Issues: https://github.com/rvaiya/keyd/issues
- IRC: `#keyd on oftc`
- 質問のポイント: 「jp レイアウト環境（fcitx）で ro キーを _ にマップしたい。unicode 機能が使えない」

### 参考: kanata という代替ツール

keyd と同様のカーネルレベルツール。日本語キーボード(JIS)の `ro` キー対応事例あり：  
https://github.com/jtroo/kanata/discussions/973

---

## 現在の設定ファイル

### /etc/keyd/default.conf（現在・PrtSc のみ有効）

```ini
[ids]
*

[main]
print = rightalt
ro = ro     ← パススルー。noop にすると隣の「む」キーが壊れるので注意

[rightcontrol+main]
print = print

[shift+main]
print = rightmeta

[rightcontrol+shift+main]
print = sysrq
```

### ~/.xprofile（現在・xmodmap 有効）

```bash
if [ -f ~/.Xmodmap ]; then
    xmodmap ~/.Xmodmap
fi
```

### crontab（現在・xmodmap 毎分実行）

```
* * * * * DISPLAY=:0 /usr/bin/xmodmap /home/minoru/.Xmodmap >> /tmp/xmodmap.log 2>&1
* * * * * sleep 30; DISPLAY=:0 /usr/bin/xmodmap /home/minoru/.Xmodmap >> /tmp/xmodmap.log 2>&1
```

---

## 最終目標の設定イメージ

```ini
# /etc/keyd/default.conf（完成形）
[ids]
*

[main]
capslock = leftcontrol
print = rightalt
ro = _          ← これが課題
kp1 = 1
kp2 = 2
kp3 = 3
kp4 = 4
kp5 = 5
kp6 = 6
kp7 = 7
kp8 = 8
kp9 = 9
kp0 = 0
kpdot = .

[rightcontrol+main]
print = print

[shift+main]
print = rightmeta

[rightcontrol+shift+main]
print = sysrq
```

完成したら：
- `~/.xprofile` の xmodmap 行を削除
- crontab の xmodmap 2行を削除
- `make keymap` ターゲットを `make keyd` に置き換え
