# git/Makefile の変更履歴

ターゲットを変えた経緯（なぜそうしたか）を、新しい順に残す。
現状の説明は Makefile のヘッダと README.md を見る。

## 2026-11-19

Makefile のヘッダにあった変更履歴を、このファイルに移した。  
Makefile のヘッダは、現状の説明だけにした。  
README.md からも、日付つきの訂正・追記と旧名の経緯を削った。

## 2026-11-18

auto-commit を git-auto から切り出した。prep のあと全変更を auto commit するだけで push はしない（メイン機専用）。  
Emacs の hydra-dired の [ から呼ぶ。commit だけ手動で行い、push は ] の git か、NIGHT SUSPEND などの自動実行に任せる運用のため。  
git-auto は auto-commit のあと push する形にした。動作は変えていない。  
呼び出し元は power-menu の NIGHT SUSPEND だけになった（hydra-dired の a は廃止）。

## 2026-11-13

nightly を git-auto に改名した。呼び出し元が power-menu の NIGHT SUSPEND だけでなく、  
Emacs の hydra-dired の a（dotfiles/git で make git-auto）にも増え、時間帯を表す名前が実態に合わなくなったため。  
サブ機で pull する前に、メイン機の push を済ませておくために使う。  
動作は変えていない。git は push のみ、git-auto は prep のあと全変更を auto commit して push する。

## 2026-11-10

nightly の呼び出し元を autobackup（backup/Makefile の git-push）から power-menu の NIGHT SUSPEND に移した。  
dotfiles の auto commit と push は、寝る前に NIGHT SUSPEND を押したときだけ走る。  
押さずに寝た夜は push されないが、変更は P1 に残り、次回の NIGHT SUSPEND か tig の P で出る。  
サブ機で pull する前は、メイン機で push する運用で補う。  
朝のフォールバックでは dotfiles を push しない（作業中の編集を auto commit しないため）。

## 2026-11-09

commit と push の分離で、env_source の auto commit と bundle が「すべて commit」（prep）のときだけ  
実行されるようになり、1ファイルだけの commit のあとに push しても Dropbox の bundle が更新されなかった。  
prep の env_source 処理を env-backup に切り出し、git（メイン機の push）の冒頭でも実行するようにした。  
env.hash が変わったときは、そのファイルだけを auto commit してから push する。  
nightly を追加した。夜間の autobackup（backup/Makefile の git-push）から呼ばれ、メイン機で prep のあと  
全変更を auto commit して push する。日中の git の動き（未 commit は警告して止まる）は変えていない。

## 2026-11-07

commit と push を分離した。commit は Emacs 側（85-git.el のメッセージバッファ）で行い、  
auto commit と commit ターゲットは廃止した。  
prep: メイン機の commit 前処理（env_source の bundle 更新、env.hash 更新、changelog-rotate）。  
      Emacs の「すべて commit」から呼ばれる。env.hash は commit に含めるため push 側では更新しない。  
git:  メイン機は未コミットの変更があれば警告して止め、なければ未 push の一覧を表示して push する。  
      サブ機は従来どおり pull。Emacs の ] はこのターゲットを直接呼ぶ。  
git-fix: 未 push のコミットが消えるため、メイン機では実行を止めるようにした（サブ機専用）。

## 2026-11-06

env/ の bindfs 窓を廃止したため、env-remount と env-check を削除した。~/.env_source を直接参照する。

## 2026-10-15

env/ の bindfs 失効対策。再起動やプロセス死亡でマウントが外れると env/ が空に見える。  
env-check を新設し、make git の冒頭（メイン/サブ共通）で自動検知・復旧する。正常時は無音。  
判定: ~/.env_source が非空なのに、env/ が非マウントまたは空なら失効とみなして再マウント。  
env-remount は、mountpoint で拾えない失効マウント（/proc/mounts に残る状態）も掃除する。  
従来メイン機の push 前に毎回実行していた env-remount 呼び出しは、env-check に一本化して削除。

## 2026-10-01

archlinux 用の分岐（arch-install の commit/push を呼ぶ処理）を削除。arch-install リポジトリは  
廃止され、現在は arch-debian-restore のサブディレクトリにある（そちらはサブ機では何もしない）。  
残すと、存在しないリポジトリへの push が失敗（Repository not found）して make git が異常終了する。

## 2026-09-30

サブ機の pull 処理を2点修正。  
(1) git stash pop を「今回 stash を作った時だけ」実行する。従来は無条件だったため、作業ツリーが  
    きれいで stash を作らなかった場合に、過去の失敗で残った古い stash が pop されて復活していた。  
(2) pull --rebase を pull --ff-only に変更。サブ機は push しないので独自コミットは持たない前提。  
    --rebase だと古い自動コミットが再適用されて混ざる（2026.09.08 の不具合と同じ型）。  
    --ff-only なら、履歴が分岐していれば書き換えずにエラーで止まる。  
    あわせて stash pop の競合を警告する（従来は競合しても || true で成功扱いのまま env-sync へ進んでいた）。

## 2026-09-15

abookが~/.env_sourceにgit統合されたため、env-syncの abook_diff（addressbook_*.gpg  
を個別に探して同期する処理）を廃止。abookの更新はenv_source全体の同期（env_diff）  
に自動的に含まれるため、別経路での同期は不要かつ二重管理の原因になっていた。  
env.bundle.gpg のパス表記ゆれ（arch-restore / arch-debian-restore 等）も  
~/Dropbox/backup/env/env.bundle.gpg に統一。

## 2026-09-08

サブ機側のpull前処理を「git add -A + auto commit」から「git stash」方式に変更。  
前者だと、サブ機で試しに書いただけの編集（コミットする意図のない変更）まで  
次のmake git実行時に問答無用でコミットされてしまい、pushしないためローカルに  
気づかれずコミットが蓄積、後日のpullで正規の変更とマージされて重複定義等の  
原因不明な不具合につながっていた。stash方式なら作業ツリーに戻るだけでコミット  
履歴を汚さない。

## 2026-08-04

env-sync の変更判定が「サブ機ローカルのDropbox同期状態」に依存しており、Dropboxの同期遅延で  
誤って「変更なし」と表示される問題があったため、判定基準を git 管理下の git/env.hash（メイン機が  
bundle生成直後に書き出し、dotfiles push経由で即時配布）に変更。Dropbox側がまだ追いついていない  
場合は「同期待ち」と明示し、誤検知を無くした。

## 2026-08-03

.env_source 側の構造変化で dotfiles/env が空になることがあるのでメイン機側の実行コードに remount処理を追加した。  
経緯の詳細は changelog-20260802.md 参照。
