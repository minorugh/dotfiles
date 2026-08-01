### メイン機ホスト名の一元管理
# author Minoru Yamada. 2026.08.01
#
# dotfiles配下にある各所のMakefile に include される共有定義。
# メイン機の機種を変更する場合は MAIN_HOSTNAME の値だけを書き換えればよい。
# env-import/Makefile はdotfiles管理外のため独立管理（変更時は両方の修正が必要）。

MAIN_HOSTNAME := P1
