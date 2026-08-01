### メイン機ホスト名の一元管理
# author Minoru Yamada. 2026.08.01
#
# dotfiles配下にある各所の Makefileに includeされる共有定義。
# メイン機を機種変更した場合は ここの MAIN_HOSTNAMEの値を書き換えるだけで各所 Makefileに反映される。
# env-import/Makefileは dotfiles管理外のため独立管理（変更時はそれぞれ両方の修正が必要）。

MAIN_HOSTNAME := P1
