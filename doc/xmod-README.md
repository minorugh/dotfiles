# ThinkPad / Emacs 執筆用 Xmodmap 作業テンプレート

```xmodmap
!==============================================================
! 日付: 2026-03-16
! 作業者: [Your Name]
! 目的:
!   - CapsLock を Control として使用
!   - PrtSc キーを Right Alt として利用
!   - Ctrl_R + PrtSc で PrintScreen を維持
!   - WM ショートカットを壊さない
!   - Emacs の既存 keybind は変更せず執筆環境向けに最適化
!==============================================================

!-------------------------------
! CapsLock modifier を解除
!-------------------------------
clear Lock        ! Lock modifier を解除（Caps → Ctrl 用）

!-------------------------------
! CapsLock → Control_L
!-------------------------------
keycode 66 = Control_L      ! CapsLock を左 Ctrl に置き換え
add Control = Control_L     ! modifier として登録

!-------------------------------
! ThinkPad 特有: PrtSc を Alt_R に
! keycode 107 が PrtSc に対応
!-------------------------------
keycode 107 = Alt_R Meta_R Print Sys_Req
!   - 通常: Alt_R
!   - Shift + PrtSc: Meta_R
!   - Ctrl_R + PrtSc: Print (スクリーンショット)
!   - Ctrl_R + Shift + PrtSc: Sys_Req

!-------------------------------
! Alt modifier 安定化
!-------------------------------
remove Mod1 = Alt_R
add Mod1 = Alt_R

!-------------------------------
! 日本語キーボード対応
!-------------------------------
keycode 97 = underscore backslash underscore backslash
! 「ろ」キーを _ / \ に割り当て

!-------------------------------
! テンキーを常時数字化
!-------------------------------
keycode 87 = 1
keycode 88 = 2
keycode 89 = 3
keycode 83 = 4
keycode 84 = 5
keycode 85 = 6
keycode 79 = 7
keycode 80 = 8
keycode 81 = 9
keycode 90 = 0
keycode 91 = period
! NumLock を使わず常に数字入力

!==============================================================
! 実行結果 / テスト
!   - Caps → Ctrl 動作確認
!   - PrtSc → Alt_R / Ctrl_R + PrtSc = PrintScreen 確認
!   - WM の Ctrl_R + → ← / PgUp PgDn ショートカット確認
!   - Emacs keybind 変更なし
!==============================================================
