// =============================================================
// textlint 設定ファイル
// =============================================================
//
// 【現在の状態】
//   2026-07-02時点、Emacs(flycheck)側では textlint チェッカーを
//   無効化中。文章チェック機能自体は使っていない。
//   → ~/.emacs.d 側の flycheck 設定で
//     (setq-default flycheck-disabled-checkers '(textlint))
//     が効いているため、このファイルがあってもflycheckからは呼ばれない。
//
// 【また使いたくなったときの手順】
//
//   1. textlint本体と、下記rulesで使っているルール一式をグローバルインストール
//      (npmのprefixを ~/.npm-global 等に変更済みなら sudo 不要のはず)
//
//        npm install -g \
//          textlint \
//          textlint-filter-rule-allowlist \
//          textlint-rule-preset-ja-technical-writing \
//          textlint-rule-preset-ja-spacing \
//          textlint-rule-no-mixed-zenkaku-and-hankaku-alphabet \
//          @textlint-ja/textlint-rule-no-insert-dropping-sa \
//          textlint-rule-prefer-tari-tari \
//          textlint-rule-ja-no-orthographic-variants \
//          textlint-rule-ja-no-inappropriate-words \
//          textlint-rule-ja-overlooked-typo
//
//   2. このファイルを ~/.textlintrc.js としてシンボリックリンク
//      (dotfiles管理なら、既存のリンク作成スクリプト/Makefileに追加)
//
//        ln -sf ~/src/github.com/minorugh/dotfiles/.textlintrc.js ~/.textlintrc.js
//
//   3. 単体で動作確認 (flycheckを介さず直接)
//
//        echo "これはテストです。" > /tmp/test.md
//        textlint /tmp/test.md
//
//      「No rules found」が出ず、正常終了 or 指摘が出ればOK。
//
//   4. Emacs側でtextlintチェッカーを再度有効化
//      ~/.emacs.d 側 flycheck 設定から下記1行を削除 (またはコメントアウト)
//
//        (setq-default flycheck-disabled-checkers '(textlint))
//
//      その後 Emacs再起動 → 対象の.mdファイルを開いて
//      C-h v flycheck-disabled-checkers で nil に戻っていることを確認。
//
// =============================================================

module.exports = {
  plugins: {
    // 今の処なにもインストールしていない
  },
  filters: {
    allowlist: {
      allow: [
        // 文字列の場合のサンプル
        "ignored-word",
        "/\\d{4}-\\d{2}-\\d{2}/", // 正規表現によるサンプル
        // ===IGNORE ===/IGNORE で囲まれた範囲の正規表現サンプル
        "/===IGNORE===[\\s\\S]*?===\\/IGNORE===/m",
        // Hugo記事のヘッダー（+++で囲まれた範囲）を無視する
        "/\\+\\+\\+[\\s\\S]*?\\+\\+\\+/m",
        // HTMLタグ（<a /a>で囲まれた範囲）を無視する
        "/\\<\\a[\\s\\S]*?\\/\\a\\>/m",
        // HTMLタグ（<p /p>で囲まれた範囲）を無視する
        "/\\<\\p[\\s\\S]*?\\/\\p\\>/m",
      ],
    },
  },
  rules: {
    "preset-ja-technical-writing": {
      // 「。」のつけ忘れのチェックを除外
      "ja-no-mixed-period": false,
      // 「？」のチェックを除外
      "no-exclamation-question-mark": false,
      // 「思う」などの弱い日本語表現のチェックを除外
      "ja-no-weak-phrase": false,
      // 対になっていない括弧のチェックを除外
      "no-unmatched-pair": false,
      // 漢字連続の警告を７文字まで許可
      "max-kanji-continuous-len": {
        max: 7,
      },
      "ja-no-successive-word": {
        // 同一単語の連続チェックを除外する単語の設定
        allowOnomatopee: false,
        allow: ["　", "・"],
      },
    },
    "preset-ja-spacing": {
      // 半角文字と全角文字の間のスペースチェックを除外
      "ja-space-between-half-and-full-width": false,
      // 括弧の内外のスペースチェックを除外
      "ja-no-space-around-parentheses": false,
    },
    "no-mixed-zenkaku-and-hankaku-alphabet": true,
    "@textlint-ja/textlint-rule-no-insert-dropping-sa": true,
    "prefer-tari-tari": true,
    "ja-no-orthographic-variants": true,
    "ja-no-inappropriate-words": true,
    "ja-overlooked-typo": true, // 見逃しそうなタイプミスを検出
  },
};
