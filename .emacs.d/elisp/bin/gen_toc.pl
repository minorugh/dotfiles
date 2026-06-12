#!/usr/bin/env perl
use strict;
use warnings;
use utf8;
use Encode;

binmode(STDOUT, ":encoding(UTF-8)");
binmode(STDIN,  ":encoding(UTF-8)");

#----------------------------------------------------------------------
# gen_toc.pl - Markdown ファイルから目次を生成する
#
# 使い方:
#   perl gen_toc.pl file.md
#   perl gen_toc.pl < file.md
#
# 出力:
#   H1〜H6 の見出しを2階層まで目次化して標準出力に表示
#   コードブロック（```）内の見出しは無視
#   不要な行（タイトル行など）は手作業で削除すること
#
# アンカー生成ルール（pandoc HTMLプレビュー準拠）:
#   - 小文字化
#   - 行頭の番号除去（例: "1. はじめに" → "はじめに"）
#   - () 内のスペースは "-" に変換
#   - 全角括弧（）は削除
#   - 英語同士・日英混在のスペースは "-" に変換
#   - 日本語同士のスペースは削除
#----------------------------------------------------------------------

my $MAX_DEPTH = 2;  # 目次に含める最大階層数

# 入力ソース: 引数ファイル or 標準入力
my $fh;
if (@ARGV) {
    open($fh, "<:encoding(UTF-8)", $ARGV[0]) or die "Cannot open $ARGV[0]: $!";
} else {
    $fh = *STDIN;
}

my $in_code_block = 0;
my @toc;

while (my $line = <$fh>) {
    chomp $line;

    # コードブロックの開閉をトグル
    if ($line =~ /^```/) {
        $in_code_block = !$in_code_block;
        next;
    }
    next if $in_code_block;

    # 見出し行を解析
    next unless $line =~ /^(#{1,6})\s+(.+)$/;
    my $level = length($1);         # 1=H1, 2=H2, ...
    my $title = $2;

    # MAX_DEPTH を超える階層は無視
    next if $level > $MAX_DEPTH;

    # アンカー用テキスト: Markdown 装飾（ボールド・イタリック・コード）を除去
    my $anchor = $title;
    $anchor =~ s/`//g;
    $anchor =~ s/(\*\*|__)(.*?)\1/$2/g;
    $anchor =~ s/(\*|_)(.*?)\1/$2/g;

    # pandoc 準拠のアンカー生成
    $anchor = lc($anchor);
    $anchor =~ s/^[0-9]+\.?\s*//;                                          # 行頭番号を削除
    $anchor =~ s{\(([^)]*)\)}{$1 =~ s/\s+/-/gr}eg;                         # () 内スペース → -
    $anchor =~ s/[（）]//g;                                                # 全角括弧を削除
    $anchor =~ s/([A-Za-z])\s+([A-Za-z])/$1-$2/g;                          # 英語同士スペース → -
    $anchor =~ s/([\p{Han}\p{Hiragana}\p{Katakana}])\s+([A-Za-z])/$1-$2/g; # 日本語+英語 → -
    $anchor =~ s/([A-Za-z])\s+([\p{Han}\p{Hiragana}\p{Katakana}])/$1-$2/g; # 英語+日本語 → -
    $anchor =~ s/\s+//g;                                                   # 残りのスペースを削除

    # インデント: H1=なし、H2=2スペース
    my $indent = "  " x ($level - 1);
    push @toc, "$indent- [$title](#$anchor)";
}

# 目次を出力
print "## 目次\n";
print "<div class=\"toc\">\n";
print join("\n", @toc), "\n";
print "</div>\n\n";
