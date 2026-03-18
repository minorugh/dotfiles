#!/usr/bin/env perl
use strict;
use warnings;
use utf8;
use Encode;

# 標準入出力の文字コードをUTF-8に設定
binmode(STDOUT, ":encoding(UTF-8)");

my $in_code_block = 0;
my @toc;

my $fh;
if (@ARGV) {
    open($fh, "<:encoding(UTF-8)", $ARGV[0]) or die "Cannot open $ARGV[0]: $!";
} else {
    $fh = *STDIN;
}

while (my $line = <$fh>) {
    chomp $line;

    # コードブロック内の見出しは無視する
    if ($line =~ /^```/) {
        $in_code_block = !$in_code_block;
    }
    next if $in_code_block;

    # 見出し（#）を解析
    if ($line =~ /^(#{1,6})\s+(.+)$/) {
        my $level = length($1) - 1; # H1を0にする（インデント用）
        my $title = $2;
        
        # 不要なMarkdown装飾をリンク用タイトルから除去（ボールド、イタリックなど）
        my $anchor_text = $title;
        $anchor_text =~ s/`//g;
        $anchor_text =~ s/(\*\*|__)(.*?)\1/$2/g;
        $anchor_text =~ s/(\*|_)(.*?)\1/$2/g;

        # GitHub風のアンカー生成
        my $anchor = lc($anchor_text);

	# 1) 行頭の番号削除
	$anchor =~ s/^[0-9]+\.?\s*//;

	# 2) () 内 → スペースを -
	$anchor =~ s{\(([^)]*)\)}{$1 =~ s/\s+/-/gr}eg;

	# 3) 全角（）削除
	$anchor =~ s/[（）]//g;

	# 4) 英語同士のスペースを  -
	$anchor =~ s/([A-Za-z])\s+([A-Za-z])/$1-$2/g;

	# 5) 日本語＋英語 → -
	$anchor =~ s/([\p{Han}\p{Hiragana}\p{Katakana}])\s+([A-Za-z])/$1-$2/g;
	$anchor =~ s/([A-Za-z])\s+([\p{Han}\p{Hiragana}\p{Katakana}])/$1-$2/g;

	# 6) 残り（日本語同士など）は削除
	$anchor =~ s/\s+//g;
	
        # 目次用行を生成（インデント）
        my $indent = "  " x $level;
        push @toc, "$indent- [$title](#$anchor)";
    }
}

# 結果を出力
print "## 目次\n";
print "<div class=\"toc\">\n";
print join("\n", @toc), "\n";
print "</div>\n\n";
