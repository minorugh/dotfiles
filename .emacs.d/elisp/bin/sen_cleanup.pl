#!/usr/bin/env perl
# sen_cleanup.pl - みのる選：没句削除スクリプト
#
# 使い方:
#   perl sen_cleanup.pl input.txt    # 通常実行
#   perl sen_cleanup.pl -n input.txt # dry-runモード
#
# 終了コード:
#   0 : 成功
#   1 : エラー（メッセージをSTDERRに出力）

use strict;
use warnings;
use File::Copy;
use Getopt::Long;
use utf8;
binmode STDOUT, ':utf8';
binmode STDERR, ':utf8';

my $dry_run = 0;
GetOptions('n' => \$dry_run) or die usage();

my ($infile) = @ARGV;
die usage() unless $infile;
die error("入力ファイルが見つかりません: $infile") unless -f $infile;

# ファイル読み込み
open my $fh, '<:utf8', $infile or die error("ファイルを開けません: $infile: $!");
my @lines = <$fh>;
close $fh;
chomp @lines;

my $ts_pattern = qr/^\d{6,8}:$/;
my $dt_pattern = qr/^\d{4}年\d{1,2}月\d{1,2}日/;

# 安全チェック1: 先頭行がタイムスタンプであること
unless ($lines[0] =~ $ts_pattern) {
    die error("先頭行がタイムスタンプではありません: '$lines[0]'\n"
            . "yyyymmdd: または yyyymm: の形式が必要です。");
}

# タイムスタンプ行の検出
my $start_idx = 0;
my $end_idx   = -1;
for my $i (1 .. $#lines) {
    if ($lines[$i] =~ $ts_pattern) {
        $end_idx = $i;
        last;
    }
}
die error("2番目のタイムスタンプ行が見つかりません。") if $end_idx == -1;

# 処理対象範囲
my @before_range = @lines[$start_idx .. $end_idx];

# 処理前の◎句リスト
my @before_maru = grep { /^◎/ } @before_range;
my $before_count = scalar @before_maru;

# 没句削除
my @after_range = grep {
    /^◎/ || /$dt_pattern/ || /$ts_pattern/ || /^\s*$/
} @before_range;

# 処理後の◎句リスト
my @after_maru = grep { /^◎/ } @after_range;
my $after_count = scalar @after_maru;

# ◎句数確認
if ($before_count != $after_count) {
    my %after_set  = map { $_ => 1 } @after_maru;
    my @lost = grep { !$after_set{$_} } @before_maru;
    my $msg = "◎の数が一致しません（処理前:$before_count 処理後:$after_count）\n";
    $msg .= "欠落した句:\n";
    for my $s (@lost) {
        (my $disp = $s) =~ s/^◎/◎ /;
        $msg .= "  $disp\n";
    }
    die error($msg);
}

# dry-runはここで終了
if ($dry_run) {
    print "DRY-RUN: ◎ $before_count 句 → $after_count 句 OK\n";
    print "処理範囲: $lines[$start_idx] ～ $lines[$end_idx]\n";
    exit 0;
}

# bak保存
my $bakfile = "$infile.tmp";
copy($infile, $bakfile) or die error("bakファイルを保存できません: $!");

# ◎を行頭から削除
s/^◎\s*// for @after_range;

# ファイル書き戻し（outfileではなくinfileに直接）
my @result = (
    @after_range,
    @lines[$end_idx+1 .. $#lines],
);

open my $out, '>:utf8', $infile or die error("ファイルに書き戻せません: $infile: $!");
print $out join("\n", @result) . "\n";
close $out;

print "OK: ◎ $after_count 句。$bakfile に退避済み。\n";
exit 0;

sub error {
    my ($msg) = @_;
    print STDERR "[ERROR] $msg\n";
    exit 1;
}

sub usage {
    return "Usage: $0 [-n] input.txt\n";
}
