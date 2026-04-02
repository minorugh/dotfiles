#!/usr/bin/perl
# howm-fix-code-comments.pl
# コードブロック内の行頭 "# " を "## " に変換してhowm listから除外する

use strict;
use warnings;
use utf8;
use open ':std', ':encoding(UTF-8)';
use Encode qw(decode_utf8);

my $file = decode_utf8($ARGV[0]) or die "Usage: $0 <howm-file>\n";

open my $fh, '<:encoding(UTF-8)', $file or die "Cannot open $file: $!";
my @lines = <$fh>;
close $fh;

my $in_code = 0;
my $modified = 0;

for my $line (@lines) {
    if ($line =~ /^```/) {
        $in_code = !$in_code;
        next;
    }
    if ($in_code) {
        if ($line =~ /^# /) {
            $line =~ s/^# /## /;
            $modified = 1;
        }
        # Toggle patterns (disabled):
        # } elsif ($line =~ /^## /) {
        #     $line =~ s/^## /# /;
        #     $modified = 1;
        # } elsif ($line =~ /^### /) {
        #     $line =~ s/^### /## /;
        #     $modified = 1;
        # }
    }
}

if ($modified) {
    open my $out, '>:encoding(UTF-8)', $file or die "Cannot write $file: $!";
    print $out @lines;
    close $out;
}
