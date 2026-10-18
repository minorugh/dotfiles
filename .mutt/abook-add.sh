#!/bin/bash
perl -MEncode -e '
    local $/ = "";
    my $head = <STDIN>;
    $head =~ s/\r//g;
    $head =~ s/\n[ \t]+/ /g;
    my ($from) = $head =~ /^From:[ \t]*(.*)$/mi;
    exit 1 unless defined $from;
    utf8::decode($from);
    binmode(STDOUT, ":utf8");
    print "From: ", Encode::decode("MIME-Header", $from), "\n";
' | abook --add-email
