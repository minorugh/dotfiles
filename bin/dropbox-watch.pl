#!/usr/bin/env perl
#
# dropbox-watch.pl
#
# 判定方法: systemd-logindが発する PrepareForSleep シグナルを、D-Busの
# 正規の購読機構（connect_to_signal）で受信する。dbus-monitorのような
# eavesdropping方式ではないため、一般ユーザー権限で確実に動作する。
# サスペンド復帰時（false）を検知した瞬間にDropboxを再起動する。
#
# 実行方法: systemd --user サービスとして常駐させる（sudo不要）
#
use strict;
use warnings;
use Net::DBus;
use Net::DBus::Reactor;
use POSIX qw(strftime);

my $LOGFILE = "$ENV{HOME}/.cache/dropbox-watch.log";

# ------------------------------------------------------------
# Dropbox 再起動処理
# ------------------------------------------------------------
sub restart_dropbox {
    sleep 10;
    system("pkill", "-x", "dropbox");
    sleep 3;
    system("dropbox start >/dev/null 2>&1 &");

    my $ts = strftime("%Y-%m-%d %H:%M:%S", localtime);
    open(my $log, '>>', $LOGFILE) or die "log write failed: $!";
    print $log "$ts dropbox restarted (resume detected via dbus)\n";
    close $log;

    rotate_log();
}

# ------------------------------------------------------------
# ログファイルの肥大化防止（1000行超で直近200行に切り詰め）
# ------------------------------------------------------------
sub rotate_log {
    return unless -e $LOGFILE;
    open(my $fh, '<', $LOGFILE) or return;
    my @lines = <$fh>;
    close $fh;
    if (@lines > 1000) {
        open(my $out, '>', $LOGFILE) or return;
        print $out @lines[-200..$#lines];
        close $out;
    }
}

# ------------------------------------------------------------
# メイン: システムD-Busに接続し、PrepareForSleepシグナルを購読して待機
# sleeping=1: サスペンド開始直前 / 0: 復帰直後（復帰時のみ反応する）
# ------------------------------------------------------------
my $bus     = Net::DBus->system;
my $service = $bus->get_service("org.freedesktop.login1");
my $manager = $service->get_object(
    "/org/freedesktop/login1",
    "org.freedesktop.login1.Manager",
);

$manager->connect_to_signal("PrepareForSleep", sub {
    my ($sleeping) = @_;
    restart_dropbox() unless $sleeping;
});

Net::DBus::Reactor->main->run;
