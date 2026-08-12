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

# ------------------------------------------------------------
# Dropbox 再起動処理
# ------------------------------------------------------------
sub restart_dropbox {
    # pkill前のdropboxのPIDを記録しておく（再起動できたかどうかの判定に使う）
    # my $old_pid = `pgrep -x dropbox`;
    # chomp $old_pid;

    sleep 8;
    system("pkill", "-x", "dropbox");
    sleep 2;
    system("dropbox", "start");
    # sleep 2;

    # dropbox start後のPIDを取得
    # my $new_pid = `pgrep -x dropbox`;
    # chomp $new_pid;

    my $ts = strftime("%Y-%m-%d %H:%M:%S", localtime);

    # PIDが取得できない、または再起動前と同じPIDのままなら再起動失敗とみなし、
    # ログに記録したうえで強制終了してsystemd（Restart=always）による
    # 自動再起動に委ねる
    # if ($new_pid eq '' || $new_pid eq $old_pid) {
    #     open(my $log, '>>', $LOGFILE) or die "log write failed: $!";
    #     print $log "$ts dropbox restart FAILED (pid unchanged: $old_pid)\n";
    #     close $log;
    #     die "dropbox restart failed (pid unchanged: $old_pid)\n";
    # }
    my $old_pid;my $new_pid;
    open(my $log, '>>', $LOGFILE) or die "log write failed: $!";
    print $log "$ts dropbox restarted (pid $old_pid -> $new_pid)\n";
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
        # 配列の後ろから200件を取得
        print $out @lines[@lines-200 .. $#lines];
        close $out;
    }
}
