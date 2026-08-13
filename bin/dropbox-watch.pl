#!/usr/bin/env perl
#
# dropbox-watch.pl
#
# systemd-logindの PrepareForSleepシグナルを D-Busの正規購読で監視。
# サスペンドからの復帰を検知したら Dropboxを再起動する。
#
use strict;
use warnings;
use Net::DBus;
use Net::DBus::Reactor;
use POSIX qw(strftime);

my $LOGFILE = "$ENV{HOME}/.cache/dropbox-watch.log";

# ------------------------------------------------------------
# メイン
# ------------------------------------------------------------
# system busに接続し systemd-logindの Managerオブジェクトを取得する。
my $bus     = Net::DBus->system;
my $service = $bus->get_service("org.freedesktop.login1");
my $manager = $service->get_object(
    "/org/freedesktop/login1",
    "org.freedesktop.login1.Manager",
    );

# サスペンド復帰の通知を監視する。
# PrepareForSleep が true ならサスペンド開始、false なら復帰
$manager->connect_to_signal("PrepareForSleep", sub {
    my ($sleeping) = @_;
    restart_dropbox() unless $sleeping;
                            });

# DBusシグナルを待ち続ける。
Net::DBus::Reactor->main->run;

# ------------------------------------------------------------
# Dropbox 再起動処理
# ------------------------------------------------------------
sub restart_dropbox {
    my $old_pid = `pgrep -x dropbox`;
    chomp $old_pid;

    sleep 8;
    system("pkill", "-x", "dropbox");
    sleep 1;
    system("dropbox", "start");
    sleep 1;

    my $new_pid = `pgrep -x dropbox`;
    chomp $new_pid;

    # 再起動前後の PIDを比較して再起動成否を判定する。
    # 失敗の場合はログに書き出したあと即終了し systemdの Restart=alwaysに委ねる。
    if ($new_pid eq '' || $new_pid eq $old_pid) {
        write_log("dropbox restart FAILED (pid unchanged: $old_pid)");
        exit 1;
    }

    my $dir_count = `find "$ENV{HOME}/Dropbox" -type d | wc -l`;
    chomp $dir_count;

    write_log("dropbox restarted (pid $old_pid -> $new_pid, dirs $dir_count)");
}

# ------------------------------------------------------------
# ログヘ書き込む
# ------------------------------------------------------------
sub write_log {
    my ($message) = @_;

    open(my $log, '>>', $LOGFILE) or die "log write failed: $!";
    my $ts = strftime("%Y-%m-%d %H:%M:%S", localtime);
    print $log "$ts $message\n";
    close $log;
}
