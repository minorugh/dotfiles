#!/usr/bin/env perl
#
# dropbox-watch.pl
#
# systemd-logindの PrepareForSleepシグナルを D-Busの正規購読で監視し、
# サスペンドからの復帰を検知したら Dropboxを再起動する。
#
use strict;
use warnings;
use Net::DBus;
use Net::DBus::Reactor;
use POSIX qw(strftime);

my $LOGFILE = "$ENV{HOME}/.cache/dropbox-watch.log";

# ------------------------------------------------------------
# メイン: D-Busに接続し PrepareForSleepシグナルを購読して待機
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
    my $old_pid = `pgrep -x dropbox`;
    chomp $old_pid;

    sleep 8;
    system("pkill", "-x", "dropbox");
    sleep 1;
    system("dropbox", "start");
    sleep 1;

    my $new_pid = `pgrep -x dropbox`;
    chomp $new_pid;

    # 再起動前後の PIDを比較し、PIDが変化したことを再起動成功の判定子とする。
    # 再起動に失敗した場合は自身を終了し、systemdの Restart=alwaysに委ねる。
    # 一般ユーザー権限で動作し、systemd --user サービスとして常駐させる。
    if ($new_pid eq '' || $new_pid eq $old_pid) {
        write_log("dropbox restart FAILED (pid unchanged: $old_pid)");
        die "dropbox restart failed (pid unchanged: $old_pid)\n";
    }

    write_log("dropbox restarted (pid $old_pid -> $new_pid)");
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
