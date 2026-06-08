#!/usr/bin/env bash
# melpa-backup.sh
# ELPAパッケージを ~/Dropbox/backup/elpa へ rsync + git commit
# autobackup.sh の run_melpa() から make 経由で呼び出される
#
# 配置: sudo cp melpa-backup.sh /usr/local/bin/melpa-backup.sh
#
# rsync の .git 除外方針:
#   --exclude='/.git' : トップレベル限定で ELPA_BAK/.git を保護
#                       パッケージ内の .git はそのまま転送・保持される
#
# 変更履歴:
#   2026-06-07  Makefile の melpa ターゲットをスクリプトに移行

set -euo pipefail

ELPA_SRC="${HOME}/src/github.com/minorugh/dotfiles/.emacs.d/elpa"
ELPA_BAK="${HOME}/Dropbox/backup/elpa"
RSYNC_LOG="/tmp/elpa-rsync.log"
CHANGELOG_DIR="${HOME}/Dropbox/CHANGELOG/elpa"
LOGFILE="${CHANGELOG_DIR}/elpa-changes.log"
STAMP=$(date '+%Y-%m-%d %H:%M')

# ── 1. rsync ────────────────────────────────────────────────────────────────
rsync -av \
  --delete \
  --filter='protect /.git/' \
  --exclude='**/.git/' \
  --itemize-changes \
  "${ELPA_SRC}/" "${ELPA_BAK}/" > "${RSYNC_LOG}" 2>&1

# ── 2. git ステージング ──────────────────────────────────────────────────────
cd "${ELPA_BAK}"
git add .

# 差分なし → 正常終了（autobackup.sh はハッシュ比較で判断）
if git diff --cached --quiet; then
  exit 0
fi

# ── 3. 変更パッケージをディレクトリ単位で集計 ───────────────────────────────
#
# rsync --itemize-changes の行頭フラグ:
#   >d......... : ディレクトリ新規作成 → 追加パッケージ
#   *deleting   : 削除（--backup 方式では PAST/ へ退避）
#
# 更新 = git ステージ済みトップディレクトリのうち 追加でも削除でもないもの

mapfile -t ADDED_DIRS < <(
  grep '^>d' "${RSYNC_LOG}" \
  | awk '{print $2}' | cut -d/ -f1 | grep -v '^\.' | grep -v '^PAST$' | sort -u
)

mapfile -t DELETED_DIRS < <(
  grep '^[*]deleting[[:space:]].*/$' "${RSYNC_LOG}" \
  | awk '{print $2}' | cut -d/ -f1 | grep -v '^\.' | grep -v '^PAST$' | sort -u
)

mapfile -t STAGED_DIRS < <(
  git diff --cached --name-only | cut -d/ -f1 | grep -v '^\.' | grep -v '^PAST$' | sort -u
)

mapfile -t UPDATED_DIRS < <(
  comm -23 \
    <(printf '%s\n' "${STAGED_DIRS[@]+"${STAGED_DIRS[@]}"}") \
    <(printf '%s\n' \
        "${ADDED_DIRS[@]+"${ADDED_DIRS[@]}"}" \
        "${DELETED_DIRS[@]+"${DELETED_DIRS[@]}"}" \
      | sort -u)
)

CNT_A=${#ADDED_DIRS[@]}
CNT_D=${#DELETED_DIRS[@]}
CNT_U=${#UPDATED_DIRS[@]}

# ── 4. CHANGELOG 蓄積ログ書き出し ───────────────────────────────────────────
mkdir -p "${CHANGELOG_DIR}"
{
  echo "${STAMP} [+${CNT_A} ~${CNT_U} -${CNT_D}]"

  if (( CNT_D > 0 )); then
    echo "  ## 削除"
    printf '  - %s\n' "${DELETED_DIRS[@]}"
  fi
  if (( CNT_A > 0 )); then
    echo "  ## 追加"
    printf '  + %s\n' "${ADDED_DIRS[@]}"
  fi
  if (( CNT_U > 0 )); then
    echo "  ## 更新"
    printf '  ~ %s\n' "${UPDATED_DIRS[@]}"
  fi

  echo ""
} >> "${LOGFILE}"

# ── 5. git commit ────────────────────────────────────────────────────────────
git commit -m "auto ELPA: ${STAMP} [+${CNT_A} ~${CNT_U} -${CNT_D}]"
