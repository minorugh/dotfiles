#!/usr/bin/env bash
# melpa-backup.sh
# ELPAパッケージ（バージョン付きディレクトリ）を ~/Dropbox/backup/elpa へ rsync ミラーする。
# 更新・削除で消えるバージョンは削除前に PAST/<パッケージ名>/ へ退避し、
# パッケージごとに直近 MAX_GENERATIONS 世代までを保持する（超過分は古い順に自動削除）。
# git管理は行わない。autobackup.sh の run_target "melpa" melpa から呼び出される。
#
# 配置: sudo cp melpa-backup.sh /usr/local/bin/melpa-backup.sh
#
# 対象ディレクトリの判定:
#   ディレクトリ名の末尾が -数字 または -数字.数字 で終わるものだけを対象にする
#   （package.el が生成するバージョン付きディレクトリのみを自動判定で拾う）
#   archives/ や key-chord/ のようなバージョン番号なしの自前パッケージ(vc.el管理等)は
#   この判定により自然に除外される
#
# 変更履歴:
#   2026-06-07  Makefile の melpa ターゲットをスクリプトに移行
#   2026-06-21  git管理を廃止し、PAST/<パッケージ名>/ への世代管理方式に変更

set -euo pipefail

ELPA_SRC="${HOME}/src/github.com/minorugh/dotfiles/.emacs.d/elpa"
ELPA_BAK="${HOME}/Dropbox/backup/elpa"
PAST_DIR="${ELPA_BAK}/PAST"
RSYNC_LOG="/tmp/elpa-rsync.log"
RSYNC_DRYRUN_LOG="/tmp/elpa-rsync-dryrun.log"
CHANGELOG_DIR="${HOME}/Dropbox/backup/elpa/LOG"
LOGFILE="${CHANGELOG_DIR}/elpa-changes.log"
STAMP=$(date '+%Y-%m-%d %H:%M')
MAX_GENERATIONS=5

# rsyncフィルタ: バージョン付きディレクトリだけを対象にする。
# include/excludeは先勝ちなので、トップレベルに無条件の `*/` を置かないこと。
# （置くと後続のバージョン限定ルールが意味をなさなくなる）
VERSIONED_FILTER=(
  --include='*-[0-9]*.[0-9]*/' --include='*-[0-9]*/'
  --include='*-[0-9]*.[0-9]*/**' --include='*-[0-9]*/**'
  --exclude='/PAST/' --exclude='*'
)

# "magit-20260607.1830" のような名前から末尾のバージョン部分を切り落として
# パッケージ名 "magit" を取り出す。ハイフンを含むパッケージ名にも対応。
pkg_name_from_dir() {
  local dirname="$1"
  echo "${dirname}" | sed -E 's/-[0-9]+(\.[0-9]+)?$//'
}

mkdir -p "${ELPA_BAK}" "${PAST_DIR}" "${CHANGELOG_DIR}"

# ── 1. dry-run で「これから消えるディレクトリ」を先に把握する ──────────────
rsync -a --dry-run --delete \
  "${VERSIONED_FILTER[@]}" \
  --itemize-changes \
  "${ELPA_SRC}/" "${ELPA_BAK}/" > "${RSYNC_DRYRUN_LOG}" 2>&1

mapfile -t TO_DELETE < <(
  grep '^[*]deleting[[:space:]].*/$' "${RSYNC_DRYRUN_LOG}" \
    | awk '{print $2}' | cut -d/ -f1 | grep -v '^\.' | grep -v '^PAST$' | sort -u
)

# ── 2. 消える前に PAST/<パッケージ名>/ へ退避 ──────────────────────────────
for dirname in "${TO_DELETE[@]+"${TO_DELETE[@]}"}"; do
  src="${ELPA_BAK}/${dirname}"
  if [ -d "${src}" ]; then
    pkg=$(pkg_name_from_dir "${dirname}")
    mkdir -p "${PAST_DIR}/${pkg}"
    cp -a "${src}" "${PAST_DIR}/${pkg}/${dirname}"
  fi
done

# ── 3. 各パッケージのPAST世代が MAX_GENERATIONS を超えたら古い順に削除 ─────
for pkgdir in "${PAST_DIR}"/*/; do
  [ -d "${pkgdir}" ] || continue
  mapfile -t gens < <(ls -1t "${pkgdir}")
  count=${#gens[@]}
  if (( count > MAX_GENERATIONS )); then
    for (( i = MAX_GENERATIONS; i < count; i++ )); do
      rm -rf "${pkgdir}${gens[$i]}"
    done
  fi
done

# ── 4. 本番rsync（実際にミラーを更新） ──────────────────────────────────
rsync -a --delete \
  "${VERSIONED_FILTER[@]}" \
  --itemize-changes \
  "${ELPA_SRC}/" "${ELPA_BAK}/" > "${RSYNC_LOG}" 2>&1

mapfile -t ADDED_DIRS < <(
  grep '^[^*]d' "${RSYNC_LOG}" \
    | awk '{print $2}' | cut -d/ -f1 | grep -v '^\.' | grep -v '^PAST$' | sort -u
)

mapfile -t DELETED_DIRS < <(
  grep '^[*]deleting[[:space:]].*/$' "${RSYNC_LOG}" \
    | awk '{print $2}' | cut -d/ -f1 | grep -v '^\.' | grep -v '^PAST$' | sort -u
)

CNT_A=${#ADDED_DIRS[@]}
CNT_D=${#DELETED_DIRS[@]}

# 差分なし → 何もせず正常終了（autobackup.sh は exit code のみで判定）
if (( CNT_A == 0 && CNT_D == 0 )); then
  exit 0
fi

# ── 5. CHANGELOG 蓄積ログ書き出し ───────────────────────────────────────
mkdir -p "${CHANGELOG_DIR}"
{
  echo "${STAMP} [+${CNT_A} -${CNT_D}]"

  if (( CNT_A > 0 )); then
    echo "  ## 追加"
    printf '  + %s\n' "${ADDED_DIRS[@]}"
  fi
  if (( CNT_D > 0 )); then
    echo "  ## 退避（PASTへ）"
    printf '  - %s\n' "${DELETED_DIRS[@]}"
  fi

  echo ""
} >> "${LOGFILE}"

exit 0
