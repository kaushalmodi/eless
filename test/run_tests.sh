#!/usr/bin/env bash
# Time-stamp: <2022-02-15 17:35:51 kmodi>

set -eu # http://redsymbol.net/articles/unofficial-bash-strict-mode
IFS=$'\n\t'

TMPDIR="${TMPDIR:-/tmp}"

EMACS="${EMACS:-emacs}"
echo "Emacs version:"
${EMACS} --version
echo "Info version:"
info --version
echo "Grep version:"
grep --version

test_run_dir="${TMPDIR}/eless-test-run-dir"
mkdir -p "${test_run_dir}"

eless_repo_root="$(git rev-parse --show-toplevel)"
eless_test_dir="${eless_repo_root}/test"
eless_test_ref_snaps="${eless_test_dir}/ref-test-snaps"

HOME="${test_run_dir}"
mkdir -p "${HOME}"/.emacs.d

cp "${eless_test_dir}"/elesscfg_for_tests "${HOME}"/.emacs.d/elesscfg

cd "${test_run_dir}" || exit

export ELESS_TEST_SNAP_DIR="${TMPDIR}/eless-test-snaps/"
export ELESS="${eless_repo_root}/eless"

# Whether or not ${ELESS_TEST_SNAP_DIR} exists, first create it and
# then delete it to remove old files if any.
mkdir -p "${ELESS_TEST_SNAP_DIR}"
rm -rf "${ELESS_TEST_SNAP_DIR}"

test_temp_dir="${test_run_dir}"/x
mkdir -p "${test_temp_dir}"

# http://redsymbol.net/articles/bash-exit-traps/
function cleanup {
    rm -rf "${test_run_dir}"
    # rm -rf "${ELESS_TEST_SNAP_DIR}"
}
trap cleanup EXIT


## Tests

file1="${test_temp_dir}"/file1
echo "abc def" > "${file1}"

file2="${test_temp_dir}"/file2
echo "abc eef" > "${file2}"

filename_with_spaces="${test_temp_dir}/filename with spaces.txt"
filename_with_spaces_escaped="${test_temp_dir}/filename\\ with\\ spaces.txt"
echo "hello" > "${filename_with_spaces}"

echo "[1] file test .."
eval "ELESS_TEST=file ${ELESS} -D ${file1}"

echo "[2] pipein test .."
eval "echo 'foo' | ELESS_TEST=pipein ${ELESS} -D"

echo "[3] pipein_dash test .."
eval "echo 'foo' | ELESS_TEST=pipein_dash ${ELESS} -D -"

echo "[4] pipein_diff test .."
eval "diff ${file1} ${file2} | ELESS_TEST=pipein_diff ${ELESS} -D"

echo "[5] pipein_grep test .."
eval "grep 'def' ${file1} | ELESS_TEST=pipein_grep ${ELESS} -D"

echo "[6] pipein_help test .."
eval "ELESS_DISABLE_SNAP=1 ${ELESS} -h | ELESS_TEST=pipein_help ${ELESS} -D"

echo "[7] pipein_info test .."
eval "info grep | ELESS_TEST=pipein_info ELESS_TEST_SNAP_NO_CONTENT=1 ${ELESS} -D"

echo "[8] man test .."
eval "ELESS_TEST=man ELESS_TEST_SNAP_NO_CONTENT=1 PAGER=\"${ELESS} -D\" man ls"

echo "[9] dired test .."
cd "${test_temp_dir}" || exit
eval "ELESS_TEST=dired ELESS_TEST_SNAP_NO_CONTENT=1 ${ELESS} -D ."

echo "[10] filename with spaces .."
eval "ELESS_TEST=filename_with_spaces ${ELESS} -D ${filename_with_spaces_escaped}"

# diff -u "${file1}" "${file2}" | ELESS_TEST="pipein_diffu" "${ELESS}"

# cd "${test_temp_dir}" || exit
# # shellcheck disable=SC2012
# ls --color=always | ELESS_TEST="pipein_ls" "${ELESS}"

# ELESS_TEST="file_gui" "${ELESS}" --gui "${file1}"

# tar caf "${test_temp_dir}"/x.tar.xz "${file1}" "${file2}"
# ELESS_TEST="archive" "${ELESS}" "${test_temp_dir}"/x.tar.xz

# diff "${file1}" "${file2}" | ELESS_TEST="pipein_diff_gui" "${ELESS}" --gui

## Diff

diff -r "${eless_test_ref_snaps}" "${ELESS_TEST_SNAP_DIR}"
