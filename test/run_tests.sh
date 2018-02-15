#!/usr/bin/env bash
# Time-stamp: <2018-02-15 22:30:02 kmodi>

echo "Emacs version:"
emacs --version

test_run_dir="/tmp/eless-test-run-dir"
mkdir -p "${test_run_dir}"

eless_repo_root="$(git rev-parse --show-toplevel)"
eless_test_dir="${eless_repo_root}/test"
eless_test_ref_snaps="${eless_test_dir}/ref-test-snaps"

HOME="${test_run_dir}"
mkdir -p "${HOME}"/.emacs.d

cp "${eless_test_dir}"/elesscfg_for_tests "${HOME}"/.emacs.d/elesscfg

cd "${test_run_dir}" || exit

export ELESS_TEST_SNAP_DIR="/tmp/eless-test-snaps/"
export ELESS="${eless_repo_root}/eless"

test_temp_dir="${test_run_dir}"/x
mkdir -p "${test_temp_dir}"

# http://redsymbol.net/articles/bash-exit-traps/
function cleanup {
    rm -rf "${test_run_dir}"
    rm -rf "${ELESS_TEST_SNAP_DIR}"
}
trap cleanup EXIT


## Tests

file1="${test_temp_dir}"/file1
echo "abc def" > "${file1}"

file2="${test_temp_dir}"/file2
echo "abc eef" > "${file2}"

ELESS_TEST="file" "${ELESS}" "${file1}"

echo 'foo' | ELESS_TEST="pipein" "${ELESS}"

echo 'foo' | ELESS_TEST="pipein_dash" "${ELESS}" -

diff "${file1}" "${file2}" | ELESS_TEST="pipein_diff" "${ELESS}"

grep 'def' "${file1}" | ELESS_TEST="pipein_grep" "${ELESS}"

ELESS_DISABLE_SNAP=1 "${ELESS}" -h | ELESS_TEST="pipein_help" "${ELESS}"

info grep | ELESS_TEST="pipein_info" ELESS_TEST_SNAP_NO_CONTENT=1 "${ELESS}"


ELESS_TEST="man" ELESS_TEST_SNAP_NO_CONTENT=1 PAGER="${eless_repo_root}/eless" man ls

cd "${test_temp_dir}" || exit
ELESS_TEST="dired" ELESS_TEST_SNAP_NO_CONTENT=1 "${ELESS}" .

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
