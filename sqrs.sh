#!/usr/bin/env bash
#
# sqrs.sh – SQLite Quick Revision System
#
# sqrs.sh is a lightweight per-document revision tracker, a rewrite of the
# original Emacs/7z system “7z-revisions.el” (written ~2018).  It provides a
# simple, transparent way to record the history of a single text file across
# edits, without requiring Git, staging, or a full VCS repository.
#
# Every time you invoke “sqrs.sh add-rev <file>”, the script:
#   • compares the current file against its previous saved version
#   • generates a diff (normal unified diff, or diff -Na style)
#   • stores that diff in a per-file SQLite database <file>.db
#   • updates the latest full copy (base64 encoded)
#   • tracks optional revision notes and SHA1 hashes
#
# This creates a compact, append-only revision history you can:
#   • inspect (list revisions with hashes/notes)
#   • diff (between any two revisions)
#   • extract or rollback (reconstruct rev N from original + patches)
#   • view raw stored diffs (exact patch data between saves)
#
# Unlike Git, sqrs.sh captures *every save or checkpoint* you choose to record,
# producing a fine-grained daily history ideal for writing, note-taking,
# configuration editing, or any workflow where you want:
#
#     “What did I change today?”  
#     “What did I change in the last 5 minutes?”  
#     “Undo that edit from this morning without losing the rest.”  
#
# Compared to the older 7z-revisions.el:
#   • Uses SQLite instead of 7z archives (simpler & faster)
#   • Stores diffs directly instead of whole files
#   • Avoids filesystem bugs with spaces/encoding
#   • Avoids temporary extraction steps
#   • Fully editor-agnostic (Emacs, Vim, shell, etc.)
#
# In short:
#   sqrs.sh acts like a tiny per-file version control system,
#   built around plain diffs and SQLite, designed for simplicity,
#   transparency, and long-term robustness.
#
#
# sqrs.sh  -  Per-document SQLite revision system
#
# Author: ChatGPT5.1 with assistance from ciscorx@gmail.com
# For a document /path/to/file.txt, the DB is /path/to/file.txt.db
#
# Always stored:
#   - original (rev 0)     : in documents.original_blob
#   - latest               : in documents.latest_blob
# All intermediate revs:
#   - revisions.diff_base64 as diff -u (prev -> current)
#
# Commands:
#   ./sqrs.sh add-doc <file>
#       Create <file>.db, store original as rev 0.
#
#   ./sqrs.sh add-rev [-n "note"] <file>
#       Add a new revision (diff vs previous), update latest.
#
#   ./sqrs.sh list <file>
#       List rev 0 (original) and rev 1..N, with hashes and notes.
#
#   ./sqrs.sh extract <file> <rev> [output-file]
#       Extract revision rev (0 = original); default output is <file>.
#
#   ./sqrs.sh rollback <file> <rev>
#       Overwrite <file> with revision rev (0 = original).
#
#   ./sqrs.sh rename <old-file> <new-file>
#       Rename the document *and* move the DB file accordingly.
#
#   ./sqrs.sh missing <file>
#       Show missing rev numbers in 1..highest_revision.
#
#   ./sqrs.sh verify <file> <rev>
#       Reconstruct rev and verify SHA1 vs stored hash.
#
#   ./sqrs.sh diff <file> <rev1> <rev2>
#       Show unified diff between two revisions (0 allowed).
#
# Requirements: bash, sqlite3, base64, sha1sum, diff, patch
#

set -euo pipefail

# Global: set for each command based on the file argument
DB_NAME=""
DOC_NORM=""

# -------------------------------------
# Color toggle (also controls LCS)
# -------------------------------------
# 0 = no color (and no LCS); 1 = color + LCS word diff
: "${SQRS_COLOR:=0}"

color_enabled() {
    # must be explicitly enabled, and stdout must be a TTY
    [[ "$SQRS_COLOR" -eq 1 ]] && [[ -t 1 ]]
}

# --- CONFIG: default color + hash behavior -------------------------------

# Change this in the script if you want color on/off by default:
#   1 = enable colored diff output by default
#   0 = disable by default
# Environment variable SQRS_COLOR (0/1) overrides this at runtime.
SQRS_COLOR_DEFAULT=1

# Hashing:
#   SQRS_HASH_DEFAULT=1  (change to 0 to disable by default)
#   SQRS_HASH env var (0/1) overrides this.
SQRS_HASH_DEFAULT=1

# -------- helpers --------

die() {
    echo "Error: $*" >&2
    exit 1
}

command_exists() {
    command -v "$1" >/dev/null 2>&1
}

ensure_deps() {
    if [[ "${_SQRS_DEPS_CHECKED:-0}" -eq 1 ]]; then
        return 0
    fi

    local missing=()
    local cmd

    for cmd in sqlite3 base64 sha1sum diff patch; do
        if ! command_exists "$cmd"; then
            missing+=("$cmd")
        fi
    done

    if ((${#missing[@]} > 0)); then
        echo "Error: required command(s) not found in PATH:" >&2
        for cmd in "${missing[@]}"; do
            echo "  - $cmd" >&2
        done
        die "Please install the missing dependencies and try again."
    fi

    _SQRS_DEPS_CHECKED=1
}

normpath() {
    local p="$1"

    if command_exists realpath; then
        if realpath --help 2>&1 | grep -q -- '--canonicalize-missing'; then
            realpath --canonicalize-missing "$p"
        else
            realpath "$p"
        fi
        return
    fi

    if command_exists readlink; then
        if readlink -f "$p" >/dev/null 2>&1; then
            readlink -f "$p"
            return
        fi
    fi

    if [[ "$p" != /* ]]; then
        p="$PWD/$p"
    fi

    p=$(echo "$p" | sed 's:/\+:/:g')
    p=$(echo "$p" | sed 's:/\./:/:g')
    p=$(echo "$p" | sed 's:/$\.::')

    if [[ "$p" != "/" ]]; then
        p="${p%/}"
    fi

    printf '%s\n' "$p"
}

set_db_for_doc() {
    local file="$1"
    DOC_NORM=$(normpath "$file")
    DB_NAME="${DOC_NORM}.db"
}

sql_escape() {
    local s="$1"
    s=${s//\'/\'\'}
    printf '%s' "$s"
}

get_document_id() {
    local doc="$1"
    local doc_esc
    doc_esc=$(sql_escape "$doc")
    sqlite3 -batch -noheader "$DB_NAME" \
        "SELECT id FROM documents WHERE document_name = '$doc_esc';"
}

get_highest_revision() {
    local doc_id="$1"
    sqlite3 -batch -noheader "$DB_NAME" \
        "SELECT highest_revision FROM documents WHERE id = $doc_id;"
}

current_timestamp() {
    date +"%Y-%m-%d %H:%M:%S"
}

# --- hashing toggle -------------------------------------------------------

hash_enabled() {
    local v="${SQRS_HASH:-$SQRS_HASH_DEFAULT}"
    [[ "$v" != "0" ]]
}

compute_hash() {
    local file="$1"
    sha1sum "$file" | awk '{print $1}'
}

# --- color toggle + helpers -----------------------------------------------

color_enabled() {
    local v="${SQRS_COLOR:-$SQRS_COLOR_DEFAULT}"
    [[ "$v" != "0" ]]
}

# For file-vs-file diffs
run_diff() {
    if color_enabled && diff --color=always /dev/null /dev/null >/dev/null 2>&1; then
        diff --color=always "$@"
    else
        diff "$@"
    fi
}

# For raw diff text (stored patches from DB)
colorize_diff_stream() {
    if ! color_enabled; then
        cat
        return
    fi

    awk '
        BEGIN {
            RED_BG   = "\033[41m";  # background red for removed words
            GREEN_BG = "\033[42m";  # background green for added words
            CYAN     = "\033[36m";
            BOLD     = "\033[1m";
            RESET    = "\033[0m";

            have_old   = 0;  # pending "< old" line
            have_minus = 0;  # pending "-old" line
        }

        # If there is an unmatched old/minus line, just print it plainly
        function flush_pending(    ) {
            if (have_old) {
                print "< " old_line;
                have_old = 0;
            }
            if (have_minus) {
                print "-" minus_line;
                have_minus = 0;
            }
        }

        # Helper: get LCS length value from table L at (i,j)
        function Lget(L, i, j,    key) {
            key = i "," j;
            return (key in L ? L[key] : 0);
        }

        # Helper: set LCS length value in table L at (i,j)
        function Lset(L, i, j, val,    key) {
            key = i "," j;
            L[key] = val;
        }

        # Word-level intraline diff using LCS over words.
        # Produces globals old_out / new_out.
        function intraline(old, new,
                           n1, n2, a, b, i, j,
                           L, keep_old, keep_new,
                           key, val, max, sep,
                           old_out_local, new_out_local,
                           w1, w2) {

            # Split into words
            n1 = split(old, a, /[ \t]+/);
            n2 = split(new, b, /[ \t]+/);

            # Build LCS length table L[i,j] using DP
            # i = 0..n1, j = 0..n2
            for (i = 0; i <= n1; i++) {
                Lset(L, i, 0, 0);
            }
            for (j = 0; j <= n2; j++) {
                Lset(L, 0, j, 0);
            }

            for (i = 1; i <= n1; i++) {
                for (j = 1; j <= n2; j++) {
                    if (a[i] == b[j]) {
                        val = Lget(L, i-1, j-1) + 1;
                    } else {
                        # max of top / left
                        val = Lget(L, i-1, j);
                        if (Lget(L, i, j-1) > val) {
                            val = Lget(L, i, j-1);
                        }
                    }
                    Lset(L, i, j, val);
                }
            }

            # Backtrack to find which words are in the LCS
            delete keep_old;
            delete keep_new;

            i = n1;
            j = n2;
            while (i > 0 && j > 0) {
                if (a[i] == b[j]) {
                    keep_old[i] = 1;
                    keep_new[j] = 1;
                    i--; j--;
                } else if (Lget(L, i-1, j) >= Lget(L, i, j-1)) {
                    i--;
                } else {
                    j--;
                }
            }

            # Build output lines with background color on changed words
            old_out_local = "";
            new_out_local = "";

            max = (n1 > n2 ? n1 : n2);

            for (i = 1; i <= max; i++) {
                sep = (i == 1 ? "" : " ");

                w1 = (i <= n1 ? a[i] : "");
                w2 = (i <= n2 ? b[i] : "");

                if (w1 == "" && w2 == "") {
                    continue;
                }

                if (w1 != "") {
                    if (keep_old[i]) {
                        old_out_local = old_out_local sep w1;
                    } else {
                        old_out_local = old_out_local sep RED_BG w1 RESET;
                    }
                }

                if (w2 != "") {
                    if (keep_new[i]) {
                        new_out_local = new_out_local sep w2;
                    } else {
                        new_out_local = new_out_local sep GREEN_BG w2 RESET;
                    }
                }
            }

            old_out = old_out_local;
            new_out = new_out_local;
        }

        {
            line = $0;

            # --- Hunk / header lines: style them but no intraline diff ---
            if (match(line, /^@@.*@@/)) {
                flush_pending();
                print BOLD CYAN line RESET;
                next;
            }
            if (match(line, /^diff /) || match(line, /^index /)) {
                flush_pending();
                print BOLD line RESET;
                next;
            }
            if (match(line, /^--- /) || match(line, /^\+\+\+ /)) {
                flush_pending();
                print BOLD line RESET;
                next;
            }

            # Plain "---" separator in -Na format: do NOT flush, just print.
            if (line == "---") {
                print BOLD CYAN line RESET;
                next;
            }

            # --- -Na style: < old / > new ---
            if (substr(line, 1, 2) == "< ") {
                flush_pending();
                old_line = substr(line, 3);   # strip "< "
                have_old = 1;
                next;
            }
            if (substr(line, 1, 2) == "> " && have_old) {
                intraline(old_line, substr(line, 3));
                print "< " old_out;
                print "> " new_out;
                have_old = 0;
                next;
            }

            # --- Unified style: -old / +new ---
            if (substr(line, 1, 1) == "-" && substr(line, 1, 3) != "---") {
                flush_pending();
                minus_line = substr(line, 2);  # store without leading "-"
                have_minus = 1;
                next;
            }
            if (substr(line, 1, 1) == "+" && have_minus) {
                intraline(minus_line, substr(line, 2));
                print "-" old_out;
                print "+" new_out;
                have_minus = 0;
                next;
            }

            # --- Context or anything else ---
            flush_pending();
            print line;
        }

        END {
            flush_pending();
        }
    '
}









# -------- DB schema --------

initialize_db() {
    [[ -n "${DB_NAME:-}" ]] || die "DB_NAME is not set in initialize_db."

    if [[ -f "$DB_NAME" ]]; then
        local has_documents
        has_documents=$(sqlite3 -batch -noheader "$DB_NAME" \
            "SELECT name FROM sqlite_master WHERE type='table' AND name='documents';")
        if [[ -n "$has_documents" ]]; then
            return 0
        fi
    fi

    sqlite3 "$DB_NAME" <<'EOF'
BEGIN;

CREATE TABLE IF NOT EXISTS documents (
    id               INTEGER PRIMARY KEY AUTOINCREMENT,
    document_name    TEXT NOT NULL UNIQUE,    -- normalized abs path
    creation_date    TEXT NOT NULL,           -- timestamp for rev 0
    highest_revision INTEGER NOT NULL,        -- max revision_id
    original_blob    TEXT NOT NULL,           -- base64 rev 0
    latest_blob      TEXT NOT NULL            -- base64 latest
);

CREATE TABLE IF NOT EXISTS revisions (
    id          INTEGER PRIMARY KEY AUTOINCREMENT,
    document_id INTEGER NOT NULL,
    revision_id INTEGER NOT NULL,             -- 0..N (0 = original metadata only)
    diff_base64 TEXT NOT NULL,                -- diff (prev→this) for rev>=1, '' for rev 0
    hash        TEXT,                         -- SHA1 or NULL (hashing disabled)
    created_at  TEXT NOT NULL,                -- timestamp of this revision
    FOREIGN KEY(document_id) REFERENCES documents(id),
    UNIQUE(document_id, revision_id)
);

CREATE TABLE IF NOT EXISTS revision_notes (
    id          INTEGER PRIMARY KEY AUTOINCREMENT,
    document_id INTEGER NOT NULL,
    revision_id INTEGER NOT NULL,
    note        TEXT NOT NULL,
    created_at  TEXT NOT NULL,
    FOREIGN KEY(document_id) REFERENCES documents(id),
    UNIQUE(document_id, revision_id)
);

COMMIT;
EOF
}

# -------- core reconstruction --------

# Reconstruct revision <rev> for document <doc_id> into <outfile>.
# rev = 0 -> original blob only
# rev >=1 -> original + diffs 1..rev
reconstruct_revision_internal() {
    local doc_id="$1"
    local rev="$2"
    local outfile="$3"

    [[ -f "$DB_NAME" ]] || die "Database '$DB_NAME' not found."

    local orig_b64
    orig_b64=$(sqlite3 -batch -noheader "$DB_NAME" \
        "SELECT original_blob FROM documents WHERE id = $doc_id;")
    [[ -n "$orig_b64" ]] || die "No original blob for document_id=$doc_id"

    if [[ "$rev" -eq 0 ]]; then
        printf '%s' "$orig_b64" | base64 -d > "$outfile"
        return 0
    fi

    local tmp
    tmp=$(mktemp) || die "Failed to create temporary file."
    if ! printf '%s' "$orig_b64" | base64 -d > "$tmp"; then
        rm -f "$tmp"
        die "Failed to decode original blob for document_id=$doc_id"
    fi

    local last_rev_applied=0
    local dfile

    while IFS='|' read -r r diff_b64; do
        [[ -n "$r$diff_b64" ]] || continue

        if ! [[ "$r" =~ ^[0-9]+$ ]]; then
            rm -f "$tmp"
            die "Invalid revision_id '$r' for document_id=$doc_id"
        fi

        if (( r != last_rev_applied + 1 )); then
            rm -f "$tmp"
            die "Non-contiguous revisions for document_id=$doc_id: expected $((last_rev_applied+1)), found $r"
        fi

        dfile=$(mktemp) || { rm -f "$tmp"; die "Failed to create temporary diff file."; }

        if ! printf '%s' "$diff_b64" | base64 -d > "$dfile"; then
            rm -f "$tmp" "$dfile"
            die "Failed to decode diff for rev=$r doc_id=$doc_id"
        fi

        if ! patch "$tmp" -i "$dfile" -t >/dev/null; then
            rm -f "$tmp" "$dfile"
            die "Patch failed at revision $r for doc_id=$doc_id"
        fi

        rm -f "$dfile"
        last_rev_applied="$r"
    done < <(
        sqlite3 -batch -noheader "$DB_NAME" \
            "SELECT revision_id, diff_base64
             FROM revisions
             WHERE document_id = $doc_id
               AND revision_id >= 1
               AND revision_id <= $rev
             ORDER BY revision_id;"
    )

    if (( last_rev_applied == 0 )); then
        rm -f "$tmp"
        die "No diffs found up to rev=$rev for document_id=$doc_id"
    fi

    if (( last_rev_applied != rev )); then
        rm -f "$tmp"
        die "Incomplete diff chain: last applied=$last_rev_applied, requested=$rev for document_id=$doc_id"
    fi

    # Build complete — handle stdout specially
    if [[ "$outfile" == "-" || "$outfile" == "/dev/stdout" ]]; then
	cat "$tmp"
	rm -f "$tmp"
    else
	mv "$tmp" "$outfile"
    fi

}

reconstruct_revision_for_file() {
    local file="$1"
    local rev="$2"
    local outfile="$3"

    [[ -n "${outfile:-}" ]] || die "Internal error: reconstruct_revision_for_file called without outfile."

    local file_norm
    file_norm=$(normpath "$file")
    set_db_for_doc "$file_norm"

    [[ -f "$DB_NAME" ]] || die "DB for '$DOC_NORM' not found (expected $DB_NAME)."

    if ! [[ "$rev" =~ ^[0-9]+$ ]]; then
        die "Revision must be a non-negative integer (0 = original)."
    fi

    local doc_id
    doc_id=$(get_document_id "$DOC_NORM")
    [[ -n "$doc_id" ]] || die "Document '$DOC_NORM' not found in DB."

    local highest
    highest=$(get_highest_revision "$doc_id")
    [[ -n "$highest" ]] || highest=0

    if (( rev > highest )); then
        die "Requested rev=$rev but highest_revision=$highest for '$DOC_NORM'."
    fi

    reconstruct_revision_internal "$doc_id" "$rev" "$outfile"
}

# -------- commands --------

add_document_cmd() {
    [[ $# -eq 1 ]] || die "Usage: add-doc <file>"

    local file="$1"
    [[ -f "$file" ]] || die "Document '$file' does not exist."

    set_db_for_doc "$file"
    initialize_db

    local creation_date content_b64 hash=""
    creation_date=$(current_timestamp)
    content_b64=$(base64 < "$DOC_NORM" | tr -d '\n')

    if hash_enabled; then
        hash=$(compute_hash "$DOC_NORM")
    fi

    local doc_esc content_esc hash_esc
    doc_esc=$(sql_escape "$DOC_NORM")
    content_esc=$(sql_escape "$content_b64")
    hash_esc=$(sql_escape "$hash")

    local existing_id
    existing_id=$(get_document_id "$DOC_NORM")
    if [[ -n "$existing_id" ]]; then
        die "Document '$DOC_NORM' already in DB ($DB_NAME). Use add-rev instead."
    fi

    local hash_sql
    if hash_enabled; then
        hash_sql="'$hash_esc'"
    else
        hash_sql="NULL"
    fi

    sqlite3 "$DB_NAME" <<EOF
BEGIN;

INSERT INTO documents (document_name, creation_date, highest_revision, original_blob, latest_blob)
VALUES ('$doc_esc', '$creation_date', 0, '$content_esc', '$content_esc');

INSERT INTO revisions (document_id, revision_id, diff_base64, hash, created_at)
VALUES (
    (SELECT id FROM documents WHERE document_name = '$doc_esc'),
    0,
    '',
    $hash_sql,
    '$creation_date'
);

COMMIT;
EOF

    echo "Added '$DOC_NORM' with original stored as rev 0. DB: $DB_NAME"
}

add_revision_cmd() {
    local note=""
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -n|--note)
                shift
                [[ $# -gt 0 ]] || die "Missing note after -n"
                note="$1"
                shift
                ;;
            --)
                shift
                break
                ;;
            -*)
                die "Unknown option to add-rev: $1"
                ;;
            *)
                break
                ;;
        esac
    done

    [[ $# -eq 1 ]] || die "Usage: add-rev [-n \"note\"] <file>"
    local file="$1"
    [[ -f "$file" ]] || die "Document '$file' does not exist."

    set_db_for_doc "$file"
    [[ -f "$DB_NAME" ]] || die "DB for '$DOC_NORM' not found (expected $DB_NAME). Run add-doc first."

    local doc_id
    doc_id=$(get_document_id "$DOC_NORM")
    [[ -n "$doc_id" ]] || die "Document '$DOC_NORM' not found in DB."

    local highest new_rev
    highest=$(get_highest_revision "$doc_id")
    [[ -n "$highest" ]] || highest=0
    new_rev=$((highest + 1))

    local latest_b64
    latest_b64=$(sqlite3 -batch -noheader "$DB_NAME" \
        "SELECT latest_blob FROM documents WHERE id = $doc_id;")
    [[ -n "$latest_b64" ]] || die "No latest_blob for doc_id=$doc_id"

    local tmp_prev tmp_diff
    tmp_prev=$(mktemp)
    tmp_diff=$(mktemp)

    printf '%s' "$latest_b64" | base64 -d > "$tmp_prev"

    if ! diff -Na "$tmp_prev" "$DOC_NORM" > "$tmp_diff"; then
        :
    fi
    rm -f "$tmp_prev"

    if [[ ! -s "$tmp_diff" ]]; then
        rm -f "$tmp_diff"
        echo "No changes detected; no new revision created."
        return 0
    fi

    local diff_b64 hash="" rev_date latest_b64_new
    diff_b64=$(base64 < "$tmp_diff" | tr -d '\n')
    rm -f "$tmp_diff"

    if hash_enabled; then
        hash=$(compute_hash "$DOC_NORM")
    fi

    rev_date=$(current_timestamp)
    latest_b64_new=$(base64 < "$DOC_NORM" | tr -d '\n')

    local diff_b64_esc hash_esc note_esc latest_b64_new_esc rev_date_esc
    diff_b64_esc=$(sql_escape "$diff_b64")
    hash_esc=$(sql_escape "$hash")
    note_esc=$(sql_escape "$note")
    latest_b64_new_esc=$(sql_escape "$latest_b64_new")
    rev_date_esc=$(sql_escape "$rev_date")

    local hash_sql
    if hash_enabled; then
        hash_sql="'$hash_esc'"
    else
        hash_sql="NULL"
    fi

    sqlite3 "$DB_NAME" <<EOF
BEGIN;

INSERT INTO revisions (document_id, revision_id, diff_base64, hash, created_at)
VALUES ($doc_id, $new_rev, '$diff_b64_esc', $hash_sql, '$rev_date_esc');
$( if [[ -n "$note" ]]; then
       printf "INSERT INTO revision_notes (document_id, revision_id, note, created_at)\nVALUES (%s, %s, '%s', '%s');\n" "$doc_id" "$new_rev" "$note_esc" "$rev_date_esc"
   fi)
UPDATE documents
SET highest_revision = $new_rev,
    latest_blob      = '$latest_b64_new_esc'
WHERE id = $doc_id;

COMMIT;
EOF

    echo "Added revision $new_rev for '$DOC_NORM'. DB: $DB_NAME"
}

list_revisions_cmd() {
    [[ $# -eq 1 ]] || die "Usage: list <file>"

    local file="$1"
    set_db_for_doc "$file"
    [[ -f "$DB_NAME" ]] || die "DB for '$DOC_NORM' not found (expected $DB_NAME)."

    local doc_id
    doc_id=$(get_document_id "$DOC_NORM")
    [[ -n "$doc_id" ]] || die "Document '$DOC_NORM' not found in DB."

    echo "Revisions for '$DOC_NORM'"
    echo "DB: $DB_NAME"
    echo "rev | date                | hash                                 | note"
    echo "----+---------------------+--------------------------------------+-----------------------"

    while IFS='|' read -r rev date hash note; do
        note=${note:-}
        printf "%-3s | %-19s | %-38s | %s\n" "$rev" "${date:-""}" "${hash:-""}" "$note"
    done < <(
        sqlite3 -batch -noheader -separator '|' "$DB_NAME" "
            SELECT 0 AS revision_id,
                   d.creation_date AS created_at,
                   r0.hash,
                   '[original]' AS note
            FROM documents d
            LEFT JOIN revisions r0
              ON r0.document_id = d.id
             AND r0.revision_id = 0
            WHERE d.id = $doc_id

            UNION ALL

            SELECT r.revision_id,
                   r.created_at,
                   r.hash,
                   IFNULL(REPLACE(n.note, char(10), '\n'), '') AS note
            FROM revisions r
            LEFT JOIN revision_notes n
              ON n.document_id = r.document_id
             AND n.revision_id = r.revision_id
            WHERE r.document_id = $doc_id
              AND r.revision_id >= 1
            ORDER BY revision_id;
        "
    )
}

note_cmd() {
    [[ $# -ge 2 ]] || die "Usage: note <file> <rev> [note-text... | -]"

    local file="$1"
    local rev="$2"
    shift 2

    if ! [[ "$rev" =~ ^[0-9]+$ ]]; then
        die "Revision must be a non-negative integer."
    fi

    set_db_for_doc "$file"
    [[ -f "$DB_NAME" ]] || die "DB for '$DOC_NORM' not found (expected $DB_NAME)."

    local doc_id
    doc_id=$(get_document_id "$DOC_NORM")
    [[ -n "$doc_id" ]] || die "Document '$DOC_NORM' not found in DB."

    local highest
    highest=$(get_highest_revision("$doc_id") 2>/dev/null || echo 0)
    highest=$(get_highest_revision "$doc_id")
    [[ -n "$highest" ]] || highest=0
    if (( rev > highest )); then
        die "Requested rev=$rev but highest_revision=$highest for '$DOC_NORM'."
    fi

    if [[ $# -eq 0 ]]; then
        local note
        note=$(sqlite3 -batch -noheader "$DB_NAME" \
            "SELECT note FROM revision_notes WHERE document_id = $doc_id AND revision_id = $rev;")
        if [[ -z "$note" ]]; then
            echo "No note for '$DOC_NORM' rev $rev."
        else
            echo "Note for '$DOC_NORM' rev $rev:"
            printf '%s\n' "$note"
        fi
        return 0
    fi

    local new_note
    if [[ "$1" == "-" && $# -eq 1 ]]; then
        new_note=$(cat)
    else
        new_note="$*"
    fi

    local note_esc ts
    note_esc=$(sql_escape "$new_note")
    ts=$(current_timestamp)

    sqlite3 "$DB_NAME" <<EOF
INSERT INTO revision_notes (document_id, revision_id, note, created_at)
VALUES ($doc_id, $rev, '$note_esc', '$ts')
ON CONFLICT(document_id, revision_id)
DO UPDATE SET note = excluded.note,
              created_at = excluded.created_at;
EOF

    echo "Set note for '$DOC_NORM' rev $rev."
}

nav_state_file_for_doc() {
    local hash
    hash=$(printf '%s' "$DOC_NORM" | sha1sum | awk '{print $1}')
    printf '/tmp/.sqrs-%s' "$hash"
}

view_next_diff_cmd() {
    [[ $# -ge 1 && $# -le 2 ]] || die "Usage: view-next-diff <file> [start-rev]"

    local file="$1"
    local start_rev_arg="${2:-}"

    local file_norm
    file_norm=$(normpath "$file")
    set_db_for_doc "$file_norm"
    [[ -f "$DB_NAME" ]] || die "DB for '$DOC_NORM' not found."

    local doc_id
    doc_id=$(get_document_id "$DOC_NORM")

    local highest
    highest=$(get_highest_revision "$doc_id")
    [[ -n "$highest" ]] || highest=0

    local state_file
    state_file=$(nav_state_file_for_doc)

    local rev
    if [[ -n "$start_rev_arg" ]]; then
        rev="$start_rev_arg"
    elif [[ -f "$state_file" ]]; then
        rev=$(cat "$state_file")
    else
        rev=1
    fi

    if ! [[ "$rev" =~ ^[0-9]+$ ]] || (( rev < 1 || rev > highest )); then
        rev=1
    fi

    echo "Diff for '$DOC_NORM' rev $rev (vs rev $((rev-1))):"
    raw_diff_cmd "$DOC_NORM" "$rev"

    local next=$((rev+1))
    if (( next > highest )); then
        echo "$rev" > "$state_file"
    else
        echo "$next" > "$state_file"
    fi
}

view_prev_diff_cmd() {
    [[ $# -ge 1 && $# -le 2 ]] || die "Usage: view-prev-diff <file> [start-rev]"

    local file="$1"
    local start_rev_arg="${2:-}"

    local file_norm
    file_norm=$(normpath "$file")
    set_db_for_doc "$file_norm"
    [[ -f "$DB_NAME" ]] || die "DB for '$DOC_NORM' not found."

    local doc_id
    doc_id=$(get_document_id "$DOC_NORM")
    [[ -n "$doc_id" ]] || die "Document '$DOC_NORM' not found in DB."

    local highest
    highest=$(get_highest_revision "$doc_id")
    [[ -n "$highest" ]] || highest=0

    if (( highest < 1 )); then
        echo "No diff revisions exist for '$DOC_NORM'."
        return 0
    fi

    local state_file
    state_file=$(nav_state_file_for_doc)

    local rev
    if [[ -n "$start_rev_arg" ]]; then
        rev="$start_rev_arg"
    elif [[ -f "$state_file" ]]; then
        rev=$(cat "$state_file")
    else
        rev="$highest"
    fi

    if ! [[ "$rev" =~ ^[0-9]+$ ]] || (( rev < 1 || rev > highest )); then
        rev="$highest"
    fi

    echo "Diff for '$DOC_NORM' rev $rev (vs rev $((rev-1))):"
    raw_diff_cmd "$DOC_NORM" "$rev"

    local prev=$((rev-1))
    if (( prev < 1 )); then
        echo
        echo "(Reached first diff revision: 1; next call will also show rev 1.)"
        echo "1" > "$state_file"
    else
        echo "$prev" > "$state_file"
    fi
}

nav_show_cmd() {
    [[ $# -eq 1 ]] || die "Usage: nav <file>"

    local file="$1"
    local file_norm
    file_norm=$(normpath "$file")
    set_db_for_doc "$file_norm"

    local state_file
    state_file=$(nav_state_file_for_doc)

    if [[ ! -f "$state_file" ]]; then
        echo "No navigation state for '$DOC_NORM'."
        echo "Next 'view-next-diff' will start at rev 1."
        return 0
    fi

    local rev
    rev=$(cat "$state_file")

    echo "Navigation pointer for '$DOC_NORM': next revision = $rev"
}

nav_reset_cmd() {
    [[ $# -ge 1 && $# -le 2 ]] || die "Usage: reset-nav <file> [rev|latest]"

    local file="$1"
    local arg="${2:-}"

    local file_norm
    file_norm=$(normpath "$file")
    set_db_for_doc "$file_norm"
    [[ -f "$DB_NAME" ]] || die "DB for '$DOC_NORM' not found."

    local doc_id
    doc_id=$(get_document_id "$DOC_NORM")
    [[ -n "$doc_id" ]] || die "Document '$DOC_NORM' not found in DB."

    local highest
    highest=$(get_highest_revision "$doc_id")
    [[ -n "$highest" ]] || highest=0

    if (( highest < 1 )); then
        die "No diff revisions exist for '$DOC_NORM'."
    fi

    local target_rev
    if [[ -z "$arg" ]]; then
        target_rev=1
    elif [[ "$arg" == "latest" ]]; then
        target_rev="$highest"
    else
        if ! [[ "$arg" =~ ^[0-9]+$ ]]; then
            die "reset-nav: revision must be an integer or 'latest'."
        fi
        target_rev="$arg"
    fi

    if (( target_rev < 1 || target_rev > highest )); then
        die "reset-nav: revision $target_rev is out of range (1..$highest)."
    fi

    local state_file
    state_file=$(nav_state_file_for_doc)

    echo "$target_rev" > "$state_file"
    echo "Navigation pointer for '$DOC_NORM' reset to revision $target_rev (highest: $highest)."
}

nav_clear_all_cmd() {
    shopt -s nullglob
    local files=(/tmp/.sqrs-*)
    if (( ${#files[@]} == 0 )); then
        echo "No navigation state files found."
        return 0
    fi

    rm -f /tmp/.sqrs-* 2>/dev/null || true
    echo "All navigation state files cleared."
}



extract_cmd() {
    [[ $# -lt 2 || $# -gt 3 ]] && die "Usage: extract <file> <rev> [output-file|-]"

    local doc="$1"
    local rev="$2"
    local outfile

    if ! [[ "$rev" =~ ^[0-9]+$ ]]; then
        die "Revision must be a non-negative integer (0 = original)."
    fi

    local doc_norm
    doc_norm=$(normpath "$doc")

    if [[ $# -eq 3 ]]; then
        outfile="$3"
    else
        outfile="$doc_norm"
    fi

    # Browse-style full-document view to terminal
    if [[ "$outfile" == "-" ]]; then
        colored_extract_stdout "$doc_norm" "$rev"
        return
    fi

    local outdir
    outdir=$(dirname "$outfile")
    if [[ ! -d "$outdir" ]]; then
        mkdir -p "$outdir" || die "Failed to create output directory '$outdir'."
    fi

    reconstruct_revision_for_file "$doc_norm" "$rev" "$outfile"
    echo "Extracted rev $rev of '$doc_norm' to '$outfile'."
}




colored_extract_stdout() {
    local doc="$1"
    local rev="$2"

    # If color is off, just dump plain reconstruction.
    if ! color_enabled; then
        reconstruct_revision_for_file "$doc" "$rev" /dev/stdout
        return
    fi

    # Normalize and set DB context
    local doc_norm
    doc_norm=$(normpath "$doc")
    set_db_for_doc "$doc_norm"

    [[ -f "$DB_NAME" ]] || die "DB for '$doc_norm' not found (expected $DB_NAME)."

    local doc_id
    doc_id=$(get_document_id "$DOC_NORM")
    [[ -n "$doc_id" ]] || die "Document '$DOC_NORM' not found in DB."

    local highest
    highest=$(get_highest_revision "$doc_id")
    [[ -n "$highest" ]] || highest=0

    local have_prev=0 have_next=0 prev_rev next_rev
    if (( rev > 0 )); then
        have_prev=1
        prev_rev=$((rev - 1))
    fi
    if (( rev < highest )); then
        have_next=1
        next_rev=$((rev + 1))
    fi

    # If no neighbours at all, just print plain.
    if (( have_prev == 0 && have_next == 0 )); then
        reconstruct_revision_for_file "$doc_norm" "$rev" /dev/stdout
        return
    fi

    local tmp_cur tmp_prev tmp_next tmp_green tmp_red_map
    tmp_cur=$(mktemp) || die "Failed to create temporary file."

    reconstruct_revision_for_file "$doc_norm" "$rev" "$tmp_cur"

    # ------------------------------------------------------------------
    # 1) GREEN view: rev vs rev-1  (new in this revision)
    # ------------------------------------------------------------------
    if (( have_prev == 1 )); then
        tmp_prev=$(mktemp) || { rm -f "$tmp_cur"; die "Failed to create temporary file."; }
        reconstruct_revision_for_file "$doc_norm" "$prev_rev" "$tmp_prev"

        tmp_green=$(mktemp) || { rm -f "$tmp_cur" "$tmp_prev"; die "Failed to create temporary file."; }

        # Use `if ! ...; then :; fi` so set -e + pipefail don’t kill us.
        if ! diff -u "$tmp_prev" "$tmp_cur" | colorize_diff_stream \
            | awk '
                /^diff /  { next }
                /^index / { next }
                /^@@ /    { next }
                /^--- /   { next }
                /^\+\+\+ /{ next }
                /^\\ No newline at end of file/ { next }

                {
                    first = substr($0,1,1)
                    rest  = substr($0,2)
                    if (first == " " || first == "+") {
                        print rest
                    }
                    # "-" lines = from prev only → drop
                }
            ' > "$tmp_green"
        then
            :
        fi
    else
        # No previous revision: green view is just the plain current revision
        tmp_green="$tmp_cur"
    fi

    # ------------------------------------------------------------------
    # 2) RED map: rev vs rev+1 (text that disappears in next revision)
    # ------------------------------------------------------------------
    if (( have_next == 1 )); then
        tmp_next=$(mktemp) || {
            [[ "$tmp_green" != "$tmp_cur" ]] && rm -f "$tmp_green"
            rm -f "$tmp_cur" "$tmp_prev"
            die "Failed to create temporary file."
        }
        reconstruct_revision_for_file "$doc_norm" "$next_rev" "$tmp_next"

        tmp_red_map=$(mktemp) || {
            [[ "$tmp_green" != "$tmp_cur" ]] && rm -f "$tmp_green"
            rm -f "$tmp_cur" "$tmp_prev" "$tmp_next"
            die "Failed to create temporary file."
        }

        if ! diff -u "$tmp_cur" "$tmp_next" | colorize_diff_stream \
            | awk '
                /^diff /  { next }
                /^index / { next }
                /^@@ /    { next }
                /^--- /   { next }
                /^\+\+\+ /{ next }
                /^\\ No newline at end of file/ { next }

                {
                    first = substr($0,1,1)
                    rest  = substr($0,2)
                    if (first == "-") {
                        # rest is the current-rev line, with red intraline from colorize_diff_stream
                        plain = rest
                        gsub(/\033\[[0-9;]*m/, "", plain)   # strip ANSI to get key
                        printf "%s\t%s\n", plain, rest       # plain\tcolored-red
                    }
                    # "+" lines exist only in next → irrelevant for rev
                }
            ' > "$tmp_red_map"
        then
            :
        fi
    fi

    # ------------------------------------------------------------------
    # 3) Merge RED onto GREEN view and print
    # ------------------------------------------------------------------
    if (( have_next == 1 )); then
        # First file: red map (plain -> colored-red)
        # Second file: green view (plain or green-colored)
        awk -F'\t' '
            NR == FNR {
                red[$1] = $2
                next
            }
            {
                orig = $0
                key  = orig
                gsub(/\033\[[0-9;]*m/, "", key)   # strip ANSI for lookup
                if (key in red) {
                    print red[key]       # to-be-deleted → red
                } else {
                    print orig           # else keep (possibly green) as-is
                }
            }
        ' "$tmp_red_map" "$tmp_green"
    else
        # Last revision: only green vs previous
        cat "$tmp_green"
    fi

    # Cleanup
    [[ "${tmp_green:-}" != "$tmp_cur" ]] && rm -f "${tmp_green:-}"
    rm -f "${tmp_cur:-}" "${tmp_prev:-}" "${tmp_next:-}" "${tmp_red_map:-}"
}




rollback_cmd() {
    [[ $# -eq 2 ]] || die "Usage: rollback <file> <rev>"

    local doc="$1"
    local rev="$2"

    if ! [[ "$rev" =~ ^[0-9]+$ ]]; then
        die "Revision must be a non-negative integer (0 = original)."
    fi

    local doc_norm
    doc_norm=$(normpath "$doc")

    local tmp
    tmp=$(mktemp) || die "Failed to create temporary file for rollback."

    local cleanup
    cleanup() {
        rm -f "$tmp"
    }
    trap cleanup RETURN

    reconstruct_revision_for_file "$doc_norm" "$rev" "$tmp"

    local target_dir
    target_dir=$(dirname "$doc_norm")
    if [[ ! -d "$target_dir" ]]; then
        mkdir -p "$target_dir" || die "Failed to create directory '$target_dir' for rollback target."
    fi

    mv "$tmp" "$doc_norm"
    echo "Rolled back '$doc_norm' to rev $rev."
}

raw_diff_cmd() {
    [[ $# -eq 2 ]] || die "Usage: raw-diff <file> <rev>"

    local file="$1"
    local rev="$2"

    if ! [[ "$rev" =~ ^[1-9][0-9]*$ ]]; then
        die "Revision must be an integer >= 1 (rev 0 has no stored diff)."
    fi

    local file_norm
    file_norm=$(normpath "$file")
    set_db_for_doc "$file_norm"

    [[ -f "$DB_NAME" ]] || die "DB for '$DOC_NORM' not found (expected $DB_NAME)."

    local doc_id
    doc_id=$(get_document_id "$DOC_NORM")
    [[ -n "$doc_id" ]] || die "Document '$DOC_NORM' not found in DB."

    local diff_b64
    diff_b64=$(sqlite3 -batch -noheader "$DB_NAME" \
        "SELECT diff_base64 FROM revisions WHERE document_id = $doc_id AND revision_id = $rev;")

    [[ -n "$diff_b64" ]] || die "No stored diff for revision $rev of '$DOC_NORM'."

    # Decode and optionally colorize
    printf '%s' "$diff_b64" | base64 -d | colorize_diff_stream
}

view_diff_cmd() {
    [[ $# -eq 2 ]] || die "Usage: view-diff <file> <rev>"

    local file="$1"
    local rev="$2"

    if ! [[ "$rev" =~ ^[0-9]+$ ]] || (( rev < 0 )); then
        die "Revision must be an integer (0 = original, >=1 has diff)."
    fi

    local file_norm
    file_norm=$(normpath "$file")
    set_db_for_doc "$file_norm"
    [[ -f "$DB_NAME" ]] || die "DB for '$DOC_NORM' not found (expected $DB_NAME)."

    local doc_id
    doc_id=$(get_document_id "$DOC_NORM")
    [[ -n "$doc_id" ]] || die "Document '$DOC_NORM' not found in DB."

    local highest
    highest=$(get_highest_revision "$doc_id")
    [[ -n "$highest" ]] || highest=0

    if (( rev > highest )); then
        die "Requested rev=$rev but highest_revision=$highest for '$DOC_NORM'."
    fi

    local created_at hash note

    if (( rev == 0 )); then
        created_at=$(sqlite3 -batch -noheader "$DB_NAME" \
            "SELECT creation_date FROM documents WHERE id = $doc_id;")
        hash=$(sqlite3 -batch -noheader "$DB_NAME" \
            "SELECT hash FROM revisions WHERE document_id = $doc_id AND revision_id = 0;")
        note="[original]"
    else
        created_at=$(sqlite3 -batch -noheader "$DB_NAME" \
            "SELECT created_at FROM revisions WHERE document_id = $doc_id AND revision_id = $rev;")
        hash=$(sqlite3 -batch -noheader "$DB_NAME" \
            "SELECT hash FROM revisions WHERE document_id = $doc_id AND revision_id = $rev;")
        note=$(sqlite3 -batch -noheader "$DB_NAME" \
            "SELECT note FROM revision_notes WHERE document_id = $doc_id AND revision_id = $rev;")
    fi

    echo "=== $DOC_NORM ==="
    echo "Revision : $rev / $highest"
    echo "Date     : ${created_at:-"(unknown)"}"
    if [[ -n "$hash" ]]; then
        echo "Hash     : $hash"
    else
        echo "Hash     : (none stored)"
    fi
    if [[ -n "$note" ]]; then
        echo "Note     :"
        printf '%s\n' "$note"
    else
        echo "Note     : (none)"
    fi
    echo "----------------------------------------"

    if (( rev == 0 )); then
        echo "(No stored diff for rev 0; this is the original content.)"
    else
        echo "Diff (rev $rev vs rev $((rev - 1))):"
        raw_diff_cmd "$DOC_NORM" "$rev"
    fi
}

browse_cmd() {
    [[ $# -ge 1 && $# -le 2 ]] || die "Usage: browse <file> [start-rev|latest]"

    local file="$1"
    local start_arg="${2:-}"

    local file_norm
    file_norm=$(normpath "$file")
    set_db_for_doc "$file_norm"
    [[ -f "$DB_NAME" ]] || die "DB for '$DOC_NORM' not found (expected $DB_NAME)."

    local doc_id
    doc_id=$(get_document_id "$DOC_NORM")
    [[ -n "$doc_id" ]] || die "Document '$DOC_NORM' not found in DB."

    local highest
    highest=$(get_highest_revision "$doc_id")
    [[ -n "$highest" ]] || highest=0

    if (( highest < 1 )); then
        echo "No diff revisions (rev >= 1) exist for '$DOC_NORM'."
        return 0
    fi

    local state_file
    state_file=$(nav_state_file_for_doc)

    local rev
    if [[ -n "$start_arg" ]]; then
        if [[ "$start_arg" == "latest" ]]; then
            rev="$highest"
        elif [[ "$start_arg" =~ ^[0-9]+$ ]]; then
            rev="$start_arg"
        else
            die "browse: start-rev must be an integer or 'latest'."
        fi
    elif [[ -f "$state_file" ]]; then
        rev=$(cat "$state_file")
    else
        rev=1
    fi

    if ! [[ "$rev" =~ ^[0-9]+$ ]] || (( rev < 1 || rev > highest )); then
        rev=1
    fi

    while :; do
        if command_exists clear; then
            clear
        else
            printf '\033c' 2>/dev/null || printf '\n\n'
        fi

        view_diff_cmd "$DOC_NORM" "$rev"

        echo
        echo "[n]ext  [p]rev  [g]oto  [q]uit"
        printf "Command: "

        local key
        IFS= read -r -n 1 key || break
        echo

        case "$key" in
            n|N)
                if (( rev < highest )); then
                    ((rev++))
                else
                    echo "(Already at last revision: $rev)"
                    sleep 0.7
                fi
                ;;
            p|P)
                if (( rev > 1 )); then
                    ((rev--))
                else
                    echo "(Already at first revision: 1)"
                    sleep 0.7
                fi
                ;;
            g|G)
                printf "Go to revision (1..%d): " "$highest"
                local line
                read -r line || true
                if [[ "$line" =~ ^[0-9]+$ ]] && (( line >= 1 && line <= highest )); then
                    rev="$line"
                else
                    echo "Invalid revision."
                    sleep 0.7
                fi
                ;;
            q|Q)
                break
                ;;
            *)
                ;;
        esac

        echo "$rev" > "$state_file"
    done
}

rename_cmd() {
    [[ $# -eq 2 ]] || die "Usage: rename <old-file> <new-file>"

    local old="$1" new="$2"

    local old_norm new_norm
    old_norm=$(normpath "$old")
    new_norm=$(normpath "$new")

    if [[ "$old_norm" == "$new_norm" ]]; then
        echo "Nothing to do: '$old_norm' and '$new_norm' are the same path."
        return 0
    fi

    local new_db="${new_norm}.db"
    if [[ -e "$new_norm" ]]; then
        die "Target file '$new_norm' already exists."
    fi
    if [[ -e "$new_db" ]]; then
        die "Target DB '$new_db' already exists."
    fi

    set_db_for_doc "$old_norm"
    [[ -f "$DB_NAME" ]] || die "DB for '$old_norm' not found (expected $DB_NAME)."
    local old_db="$DB_NAME"

    local doc_id
    doc_id=$(get_document_id "$old_norm")
    [[ -n "$doc_id" ]] || die "Document '$old_norm' not found in DB."

    if [[ -f "$old_norm" ]]; then
        mkdir -p "$(dirname "$new_norm")"
        mv "$old_norm" "$new_norm"
    else
        echo "Warning: original file '$old_norm' not found; only renaming DB entry." >&2
    fi

    local new_esc
    new_esc=$(sql_escape "$new_norm")
    sqlite3 "$DB_NAME" \
        "UPDATE documents SET document_name = '$new_esc' WHERE id = $doc_id;"

    mkdir -p "$(dirname "$new_db")"
    mv "$old_db" "$new_db"

    echo "Renamed doc '$old_norm' → '$new_norm' and DB '$old_db' → '$new_db'."
}

missing_cmd() {
    [[ $# -eq 1 ]] || die "Usage: missing <file>"

    local file="$1"
    set_db_for_doc "$file"
    [[ -f "$DB_NAME" ]] || die "DB for '$DOC_NORM' not found (expected $DB_NAME)."

    local doc_id
    doc_id=$(get_document_id "$DOC_NORM")
    [[ -n "$doc_id" ]] || die "Document '$DOC_NORM' not found in DB."

    local highest
    highest=$(get_highest_revision "$doc_id")
    [[ -n "$highest" ]] || highest=0

    if (( highest == 0 )); then
        echo "Only original (rev 0) exists for '$DOC_NORM'. No diff revisions."
        return 0
    fi

    echo "Checking missing revisions for '$DOC_NORM' (1..$highest):"

    local missing
    missing=$(sqlite3 -batch -noheader "$DB_NAME" "
        WITH RECURSIVE seq(r) AS (
            SELECT 1
            UNION ALL
            SELECT r + 1 FROM seq WHERE r < $highest
        )
        SELECT r
        FROM seq
        WHERE r NOT IN (
            SELECT revision_id
            FROM revisions
            WHERE document_id = $doc_id
        )
        ORDER BY r;
    ")

    if [[ -z "$missing" ]]; then
        echo "No missing revisions in 1..$highest."
        return 0
    fi

    while IFS= read -r r; do
        [[ -n "$r" ]] || continue
        echo "Missing revision: $r"
    done <<< "$missing"
}

verify_cmd() {
    [[ $# -eq 2 ]] || die "Usage: verify <file> <rev>"

    local doc="$1" rev="$2"

    if ! [[ "$rev" =~ ^[0-9]+$ ]]; then
        die "Revision must be a non-negative integer (0 = original)."
    fi

    set_db_for_doc "$doc"
    [[ -f "$DB_NAME" ]] || die "DB for '$DOC_NORM' not found (expected $DB_NAME)."

    local doc_id
    doc_id=$(get_document_id "$DOC_NORM")
    [[ -n "$doc_id" ]] || die "Document '$DOC_NORM' not found in DB."

    local highest
    highest=$(get_highest_revision "$doc_id")
    [[ -n "$highest" ]] || highest=0

    if (( rev > highest )); then
        die "Requested rev=$rev but highest_revision=$highest for '$DOC_NORM'."
    fi

    local stored_hash
    stored_hash=$(sqlite3 -batch -noheader "$DB_NAME" \
        "SELECT hash FROM revisions WHERE document_id = $doc_id AND revision_id = $rev;")

    if [[ -z "$stored_hash" ]]; then
        echo "No stored hash for rev $rev of '$DOC_NORM' (hashing may have been disabled)." >&2
        exit 1
    fi

    local tmp
    tmp=$(mktemp) || die "Failed to create temporary file for verification."
    if ! reconstruct_revision_internal "$doc_id" "$rev" "$tmp"; then
        rm -f "$tmp"
        die "Failed to reconstruct revision $rev for '$DOC_NORM' during verification."
    fi

    local current_hash
    current_hash=$(sha1sum "$tmp" | awk '{print $1}')
    rm -f "$tmp"

    if [[ "$stored_hash" == "$current_hash" ]]; then
        echo "Revision $rev hash verified for '$DOC_NORM'."
    else
        echo "Hash mismatch for rev $rev of '$DOC_NORM'." >&2
        echo "Stored:   $stored_hash" >&2
        echo "Computed: $current_hash" >&2
        exit 1
    fi
}

diff_cmd() {
    [[ $# -eq 3 ]] || die "Usage: diff <file> <rev1> <rev2>"

    local doc="$1" rev1="$2" rev2="$3"

    if ! [[ "$rev1" =~ ^[0-9]+$ && "$rev2" =~ ^[0-9]+$ ]]; then
        die "Revisions must be non-negative integers (0 = original allowed)."
    fi

    local tmp1 tmp2
    tmp1=$(mktemp) || die "Failed to create temporary file for rev1."
    tmp2=$(mktemp) || { rm -f "$tmp1"; die "Failed to create temporary file for rev2."; }

    local cleanup
    cleanup() {
        rm -f "$tmp1" "$tmp2"
    }
    trap cleanup RETURN

    reconstruct_revision_for_file "$doc" "$rev1" "$tmp1"
    reconstruct_revision_for_file "$doc" "$rev2" "$tmp2"

    local doc_norm
    doc_norm=$(normpath "$doc")

    echo "Diff between rev $rev1 and rev $rev2 of '$doc_norm':"
    run_diff -u "$tmp1" "$tmp2" || true
}

usage() {
    cat <<EOF
Usage: $0 <command> [args...]

Commands (per-document DB: <file>.db):

  add-doc <file>
      Create DB for <file> and store original as rev 0.

  add-rev [-n "note"] <file>
      Add new revision (diff from previous), update latest.

  list <file>
      List rev 0 and all diff revisions with hashes/notes.

  note <file> <rev> [text... | -]
      Show or update the note for a given revision.
      Without text, prints the current note (if any).
      With text, sets/replaces the note for that revision.
      With '-', reads multiline note from stdin.

   extract <file> <rev> [output-file]
      Extract revision rev (0 = original) to output-file
      or overwrite <file> if omitted.

  raw-diff <file> <rev>
      Output the raw stored unified diff for revision <rev>
      (diff from rev-1 -> rev). Rev must be >= 1.

  view-diff <file> <rev>
      Show metadata (hash, note) and the stored diff for a single revision.

  browse <file> [start-rev|latest]
      Interactive diff browser for <file>. Use:
        n / p  - next / previous revision
        g      - go to specific revision
        q      - quit

  view-next-diff <file> [start-rev]
      Show the diff for a revision and advance a per-file cursor.
      With start-rev, starts at that revision; otherwise uses the last
      position or rev 1.

  view-prev-diff <file> [start-rev]
      Show the diff for a revision and move the cursor backwards.
      With start-rev, starts at that revision; otherwise uses the last
      position or the highest revision.

  nav <file>
      Show current next/previous diff pointer for <file>.

  reset-nav <file> [rev|latest]
      Reset navigation pointer for <file>.
      With no rev, sets pointer to 1.
      With 'latest', sets pointer to the highest revision.
      With a number, sets pointer to that revision (must be in range).

  clear-all-nav
      Remove all diff navigation pointers stored in /tmp.

  rollback <file> <rev>
      Overwrite <file> with the content of revision rev.

  rename <old-file> <new-file>
      Update doc name in DB and move <old-file>.db → <new-file>.db.

  missing <file>
      Show missing revision numbers between 1..highest_revision.

  verify <file> <rev>
      Reconstruct revision and verify SHA1 vs stored hash.
      If no hash was stored (hashing disabled), exits with an error.

  diff <file> <rev1> <rev2>
      Unified diff between two revisions (0 = original allowed).

Environment:

  SQRS_HASH=0    Disable hashing (hash column is NULL).
  SQRS_HASH=1    Enable hashing.

  SQRS_COLOR=0   Disable colored diff output.
  SQRS_COLOR=1   Enable colored diff output.

Defaults (can be edited at the top of this script):

  SQRS_HASH_DEFAULT=$SQRS_HASH_DEFAULT
  SQRS_COLOR_DEFAULT=$SQRS_COLOR_DEFAULT

EOF
}

# -------- main dispatch --------

if [[ $# -lt 1 ]]; then
    usage
    exit 1
fi

cmd="$1"
shift

ensure_deps

case "$cmd" in
    add-doc)        add_document_cmd "$@" ;;
    add-rev)        add_revision_cmd "$@" ;;
    list)           list_revisions_cmd "$@" ;;
    extract)        extract_cmd "$@" ;;
    rollback)       rollback_cmd "$@" ;;
    rename)         rename_cmd "$@" ;;
    missing)        missing_cmd "$@" ;;
    verify)         verify_cmd "$@" ;;
    diff)           diff_cmd "$@" ;;
    raw-diff)       raw_diff_cmd "$@" ;;
    note)           note_cmd "$@" ;;
    nav)            nav_show_cmd "$@" ;;
    reset-nav)      nav_reset_cmd "$@" ;;
    clear-all-nav)  nav_clear_all_cmd "$@" ;;
    view-next-diff) view_next_diff_cmd "$@" ;;
    view-prev-diff) view_prev_diff_cmd "$@" ;;
    view-diff)      view_diff_cmd "$@" ;;
    browse)         browse_cmd "$@" ;;
    -h|--help|help) usage ;;
    *)              die "Unknown command: $cmd" ;;
esac

