#!/bin/bash

#    This script allows viewing updating or creating a 7z-revisions.el archive without the
#    need for emacs, useful if you're in a terminal that lacks screen, emacs, or emacsclient.  

# Usages:
#    $0 [ -n NOTE_MESSAGE ] [ -d DIRECTORY_OF_ARCHIVE ] TARGET_DOCUMENT
#    $0 [ -L ] [ -n NOTE_MESSAGE ] [ -c DIRECTORY_OF_DOCUMENT ] -a TARGET_ARCHIVE 
#    $0 [ -d DIRECTORY_OF_ARCHIVE ] -k  RENAMED_NEW_DOCUMENT_AND_ARCHIVE_NAME TARGET_DOCUMENT
#    $0 -e REVISION_DIFF_TO_VIEW [ -c DIRECTORY_OF_DOCUMENT ] [ -a TARGET_ARCHIVE |  TARGET_DOCUMENT ] 
#    $0 -E REVISION_TO_ROLLBACK_TO [ -c DIRECTORY_OF_DOCUMENT ] [ -a TARGET_ARCHIVE |  TARGET_DOCUMENT ] 
#    Flags:
#    -d Use this option to designate the directory for the archive. If not provided, the script will assume the archive is in the same directory as the target document. If you provide an archive file path, that archive will be the focus of the operation.
#    -L Extract the latest version of a document to a directory (-c to specify directory).  The default path found in signature file is used if the -c flag is omitted.
#    -n Add a note in the notes file during update.
#    -a Specify a particular archive to update, useful when the document linked to the archive is located somewhere else.
#    -k Renames document and archive
#    -z Alter document path, as shown in archive.
#    -l list all revision numbers in archive
#    -e view a specific diff of a revision via standard output
#    -E extract a specific revision, overwriting target document with it

# Requirements: dos2unix

# Author/maintainer: ciscorx@gmail.com
# set -x
SIGNATURE_FILE='7z-revisions.el created this archive'  # please dont put a timestamp in this string or bad things may happen
VERSION=3.9
HASH_FILE=""
HASH_FILE_PRE="7zr-sha1-"
NOTES_FILE=""
NOTES_FILE_PRE="7zr-notes-"
NOTES_FILE_POST=".7zrn"
TARGET_FILE=""
TARGET_PATH=""
ARCHIVE=""
ARCHIVE_DIR=""
ARCHIVE_CREATED_DATETIME=""
DOCUMENT=""
DOCUMENT_DIR=""
NEW_NAME=""
NEW_DOCUMENT_DIR=""
LATEST_DOC_PRE="7zr-latest-"
TARGET_HASH=""
NOTE=""
REV=""
END_OF_LINE_ENCODING=
BUFFER_FILE_ENCODING_SYSTEM=
A_FLAG_INVOKED=false
L_FLAG_INVOKED=false
L_UPPERCASE_FLAG_INVOKED=false
E_FLAG_INVOKED=false
E_UPPERCASE_FLAG_INVOKED=false
Z_FLAG_INVOKED=false
K_FLAG_INVOKED=false
U_FLAG_INVOKED=false
J_FLAG_INVOKED=false
U_UPPERCASE_FLAG_INVOKED=false
J_UPPERCASE_FLAG_INVOKED=false
X_UPPERCASE_FLAG_INVOKED=false
M_FLAG_INVOKED=false

ORIGINAL_VERSION=
TIMESTAMP=`date +%Y-%m-%d_%H%M%S`
TEXT_END_BOUNDS=7777    # all tags in text must reside with in the first 7777 characters of the beginning of the document in order to be seen
UPDATE_TAGS_IN_TEXTP=t  # allow tags to be updated in text

declare -ga TMPDIR_PER_DOC_order_list
typeset -ag IDX2REV
typeset -Ag REV2IDX
declare -gA VALUES_IN_TEXT

declare -ga KEYS_OF_TAGS_TO_AUTO_REPLACE
KEYS_OF_TAGS_TO_AUTO_REPLACE=("rev" "last_hash" "latest_datetime"   "original_version" )

declare -gA HASHFILE
declare -gA TAGS_IN_TEXT=(
    ["rev"]="7z-revisions.el_rev="
    ["original_version"]="7z-revisions.el_original-version="
    ["directory_of_document"]="7z-revisions.el_directory-of-document="
    ["directory_of_archive"]="7z-revisions.el_directory-of-archive="
    ["archive_prefix"]="7z-revisions.el_archive-prefix="
    ["archive_extension"]="7z-revisions.el_archive-extension="
    ["track_hashes"]="7z-revisions.el_track-sha1sum-hashes="
    ["last_hash"]="7z-revisions.el_sha1-of-last-revision="
    ["latest_datetime"]="7z-revisions.el_latest-datetime="
)
declare -gA VALUES_IN_METAFILE
declare -gA TAGS_IN_METAFILE=(
    ["rev"]="latest_revision="
    ["original_version"]="original-version="
    ["document_name"]="document_name="
    ["directory_of_document"]="document-directory="
    ["archive_prefix"]="archive-prefix="
    ["archive_extension"]="archive-extension="
    ["archive_created_datetime"]="archive-created_datetime="
    ["directory_of_document"]="directory-of-document="
    ["directory_of_archive"]="directory-of-archive="
    ["track_hashes"]="track-sha1sum-hashes="
    ["last_hash"]="sha1sum-of-last-revision="
    ["latest_datetime"]="latest-datetime="
    ["sha1_of_last_revision"]="sha1-of-last-revision="
    ["os_type"]="created_on_os_system-type="
    ["buffer_file_coding_system"]="buffer-file-coding-system="
    ["end_of_line_encoding"]="end-of-line-encoding="
    ["editor_version"]="7z-revision-version="
)

# Beware: the following TMP_DIR directory will be deleted at the end of the program
BASE_TEMP=/tmp/a/
USR_TMP_DIR="${BASE_TEMP}$(basename $0 '.sh')${USER}/"
mkdir -p "${USR_TMP_DIR}"
TMP_DIR=`mktemp -d -p ${BASE_TEMP}`/
TMPDIR_PER_DOC=''

SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
echo $$ > /tmp/killthis

function usage() {                                 # Function: Print a help message.
  echo "This script will update or create a 7z-revisions.el archive without the use of emacs.  If the -d directory_of_archive option is omitted, then the directory of the target document will be assumed.  If an actual archive file is passed as -d then that archive will be used for the archive.  Use the -a flag to update the document associated with the archive in question.  Use the -L flag to extract a copy of the latest revision from the archive. Use -k for renaming archive and document, -z to alter documents path, -l to list all revision numbers in archive.  -e to view a diff of a specific revision via standard output, -E to extract a revision, overwriting target document."
  echo "Usage: $0 [ -n NOTE_MESSAGE ] [ -d DIRECTORY_OF_ARCHIVE ] TARGET_DOCUMENT" 1>&2 

  echo "Usage: $0 [ -L ] [ -n NOTE_MESSAGE ] [ -c DIRECTORY_OF_DOCUMENT ] -a TARGET_ARCHIVE" 1>&2 

  echo "Usage: $0 -k NEW_DOCUMENT_AND_ARCHIVE_NAME -a TARGET_ARCHIVE" 1>&2

  echo "Usage: $0 -e REVISION_DIFF_TO_VIEW [ -a TARGET_ARCHIVE | TARGET_DOCUMENT ]" 1>&2

  echo "Usage: $0 -E REVISION_TO_ROLLBACK_TO [ -a TARGET_ARCHIVE | TARGET_DOCUMENT ]" 1>&2
}
function exit_abnormal() {                         # Function: Exit with error.
  usage

# rm  -rf "${TMP_DIR}"
  exit 1
}

function exit_normal() {
    
# rm  -rf "${TMP_DIR}"
exit 0
}

function set_TMPDIR_PER_DOC() {

    TMPDIR_PER_DOC="${USR_TMP_DIR}${DOCUMENT}_diffs/"
    }

function strip_extension() {  
    local filename="$1"
    base="${filename%.*}"
    echo "$base"
}

function get_extension() {
    local filename="$1"
    local extension="${filename##*.}"
    echo "$extension"
}

function strip_timestamp() {
    string="$1"
    echo "${string%[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]_[0-9][0-9][0-9][0-9][0-9][0-9]}"

}

function set_END_OF_LINE_ENCODING() {

    declare -g END_OF_LINE_ENCODING
    if [ -z `dos2unix -ic "${TARGET_FILE}"` ]; then
	END_OF_LINE_ENCODING=unix
    else
	END_OF_LINE_ENCODING=windows
    fi
}

function valid_filename() {
    local filename="$1"

    # check for reserved names, case-insensitive
    if [[ "${filename,,}" =~ ^(con|prn|aux|nul|com[1-9]|lpt[1-9])$ ]]; then
        echo "Invalid: filename is a reserved name"
        return 1
    fi

    if [[ "$filename" =~ [\ \/:*?\"\<\>\|] ]]; then
	echo "Invalid: filename contains forbidden characters"
	return 1
    fi

    # check for trailing space or period
    if [[ "$filename" =~ [[:space:].]$ ]]; then
        echo "Invalid: filename ends with a space or a period"
        return 1
    fi

    # check for extension
    if ! [[ "$filename" =~ \..+$ ]]; then
        echo "Invalid: filename does not contain an extension"
        return 1
    fi

    echo "Valid filename"
    return 0
}

function get_note() {
    local rev="$1"
    local filename="$2"
    awk -F'"' "/\"${rev}\"/ {line=\$4} END{print line}" "${filename}"
}

function put_note() {
    local rev="$1"
    local hash="$2"
    local tab="$3"
    local filename="$4"
    echo "(puthash \"${rev}\" \"${hash}\" ${tab})" >> "${filename}"
}

function print_missing_revs() {

    set_TMPDIR_PER_DOC
    create_TMPDIR_PER_DOC_order_list 
    echo 'The following revision numbers, which are not present, are consecutive to revisions that are present:'
    awk '
BEGIN {
    prev = -1; # set to a value which is not expected as the first number
}
{
    curr = int($1); # convert to integer

    if(prev != -1 && curr - prev > 1) {
        for(i=prev+1; i<curr; i++) {
            print i;
        }
    }
    
    prev = curr; # update previous number
}
' <(printf "%s\n" "${TMPDIR_PER_DOC_order_list[@]}")
    exit 0
}

function rename_file_and_archive() {
    
    local new_name="$1"
    local new_name_sans_extension=$(strip_extension "${new_name}") 
    local new_extension=$(get_extension "${new_name}")
    local old_name="${DOCUMENT}"
    local old_timestamp
    local notes_name_change_msg="Filename changed from ${old_name} to ${new_name} on ${TIMESTAMP}  :"
    local notes_tab='7zr-notestab'
    local hash_tab='7zr-hasht'
    local hash_name_change_msg=""
    get_signature_file
    get_document_name
    local latest_rev=$(get_latest_rev)
    get_hash_file
    get_notes_file
    local old_note=$(get_note "${latest_rev}" "${TMP_DIR}${NOTES_FILE}")
    local new_note="${old_note}\n${notes_name_change_msg}"
    put_note "${latest_rev}" "${new_note}" "${notes_tab}" "${TMP_DIR}${NOTES_FILE}" 
    # old_timestamp=$(strip_extension "${ORIGINAL_VERSION}")
    # old_timestamp=$(strip_timestamp "${old_timestamp}")
    local extension=$(get_extension "${ORIGINAL_VERSION}")
local archive_created_datetime_tag="${TAGS_IN_METAFILE['archive_created_datetime']}"
    old_timestamp=$(awk -v pattern="${archive_created_datetime_tag}" '$0 ~ pattern {sub(".*" pattern, ""); print $0}' "${TMP_DIR}${SIGNATURE_FILE}")
    local new_original_version="${new_name_sans_extension}${old_timestamp}.${new_extension}"
    put_note "${new_original_version}" "${latest_rev}" "${hash_tab}" "${TMP_DIR}${HASH_FILE}"
    cat >> ${TMP_DIR}${HASH_FILE} <<EOF
(add-to-list '7zr-document-namechanges (list "${ORIGINAL_VERSION}" "${new_original_version}" "${TIMESTAMP}")
EOF
    local  document_name_tag=${TAGS_IN_METAFILE["document_name"]}
    sed -i "s/${document_name_tag}${old_name}/${document_name_tag}${new_name}/g" "${TMP_DIR}${SIGNATURE_FILE}"

    local original_version_tag=${TAGS_IN_METAFILE["original_version"]}
    
    sed -i "s/${original_version_tag}${ORIGINAL_VERSION}/${original_version_tag}${new_original_version}/g" "${TMP_DIR}${SIGNATURE_FILE}"


    if [ -f "${ARCHIVE_DIR}/${new_name}.7z" ]; then
	echo "error: target archive already exists."
	exit_abnormal
    fi

    if [ -f "${DOCUMENT_DIR}/${new_name}" ]; then
	echo "error: target document already exists."
	exit_abnormal
    fi
    7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on -bso0 -bsp0  "${ARCHIVE}" "${TMP_DIR}${SIGNATURE_FILE}" "${TMP_DIR}${HASH_FILE}"  "${TMP_DIR}${NOTES_FILE}"  

    7z rn -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on -bso0 -bsp0 "${ARCHIVE}"  "${NOTES_FILE}" "${NOTES_FILE_PRE}${new_original_version}${NOTES_FILE_POST}" 

    7z rn -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on -bso0 -bsp0 "${ARCHIVE}"  "${HASH_FILE}" "${HASH_FILE_PRE}${new_original_version}" 

    7z rn -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on -bso0 -bsp0 "${ARCHIVE}"  "${LATEST_DOC_PRE}${ORIGINAL_VERSION}" "${LATEST_DOC_PRE}${new_original_version}" 
    
    7z rn -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on -bso0 -bsp0 "${ARCHIVE}"  "${ORIGINAL_VERSION}" "${new_original_version}"
    mv "${ARCHIVE}" "${ARCHIVE_DIR}/${new_name}.7z"
    mv "$TARGET"  "${DOCUMENT_DIR}/${new_name}" 
    echo "document renamed"
    exit_normal
}

function populate_table() {
    
    local file=$1
    local -n tags=$2
    local -n values=$3
    local limit=$4


    # If a limit is provided, use dd to get the first N characters of the file
    if [[ "$limit" != "" ]]; then
        file_content=$(dd if="$file" bs=1 count="$limit" 2>/dev/null)
    else
        file_content=$(cat "$file")
    fi

    for key in "${!tags[@]}"
    do
        local value=$(echo "$file_content" | grep -oP "${tags[$key]}\K[\w_:.-]+")
        if [[ "$value" != "" ]]; then
            values[$key]=$value
        fi
    done

# example usage:
# Populate VALUES_IN_TEXT from textfile.txt
#populate_table "textfile.txt" TAGS_IN_TEXT VALUES_IN_TEXT 7777

# Populate VALUES_IN_METAFILE from metafile.txt
#populate_table "metafile.txt" TAGS_IN_METAFILE VALUES_IN_METAFILE
}

function replace_tags_in_text() {
    local file="${TARGET_FILE}"

# Populate VALUES_IN_METAFILE from metafile.txt
    populate_table "${TMP_DIR}${SIGNATURE_FILE}" TAGS_IN_METAFILE VALUES_IN_METAFILE

    # Read the first 7777 characters of the file into a variable
    first_chars=$(head -c ${TEXT_END_BOUNDS} "${file}")

    local file_changedp=false
    for key in "${KEYS_OF_TAGS_TO_AUTO_REPLACE[@]}"; do
	
	local replacement="${VALUES_IN_METAFILE[$key]}"
	
	
	if [[ ! -z `grep "${TAGS_IN_TEXT[$key]}" <<< "${first_chars}"` ]]; then
	    
	    first_chars=$(echo -e "$first_chars" | perl -pe 'BEGIN { $prompt = shift; $answer = shift } s|($prompt)\S*|${1}$answer|g' "${TAGS_IN_TEXT[$key]}" "${replacement}") 
	    file_changedp=true
	fi
    done

    if $file_changedp; then
    # Replace the first 7777 characters of the file with the modified string
       echo -e "${first_chars}$(tail -c +$(( TEXT_END_BOUNDS + 1 )) ${file})" > "${file}"

    fi

}

function get_signature_file() {


    7z e -bso0 -bsp0 -aoa -o"${TMP_DIR}" "${ARCHIVE}" "${SIGNATURE_FILE}"  || { echo "Error extracting ${SIGNATURE_FILE} from ${ARCHIVE}"; exit 1; }

    local original_version_tag=${TAGS_IN_METAFILE['original_version']}
    ORIGINAL_VERSION=$(awk -v pattern="${original_version_tag}" '$0 ~ pattern {sub(".*" pattern, ""); print $0}'  "${TMP_DIR}${SIGNATURE_FILE}") 
    if [ -z "${ORIGINAL_VERSION}" ]; then
	# the ORIGINAL_VERSION is going to be the filename in the archive that is both shortest in  length  and contains a timestamp 
	ORIGINAL_VERSION=`7z -ba l "${ARCHIVE}" | awk '/[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{6}\.[a-zA-Z0-9]+$/ {if (!min || length($6)<min) {min=length($6); shortest=$6}} END {print shortest}'`
    fi
    
    if [ -z "${ORIGINAL_VERSION}" ]; then
	echo "Unable to determine the original version name of the document"
	exit_abnormal
	
    fi
    
    local archive_created_datetime_tag="${TAGS_IN_METAFILE['archive_created_datetime']}"
    ARCHIVE_CREATED_DATETIME=$(awk -v pattern="$archive_created_datetime_tag" '$0 ~ pattern {sub(".*" pattern, ""); print $0}' "${TMP_DIR}${SIGNATURE_FILE}")
    HASH_FILE="${HASH_FILE_PRE}${ORIGINAL_VERSION}"
    NOTES_FILE="${NOTES_FILE_PRE}${ORIGINAL_VERSION}${NOTES_FILE_POST}"
    LATEST="7zr-latest-${ORIGINAL_VERSION}"

    }

function get_hash_file() {

    7z e -bso0 -bsp0 -aoa -o"${TMP_DIR}" "${ARCHIVE}" "${HASH_FILE}"  || { echo "Error extracting ${HASH_FILE} from ${ARCHIVE}"; exit 1; }
}

function get_notes_file() {

    7z e -bso0 -bsp0 -aoa -o"${TMP_DIR}" "${ARCHIVE}" "${NOTES_FILE}"  || { echo "Error extracting ${NOTES_FILE} from ${ARCHIVE}"; exit 1; }
}

    
function get_latest_rev() {

    if [ -z `grep latest_revision= "${TMP_DIR}${SIGNATURE_FILE}"` ];  then
	
	# theres no latest_revision line!
	LATEST_REVISION=`7z -ba l "${ARCHIVE}" | awk -F'.' 'BEGIN {max = 0} {if ($6+0 == $6 && $6>max) max=$6} END {print max}'`
	
    else
	
	LATEST_REVISION=`grep 'latest_revision=' "${TMP_DIR}${SIGNATURE_FILE}" | awk -F'=' '{print $2}'` 
	
    fi
    LATEST_REVISION="${LATEST_REVISION}.0"

    local original_version_tag=${TAGS_IN_METAFILE['original_version']}
    ORIGINAL_VERSION=$(awk -v pattern="${original_version_tag}" '$0 ~ pattern {sub(".*" pattern, ""); print $0}'  "${TMP_DIR}${SIGNATURE_FILE}") 
    if [ -z "${ORIGINAL_VERSION}" ]; then
	ORIGINAL_VERSION=`7z -ba l "${ARCHIVE}" | awk '/[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{6}\.[a-zA-Z0-9]+$/ {if (!min || length($6)<min) {min=length($6); shortest=$6}} END {print shortest}'`
    fi
    
    if [ -z "${ORIGINAL_VERSION}" ]; then
	echo "Unable to determine the original version name of the document"
	exit_abnormal
	
    fi
    HASH_FILE="${HASH_FILE_PRE}${ORIGINAL_VERSION}"
    NOTES_FILE="${NOTES_FILE_PRE}${ORIGINAL_VERSION}${NOTES_FILE_POST}"
    LATEST="7zr-latest-${ORIGINAL_VERSION}"
    
    TARGET_HASH=`sha1sum "${TARGET_FILE}"|awk '{print $1}'`

    echo "${LATEST_REVISION}"
    }


function get_document_name() {

	if [ -z "${DOCUMENT_DIR}" ]; then
	    
	    DOCUMENT_DIR=`grep directory-of-document= "${TMP_DIR}${SIGNATURE_FILE}"   | awk -F'=' '{print  $2}'`
	fi
	if [ -z "${DOCUMENT}" ]; then
	     
	    DOCUMENT=`grep document_name= "${TMP_DIR}${SIGNATURE_FILE}"   | awk -F'=' '{print  $2}'`
	fi
    }


function get_latest() {


    get_signature_file
    
    7z e -bso0 -bsp0 -aoa -o"${TMP_DIR}"  "${ARCHIVE}" "${LATEST}"
    if [ ${L_UPPERCASE_FLAG_INVOKED} = true ]; then
	if [ -z "${DOCUMENT_DIR}" ]; then
	    
	    DOCUMENT_DIR=`grep directory-of-document= "${TMP_DIR}${SIGNATURE_FILE}"   | awk -F'=' '{print  $2}'`
	fi
	if [ -z "${DOCUMENT}" ]; then
	     
	    DOCUMENT=`grep document_name= "${TMP_DIR}${SIGNATURE_FILE}"   | awk -F'=' '{print  $2}'`
	fi
	if  [ -f "${DOCUMENT_DIR}/${DOCUMENT}" ]; then
	    echo "Even though target file already exists, OVERWRITING with latest revision!"

	    cp "${TMP_DIR}${LATEST}" "${DOCUMENT_DIR}/${DOCUMENT}"
	    exit_normal
	else
	    cp "${TMP_DIR}${LATEST}" "${DOCUMENT_DIR}/${DOCUMENT}"
	    echo "File ${LATEST} extracted to ${DOCUMENT_DIR}/${DOCUMENT}"
	    exit_normal
	fi
    fi
    
}

function get_all() {
    get_signature_file
    
     
    if [ ${L_UPPERCASE_FLAG_INVOKED} = true ]; then
	if [ -z "${DOCUMENT_DIR}" ]; then
	    
	    DOCUMENT_DIR=`grep directory-of-document= "${TMP_DIR}${SIGNATURE_FILE}"   | awk -F'=' '{print  $2}'`
	fi
	if [ -z "${DOCUMENT}" ]; then
	    
	    DOCUMENT=`grep document_name= "${TMP_DIR}${SIGNATURE_FILE}"   | awk -F'=' '{print  $2}'`
	fi
    fi

    set_TMPDIR_PER_DOC

    mkdir -p "${TMPDIR_PER_DOC}" 
    7z x -bso0 -bsp0 -aoa -o"${TMPDIR_PER_DOC}"  "${ARCHIVE}" 
    chmod -R 777 "${TMPDIR_PER_DOC}"
}

    
function load_HASHFILE() {
    local rev
    local hash
    while read -r line; do
	rev=`echo $line | awk '{print $1}'`
	hash=`echo $line | awk '{print $2}'`
	HASHFILE["${rev}"]="${hash}"    
    done < <(cat "${TMPDIR_PER_DOC}${HASH_FILE_PRE}${ORIGINAL_VERSION}"  | awk '{ gsub(/"/, "", $3); gsub(/"/, "", $2); print $2 " "  $3 }')
    }
	     
function verify_hash() {
    local rev="$1"
    local hash_performed=`sha1sum ${TMPDIR_PER_DOC}wip | awk '{print $1}'`
    # local fetched_hash=`awk -v r="$rev" '$2 == "\"" r "\"" { gsub(/"/, "", $3); print $3 }' "${TMPDIR_PER_DOC}${HASH_FILE_PRE}${ORIGINAL_VERSION}"`
    local fetched_hash="${HASHFILE[$rev]}"
    if [[ ${fetched_hash} == '' ]]; then
	echo "rev not found"
    elif [[ ${hash_performed} == '' ]]; then
	echo "wip file not found"
    elif [[ ${hash_performed} == ${fetched_hash} ]]; then
	echo "verified"
    else
	echo "mismatched"
    fi
    }

function create_TMPDIR_PER_DOC_order_list() {

    mkdir -p "${TMPDIR_PER_DOC}"

    declare -ga TMPDIR_PER_DOC_order_list=()
    local -i cnt=0
    while read -r line; do 
        TMPDIR_PER_DOC_order_list+=("$line")
	REV2IDX[`echo $line | awk -F"|" '{print $1}'`]=$cnt
	cnt+=1
    done < <(7z l -ba "${ARCHIVE}"  | awk '/\W[0-9]+\.[0-9]+$/ { print $6"|"$1"|"$2}'  | sort -k1 -n )

    #done < <(7z l -ba "${ARCHIVE}"  | awk '/\W[0-9]+\.[0-9]+$/ { command = "date -d "$1" +%A"; command | getline day;  close(command); print "D="$1" T="$2" d="day" rev= "$6}'  | sort -k5 -n )
    
}

function extract_revision()  {
    
    # populate_rev2idx
    create_TMPDIR_PER_DOC_order_list 
    lines=${#TMPDIR_PER_DOC_order_list[@]}
    lineno_found=${REV2IDX["$REV"]}
    
    lastrev=''
    lastrev_idx=''
    local -i direction=1
    local -a dtag=()
    if [[ -z  ${lineno_found} ]]; then
	
	 echo "Diff file of revision $REV not found in archive ${ARCHIVE}!"
	exit_abnormal
    fi
    # verify if there is a lastrev and that the hashes match
    if [ -f "${TMPDIR_PER_DOC}lastrev.txt" ]; then
	 line=`cat "${TMPDIR_PER_DOC}lastrev.txt"`
	 lastrev=`echo $line | awk '{print $1}'`
	 lastrev_idx=`echo $line | awk '{print $2}'`
	 direction=`echo $line | awk '{print $3}'`
	 if [[ ! -z ${lastrev}  ]]; then
	     verify_msg=$(verify_hash ${lastrev})
	     if [[ $verify_msg != 'verified' ]]; then
		 lastrev=''
		 direction=''
		 lastrev_idx=''
	     fi
	 fi
    fi

    if [ -z "$lastrev" ]; then
	# is it closer to start or end of archive
	if (( lineno_found < lines / 2 )); then
	    direction=1
	    lastrev=0.0
	    lastrev_idx=-1
	    cp "${TMPDIR_PER_DOC}${ORIGINAL_VERSION}" "${TMPDIR_PER_DOC}wip"
	else

	    direction=-1
	    lastrev=`echo ${TMPDIR_PER_DOC_order_list[-1]} | awk -F"|" '{print $1}'`

	    lastrev_idx=$(( lines - 1 ))
	    cp "${TMPDIR_PER_DOC}${LATEST_DOC_PRE}${ORIGINAL_VERSION}" "${TMPDIR_PER_DOC}wip"
	fi
    else
	
        difference=$(( $lastrev_idx - $lineno_found ))
	abs_difference=${difference#-}
	
	difference_from_end=$(( $lines - $lineno_found ))

	difference_from_beginning=$(( $lineno_found ))
	
	# if rev is only 1 step away from last rev
	if [[ abs_difference == "1" ]]; then
	    if [[ ${difference:0:1} == "-" ]]; then
		direction=1
	    else
		direction=-1
	    fi
	    
	# if rev is closer to lastrev than to either the start or end of the archive, then start at  lastrev
	elif (( lineno_found < lastrev_idx &&  difference_from_beginning < abs_difference )); then
	    direction=1
	    lastrev=0.0
	    lastrev_idx=0
	    cp ${TMPDIR_PER_DOC}${ORIGINAL_VERSION} ${TMPDIR_PER_DOC}wip
	elif (( lineno_found < lastrev_idx  && difference_from_beginning >= abs_difference )); then
	    direction=1
	elif (( lineno_found >= lastrev_idx  &&  difference_from_end < abs_difference )); then
	    direction=-1
	    lastrev_idx=$(( lines - 1 ))

	    lastrev=`echo ${TMPDIR_PER_DOC_order_list[-1]} | awk -F"|" '{print $1}'`
	    cp ${TMPDIR_PER_DOC}${LATEST_DOC_PRE}${ORIGINAL_VERSION} ${TMPDIR_PER_DOC}wip
	else
	    direction=-1
	fi
    fi
    
    # start walking lastrev_idx: in order to walk upward toward
    # ancestors we would need to apply the patch of the number we're
    # currently on, and if going downward, apply patch of nearest
    # progeny)
    local patch_idx=$lastrev_idx
    local -a dtag=("")
    if [[ $direction == -1 ]]; then
	dtag=("-R")
    fi
    while (( lastrev_idx != lineno_found )); do
	
        if [[ $direction == 1 ]]; then
	    patch_idx=$(( lastrev_idx + 1 ))
	else
	    patch_idx=$lastrev_idx
	fi
	patchrev=`echo ${TMPDIR_PER_DOC_order_list[${patch_idx}]} | awk -F"|" '{print $1}'`
	patch -p0 "${TMPDIR_PER_DOC}wip" "${dtag[@]}" -t <  "${TMPDIR_PER_DOC}${patchrev}"
        lastrev_idx=$(( lastrev_idx + direction ))	
	echo "lastrev_idx=${lastrev_idx} lineno_found=${lineno_found}"

	if (( lastrev_idx < 0 || lastrev_idx > $lines )); then
	    echo "Something has gone terribly wrong.  Aborting."
	    exit 1
	fi
	
    done
     
    echo -e "${lastrev} ${lastrev_idx} ${direction}" > "${TMPDIR_PER_DOC}lastrev.txt" 
    cp "${TMPDIR_PER_DOC}wip" "${DOCUMENT_DIR}${DOCUMENT}"
    echo "Overwriting ${DOCUMENT} with rev ${TMPDIR_PER_DOC_order_list[${lineno_found}]} !!"
    exit_normal

}
    



function quit_if_rev_diff_file_not_found() {

    output_msg=`7z l -ba -bso0 -bsp0 -aoa  "${ARCHIVE}" "${REV}" | awk -v pattern=^${REV//./\\.} '$6 ~ pattern'`    
    
    if [  "${output_msg}" = '' ]; then
	echo "Diff file of revision $REV not found in archive ${ARCHIVE}!"
	exit_abnormal
    fi
}

function print_rev_diff() {
    get_signature_file
    
    output_msg=`7z e -o"${TMP_DIR}"  "${ARCHIVE}" "${REV}" | awk -v pattern=^No\ files\ to\ process '$0 ~ pattern'`
    
    if [[ -z "${output_msg}" ]]; then
	cat "${TMP_DIR}${REV}"
	set_TMPDIR_PER_DOC
	mkdir -p "${TMPDIR_PER_DOC}"

	local revidx=${REV2IDX["$REV"]}
	local lastrevdiffprinted
	create_TMPDIR_PER_DOC_order_list

        lastrevdiffidxprinted=$revidx
	echo ${TMPDIR_PER_DOC_order_list[$revidx]}
	echo "$lastrevdiffidxprinted" > "${TMPDIR_PER_DOC}last_rev_diff_idx_printed.txt"
	exit_normal
    else
	echo "Diff file of revision $REV not found in archive ${ARCHIVE}!"
	exit_abnormal
	
    fi
    
}

function print_next_rev_diff() {
    declare -i lastrevdiffidxprinted

	set_TMPDIR_PER_DOC
	
	create_TMPDIR_PER_DOC_order_list
	if [ ! -f "${TMPDIR_PER_DOC}last_rev_diff_idx_printed.txt" ]; then


	    lastrevdiffidxprinted=0
	    
	    echo "$lastrevdiffidxprinted" > "${TMPDIR_PER_DOC}last_rev_diff_idx_printed.txt"
	else
	    
	    lastrevdiffidxprinted=`head -1 "${TMPDIR_PER_DOC}last_rev_diff_idx_printed.txt"`
	    lastrevdiffidxprinted+=1
	fi
	
	lines=${#TMPDIR_PER_DOC_order_list[@]}
	if (( lastrevdiffidxprinted <= lines )); then
	    echo "$lastrevdiffidxprinted" > "${TMPDIR_PER_DOC}last_rev_diff_idx_printed.txt"
	    
	    REV=`echo ${TMPDIR_PER_DOC_order_list[$lastrevdiffidxprinted]} | awk -F"|" '{print $1}'`
	    
	    7z e -bso0 -bsp0 -aoa -o"${TMPDIR_PER_DOC}" "${ARCHIVE}" "${REV}"
	    
	    cat "${TMPDIR_PER_DOC}${REV}"
	    
	    echo "rev ${TMPDIR_PER_DOC_order_list[$lastrevdiffidxprinted]}"
	else
	    echo "Error: Out of range."
	    exit 1
	fi
	
	exit 0
}


function print_prev_rev_diff() {
    declare -i lastrevdiffidxprinted
    
    set_TMPDIR_PER_DOC
    
    create_TMPDIR_PER_DOC_order_list
    if [ ! -f "${TMPDIR_PER_DOC}last_rev_diff_idx_printed.txt" ]; then
	

	lastrevdiffidxprinted=$(( ${#TMPDIR_PER_DOC_order_list[@]} - 1 ))
	echo "$lastrevdiffidxprinted" > "${TMPDIR_PER_DOC}last_rev_diff_idx_printed.txt"
    else
	
	lastrevdiffidxprinted=`head -1 "${TMPDIR_PER_DOC}last_rev_diff_idx_printed.txt"`
	lastrevdiffidxprinted+=-1
    fi
    
    if (( lastrevdiffidxprinted >= 0 )); then
	echo "$lastrevdiffidxprinted" > "${TMPDIR_PER_DOC}last_rev_diff_idx_printed.txt"
	

	REV=`echo ${TMPDIR_PER_DOC_order_list[$lastrevdiffidxprinted]} | awk -F"|" '{print $1}'`
	7z e -bso0 -bsp0 -aoa -o"${TMPDIR_PER_DOC}" "${ARCHIVE}" "${REV}"
	cat "${TMPDIR_PER_DOC}${REV}"
	

	echo "rev ${TMPDIR_PER_DOC_order_list[$lastrevdiffidxprinted]}"
    else
	echo  "Error: Out of range."
	exit 1
    fi
    exit 0
    }

function alter_directory_of_document() {
    
    7z e -bso0 -bsp0 -aoa -o"${TMP_DIR}" "${ARCHIVE}" "${SIGNATURE_FILE}"  || { echo "Error extracting ${SIGNATURE_FILE} from ${ARCHIVE}"; exit 1; }

    newpath="$1"

    perl -pi -e "s|(directory-of-document)=.*|\1=${newpath}|" "${TMP_DIR}${SIGNATURE_FILE}"

    7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on -bso0 -bsp0 "${ARCHIVE}" "${TMP_DIR}${SIGNATURE_FILE}"
    echo "updated document path to $newpath"
    exit_normal    
}

function get_document_name_and_dir() {
 
    7z e -bso0 -bsp0 -aoa -o"${TMP_DIR}" "${ARCHIVE}" "${SIGNATURE_FILE}"  || { echo "Error extracting ${SIGNATURE_FILE} from ${ARCHIVE}"; exit 1; }
    DOCUMENT_DIR=`grep directory-of-document= "${TMP_DIR}${SIGNATURE_FILE}" | awk -F'=' '{print $2}'`
    TARGET_PATH="${DOCUMENT_DIR}"
    
    DOCUMENT=`grep document_name= "${TMP_DIR}${SIGNATURE_FILE}" | awk -F'=' '{print $2}'`
    TARGET="${DOCUMENT}"
    TARGET_FILE="${DOCUMENT_DIR}/${DOCUMENT}"
    
    TARGET_EXTENSION="${DOCUMENT##*.}"

    TARGET_SANS_EXTENSION="${DOCUMENT%.*}"

    }

function add_decimal_point_to_REV_if_there_isnt_one() {

    # Check if the variable contains only numbers and possibly a single decimal point
    if [[ ! "$REV" =~ ^[0-9]*\.?[0-9]+$ ]]; then
        echo "Error: revision number must be a real number or integer"
        exit 1
    fi

    # If there's no decimal point, add .0
    if [[ ! "$REV" =~ \. ]]; then
        REV="${REV}.0"
    fi

}


function lazy_user() {

    #  for the sake of laziness, let the user specify either the archive or the document when listing the revisions
    if [ ${TARGET_EXTENSION} = '7z' ]; then
	ARCHIVE=$TARGET
    fi
    }

#########-    END FUNCTION DEFS   -########

while getopts ":z:n:d:k:a:c:mlLE:e:ujUJX" options; do
  case "${options}" in                    
    n)                                    # If the option is n,
      NOTE="${OPTARG}"                      # set $NOTE to specified value.
      ;;
    d)                                    # If the option is d,
      if [ -f "${OPTARG}" ]; then
	  ARCHIVE="${OPTARG}"
	  ARCHIVE_DIR=`dirname $(realpath "${OPTARG}")`
      elif [  -d "${OPTARG}" ];  then
	  ARCHIVE_DIR="${OPTARG}"
	  
      else
	  
        echo "Error: ARCHIVE_DIR must exist if -d flag is invoked."
	exit_abnormal                     # Exit abnormally.
      fi
      ;;
    c)
	DOCUMENT_DIR="${OPTARG}"
	
      if [ -f "${DOCUMENT_DIR}" ]; then
	  DOCUMENT="${DOCUMENT_DIR}"
	  DOCUMENT_DIR=`dirname $(realpath "${DOCUMENT_DIR}")`
	  
      elif [ ! -d "${DOCUMENT_DIR}" ]; then       # verify directory exists
        echo "Error: DOCUMENT_DIR must exist if -c flag is invoked."
        exit_abnormal                     # Exit abnormally.
      fi
      ;;
    a)
      A_FLAG_INVOKED=true
      if [ -f "${OPTARG}" ]; then
	  ARCHIVE="${OPTARG}"
	  ARCHIVE_DIR=`dirname $(realpath "${OPTARG}")`
      elif [  -d "${OPTARG}" ];  then
	  ARCHIVE_DIR="${OPTARG}"
      else 
        echo "Error: ARCHIVE_DIR must exist for the -a flag to be invoked."
        exit_abnormal                     # Exit abnormally.
      fi
      ;;
    k)
      K_FLAG_INVOKED=true    # rename document and archive
      NEW_NAME="${OPTARG}"
      err_msg=$(valid_filename   "${NEW_NAME}")
      if [[ $? -eq 1 ]]; then
	  echo "${err_msg}"
	  exit 1
      fi
      ;;
    m)
      M_FLAG_INVOKED=true
      ;;
    E)
      E_UPPERCASE_FLAG_INVOKED=true
      
      REV="${OPTARG}"
      
      add_decimal_point_to_REV_if_there_isnt_one
      ;;
    e)
      E_FLAG_INVOKED=true

      REV="${OPTARG}"

      add_decimal_point_to_REV_if_there_isnt_one
      ;;
    L)
      L_UPPERCASE_FLAG_INVOKED=true
      ;;
    l)
      L_FLAG_INVOKED=true
      ;;
    u)
      U_FLAG_INVOKED=true
      ;;
    j)
      J_FLAG_INVOKED=true
      ;;

    U)
      U_UPPERCASE_FLAG_INVOKED=true
      ;;
    J)
      J_UPPERCASE_FLAG_INVOKED=true
      ;;
    X)
      X_UPPERCASE_FLAG_INVOKED=true
      ;;
    z)
      Z_FLAG_INVOKED=true
      if [ -f "${OPTARG}" ]; then
	  #ARCHIVE="${OPTARG}"
	  NEW_DOCUMENT_DIR=`dirname $(realpath "${OPTARG}")`
      elif [  -d "${OPTARG}" ];  then
	  NEW_DOCUMENT_DIR="${OPTARG}"
      else 
        echo "Error: either NEW_DOCUMENT_DIR or NEW_DOCUMENT must exist in order for the -z flag to be invoked, and in order to alter the documents intented path."
        exit_abnormal                   
      fi
      ;;
    :)                                    # If expected argument omitted:
      echo "Error: -${OPTARG} requires an argument."
      exit_abnormal                       # Exit abnormally.
      ;;
    *)                                    # If unknown (any other) option:
      exit_abnormal                       # Exit abnormally.
      ;;
  esac
done

shift $((OPTIND -1))

if [ ${A_FLAG_INVOKED} = false ]; then 
    for arg
    do
	TARGET_FILE="$arg"
    done

    if [ -z "${TARGET_FILE}" ]; then
	echo "No target file specified"
	exit_abnormal
    elif [ ! -f "${TARGET_FILE}" ]; then
	echo "Target file doesnt exist"
	exit_abnormal
    fi

    
    TARGET=`basename "${TARGET_FILE}"`
    TARGET_EXTENSION="${TARGET##*.}"
    DOCUMENT="${TARGET}"

    TARGET_SANS_EXTENSION="${TARGET%.*}"
    if [  -z "${DOCUMENT_DIR}" ]; then
	TARGET_PATH=`dirname $(realpath "${TARGET_FILE}")`
    else
	
	TARGET_PATH="${DOCUMENT_DIR}"
    fi
else
    # A_FLAG_INVOKED = true
    get_document_name_and_dir
fi



if [ -z "${ARCHIVE_DIR}" ]; then
    ARCHIVE="${TARGET_FILE}.7z"
    
    ARCHIVE_DIR="${TARGET_PATH}"
else
    ARCHIVE="${ARCHIVE_DIR}/${TARGET}.7z"
fi


if [ ${K_FLAG_INVOKED} = true ]; then
    rename_file_and_archive "${NEW_NAME}"
    echo "renaming to a new name::"
    exit_normal
fi


if [ ${L_UPPERCASE_FLAG_INVOKED} = true ]; then
    get_latest
fi

if [ ${Z_FLAG_INVOKED} = true ]; then
    alter_directory_of_document "$NEW_DOCUMENT_DIR"
fi




if [ ${L_FLAG_INVOKED} = true ]; then
    #  for the sake of laziness, let the user specify either the archive or the document when listing the revisions
    if [ ${TARGET_EXTENSION} = '7z' ]; then
	ARCHIVE=$TARGET
    fi
   7z -ba l "${ARCHIVE}" | awk '/[0-9]+\.[0-9]+/'
   exit_normal
fi
 

if [ ${L_UPPERCASE_FLAG_INVOKED} = true ]; then
    #  for the sake of laziness, let the user specify either the archive or the document when listing the revisions
    if [ ${TARGET_EXTENSION} = '7z' ]; then
	ARCHIVE=$TARGET
    fi
    get_latest
fi


if [ ${E_FLAG_INVOKED} = true ]; then
    #  for the sake of laziness, let the user specify either the archive or the document when listing the revisions
    if [ ${TARGET_EXTENSION} = '7z' ]; then
	ARCHIVE=$TARGET
    fi
   print_rev_diff 
fi
 

if [ ${X_UPPERCASE_FLAG_INVOKED} = true ]; then
    lazy_user
    get_all
    echo "7z-revisions archive ${ARCHIVE} extracted to ${TMPDIR_PER_DOC}" 
    exit 0
fi


if [ ${E_UPPERCASE_FLAG_INVOKED} = true ]; then
    lazy_user
    get_all
    extract_revision
    exit 0
fi

if [ ${U_FLAG_INVOKED} = true ]; then
    print_prev_rev_diff
fi


if [ ${J_FLAG_INVOKED} = true ]; then
    print_next_rev_diff
fi


if [ ${M_FLAG_INVOKED} = true ]; then
    print_missing_revs
fi



TIMESTAMP=`date +%Y-%m-%d_%H%M%S`
# If the archive does not exist
if [[ ! -f "${ARCHIVE}" ]]; then
  
    
    # Create the  signature file
    TARGET_HASH=$(md5sum ${TARGET} | awk '{print $1}')
    ORIGINAL_VERSION="${TARGET_SANS_EXTENSION}${TIMESTAMP}.${TARGET_EXTENSION}"
    HASH_FILE="${HASH_FILE_PRE}${ORIGINAL_VERSION}"
    NOTES_FILE="${NOTES_FILE_PRE}${ORIGINAL_VERSION}${NOTES_FILE_POST}"
    BUFFER_FILE_ENCODING_SYSTEM=`file -i "${TARGET_FILE}"`
    TARGET_HASH=`sha1sum "${TARGET_FILE}"|awk '{print $1}'`
    set_END_OF_LINE_ENCODING
    

    cat > "${TMP_DIR}${SIGNATURE_FILE}" <<EOF
${TAGS_IN_METAFILE['rev']}-1.0
${TAGS_IN_METAFILE['original_version']}${ORIGINAL_VERSION}
${TAGS_IN_METAFILE['document_name']}${TARGET}
${TAGS_IN_METAFILE['archive_created_datetime']}${TIMESTAMP}
${TAGS_IN_METAFILE['track_hashes']}t
${TAGS_IN_METAFILE['last_hash']}${TARGET_HASH}
${TAGS_IN_METAFILE['latest_datetime']}${TIMESTAMP}
${TAGS_IN_METAFILE['directory_of_document']}${TARGET_PATH}
${TAGS_IN_METAFILE['os_type']}${OSTYPE}
${TAGS_IN_METAFILE['buffer_file_coding_system']}${BUFFER_FILE_ENCODING_SYSTEM}	
${TAGS_IN_METAFILE['end_of_line_encoding']}${END_OF_LINE_ENCODING}
${TAGS_IN_METAFILE['editor_version']}${VERSION}
EOF

    # Create hashfile
    cat > "${TMP_DIR}${HASH_FILE}" <<EOF 
(puthash "${ORIGINAL_VERSION}" "${TARGET_HASH}" 7zr-hasht)
EOF

    # Create  notes file
    cat > "${TMP_DIR}${NOTES_FILE}" <<EOF
;;  7zr-revisions.el   notes file for  ${ORIGINAL_VERSION}
(puthash "${ORIGINAL_VERSION}" "${NOTE}" 7zr-notestab)
EOF
    
    # Copy the target file
    cp "${TARGET_FILE}" "${TMP_DIR}7zr-latest-${ORIGINAL_VERSION}"
    
    cp "${TMP_DIR}7zr-latest-${ORIGINAL_VERSION}" "${TMP_DIR}${ORIGINAL_VERSION}"

    if [ -z "${ARCHIVE}" ]; then
	echo "archive has no name"
	exit_abnormal
    fi
    echo "creating archive ${ARCHIVE}"
    # Add the signature file and the copied target file to the archive
    7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on -bso0 -bsp0  "${ARCHIVE}" "${TMP_DIR}${SIGNATURE_FILE}" "${TMP_DIR}${HASH_FILE}"  "${TMP_DIR}${NOTES_FILE}" "${TMP_DIR}7zr-latest-${ORIGINAL_VERSION}" "${TMP_DIR}${ORIGINAL_VERSION}"
    
else

    
################# archive exists !
    ############# update archive
# rm -rf  "${TMP_DIR}${SIGNATURE_FILE}"
    7z e -bso0 -bsp0 -aoa -o"${TMP_DIR}" "${ARCHIVE}" "${SIGNATURE_FILE}"  || { echo "Error extracting ${SIGNATURE_FILE} from ${ARCHIVE}"; exit 1; }
    
    if [ -z `grep latest_revision= "${TMP_DIR}${SIGNATURE_FILE}"` ];  then
	
	# theres no latest_revision line!
	LATEST_REVISION=`7z -ba l "${ARCHIVE}" | awk -F'.' '{print $5}' | awk  'BEGIN {max = 0} {if ($4+0 == $4 && $4>max) max=$4} END {print max}'`
	
	((LATEST_REVISION++))

	LATEST_REVISION="${LATEST_REVISION}.0"
	(echo "latest_revision=${LATEST_REVISION}";cat "${TMP_DIR}${SIGNATURE_FILE}")>"${TMP_DIR}tmpout"
	cp "${TMP_DIR}tmpout" "${TMP_DIR}${SIGNATURE_FILE}"
    else
	# just increment the line 

	perl -pi -e "s/(${TAGS_IN_METAFILE['rev']})(-?\d+)(\.\d+)?/\"\$1\" .  (sprintf(\"%.1f\", \$2+1))/eg" "${TMP_DIR}${SIGNATURE_FILE}"


	LATEST_REVISION=$(awk -v tag="${TAGS_IN_METAFILE['rev']}" '$0 ~ tag {sub(tag, "", $0); print $0}' "${TMP_DIR}${SIGNATURE_FILE}")
	if [[ ${LATEST_REVISION} == "0.0" ]]; then
	    LATEST_REVISION="1.0"
            # the revisions start at 1.0 and not 0.0; 0.0 would be considered the unrevised original version	
            #  so we must increment the rev tag again to 1.0
	    perl -pi -e "s/(${TAGS_IN_METAFILE['rev']})-?(\d+)(\.\d+)?/\"\$1\" .  \"1.0\"/eg" "${TMP_DIR}${SIGNATURE_FILE}"
	fi
    fi
    original_version_tag=${TAGS_IN_METAFILE['original_version']}
    ORIGINAL_VERSION=$(awk -v pattern="${original_version_tag}" '$0 ~ pattern {sub(".*" pattern, ""); print $0}'  "${TMP_DIR}${SIGNATURE_FILE}") 
    if [ -z "${ORIGINAL_VERSION}" ]; then
	ORIGINAL_VERSION=`7z -ba l "${ARCHIVE}" | awk '/[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{6}\.[a-zA-Z0-9]+$/ {if (!min || length($6)<min) {min=length($6); shortest=$6}} END {print shortest}'`
    fi
    
    if [ -z "${ORIGINAL_VERSION}" ]; then
	echo "Unable to determine the original version name of the document"
	exit_abnormal
	
    fi
    HASH_FILE="${HASH_FILE_PRE}${ORIGINAL_VERSION}"
    NOTES_FILE="${NOTES_FILE_PRE}${ORIGINAL_VERSION}${NOTES_FILE_POST}"
    LATEST="7zr-latest-${ORIGINAL_VERSION}"
    

    replace_tags_in_text

    
    TARGET_HASH=`sha1sum "${TARGET_FILE}"|awk '{print $1}'`

    if [ -z `grep ${TAGS_IN_METAFILE['last_hash']} "${TMP_DIR}${SIGNATURE_FILE}"` ];  then
	
    cat >> "${TMP_DIR}${SIGNATURE_FILE}" <<EOF
${TAGS_IN_METAFILE['last_hash']}${TARGET_HASH}
EOF
    else
	sed -i  "s/${TAGS_IN_METAFILE['last_hash']}.*/${TAGS_IN_METAFILE['last_hash']}${TARGET_HASH}/g"  "${TMP_DIR}${SIGNATURE_FILE}"
    fi
    
    if [ -z `grep ${TAGS_IN_METAFILE['latest_datetime']} "${TMP_DIR}${SIGNATURE_FILE}"` ];  then

    cat >> "${TMP_DIR}${SIGNATURE_FILE}" <<EOF
${TAGS_IN_METAFILE['latest_datetime']}${TIMESTAMP}
EOF
    else
	sed -i  "s/${TAGS_IN_METAFILE['latest_datetime']}.*/${TAGS_IN_METAFILE['latest_datetime']}${TIMESTAMP}/g"  "${TMP_DIR}${SIGNATURE_FILE}"
    fi


    if [ -z `grep ${TAGS_IN_METAFILE['os_type']} "${TMP_DIR}${SIGNATURE_FILE}"` ];  then

    cat >> "${TMP_DIR}${SIGNATURE_FILE}" <<EOF
${TAGS_IN_METAFILE['os_type']}${OSTYPE}
EOF
    fi

    if [ -z `grep \"${TAGS_IN_METAFILE['buffer_file_coding_system']}\" "${TMP_DIR}${SIGNATURE_FILE}"` ];  then

    BUFFER_FILE_ENCODING_SYSTEM=`file -i "${TARGET_FILE}"`
    cat >> "${TMP_DIR}${SIGNATURE_FILE}" <<EOF
${TAGS_IN_METAFILE['buffer_file_coding_system']}${BUFFER_FILE_ENCODING_SYSTEM}	
EOF
    fi

    if [ -z `grep "${TAGS_IN_METAFILE['end_of_line_encoding']}" "${TMP_DIR}${SIGNATURE_FILE}"` ];  then
    set_END_OF_LINE_ENCODING
    cat >> "${TMP_DIR}${SIGNATURE_FILE}" <<EOF
${TAGS_IN_METAFILE['end_of_line_encoding']}${END_OF_LINE_ENCODING}
EOF
    fi
    
    7z e -bso0 -bsp0 -aoa -o"${TMP_DIR}"  "${ARCHIVE}" "${ORIGINAL_VERSION}" "${HASH_FILE}" "${NOTES_FILE}" "${LATEST}"
    
    # edit hashfile
    cat >> "${TMP_DIR}${HASH_FILE}" <<EOF
(puthash "${LATEST_REVISION}" "${TARGET_HASH}" 7zr-hasht)
EOF

    # edit NOTESfile
    if [ ! -z "${NOTE}" ]; then
	cat >> "${TMP_DIR}${NOTES_FILE}" <<EOF
(puthash "${LATEST_REVISION}" "${NOTE}" 7zr-notestab)
EOF
    fi
    # Create the diff file
    diff -Na "${TMP_DIR}${LATEST}"  "${TARGET_FILE}" > "${TMP_DIR}${LATEST_REVISION}"
    cp "${TARGET_FILE}"   "${TMP_DIR}${LATEST}" 
    if [ ! -s  "${TMP_DIR}${LATEST_REVISION}" ]; then
	echo "File is unchanged!  No action has been performed."
	exit_abnormal
    fi

    echo "Updating archive ${ARCHIVE}"
    7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on -bso0 -bsp0  "${ARCHIVE}" "${TMP_DIR}${SIGNATURE_FILE}" "${TMP_DIR}${LATEST}" "${TMP_DIR}${NOTES_FILE}" "${TMP_DIR}${HASH_FILE}" "${TMP_DIR}${LATEST_REVISION}"
    
fi
exit_normal
