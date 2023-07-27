#!/bin/bash

#    This script allows updating or creating a 7z-revisions.el archive without the
#    need for emacs, useful if you're in a terminal that lacks screen, emacs, or emacsclient.  You currently still need emacs, however, to view the revisions.

# Usages:
#    $0 [ -n NOTE_MESSAGE ] [ -d DIRECTORY_OF_ARCHIVE ] TARGET_DOCUMENT
#    $0 [ -l ] [ -n NOTE_MESSAGE ] [ -c DIRECTORY_OF_DOCUMENT ] -a TARGET_ARCHIVE 
#    $0 [ -d DIRECTORY_OF_ARCHIVE ] -k  RENAMED_NEW_DOCUMENT_AND_ARCHIVE_NAME TARGET_DOCUMENT

#    Flags:
#    -d Use this option to designate the directory for the archive. If not provided, the script will assume the archive is in the same directory as the target document. If you provide an archive file path, that archive will be the focus of the operation.
#    -l Extract the latest version of a document to a directory (-c to specify directory).  The default path found in signature file is used if the -c flag is omitted.
#    -n Add a note in the notes file during update.
#    -a Specify a particular archive to update, useful when the document linked to the archive is located somewhere else.
#    -k Renames document and archive

# Requirements: dos2unix

# Author/maintainer: ciscorx@gmail.com

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
END_OF_LINE_ENCODING=
BUFFER_FILE_ENCODING_SYSTEM=
A_FLAG_INVOKED=false
L_FLAG_INVOKED=false
Z_FLAG_INVOKED=false
K_FLAG_INVOKED=false
ORIGINAL_VERSION=
TIMESTAMP=`date +%Y-%m-%d_%H%M%S`

TEXT_END_BOUNDS=7777    # all tags in text must reside with in the first 7777 characters of the beginning of the document in order to be seen
UPDATE_TAGS_IN_TEXTP=t  # allow tags to be updated in text


declare -A VALUES_IN_TEXT
declare -A TAGS_IN_TEXT
TAGS_IN_TEXT["rev"]="7z_1revisions.el_rev="
TAGS_IN_TEXT["original_version"]="7z-1revisions.el_original-version="
TAGS_IN_TEXT["directory_of_document"]="7z-1revisions.el_directory-of-document="
TAGS_IN_TEXT["directory_of_archive"]="7z-1revisions.el_directory-of-archive="
TAGS_IN_TEXT["archive_prefix"]="7z-1revisions.el_archive-prefix="
TAGS_IN_TEXT["archive_extension"]="7z-1revisions.el_archive-extension="
TAGS_IN_TEXT["track_hashes"]="7z-1revisions.el_track-sha1sum-hashes="
TAGS_IN_TEXT["sha1_of_last_revision"]="7z-1revisions.el_sha1-of-last-revision="


declare -A VALUES_IN_METAFILE
declare -A TAGS_IN_METAFILE
TAGS_IN_METAFILE["rev"]="latest_revision="
TAGS_IN_METAFILE["original_version"]="original-version="
TAGS_IN_METAFILE["document_name"]="document_name="
TAGS_IN_METAFILE["directory_of_document"]="document-directory="
TAGS_IN_METAFILE["archive_prefix"]="archive-prefix="
TAGS_IN_METAFILE["archive_extension"]="archive-extension="
TAGS_IN_METAFILE["archive_created_datetime"]="archive-created_datetime="
TAGS_IN_METAFILE["directory_of_document"]="directory-of-document="
TAGS_IN_METAFILE["directory_of_archive"]="directory-of-archive="
TAGS_IN_METAFILE["track_hashes"]="track-sha1sum-hashes="
TAGS_IN_METAFILE["last_hash"]="last-sha1sum="
TAGS_IN_METAFILE["sha1_of_last_revision"]="sha1-of-last-revision="
TAGS_IN_METAFILE["os_type"]="created_on_os_system-type="
TAGS_IN_METAFILE["buffer_file_coding_system"]="buffer-file-coding-system="
TAGS_IN_METAFILE["end_of_line_encoding"]="end-of-line-encoding="
TAGS_IN_METAFILE["editor_version"]="7z-revision-version="

# Beware: the following TMP_DIR directory will be deleted at the end of the program
TMP_DIR=`mktemp -d`/

usage() {                                 # Function: Print a help message.
  echo "This script will update or create a 7z-revisions.el archive without the use of emacs.  If the -d directory_of_archive option is omitted, then the directory of the target document will be assumed.  If an actual archive file is passed as -d then that archive will be used for the archive.  Use the -a flag to update the document associated with the archive in question.  Use the -l flag to extract a copy of the latest revision from the archive."
  echo "Usage: $0 [ -n NOTE_MESSAGE ] [ -d DIRECTORY_OF_ARCHIVE ] TARGET_DOCUMENT" 1>&2 

  echo "Usage: $0 [ -l ] [ -n NOTE_MESSAGE ] [ -c DIRECTORY_OF_DOCUMENT ] -a TARGET_ARCHIVE" 1>&2 
}
exit_abnormal() {                         # Function: Exit with error.
  usage

# rm  -rf "${TMP_DIR}"
  exit 1
}

exit_normal() {
    
# rm  -rf "${TMP_DIR}"
exit 0
}

strip_extension() {  
    local filename="$1"
    base="${filename%.*}"
    echo "$base"
}

get_extension() {
    local filename="$1"
    local extension="${filename##*.}"
    echo "$extension"
}

strip_timestamp() {
    string="$1"
    echo "${string%[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]_[0-9][0-9][0-9][0-9][0-9][0-9]}"

}

valid_filename() {
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

get_note() {
    local rev="$1"
    local filename="$2"
    awk -F'"' "/\"${rev}\"/ {line=\$4} END{print line}" "${filename}"
}

put_note() {
    local rev="$1"
    local hash="$2"
    local tab="$3"
    local filename="$4"
    echo "(puthash \"${rev}\" \"${hash}\" ${tab})" >> "${filename}"
}

    
rename_file_and_archive() {
    
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

populate_table() {
    
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
        local value=$(echo "$file_content" | grep -oP "${tags[$key]}\K\w+")
        if [[ "$value" != "" ]]; then
            values[$key]=$value
        fi
    done

# Populate VALUES_IN_TEXT from textfile.txt
#populate_table "textfile.txt" TAGS_IN_TEXT VALUES_IN_TEXT 7777

# Populate VALUES_IN_METAFILE from metafile.txt
#populate_table "metafile.txt" TAGS_IN_METAFILE VALUES_IN_METAFILE
}

replace_tags_in_text() {

    # Read the first 7777 characters of the file into a variable
    first_chars=$(head -c $TEXT_END_BOUNDS $file)
    
    # Loop over TAGS_IN_TEXT keys
    for key in "${!TAGS_IN_TEXT[@]}"
    do
	# Get corresponding value in VALUES_IN_METAFILE
	replacement=${VALUES_IN_METAFILE[$key]}
	
	# Perl one-liner to do in-place replacement
	first_chars=$(echo "$first_chars" | perl -pe "s|($TAGS_IN_TEXT[$key])\S*|$1$replacement|g")
    done
    
    # Replace the first 7777 characters of the file with the modified string
    echo "$first_chars$(tail -c +$((TEXT_END_BOUNDS+1)) $file)" > $file

}

get_signature_file() {


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

get_hash_file() {

    7z e -bso0 -bsp0 -aoa -o"${TMP_DIR}" "${ARCHIVE}" "${HASH_FILE}"  || { echo "Error extracting ${HASH_FILE} from ${ARCHIVE}"; exit 1; }
}
get_notes_file() {

    7z e -bso0 -bsp0 -aoa -o"${TMP_DIR}" "${ARCHIVE}" "${NOTES_FILE}"  || { echo "Error extracting ${NOTES_FILE} from ${ARCHIVE}"; exit 1; }
}

    
get_latest_rev() {

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


get_document_name() {

	if [ -z "${DOCUMENT_DIR}" ]; then
	    
	    DOCUMENT_DIR=`grep directory-of-document= "${TMP_DIR}${SIGNATURE_FILE}"   | awk -F'=' '{print  $2}'`
	fi
	if [ -z "${DOCUMENT}" ]; then
	     
	    DOCUMENT=`grep document_name= "${TMP_DIR}${SIGNATURE_FILE}"   | awk -F'=' '{print  $2}'`
	fi
    }


get_latest() {


    get_signature_file
    
    7z e -bso0 -bsp0 -aoa -o"${TMP_DIR}"  "${ARCHIVE}" "${LATEST}"
    if [ ${L_FLAG_INVOKED} = true ]; then
	if [ -z "${DOCUMENT_DIR}" ]; then
	    
	    DOCUMENT_DIR=`grep directory-of-document= "${TMP_DIR}${SIGNATURE_FILE}"   | awk -F'=' '{print  $2}'`
	fi
	if [ -z "${DOCUMENT}" ]; then
	     
	    DOCUMENT=`grep document_name= "${TMP_DIR}${SIGNATURE_FILE}"   | awk -F'=' '{print  $2}'`
	fi
	if  [ -f "${DOCUMENT_DIR}/${DOCUMENT}" ]; then
	    echo "Target file already exists!"
	    exit_abnormal
	else
	    cp "${TMP_DIR}${LATEST}" "${DOCUMENT_DIR}/${DOCUMENT}"
	    echo "File ${LATEST} extracted to ${DOCUMENT_DIR}/${DOCUMENT}"
	    exit_normal
	fi
    fi
    
}

alter_directory_of_document() {
    
    7z e -bso0 -bsp0 -aoa -o"${TMP_DIR}" "${ARCHIVE}" "${SIGNATURE_FILE}"  || { echo "Error extracting ${SIGNATURE_FILE} from ${ARCHIVE}"; exit 1; }

    newpath="$1"

    perl -pi -e "s|(directory-of-document)=.*|\1=${newpath}|" "${TMP_DIR}${SIGNATURE_FILE}"

    7z a -bso0 -bsp0 "${ARCHIVE}" "${TMP_DIR}${SIGNATURE_FILE}"
    echo "updated document path to $newpath"
    exit_normal    
}

get_document_name_and_dir() {
 
    7z e -bso0 -bsp0 -aoa -o"${TMP_DIR}" "${ARCHIVE}" "${SIGNATURE_FILE}"  || { echo "Error extracting ${SIGNATURE_FILE} from ${ARCHIVE}"; exit 1; }
    DOCUMENT_DIR=`grep directory-of-document= "${TMP_DIR}${SIGNATURE_FILE}" | awk -F'=' '{print $2}'`
    TARGET_PATH="${DOCUMENT_DIR}"
    
    DOCUMENT=`grep document_name= "${TMP_DIR}${SIGNATURE_FILE}" | awk -F'=' '{print $2}'`
    TARGET="${DOCUMENT}"
    TARGET_FILE="${DOCUMENT_DIR}/${DOCUMENT}"
    
    TARGET_EXTENSION="${DOCUMENT##*.}"

    TARGET_SANS_EXTENSION="${DOCUMENT%.*}"

    }

while getopts ":z:n:d:k:a:c:l" options; do
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
    l)
      L_FLAG_INVOKED=true
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


if [ ${L_FLAG_INVOKED} = true ]; then
    get_latest
fi

if [ ${Z_FLAG_INVOKED} = true ]; then
    alter_directory_of_document "$NEW_DOCUMENT_DIR"
fi




 
 

# If the archive does not exist
if [[ ! -f "${ARCHIVE}" ]]; then
  
    
    # Create the  signature file
    TARGET_HASH=$(md5sum ${TARGET} | awk '{print $1}')
    TIMESTAMP=`date +%Y-%m-%d_%H%M%S`
    ORIGINAL_VERSION="${TARGET_SANS_EXTENSION}${TIMESTAMP}.${TARGET_EXTENSION}"
    HASH_FILE="${HASH_FILE_PRE}${ORIGINAL_VERSION}"
    NOTES_FILE="${NOTES_FILE_PRE}${ORIGINAL_VERSION}${NOTES_FILE_POST}"
    BUFFER_FILE_ENCODING_SYSTEM=`file -i "${TARGET_FILE}"`
    TARGET_HASH=`sha1sum "${TARGET_FILE}"|awk '{print $1}'`
    if [ -z `dos2unix -ic "${TARGET_FILE}"` ]; then
	END_OF_LINE_ENCODING=unix
    else
	END_OF_LINE_ENCODING=windows
    fi

    cat > "${TMP_DIR}${SIGNATURE_FILE}" <<EOF
${TAGS_IN_METAFILE['rev']}-1.0
${TAGS_IN_METAFILE['original_version']}${ORIGINAL_VERSION}
${TAGS_IN_METAFILE['document_name']}${TARGET}
${TAGS_IN_METAFILE['archive_created_datetime']}${TIMESTAMP}
${TAGS_IN_METAFILE['track_hashes']}t
${TAGS_IN_METAFILE['last_hash']}${TARGET_HASH}
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
	LATEST_REVISION=`7z -ba l "${ARCHIVE}" | awk -F'.' 'BEGIN {max = 0} {if ($6+0 == $6 && $6>max) max=$6} END {print max}'`
	
	((LATEST_REVISION++))
	(echo "latest_revision=${LATEST_REVISION}";cat "${TMP_DIR}${SIGNATURE_FILE}")>"${TMP_DIR}tmpout"
	cp "${TMP_DIR}tmpout" "${TMP_DIR}${SIGNATURE_FILE}"
    else
	# just increment the line 
	perl -pi -e 's/(latest_revision=)(-?\d+(\.\d+)?)/$1.($2+1)/eg' "${TMP_DIR}${SIGNATURE_FILE}"
	
	LATEST_REVISION=`grep 'latest_revision=' "${TMP_DIR}${SIGNATURE_FILE}" | awk -F'=' '{print $2}'` 
	if [ ${LATEST_REVISION} = "0" ]; then
	    LATEST_REVISION="1"
            # the revision list index starts at 1 and not 0	
	    perl -pi -e 's/(latest_revision=)(-?\d+(\.\d+)?)/$1.($2+1)/eg' "${TMP_DIR}${SIGNATURE_FILE}"
	fi
    fi
    LATEST_REVISION="${LATEST_REVISION}.0"
    
    
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
    
    TARGET_HASH=`sha1sum "${TARGET_FILE}"|awk '{print $1}'`
    sed -i  "s/${TAGS_IN_METAFILE['last_hash']}.*/${TAGS_IN_METAFILE['last_hash']}${TARGET_HASH}/g"  "${TMP_DIR}${SIGNATURE_FILE}"
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
    echo "Updating archive ${ARCHIVE}"
    7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on -bso0 -bsp0  "${ARCHIVE}" "${TMP_DIR}${SIGNATURE_FILE}" "${TMP_DIR}${LATEST}" "${TMP_DIR}${NOTES_FILE}" "${TMP_DIR}${HASH_FILE}" "${TMP_DIR}${LATEST_REVISION}"
    
fi
exit_normal
