#!/bin/bash

# This script will update or create a 7r-revisions archive without the use of emacs, useful in a terminal where screen or emacs or emacsclient is not available. 

# Author/maintainer: ciscorx@gmail.com

if [ -z "$1" ]; then
    echo "No target file specified"
    exit 1
elif [ ! -f "$1" ]; then
    echo "Target file doesnt exist"
    exit 1
fi

NOTE=
if [ ! -z "$2" ]; then
    NOTE="$2"
fi


VERSION=3.9

# Beware: the following TMP_DIR directory will be deleted at the end of the program
TMP_DIR=`mktemp -d`/

TARGET=`basename "$1"`
TARGET_EXTENSION="${TARGET##*.}"
TARGET_SANS_EXTENSION="${TARGET%.*}"
TARGET_PATH=`realpath $1`
END_OF_LINE_ENCODING=
BUFFER_FILE_ENCODING_SYSTEM=
ORIGINAL_VERSION=
ARCHIVE="$1.7z"
SIGNATURE_FILE='7z-revisions.el created this archive'
HASH_FILE=

# If the archive does not exist
if [[ ! -e "${ARCHIVE}" ]]; then
    echo "Creating initial archive..."
    
    # Create the  signature file
    TIMESTAMP=`date +%Y-%m-%d_%H%M%S`
    ORIGINAL_VERSION="${TARGET_SANS_EXTENSION}${TIMESTAMP}.${TARGET_EXTENSION}"
    HASH_FILE="7zr-sha1-${ORIGINAL_VERSION}"
    NOTES_FILE="7zr-notes-${ORIGINAL_VERSION}.7zrn"
    BUFFER_FILE_ENCODING_SYSTEM=`file -i $1`
    TARGET_HASH=`sha1sum $1|awk '{print $1}'`
    if [ -z `dos2unix -ic $1` ]; then
	END_OF_LINE_ENCODING=unix
    else
	END_OF_LINE_ENCODING=windows
    fi

    cat > "${TMP_DIR}${SIGNATURE_FILE}" <<EOF
latest_revision=-1.0
original_version=${ORIGINAL_VERSION}
document_name=${TARGET}
archive_created_datetime=${TIMESTAMP}
track-sha1sum-hashes=t
directory-of-archive=${TARGET_PATH}
created_on_os_system_type=${OSTYPE}
buffer-file-coding-system=${BUFFER_FILE_ENCODING_SYSTEM}	
end-of-line-encoding=${END_OF_LINE_ENCODING}
7z-revisions-version=${VERSION}
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
    cp $1 "${TMP_DIR}7zr-latest-${ORIGINAL_VERSION}"
    
    cp "${TMP_DIR}7zr-latest-${ORIGINAL_VERSION}" "${TMP_DIR}${ORIGINAL_VERSION}"
    
    # Add the signature file and the copied target file to the archive
    7z a -bso0 -bsp0 "${ARCHIVE}" "${TMP_DIR}${SIGNATURE_FILE}" "${TMP_DIR}${HASH_FILE}"  "${TMP_DIR}${NOTES_FILE}" "${TMP_DIR}7zr-latest-${ORIGINAL_VERSION}" "${TMP_DIR}${ORIGINAL_VERSION}"
    
else

    
    ### archive exists !
    ############# update archive

    7z e -bso0 -bsp0 -o"${TMP_DIR}" "${ARCHIVE}" "${SIGNATURE_FILE}"  || { echo "Error extracting ${SIGNATURE_FILE} from ${ARCHIVE}"; exit 1; }
    echo "Updating archive..."
    
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
	
    fi
    LATEST_REVISION="${LATEST_REVISION}.0"
    
    ORIGINAL_VERSION=`grep 'original_version=' "${TMP_DIR}${SIGNATURE_FILE}" | awk -F'=' '{print $2}'`
    if [ -z "${ORIGINAL_VERSION}" ]; then
	ORIGINAL_VERSION=`7z -ba l "${ARCHIVE}" | awk '/[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{6}\.[a-zA-Z0-9]+$/ {if (!min || length($6)<min) {min=length($6); shortest=$6}} END {print shortest}'`
    fi
    
    if [ -z "${ORIGINAL_VERSION}" ]; then
	echo "Unable to determine the original version name of the document"
	exit 1
    fi
    echo "Original_version is ${ORIGINAL_VERSION}"
    HASH_FILE="7zr-sha1-${ORIGINAL_VERSION}"
    NOTES_FILE="7zr-notes-${ORIGINAL_VERSION}.7zrn"
    LATEST="7zr-latest-${ORIGINAL_VERSION}"
    TARGET_HASH=`sha1sum $1|awk '{print $1}'`
    7z -o"${TMP_DIR}" e "${ARCHIVE}" "${ORIGINAL_VERSION}" "${HASH_FILE}" "${NOTES_FILE}" "${LATEST}"
   
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
    diff -Na "${TMP_DIR}${LATEST}"  "$1" > "${TMP_DIR}${LATEST_REVISION}"
    cp "$1"   "${TMP_DIR}${LATEST}" 

    7z a -bso0 -bsp0 "${ARCHIVE}" "${TMP_DIR}${SIGNATURE_FILE}"  "${TMP_DIR}${LATEST}" "${TMP_DIR}${NOTES_FILE}" "${TMP_DIR}${HASH_FILE}" "${TMP_DIR}${LATEST_REVISION}"
fi

rm  -rf "${TMP_DIR}"

