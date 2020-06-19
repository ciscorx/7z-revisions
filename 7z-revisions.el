;; 7z-revisions.el --- This Emacs app is not a version control system,
;; but simply an incremental backup system that easily lets one browse
;; and search past saves of a single file, keeping incremental diffs
;; in a .7z archive, facilitating quick audits.  Also, provides word
;; by word differential highlighting.  Also, provides syntax coloring
;; when viewing raw diff files.  Useful when git may be considered
;; over-kill.  Compatible with windows and linux, and likely mac.
;;
;; authors/maintainers: ciscorx@gmail.com
;; version: 2.9
;; commit date: 2020-06-19
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3 of the License,
;; or (at your option) any later version.
;;
;; Commentary:
;;
;; Wouldnt it be nice to be able to go back and double check to see
;; exactly what you saved, when you saved it, in addition to perhaps
;; what you may have accidentally deleted?  7z-revisions is now
;; available, an indispensable #emacs #orgmode companion that does
;; exactly that.  With the 7z-revisions-mode minor mode, the current
;; buffer is saved to a 7-zip archive of the same name, whenever a
;; save-buffer command is issued, incrementally saving the latest revision
;; by adding a new patch to the archive.  Optionally, the .7z
;; extension can be altered to something else, such as .8z, by setting
;; the global variable 7zr-archive-extension to ".8z".  Additionally,
;; the function 7z-revisions can be called interactively to view or
;; consolidate past revisions in the archive.
;;
;; If your document anywhere contains a specific tag in the text, such
;; as 7z-revisions.el_rev= followed by nothing or any number,
;; then upon execution of the function
;; 7zr-update-7z-revisions-tags-in-text, which is automatically called
;; upon save if the 7zr-auto-update-tags-in-text-p variable is set to
;; t, the highest revision number, incremented by 1, will be added to
;; the end of that tag, in this case at the end of that equals sign,
;; replacing the previous number, if present.  A tag, for instance, of
;; 7zr-revisions.el_directory-of-archive=../ will specify the parent
;; directory as the directory where the archive resides, etc.  For
;; another example,
;; 7z-revisions.el_sha1-of-last-revision=
;; causes the insertion of the sha1sum hash value of the last revision
;; to be placed after the tag, in this case appearing after the equals
;; sign, which e.g. in a blockchain sort of way, establishes a
;; forensically authentic journal to show ones work.
;;
;; For your convenience, the tags can be inserted into the document or
;; into the metadata file (the created-by-message file) using the
;; 7zr-select-tag-to-insert-into-document function.  However, the tag
;; which displays the current revision must be present in the
;; document, or else none of the other tags will work.
;; All tags must be present in the first 7777 characters of the
;; document, or they will not work.<br/>
;; 
;; Some useful commands for when editing your document (these are also menu options):
;;     M-x 7zr-line-last-changed-on
;;     M-x 7zr-goto-line-of-last-revision
;;     M-x 7zr-create-file-for-archive-created-by-message
;;     M-x 7zr-input-archive-directory
;;     M-x 7zr-archive-edit-metadata-file
;;     M-x 7zr-archive-save-metadata-file
;;     M-x 7zr-select-tag-to-insert-into-document
;;     M-x 7zr-update-7z-revisions-tags-in-text
;;     M-x 7zr-notes-annotation
;;     M-x 7zr-modify-raw-hash-file
;;     M-x 7zr-modify-raw-notes-file
;;     M-z 7zr-rename-document-and-its-archive
;;     M-x 7z-revisions-mode
;;     M-x 7z-revisions
;;
;; When 7z-revisions-mode is active, the following two sequence key
;; bindings take effect: F2 F3 = 7z-revisions, F2 c = edit note for
;; next revision, F2 CTRL-c = edit raw notes file, F2 t = auto update
;; tags, F2 3 = update tags, F2 CTRL-s = update tags & save buffer, F2
;; l = goto line last changed, F2 p = when was line of point last
;; changed, F2 s = select tag to insert, F2 CTRL-r = rename document &
;; archive, F2 C-d = input (set) default archive directory, F2 C-f = edit
;; metafile (created-by-message file), F2 ` = exit 7z-revisions-mode.
;;
;; When 7z-revisions is called, the following key bindings take
;;   effect: Enter = view the selected revision, j = view raw diff
;;   file of selected revision, C-c C-a = view all selected diff files
;;   in one buffer, q = quit, C-c c = consolidate region, g = goto
;;   date, h = toggle highlight differences, c = edit revision note, w
;;   = display day of the week
;;
;; While viewing a revision: q = quit, n = next, p = previous.  Also,
;;   when highlight changes is enabled, d = jump to next
;;   difference/change, e = jump to previous change, j = view the raw
;;   diff file, w = display day of the week, g = Quit 7z-revisions and
;;   then try to goto the line in your document corresponding to the
;;   last line viewed from 7z-revisions.
;;
;; While viewing a raw diff file: q = quit, n = next, p = previous, w
;;   = display day of the week of diff files timestamp, r = switch to
;;   revision view, d = jump to next change hunk, e = jump to previous
;;   change hunk, g = Quit 7z-revisions and then try to goto the line
;;   in your document corresponding to the change hunk that was at
;;   point.
;;
;;
;; There are also some functions in the menu which provide for
;; consoldating the current days worth of changes, or last hour worth
;; of changes, etc.
;;
;; Also, sha1sum hash values for each revision are saved in the
;; hashtable stored in the archive and can be search from the menu.
;;
;; While in dired-mode, the key binding z = 7zr-dired-7z-revisions, which
;;   views the 7z-revisions archive at point, comes into effect.
;;
;; Required features:
;;   hl-line+.el  ( not required, but aesthetic )
;;   p7zip
;;   diffutils  ( just the diff command )
;;
;; When running on windows, access to additional dos commands is necessary, such as diff, awk, fciv, and optionally grep.
;;   Install diffutils for windows:
;;     http://gnuwin32.sourceforge.net/packages/diffutils.htm and then
;;     append C:\Program Files\GnuWin32\bin, or whatever directory
;;     that happens to contain diff.exe, to the path variable in
;;     control panel -> system -> Advanced -> Environment Variables.
;;     Alternatively, you could just throw all the files in
;;     c:\windows\system32
;;   Download awk from
;;     http://gnuwin32.sourceforge.net/packages/gawk.htm
;;   Download the sha1sum equivalent, fciv,
;;     https://www.microsoft.com/en-us/download/confirmation.aspx?id=11533
;;   Download 7zip https://www.7-zip.org/download.html and then put
;;     7z.exe and 7z.dll in windows/system32 directory, or any
;;     directory listed in the path environment variable
;;   Download grep http://gnuwin32.sourceforge.net/packages/grep.htm
;;     (optional) and then follow the same instructions as above
;;
;; Known Bugs:
;;
;; - Does not work when a file is opened via tramp
;; - File names must contain at least 1 alphabetical character or
;;     underscore or hyphen, and in this regard, cannot take the form
;;     of a real number, e.g. "1.0".  (let's call this a constraint
;;     for now)
;; - Each archive can only track one file.  (let's call this a
;;     constraint also)
;; - Words added to beginning of line additionally highlight following
;;     word green. In some cases highlighting is off by 1 word.
;; 
;;  This program was written using emacs 23.3.1 on ubuntu 12.04, but
;;     is compatible with windows-xp and probably windows 7, and
;;     likely mac.

;;; 7zr-summary-mode.el begins ------------------------


 (if (boundp 'evil-mode)
   (add-to-list 'evil-emacs-state-modes '7zr-summary-mode)
   )

;; GLOBAL VARIABLES ----------------------
(setq 7z-revisions-version 2.9)
(setq 7zr-track-md5sum-hashes-p t) ; setting this to nil may speed things up a bit, but dont fiddle with this yet
(setq 7zr-track-md5sum-hashes-p_default t) ; setting this to nil may speed things up a bit, but dont fiddle with this yet
(setq 7zr-archive-extension ".7z")
(setq 7zr-archive-extension-suffix (replace-regexp-in-string "\\." "" 7zr-archive-extension))
(setq 7zr-archive-extension_default ".7z")
(setq 7zr-archive-prefix "")  ; hide archive file by using a "." character as the prefix (only works on linux and mac os)
(setq 7zr-archive-prefix_default "")  ; hide archive file by using a "." character as the prefix (only works on linux and mac os)
(setq 7zr-temp-directory-windows-nt "/tmp/")   ; temp directory to use on windows machines
(setq 7zr-temp-directory-linux "/tmp/")
(setq 7zr-temp-directory-apple "/tmp/")
(setq 7zr-view_highlightp t)  ; whether to show diff highlighting of added and deleted text when viewing a revision
(setq 7zr-view_date "")
(setq 7zr-tag-in-text-end-bounds 7777)  ; all tags in text must reside with in the first 7777 characters of the beginning of the document in order to be seen
(setq 7zr-auto-update-tags-in-text-p t)  ; automatically update tags in text before each save
(setq 7zr-update-7z-revisions-tags-in-textp t)  ; allow tags to be updated in text
(setq 7zr-update-7z-revisions-tag-in-text_rev "7z-revisions.el_rev=")
(setq 7zr-update-7z-revisions-tag-in-text_original-version "7z-revisions.el_original-version=")
(setq 7zr-update-7z-revisions-tag-in-text_directory-of-document "7z-revisions.el_directory-of-document=")
(setq 7zr-update-7z-revisions-tag-in-text_directory-of-archive "7z-revisions.el_directory-of-archive=")   
(setq 7zr-update-7z-revisions-tag-in-text_archive-prefix "7z-revisions.el_archive-prefix=")
(setq 7zr-update-7z-revisions-tag-in-text_archive-extension "7z-revisions.el_archive-extension=")
(setq 7zr-update-7z-revisions-tag-in-text_track-md5sum-hashes "7z-revisions.el_track-sha1sum-hashes=")
(setq 7zr-update-7z-revisions-tag-in-text_sha1-of-last-revision "7z-revisions.el_sha1-of-last-revision=")
(setq 7zr-update-7z-revisions-tag-in-text-menu-option (concat "Update " 7zr-update-7z-revisions-tag-in-text_rev " tag"))
; (setq 7zr-buffer (minibuffer-selected-window))
; (setq 7zr-buffer-FQPN (buffer-file-name 7zr-buffer))
; (setq 7zr-buffer-filename (file-name-nondirectory 7zr-buffer-FQPN))
(setq 7zr-original-version "")
(setq 7zr-pointer-lastviewed "")
(setq 7zr-pointer-lastviewed_last "")
(setq 7zr-pointer-lastviewed_2nd_last "") 
(setq 7zr-pointer-lastviewed_3rd_last "") 
(setq 7zr-pointer-lastviewed_nearest_ancestor "")
(setq 7zr-pointer-lastviewed_nearest_progeny "")
(setq 7zr-pointer-lastviewed_last_nearest_progeny "")
(setq 7zr-pointer-lastviewed_raw_diff_file "")
(setq 7zr-pointer-lastviewed_raw_diff_file2 "")
(setq 7zr-temp-string-variable "")
(setq 7zr-temp-number-variable 0)
(setq 7zr-prepend-to-reconstruct_wip "7zr-prepend-to-reconstruct_wip")
(setq 7zr-prepend-to-diff-file "diff_")
(setq 7zr-prepend-to-latest-revision "7zr-latest-")
(setq 7zr-prepend-to-current-version "7zr-current-")   ; not used
(setq 7zr-prepend-to-original-version "7zr-original-version-")   ; not used yet
(setq 7zr-prepend-to-notes-file "7zr-notes-")
(setq 7zr-notes-file-extension ".7zrn") 
(setq 7zr-prepend-to-hash-file "7zr-sha1-")
(setq 7zr-prepend-to-rej "7zr-rej-")
(setq 7zr-construct-slow_last "0")
(setq 7zr-hash-of-hash-file "")
(setq 7zr-hash-of-notes-file "")
(setq 7zr-track-md5sum-hashes_string "")  ; initial value discarded as set internally 
(setq 7zr-summary-rev-at-point "")
(setq 7zr-summary-reconst-last "")  
(setq 7zr-archive-created_datetime "")
(setq 7zr-archive-created-by-message "7z-revisions.el created this archive")  ;; this string should probably not end in a file extension which has a 7zr-turn-on-7zr-revisions-mode hook associated with it, or it will cause emacs to crash when creating a new archive  
; (defvar 7zr-hasht (make-hash-table :size 20000 :test 'equal))  ; this must instead be a buffer local variable
(setq 7zr-summary-last-line 0)  ; line number of revision in the 7z-revisions display, not to be mistaken for the last line number of point in the document ( which would be related to 7zr-view-last-pos or 7zr-view-last-line)
(setq 7zr-view-last-column 0)
(setq 7zr-view-last-line 0)  ; line number of point when 7z-revisions is invoked
(setq 7zr-last-column 0)  ; this is supposed to be a buffer local variable thats also been made global for debugging purposes
(setq 7zr-patch-number 0.0)
(setq 7zr-revisions_tmpbuffer "")
(setq 7zr-revisions_tmpbuffer_buffer nil)
;(setq 7zr-revisions-examine-tmpbuffer "7zr_revisions_examine_tmpbuffer")
(setq 7zr-revisions_tmpbuffer_lines 0)
(setq 7zr-revisions_original_buffer (current-buffer))
(setq 7zr-summary_discard_rejp nil)
(setq 7zr-map-difference_line_pos_last 1) 
(setq 7zr-map-differences_column_pos_last 0)
(setq 7zr-document-directory "")   ;; initial value is overwritten
(setq 7zr-archive-directory-full "")
(setq 7zr-archive-directory_default "")  ;; e.g. set to "../" to make put archives in parent directory, but leave blank for same directory as document
(setq 7zr-archive-directory "")  ;; for internal use only, intial value is overwritten
(setq 7zr-directory-of-document "")  ;; not implemented yet
(setq 7zr-lastviewed_saved "")
(setq 7zr-revisions_tmpbuffer "")  ;; name of buffer that lists all revisions
(setq 7zr-revisions_lastbuffer "")  ;; name of the buffer from which 7z-revisions was called
(setq 7zr-disable-summary-goto-sha1 nil)  ;; used for windows compatibility
(setq 7zr-diff-command "diff -Na")
(setq 7zr-patch-command "patch -p0")
(setq 7zr-add-to-archive-command "7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on")
(setq 7zr-buffer "")       ; buffer of active document, before calling 7z-revisions
(setq 7zr-sha1sum-command-windows "fciv.exe -sha1")
(setq 7zr-sha1sum-post-command-windows " | more +3 ")
(setq 7zr-sha1sum-command-linux "sha1sum")
(setq 7zr-sha1sum-post-command-linux " ")
(setq 7zr-document-namechanges ())
(setq 7zr-last-line 1)
(setq 7zr-misc_save-pre-excursion-location 0)
(setq 7zr-misc_save-directory-path "")
(setq 7zr-misc_No-newline-pos1 nil)   ; boolean return value from 7zr-remove-No-newline-msgs-and-return-their-locations-via-globals, indicating a msg was found not at end of file
(setq 7zr-misc_No-newline-pos2 nil)  ; boolean return value from 7zr-remove-No-newline-msgs-and-return-their-locations-via-globals, indicating a msg was found at end of file
(setq 7zr-update-7z-revisions-tag-in-metadata-file_rev "latest_revision=")
(setq 7zr-update-7z-revisions-tag-in-metadata-file_original-version "original-version=")
(setq 7zr-update-7z-revisions-tag-in-metadata-file_document_name "document_name=")
(setq 7zr-update-7z-revisions-tag-in-metadata-file_directory-of-document "document-directory")
;(setq 7zr-update-7z-revisions-tag-in-metadata-file_archive-filename-including-fullpath "archive-filename-including-fullpath=")
;(setq 7zr-update-7z-revisions-tag-in-metadata-file_archive-filename-including-relativepath "archive-filename-including-relativepath=")
(setq 7zr-update-7z-revisions-tag-in-metadata-file_archive-prefix "archive-prefix=")
(setq 7zr-update-7z-revisions-tag-in-metadata-file_archive-extension "archive-extension=")
(setq 7zr-update-7z-revisions-tag-in-metadata-file_archive-created_datetime "archive-created_datetime=")
(setq 7zr-update-7z-revisions-tag-in-metadata-file_directory-of-document "directory-of-document=")
(setq 7zr-update-7z-revisions-tag-in-metadata-file_directory-of-archive "directory-of-archive=")
(setq 7zr-update-7z-revisions-tag-in-metadata-file_track-md5sum-hashes "track-sha1sum-hashes=")
(setq 7zr-update-7z-revisions-tag-in-metadata-file_sha1-of-last-revision "sha1-of-last-revision=")
(setq 7zr-update-7z-revisions-tag-in-metadata-os_type "created_on_os_system-type=")
(setq 7zr-update-7z-revisions-tag-in-metadata-buffer_file_coding_system "buffer-file-coding-system=")
(setq 7zr-os_type (symbol-name system-type))
(setq 7zr-os_is_windows_p (string-match "windows" 7zr-os_type))
(setq 7zr-buffer_file_coding_system buffer-file-coding-system)
(setq 7zr-buffer_is_msdos_p (string-match "dos" (symbol-name buffer-file-coding-system)))


;;(setq 7zr-update-7z-revisions-tag-in-metadata-file_compile-staging-directory "compile-staging-directory=")

					; the following choices are only for the main text document
(setq 7zr-tag_choice->tag_text
      '(("revision" . 7zr-update-7z-revisions-tag-in-text_rev )
	("directory of archive" . 7zr-update-7z-revisions-tag-in-text_directory-of-archive )
	("archive prefix" . 7zr-update-7z-revisions-tag-in-text_archive-prefix )
	("archive extension" . 7zr-update-7z-revisions-tag-in-text_archive-extension )
	("name of original version" . 7zr-update-7z-revisions-tag-in-text_original-version )
	("hashvalue checksum of last revision" . 7zr-update-7z-revisions-tag-in-text_sha1-of-last-revision )	
;	("track sha1sum hashes" . 7zr-update-7z-revisions-tag-in-text_track-md5sum-hashes)
;	(7zr-update-7z-revisions-tag-in-metadata-file_archive-filename-including-fullpath . 7zr-update-7z-revisions-tag-in-metadata-file_archive-filename-including-fullpath)
;	(7zr-update-7z-revisions-tag-in-metadata-file_archive-filename-including-relativepath . 7zr-update-7z-revisions-tag-in-metadata-file_archive-filename-including-relativepath)
	))
					; the following choices are only available for use in the metadata file (i.e. created-by-message file)
(setq 7zr-tag_choice->metadata-file-tag_text
      '(("revision" . 7zr-update-7z-revisions-tag-in-metadata-file_rev )
	("directory of archive" . 7zr-update-7z-revisions-tag-in-metadata-file_directory-of-archive  )
	("original version" . 7zr-update-7z-revisions-tag-in-metadata-file_original-version  )
	("track sha1sum hashes" . 7zr-update-7z-revisions-tag-in-metadata-file_track-md5sum-hashes )
	("document name" . 7zr-update-7z-revisions-tag-in-metadata-file_document_name )
	("document directory" . 7zr-update-7z-revisions-tag-in-metadata-file_directory-of-document )
	("archive prefix" . 7zr-update-7z-revisions-tag-in-metadata-file_archive-prefix )
	("archive extension" . 7zr-update-7z-revisions-tag-in-metadata-file_archive-extension )
	("archive created datetime" . 7zr-update-7z-revisions-tag-in-metadata-file_archive-created_datetime )
;	(7zr-update-7z-revisions-tag-in-metadata-file_relative-directory-of-document . 7zr-update-7z-revisions-tag-in-metadata-file_relative-directory-of-document )
;	("hashvalue checksum of last revision" . 7zr-update-7z-revisions-tag-in-metadata-file_sha1-of-last-revision )	
	)
      )

(defun 7zr-reconstruct-rev-from-patches ( rev )  ; default for linux and mac os
;  "This just calls 7zr-reconstruct-rev-from-patches_LINUX"
   "This function calls 7zr-reconstruct-rev-from-patches_MSWINDOWS when running under mswindows or when viewing a revision of a DOS buffer-file-coding-system encoded file.  Otherwise, it calls 7zr-reconstruct-rev-from-patches_LINUX"

   (if (or 
	7zr-active-document_buffer_is_msdos_p 
	7zr-os_is_windows_p 
	) ; debug 
       (7zr-reconstruct-rev-from-patches_MSWINDOWS rev)
					; else assume linux
     (7zr-reconstruct-rev-from-patches_LINUX rev)
 ;    (7zr-reconstruct-rev-from-patches_MSWINDOWS rev)
     )

   )

(setq 7zr-mswindows-requirements-failed nil)
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (setq 7zr-temp-directory 7zr-temp-directory-windows-nt)
  (cond
	((not (executable-find "diff.exe"))
	 (setq 7zr-mswindows-requirements-failed "Need diff.exe from http://gnuwin32.sourceforge.net/packages/diffutils.htm"))
	((not (executable-find "awk.exe"))
	 (setq 7zr-mswindows-requirements-failed "Need awk.exe from http://gnuwin32.sourceforge.net/packages/gawk.htm"))
	((not (executable-find "fciv.exe"))
	 (setq 7zr-mswindows-requirements-failed "Need fciv.exe for sha1sum https://www.microsoft.com/en-us/download/confirmation.aspx?id=11533"))
	((not (executable-find "7z.exe"))
	 (setq 7zr-mswindows-requirements-failed "Need 7z.exe from https://www.7-zip.org/download.html")))
  (unless (executable-find "grep.exe")  ; dont fail if we cant find grep since its only used in one trivial function
    (setq 7zr-disable-summary-goto-sha1 t))
  (when 7zr-mswindows-requirements-failed
    (error 7zr-mswindows-requirements-failed))
  (setq 7zr-sha1sum-command 7zr-sha1sum-command-windows)
  (setq 7zr-sha1sum-post-command 7zr-sha1sum-post-command-windows)
  (defun 7zr-awk-cmd-string ( fieldnum )
    (concat " | awk \"{print $" (number-to-string fieldnum) "}\"")
    )
  (defun 7zr-reconstruct-rev-from-patches ( rev )
  "This just calls 7zr-reconstruct-rev-from-patches_MSWINDOWS"
    (7zr-reconstruct-rev-from-patches_MSWINDOWS rev)
    ))
  
 ((string-equal system-type "darwin") ; Mac OS X
  (setq 7zr-temp-directory 7zr-temp-directory-apple)
  (setq 7zr-sha1sum-command 7zr-sha1sum-command-linux)
  (setq 7zr-sha1sum-post-command 7zr-sha1sum-post-command-linux)
  (defun 7zr-awk-cmd-string ( fieldnum )
    (concat " | awk '{print $" (number-to-string fieldnum) "}'")
    ))

 ((string-equal system-type "gnu/linux") ; linux
  (setq 7zr-sha1sum-command 7zr-sha1sum-command-linux)
  (setq 7zr-sha1sum-post-command 7zr-sha1sum-post-command-linux)
  (setq 7zr-temp-directory 7zr-temp-directory-linux)
  (defun 7zr-awk-cmd-string ( fieldnum )
    (concat " | awk '{print $" (number-to-string fieldnum) "}'")
    ))

 (t  ; default os
  (setq 7zr-sha1sum-command 7zr-sha1sum-command-linux)
  (setq 7zr-sha1sum-post-command 7zr-sha1sum-post-command-linux)
  (setq 7zr-temp-directory 7zr-temp-directory-linux)
  (defun 7zr-awk-cmd-string ( fieldnum )
    (concat " | awk '{print $" (number-to-string fieldnum) "}'")
     ))
 )
 


; (add-hook 'after-change-major-mode-hook '7zr-toggle-highlight)
; (mouse-leave-buffer-hook)

;; BUFFER LOCAL VARIABLES--------------------------------------
;(make-variable-buffer-local '7zr-diff-queue_position)
(setq 7zr-diff-queue_position 0)
;(make-variable-buffer-local '7zr-diff-queue_length)
;(make-variable-buffer-local '7zr-diff-queue)
;(make-variable-buffer-local '7zr-active-document)           ; 7zr-buffer
;(make-variable-buffer-local '7zr-active-document_original-version)  ; 7zr-original-version
;(make-variable-buffer-local '7zr-active-document_buffer)
;(make-variable-buffer-local '7zr-active-document_pointer_lastviewed)
;(make-variable-buffer-local '7zr-active-document_pointer_lastviewed_last)
;(make-variable-buffer-local '7zr-active-document_pointer_lastviewed_2nd_last)
;(make-variable-buffer-local '7zr-active-document_pointer_lastviewed_nearest_ancestor)
;(make-variable-buffer-local '7zr-active-document_pointer_lastviewed_nearest_progeny)
;(make-variable-buffer-local '7zr-active-document_patch_number)

;; HOOKS----------------------------------------
;; add hook to dired mode to view 7z-revisions archives using the z command
(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map (kbd "z")  (lambda () (interactive) (7zr-dired-7z-revisions)))
	    )
	  )

(defcustom 7zr-summary-mode-hook '(7zr-summary-mode-hook-identify)
  "Normal hook run when entering 7zr-summary mode and many related modes."
  :type 'hook
  :options '(7zr-turn-on-hl-line-mode)
  :group '7zr)

(defvar 7zr-summary-mode-variant nil
  "Non-nil if this buffer's major mode is a variant of 7zr-summary mode.
Use (derived-mode-p \\='7zr-summary-mode) instead.")


(defvar 7zr-summary-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\e\t" 'kill-buffer)
    (define-key map (kbd "<RET>") '7zr-summary-view-revision)
    (define-key map (kbd "C-c q") '7zr-summary-quit)
    (define-key map (kbd "q") '7zr-summary-quit)
    (define-key map (kbd "C-c C-a") '7zr-summary-all-diffs-of-region-to-1-buffer)
    (define-key map (kbd "t") '7zr-jump-to-this-timestamp-for-all-7z-buffers)
    (define-key map (kbd "n") '7zr-summary-forward-line)
    (define-key map (kbd "p") '7zr-summary-previous-line)
    (define-key map (kbd "w") '7zr-summary-dow)
    (define-key map (kbd "<down>") '7zr-summary-forward-line)
    (define-key map (kbd "<up>") '7zr-summary-previous-line)
    (define-key map (kbd "v") '7zr-summary-view-revision-note)
    (define-key map (kbd "c") '7zr-notes-annotation)
    (define-key map (kbd "g") '7zr-summary-goto-date)
    (define-key map (kbd "#") '7zr-summary-goto-sha1)
    (define-key map (kbd "C-c c") '7zr-summary-consolidate-region)
    (define-key map (kbd "h") '7zr-view-toggle-highlight-changes)
    (define-key map (kbd "u") '7zr-summary-view-raw-diff-file)
    (define-key map [menu-bar 7zr-summary]
      (cons "7zr-summary" (make-sparse-keymap "7zr-summary")))


    (define-key map [menu-bar 7zr-summary view-revision]
      '(menu-item "View Revision" 7zr-summary-view-revision
                  :help "View the Revision at Point"))
    (define-key map [menu-bar 7zr-summary view-all-diff-files]
      '(menu-item "View all diffs in region" 7zr-summary-all-diffs-of-region-to-1-buffer
                  :help "View all diff files in region to one buffer."))
    (define-key map [menu-bar 7zr-summary view-diff-file]
      '(menu-item "View diff file" 7zr-summary-view-raw-diff-file
                  :help "View the diff file associated with selected revision."))
    (define-key map [menu-bar 7zr-summary goto-sha]
      '(menu-item "Goto sha1 hashvalue" 7zr-summary-goto-sha1
		  :enable (not 7zr-disable-summary-goto-sha1)
                  :help "Find revision pertaining to given sha1sum hash value."))
    (define-key map [menu-bar 7zr-summary goto-date]
      '(menu-item "Goto Date" 7zr-summary-goto-date
                  :help "Goto a particular date or nearest to at any rate."))
    (define-key map [menu-bar 7zr-summary goto-date-all]
      '(menu-item "Goto Date for all 7z buffers" 7zr-jump-to-this-timestamp-for-all-7z-buffers
                  :help "jump-to-this-timestamp-for-all-7z-buffers"))

    (define-key map [menu-bar 7zr-summary sep3] menu-bar-separator)
    (define-key map [menu-bar 7zr-summary consolidate_region]
      '(menu-item "Consolidate Region" 7zr-summary-consolidate-region
                  :help "Consolidate selected region"))
    (define-key map [menu-bar 7zr-summary consolidate_hour]
      '(menu-item "Consolidate Last Hour" 7zr-summary-consolidate-last-hour
                  :help "Consolidate last hours worth of revisions"))
    (define-key map [menu-bar 7zr-summary consolidate_today]
      '(menu-item "Consolidate Todays Changes" 7zr-summary-consolidate-today
                  :help "Consolidate todays revisions"))
    (define-key map [menu-bar 7zr-summary sep2] menu-bar-separator)
    (define-key map [menu-bar 7zr-summary discard_rej]
      '(menu-item "Discard .rej" 7zr-summary_discard_rej
                  :button (:toggle . (bound-and-true-p 7zr-summary_discard_rejp))
                  :help "In the unlikely event that patch function produces .rej files, discard them."))
   (define-key map [menu-bar 7zr-summary highlight_changes]
      '(menu-item "Highlight Changes" 7zr-view-toggle-highlight-changes
                  :button (:toggle . (bound-and-true-p 7zr-view_highlightp))
                  :help "Highlight changes between revisions."))
   (define-key map [menu-bar 7zr-summary toggle-auto-tags]
      '(menu-item "Toggle tag updates on saves" 7zr-toggle-auto-tags
                  :button (:toggle . (bound-and-true-p 7zr-auto-update-tags-in-text-p))
                  :help "updates tags every time buffer is saved."))

   (define-key map [menu-bar 7zr-summary update_revision_number_in_text]
      '(menu-item 7zr-update-7z-revisions-tag-in-text-menu-option 7zr-toggle-update-7z-revisions-tags-in-text
                  :button (:toggle . (bound-and-true-p 7zr-update-7z-revisions-tags-in-textp))
		  :visible nil
                  :help "Update 7z-revisions.el rev tag."))
   (define-key map [menu-bar 7zr-summary edit-note]
     '(menu-item "Edit Note" 7zr-notes-annotation
		 :help "Edit current revision note."))

   (define-key map [menu-bar 7zr-summary view-note]
     '(menu-item "View Note" 7zr-summary-view-revision-note
		 :help "View current revision note."))

   (define-key map [menu-bar 7zr-summary dow]
     '(menu-item "DOW" 7zr-summary-dow
		 :help "What's the day of the week of current line?"))

   (define-key map [menu-bar 7zr-summary forward-line]
     '(menu-item "Forward Line" 7zr-summary-forward-line
		 :help "go down one line"))
   (define-key map [menu-bar 7zr-summary previous-line]
     '(menu-item "Previous Line" 7zr-summary-previous-line
		 :help "go up one line"))

   (define-key map [menu-bar 7zr-summary quit]
      '(menu-item "Quit" 7zr-summary-quit
                  :help "Close all 7zr-revision-buffers"))
    (define-key map [menu-bar 7zr-summary sep] menu-bar-separator)
    map)
  "Keymap for `7zr-summary-mode'.")


(define-derived-mode 7zr-summary-mode nil "7zr-sum"
  "Major mode for humans to read the summary of a 7z archive.
In this mode, hl-mode is active and one can select a revision
to view from a 7z archive.
\\{7zr-summary-mode-map}
Turning on 7z-summary mode runs the normal hook `7zr-summary-mode-hook'."
  (set (make-local-variable '7zr-summary-mode-variant) t)
 ; (hl-line-mode t)
)

;(defalias 'indented-7zr-summary-mode '7zr-summary-mode)

;; This can be made a no-op once all modes that use 7zr-summary-mode-hook
;; are "derived" from 7zr-summary-mode.  

(defun 7zr-summary-mode-hook-identify ()
  "Mark that this mode has run `7zr-summary-mode-hook'."
  (set (make-local-variable '7zr-summary-mode-variant) t))


(defvar 7z-revisions-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f2> C-s") '7zr-save-buffer)
    (define-key map (kbd "<f2> `") '7z-revisions-mode)
    (define-key map (kbd "<f2> <f3>") '7z-revisions)
    (define-key map (kbd "<f2> 3") '7zr-update-7z-revisions-tags-in-text)
    (define-key map (kbd "<f2> t") '7zr-toggle-auto-tags)
    (define-key map (kbd "<f2> l") '7zr-goto-line-of-last-revision)
    (define-key map (kbd "<f2> p") '7zr-line-last-changed-on)
    (define-key map (kbd "<f2> s") '7zr-select-tag-to-insert-into-document)
    (define-key map (kbd "<f2> C-h") '7zr-modify-raw-hash-file)
;    (define-key map (kbd "<f2> C-o") '7zr-order-raw-notes-file)    
    (define-key map (kbd "<f2> C-c") '7zr-modify-raw-notes-file)    
    (define-key map (kbd "<f2> c") '7zr-notes-annotation)
    (define-key map (kbd "<f2> C-r") '7zr-rename-document-and-its-archive)
    (define-key map (kbd "<f2> C-f") '7zr-archive-edit-metadata-file)
    (define-key map (kbd "<f2> C-d") '7zr-input-archive-directory)    
    (define-key map [menu-bar 7z-revisions-mode]
      (cons "7z-revisions" (make-sparse-keymap "7z-revisions")))
    (define-key map [menu-bar 7z-revisions-mode exit-7z-revisions-mode]
      '(menu-item "Exit 7z-revisions-mode" 7z-revisions-mode
                  :help "exits mode"))
    (define-key map [menu-bar 7z-revisions-mode sep6] menu-bar-separator)
    
    (define-key map [menu-bar 7z-revisions-mode edit-metadata-file]
      '(menu-item "Edit Metafile" 7zr-archive-edit-metadata-file
		  :help "Edit the Metadata File (created-by-message file) associated with this document."))
   
    (define-key map [menu-bar 7z-revisions-mode sep5] menu-bar-separator)
    
    (define-key map [menu-bar 7z-revisions-mode set-archive-dir]
      '(menu-item "Set Archive Dir" 7zr-input-archive-directory
    		  :help "Set relative archive directory."))
   
    (define-key map [menu-bar 7z-revisions-mode rename-document-and-its-archive]
      '(menu-item "Rename Document & Archive" 7zr-rename-document-and-its-archive
		  :help "Rename this document, along with its archive; use with caution."))
    
    (define-key map [menu-bar 7z-revisions-mode modify-raw-notes-file]
      '(menu-item "Modify Raw Notes File" 7zr-modify-raw-notes-file
		  :help "Use with caution."))
    
    (define-key map [menu-bar 7z-revisions-mode modify-raw-hash-file]
      '(menu-item "Modify Raw Hash File" 7zr-modify-raw-hash-file
		  :help "Use with caution.  Perhaps best not to use at all!"))
    (define-key map [menu-bar 7z-revisions-mode sep4] menu-bar-separator)
    (define-key map [menu-bar 7z-revisions-mode edit-notes-annotation]
      '(menu-item "Notes Annotation" 7zr-notes-annotation
                  :help "Edit note to be applied to next revision."))
    (define-key map [menu-bar 7z-revisions-mode select-tag-to-insert]
      '(menu-item "Insert Tag" 7zr-select-tag-to-insert-into-document
                  :help "Select a tag to insert into the document"))
    (define-key map [menu-bar 7z-revisions-mode line-last-changed-on]
      '(menu-item "Line last changed on" 7zr-line-last-changed-on
                  :help "When was current line last changed?"))
    (define-key map [menu-bar 7z-revisions-mode goto-line-of-last-revision]
      '(menu-item "Goto last changed line" 7zr-goto-line-of-last-revision
                  :help "Goto the line changed in the last revision"))
    (define-key map [menu-bar 7z-revisions-mode sep3] menu-bar-separator)
    (define-key map [menu-bar 7z-revisions-mode save-buffer]
      '(menu-item "Update Tags & Save Buffer" 7zr-save-buffer
                  :help "Saves buffer, first updating tags"))
    (define-key map [menu-bar 7z-revisions-mode update-tags]
      '(menu-item "Update Tags" 7zr-update-7z-revisions-tags-in-text
                  :help "updates tags"))
    (define-key map [menu-bar 7z-revisions-mode toggle-auto-tags]
      '(menu-item "Auto-update tags on save" 7zr-toggle-auto-tags
                  :button (:toggle . (bound-and-true-p 7zr-auto-update-tags-in-text-p))
                  :help "updates tags every time buffer is saved."))
    (define-key map [menu-bar 7z-revisions-mode sep2] menu-bar-separator)
    (define-key map [menu-bar 7z-revisions-mode 7z-revisions]
      '(menu-item "7z-revisions" 7z-revisions
                  :help "List all revisions in archive"))

    (define-key map [menu-bar 7z-revisions-mode sep] menu-bar-separator)
    map)
  "Keymap for `7z-revisions-mode'.")




(defconst 7zr-minibuffer-input-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'exit-minibuffer)
    (define-key map [menu-bar 7zr-minibuffer-input]
      (cons "7zr-minibuffer-input" (make-sparse-keymap "7zr-minibuffer-input")))
    map)
"Keymap for `7zr-minibuffer-input-mode'.")

(defconst 7zr-metafile-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") '7zr-metafile-exit)
    (define-key map [menu-bar 7zr-metafile]
      (cons "7zr-metafile" (make-sparse-keymap "7zr-metafile")))
    (define-key map [menu-bar 7zr-metafile view-revision]
      '(menu-item "save and exit metafile mode" 7zr-metafile-exit
                  :help "Get out of this buffer"))
    map)
"Keymap for `7zr-metafile-mode'.")

(define-derived-mode 7zr-metafile-mode nil "7zr-meta"
  "Major mode for humans to edit a raw file archived in 7z-revisions
\\{7zr-metafile-mode-map}
Turning on 7zr-metafile-mode runs the normal hook `7zr-metafile-mode-hook'."
  (set (make-local-variable '7zr-metafile-mode-variant) t)
  (message "Press C-c C-c to save and return to last buffer, when finished editing.  You will need to disable read-only on buffer before editing.")
 ; (hl-line-mode t)
)

(defun 7zr-metafile-mode-hook-identify ()
  "Mark that this mode has run `7zr-metafile-mode-hook'."
  (set (make-local-variable '7zr-metafile-mode-variant) t))


(defun 7zr-turn-on-hl-line-mode ()
  (interactive)
  (if (fboundp 'hl-line-mode)
      (hl-line-mode 1))
)


(defun 7zr-toggle-update-7z-revisions-tags-in-text ()
  (interactive)
  (if (eql 7zr-update-7z-revisions-tags-in-textp t)
      (setq 7zr-update-7z-revisions-tags-in-textp nil)
    (setq 7zr-update-7z-revisions-tags-in-textp t)
    )
  )
       

(defun 7zr-trim-l (str)
  (replace-regexp-in-string "^[ \t]+"  "" str) )

(defun 7zr-trim-r (str)
  (replace-regexp-in-string "[ \t]+$"  "" str) )

(defun 7zr-trim (str)
  (7zr-trim-r (7zr-trim-l str))
  )

(defun 7zr-s-trim-whitespace-r (s)
   "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun 7zr-s-trim-whitespace-l (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))

(defun 7zr-s-trim (s)
    "Remove whitespace at the beginning and end of S."
  (s-trim-left (s-trim-right s)))

(defun 7zr-flush-lines-to-double-space-max-line-spacing-for-buffer ()
  (save-excursion
    ;; this probably needs to change for windows
    (goto-char (point-min))
    (while (re-search-forward "\n\n\n")      
      (replace-match "\n\n")
      )
    )
  )

(defun 7zr-summary_discard_rej ()
  (interactive)
  (if 7zr-summary_discard_rejp
      (setq 7zr-summary_discard_rejp nil)
    (setq 7zr-summary_discard_rejp t)
    )
  )

(defun 7zr-encoded-time-gt (x y)
  "is the first tuple greater than the second?"
  (let ((x1 (car x))
	(x2 (car (cdr x)))
	(y1 (car y))
	(y2 (car (cdr y))))
    (if (> x1 y1) 
	t
      (if (and (= x1 y1) (> x2 y2))
	  t 
	nil))
    )
  )


(defun 7zr-encoded-time-gte (x y)
  "is the first tuple greater than or equal to the second?"
  (let ((x1 (car x))
	(x2 (car (cdr x)))
	(y1 (car y))
	(y2 (car (cdr y))))
    (if (> x1 y1) 
	t
      (if (and (= x1 y1) (>= x2 y2))
	  t 
	nil))
    )
  )

(defun 7zr-encoded-time-lt (x y)
  "is the first tuple less than the second?"
  (let ((x1 (car x))
	(x2 (car (cdr x)))
	(y1 (car y))
	(y2 (car (cdr y))))
    (if (< x1 y1) 
	t
      (if (and (= x1 y1) (< x2 y2))
	  t 
	nil))
    )
  )

(defun 7zr-encoded-time-lte (x y)
  "is the first tuple less than or equal to the second?"
  (let ((x1 (car x))
	(x2 (car (cdr x)))
	(y1 (car y))
	(y2 (car (cdr y))))
    (if (< x1 y1) 
	t
      (if (and (= x1 y1) (<= x2 y2))
	  t 
	nil))
    )
  )

(defun 7zr-current-date-time-win ()
  "insert the current date and time into current buffer, the format of which is compatible with windows xp filenames.
Uses `current-date-time-format' for the formatting the date/time."
       (interactive)
       (format-time-string 7zr-date-time-format-win (current-time))
       )
  

(defun 7zr-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time.  Don't use this; only use 7zr-current-date-time-win."
       (interactive)
       (format-time-string 7zr-date-time-format (current-time))
       )

(defun 7zr-encoded-date-todays ()
  (interactive)
  (let (*day* *year* *month* *hour* *minute* *second* current_time)
    (setq current_time (current-time))
    (setq *day* (string-to-number (format-time-string "%e" current_time)))
    (setq *year* (string-to-number (format-time-string "%Y" current_time)))
    (setq *month* (string-to-number (format-time-string "%m" current_time)))
    (encode-time 0 0 0 *day* *month* *year*)
    )
  )


(defun 7zr-encoded-date-now ()
  (interactive)
  (let (*day* *year* *month* *hour* *minute* *second* current_time)
    (setq current_time (current-time))
    (setq *day* (string-to-number (format-time-string "%e" current_time)))
    (setq *year* (string-to-number (format-time-string "%Y" current_time)))
    (setq *month* (string-to-number (format-time-string "%m" current_time)))
    (setq *hour* (string-to-number (format-time-string "%H" current_time)))
    (setq *minute* (string-to-number (format-time-string "%M" current_time)))
    (setq *second* (string-to-number (format-time-string "%S" current_time)))
    
    (encode-time *second* *minute* *hour* *day* *month* *year*)
    )
  )

(defun 7zr-encoded-date-beginning_of_the_month ()
  (interactive)
  (let (*day* *year* *month* current_time)
    (setq current_time (current-time))
    (setq *day* (string-to-number (format-time-string "%e" current_time)))
    (setq *year* (string-to-number (format-time-string "%Y" current_time)))
    (setq *month* (string-to-number (format-time-string "%m" current_time)))
    (encode-time 0 0 0 1 *month* *year*)
    )
  )

(defun 7zr-encoded-date-tomorrows ()
  (interactive)
  (time-add (7zr-encoded-date-todays) (days-to-time 1))
)

(defun 7zr-encoded-date-yesterdays ()
  (interactive)
  (time-subtract (7zr-encoded-date-todays) (days-to-time 1))
)


(defun 7zr-encoded-date-last_mondays ()
  (interactive)
  (time-subtract (7zr-encoded-date-todays) (days-to-time (string-to-number (format-time-string "%u" (current-time)))))
)
				       
(defvar 7zr-match-timestamp "^\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)" 
"Matches the timestamp found in the .7z archive file")

(defvar 7zr-match-timestamp2 "\\(20[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)"
  "regex pattern of timestamp listed from 7zip")

(defvar 7zr-match-timestamp3 "\\(20[0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\) \\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)"
  "regex pattern of timestamp, getting year month day hour minute second individually")

(defvar 7zr-match-timestamp4 "\\(20[0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\) \\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)"
  "regex pattern of timestamp, getting year month day hour minute individually, used in the function 7zr-parse-timestamp3-to-encoded-timetuple")

(defvar 7zr-match-timestamp5 "\\(20[0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)"
  "regex pattern of timestamp, getting year month day individually, used in the function 7zr-parse-timestamp3-to-encoded-timetuple")



(defun 7zr-summary-find-encoded-datetime ( target afterp )
"compare target timestamp tuple to each of the timestamps in the
archive, starting from the bottom and working up, to find out
where it lies."
  (let (timetuple found_a_timestamp lastpoint lastlinenum linenumstring)
    (save-excursion
      (save-match-data
	(goto-char (point-max))
	(if (eql (line-beginning-position) (line-end-position))
	    (forward-char -1))
	(setq lastlinenum (format-mode-line "%l"))
	
;
	(while (and 
		(setq found_a_timestamp (re-search-backward 7zr-match-timestamp2 nil t))
		(setq timetuple (7zr-parse-timestamp3-to-encoded-timetuple (match-string 0)))
		(7zr-encoded-time-gt timetuple target)
		)
	  nil
	  )
	(setq linenumstring (format-mode-line "%l"))

	(if (and (string= linenumstring "2")
		 (not found_a_timestamp)
		 )
	    0
	  (if (string= linenumstring lastlinenum)		   
	      -1
	    (beginning-of-line)
	    (if afterp
		(forward-line)
	      )
	    (point)
	    )
	  )
	)   ; save-match-data
      )  ; save-excursion
    )    ; let
  )



(defun 7zr-parse-timestamp3-to-encoded-timetuple ( dateinq )
  (save-match-data
    (let ((result nil) (y 0) (w 0) (m 0) (d 0) (h 0) (min 0)
	  (7zr-match_years "\\([0-9]+\\)[[:blank:]]*y")
	  (7zr-match_weeks "\\([0-9]+\\)[[:blank:]]*w")
	  (7zr-match_months "\\([0-9]+\\)[[:blank:]]*m")
	  (7zr-match_days "\\([0-9]+\\)[[:blank:]]*d")
	  (7zr-match_hours "\\([0-9]+\\)[[:blank:]]*h")
	  (7zr-match_minutes "\\([0-9]+\\)[[:blank:]]*min")
	  (patterns (list
		     (list "\\([0-9]+\\)[[:blank:]]*y" '(setq y (match-string 1 dateinq)))
		     (list "\\([0-9]+\\)[[:blank:]]*w" '(setq w (match-string 1 dateinq)))
		     (list "\\([0-9]+\\)[[:blank:]]*m" '(setq m (match-string 1 dateinq)))
		     (list "\\([0-9]+\\)[[:blank:]]*d" '(setq d (match-string 1 dateinq)))
		     (list "\\([0-9]+\\)[[:blank:]]*h" '(setq h (match-string 1 dateinq)))
		     (list "\\([0-9]+\\)[[:blank:]]*min" '(setq min (match-string 1 dateinq))))))			  
      (cond
       ((string-match 7zr-match-timestamp3 dateinq)      
	(encode-time (string-to-number (match-string 6 dateinq)) (string-to-number (match-string 5 dateinq)) (string-to-number (match-string 4 dateinq)) (string-to-number (match-string 3 dateinq)) (string-to-number (match-string 2 dateinq)) (string-to-number (match-string 1 dateinq))))
					; else try the timestamp4 pattern

       ((string-match 7zr-match-timestamp4 dateinq)
	(encode-time 0 (string-to-number (match-string 5 dateinq)) (string-to-number (match-string 4 dateinq)) (string-to-number (match-string 3 dateinq)) (string-to-number (match-string 2 dateinq)) (string-to-number (match-string 1 dateinq))))	 	  
					; else try timestamp5 pattern
       ((string-match 7zr-match-timestamp5 dateinq)
	(encode-time 0 0 0 (string-to-number (match-string 3 dateinq)) (string-to-number (match-string 2 dateinq)) (string-to-number (match-string 1 dateinq))))
					; else try the other patterns
       ((dolist (x patterns result)
	  (if (string-match-p x dateinq)
	      (setq result t)))
	(dolist (x patterns)
	  (if (string-match (car x) dateinq)
	      (eval (car (cdr x)))))
	)
       (t
	(message "invalid date!")
	))  ; cond
      )
    )
  )



(defun 7zr-summary-goto-sha1 ()
  (interactive)
  (if 7zr-disable-summary-goto-sha1
      (message "Missing grep.exe, which can be downloaded from http://gnuwin32.sourceforge.net/packages/grep.htm")
    (let (inputted_sha1 actual_sha1 inputted_timetuple sha1point begin end point_saved revision)
      (setq inputted_sha1 (read-string "Enter sha1sum hashvalue:"))

      (save-window-excursion
	(shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " 7zr-prepend-to-hash-file (7zr-shell-quote-argument 7zr-current-original-version)))
	(if (string= "" (setq revision (7zr-s-trim-whitespace-r (shell-command-to-string (concat "grep " (7zr-shell-quote-argument (concat 7zr-temp-directory 7zr-prepend-to-hash-file  7zr-current-original-version)) " -e " inputted_sha1  (7zr-awk-cmd-string 2))))))
	    (message "Hashvalue not found in the hashtable")
	  (setq actual_sha1 (7zr-s-trim-whitespace-r (shell-command-to-string (concat "grep " (7zr-shell-quote-argument (concat 7zr-temp-directory 7zr-prepend-to-hash-file  7zr-current-original-version)) " -e " inputted_sha1  (7zr-awk-cmd-string 3)))))
	  (setq revision (replace-regexp-in-string "\"" "" revision))
	  (save-excursion
	    (goto-char (point-min))
	    (forward-line 1)
	    (setq begin (point))
	    (goto-char (point-max))
	    (beginning-of-line)
	    (forward-line -1)
	    (setq end (point))
	    )
	  (setq point_saved (point))
	  (narrow-to-region begin end)
	  (goto-char (point-min))
	  (unless (search-forward revision nil t)  ; re-search-forward is troublesome because of the . in the revision number
	    (message (concat "Revision " revision " not found!"))
	    (goto-char point_saved)
	    )
	 
	  (widen)
	  
	  (message (concat "Revision " revision " = " actual_sha1))
	  )
	)
      )
    )
  )


(defun 7zr-summary-goto-date ()
  (interactive)
  (let (inputted_date inputted_timetuple datepoint)
    (setq inputted_date (read-string "Enter date:"))
    (unless (string-match-p ":" inputted_date)
      (setq inputted_date (concat inputted_date " 0:00"))
      )
	    (condition-case ex
		(setq inputted_timetuple (7zr-parse-timestamp3-to-encoded-timetuple inputted_date))
        ('error (message (format " Invalid date! " ))))
      (when inputted_timetuple
	(setq datepoint (7zr-summary-find-encoded-datetime inputted_timetuple nil))
	(if (= datepoint -1)
	     (message "date not found")
	  (goto-char datepoint)
	  )
	)
      ) ; let
  
  )


(defun 7zr-summary-dow ()
  (interactive)
  (save-match-data
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward 7zr-match-timestamp3 (line-end-position) t)
	  
	  (message (format-time-string "%a" (7zr-parse-timestamp3-to-encoded-timetuple (match-string-no-properties 0))))
	)
      )
    )
  )

      
(defun 7zr-summary-consolidate-last-hour ()
  (interactive)
  (let (begin end today tomorrow lasthour)
    (when (yes-or-no-p "Consolidate all of the last hours worth of revisions into one diff? ... Srsly?")           
      (setq lasthour (time-subtract (7zr-encoded-date-now) (days-to-time .04166)))
      (setq end (point-max)) 
      (setq begin (7zr-summary-find-encoded-datetime lasthour nil))      
      (if (eql begin -1)
	  (message "There were no revisions over the last hour to consolidate!")
	(7zr-summary-consolidate-region begin end)
	)
      
      )
    )
  )


(defun 7zr-summary-consolidate-today ()
  (interactive)
  (let (begin end today tomorrow)
    (when (yes-or-no-p "Consolidate all of todays revisions into one diff? ... Really?")           
      (setq today (7zr-encoded-date-todays))
      (setq end (point-max)) 
      (setq begin (7zr-summary-find-encoded-datetime today nil))      
      (if (eql begin -1)
	  (message "There were no revisions today to consolidate!")
	(7zr-summary-consolidate-region begin end)
	)
      
      )
    )
  )


(defun 7zr-copy-attrib-modified-datetime ( datetime-reference-file target-file  )
  "Copy only the modified date of datetime-reference-file to that of target-file, similar to shell:touch -r"
  (set-file-times target-file  (car (nthcdr 5 (file-attributes (datetime-reference-file)))))
  )


(defun 7zr-summary-consolidate-region ( from to )
  "Deletes all revision patch files in region, except for the
lowest level patch file and the highest level patch file, which
is updated with a new diff to reflect all changes represented by
the patch files to be deleted in the region, the revision notes of which are all concatonated together and stored under the lowest level revision in the range."
; the files to be diffed are from and to, but from patch will
; not be deleted

  (interactive "r")
  (if (> from to)
      (let ((tem to))
	(setq to from from tem)))
  (let
      ( (abort_function nil) from-patch to-patch froms-nearest-ancestor end-at-latest-version start-at-original-version 7zr-consolidate-line_from point-from-minus-first-line max-patch max-line datetime-reference-timestamp new_note notes_revlist)
    (setq 7zr-summary-consolidate-string "")
    (setq 7zr-patch-command-called-x-times 0)
    (setq 7zr-summary-consolidate-to-minus-last-patch 0)
    (setq 7zr-summary-consolidate-from-minus-1 "")
    (save-excursion
      (save-restriction
	(goto-char from)  ; from will actually be 1 up from selected from
	(beginning-of-line)
	(setq 7zr-consolidate-line_from (line-number-at-pos))
	
	(if (eql 7zr-consolidate-line_from 1)
	    (progn
	      (setq start-at-original-version t)
	      (setq from-patch "0.0")
	      )
	  (progn
	    (setq start-at-original-version nil)
	    (looking-at "\\([0-9]+\.[0-9]+\\)")
	    (setq froms-nearest-ancestor (match-string-no-properties 1))
	    (setq from-patch froms-nearest-ancestor)
	    )
	  )
	(forward-line 1)
	(beginning-of-line)
	(setq point-from-minus-first-line (point))
	(forward-line)
	
	(goto-char to)
	(beginning-of-line)
	(setq end-at-latest-version nil)
	
	(unless (looking-at "\\([0-9]+\.[0-9]+\\)")
	  (setq end-at-latest-version t)
	  (forward-line -1)
	  (looking-at "\\([0-9]+\.[0-9]+\\)")
	  )
	
	(setq to-patch (match-string-no-properties 1))
	(forward-line -1)
	(setq 7zr-consolidate-line_to (line-number-at-pos))
	(setq point-to-minus-last-line (line-end-position))
	
	(goto-char (point-max))
	(beginning-of-line)
	(forward-line -1)
	(setq max-line (line-number-at-pos))
	(when (< max-line 3)
	  (message "Not enough revisions to consolidate.")
	  (setq abort_function t)
	  )
	(looking-at "\\([0-9]+\.[0-9]+\\)")
	(setq max-patch (match-string-no-properties 1))
	
	(narrow-to-region point-from-minus-first-line point-to-minus-last-line)
	(goto-char (point-min))
	(while (re-search-forward "\\([0-9]+\.[0-9]+\\)[[:blank:]]+\\([0-9]+\\)[[:blank:]]+\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)" nil t)
	  (setq 7zr-summary-consolidate-string (concat 7zr-summary-consolidate-string " " (match-string-no-properties 1)))
	  )
	)   ; save-restriction
      ) ; save-excursion
    (if (and (not abort_function)
	     (yes-or-no-p (concat "Consolidate the highlighted diff files, from " from-patch " to " to-patch " ? ")))
	(progn
	  (if  start-at-original-version
	      (progn
		(save-window-excursion
		  (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " "  (7zr-shell-quote-argument 7zr-original-version)))
		  (7zr-rename-file-if-exists (concat 7zr-temp-directory  7zr-original-version) (concat 7zr-temp-directory "rev0.0_of_" 7zr-original-version))

		  )
		)
	    ; else it starts at from-patch
	    (setq notes_revlist (7zr-reconstruct-slow from-patch point-from-minus-first-line))
	    ) ; if start at original version
	  (if end-at-latest-version  ; ends at latest version ?
	      (progn
		(save-window-excursion
		  (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " 7zr-prepend-to-latest-revision (7zr-shell-quote-argument 7zr-original-version)))
		  (setq datetime-reference-timestamp (car (nthcdr 5 (file-attributes (concat 7zr-prepend-to-latest-revision 7zr-original-version ))))) 
;		  (7zr-copy-attrib-modified-datetime (concat 7zr-temp-directory "7zr-datetime-reference")
;						     (concat 7zr-temp-directory 7zr-prepend-to-latest-revision 7zr-original-version)) 
;		  (shell-command  (concat "touch -r " 7zr-temp-directory 7zr-prepend-to-latest-revision 7zr-original-version " " 7zr-temp-directory "7zr-datetime-reference"))
		  (7zr-rename-file-if-exists (concat 7zr-temp-directory 7zr-prepend-to-latest-revision 7zr-original-version) (concat 7zr-temp-directory "rev" to-patch "_of_" 7zr-original-version ))
		  
		  )
		)
	  ; else it ends at to-patch
	    (setq notes_revlist (7zr-reconstruct-slow to-patch to))
	    (setq datetime-reference-timestamp (car (nthcdr 5 (file-attributes (concat 7zr-temp-directory to-patch ))))) 
	    ) ; if

	  (save-window-excursion
	    (shell-command (concat 7zr-diff-command " "  (7zr-shell-quote-argument (concat 7zr-temp-directory "rev" from-patch "_of_"  7zr-original-version)) " "  (7zr-shell-quote-argument (concat 7zr-temp-directory "rev" to-patch "_of_" 7zr-original-version))  " > " (7zr-shell-quote-argument (concat 7zr-temp-directory "tmpdiff"))))
	    (7zr-rename-file-if-exists (concat 7zr-temp-directory "tmpdiff") (concat 7zr-temp-directory to-patch))
		      

	    (shell-command (concat "7z d " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-trim-r (7zr-trim-l 7zr-summary-consolidate-string))))
	    (set-file-times (concat 7zr-temp-directory to-patch) datetime-reference-timestamp)
;	    (7zr-copy-attrib-modified-datetime (concat 7zr-temp-directory 7zr-datetime-reference) (concat 7zr-temp-directory to-patch))
;(shell-command (concat "touch -r " 7zr-temp-directory "7zr-datetime-reference " 7zr-temp-directory to-patch))
	    (shell-command (concat "7z u -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-shell-quote-argument (concat 7zr-temp-directory to-patch))))
	    (7zr-delete-file-if-exists (concat  7zr-temp-directory "7zr-datetime-reference"))

	    (setq new_note (concat "Consolidated from " from-patch " to " to-patch "  on " (7zr-current-date-time-win) " deleting the following diffs: " (7zr-trim-r (7zr-trim-l 7zr-summary-consolidate-string)) ", which had the following notes: " notes_revlist ))
	    (puthash to-patch new_note 7zr-notestab)
	    (7zr-add-string-to-archived-file (concat "(puthash \"" to-patch "\" \"" new_note "\" 7zr-notestab)\n") 7zr-prepend-to-notes-file 7zr-original-version 7zr-notes-file-extension)

	    )
	  (toggle-read-only 0)
	  (kill-region (- point-from-minus-first-line 1) point-to-minus-last-line)
	  (toggle-read-only 1)	    
	
	  )   	    
      )    ;  if yes-or-no-p
    )  ; let
  )  ; 7zr-summary-find-encoded-datetime


(defun 7zr-summary-all-diffs-of-region-to-1-buffer ( from to )
  (interactive "r")
  (if (> from to)
      (let ((tem to))
	(setq to from from tem)))
  (let
      ( (abort_function nil) from-patch to-patch    line_from  point-from-minus-first-line (diffs_stack ()) major-mode-of-file  )
    (setq 7zr-summary-consolidate-string "")


    (setq 7zr-patch-command-called-x-times 0)
    (setq 7zr-summary-consolidate-to-minus-last-patch 0)

    (save-excursion
      (save-restriction
	(goto-char from)  ; from will actually be 1 up from selected from
	(beginning-of-line)
	(setq line_from (line-number-at-pos))
	
	(cond ((= line_from 1)
	       (setq line_from 2)
	       (setq from-patch "first"))  ;; if original version is highlighted, then make from-patch "first" instead
	  
	      ((looking-at "\\([0-9]+\.[0-9]+\\)")
	       (if (string= (setq from-patch (match-string-no-properties 1)) "" )
		   (setq abort_function t)))
	      (t (setq abort_function t))
	      )	    	    
	
;;	(forward-line 1)
	(beginning-of-line)
	(setq point-from-minus-first-line (point))
	(forward-line)
	
	(goto-char to)
	(beginning-of-line)
	
	(unless (looking-at "\\([0-9]+\.[0-9]+\\)")
	  (if (or (= line_from (line-number-at-pos))
		  (= 1         (line-number-at-pos)))
	      (setq abort_function t)  ;; from is at last line
	    (forward-line -1))    ;; else assume that just end is at last line
	  )
	(looking-at "\\([0-9]+\.[0-9]+\\)")		
	(setq to-patch (match-string-no-properties 1))
;;	(forward-line -1)
	(setq 7zr-consolidate-line_to (line-number-at-pos))
	(setq point-to-minus-last-line (line-end-position))
	
	(narrow-to-region point-from-minus-first-line point-to-minus-last-line)
	(goto-char (point-min))
	(while (re-search-forward "\\([0-9]+\.[0-9]+\\)[[:blank:]]+\\([0-9]+\\)[[:blank:]]+\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)" nil t)
	  (setq 7zr-summary-consolidate-string (concat 7zr-summary-consolidate-string " " (match-string-no-properties 1)))
	  (push (list (match-string-no-properties 1) (match-string-no-properties 3) (point)) diffs_stack)  ;; diffs_stack contains rev datetime and pos
	  )
	)   ; save-restriction
      ) ; save-excursion
    (if (and (not abort_function)
	     (yes-or-no-p (concat "view diff files, from " from-patch " to " to-patch " ? ")))
	(progn 
	  (setq mark-active nil)
	  (setq 7zr-all-diffs-buffer (concat "7zr-diffs_from_" from-patch "_to_" to-patch "_of_" 7zr-buffer))
	  (setq 7zr-last-buffer (current-buffer))
	  (set-buffer (get-buffer-create 7zr-all-diffs-buffer))
	  (erase-buffer)
	  (7zr-set-some-buffer-local-vars "7zr-summary-all-diffs-of-region-to-1-buffer" 7zr-last-buffer)
	  (let (diffs_stack_tuple patch_ptr patch_ptr_pos patch_ptr_datetime_str diff_file_begin_pos diff_file_end_pos diff_file_size hunk_header_begin_pos hunk_header_end_pos hunk_header hunk_datetime_str)


	    (setq diffs_stack_tuple (pop diffs_stack))
	    (while diffs_stack_tuple                     ;; for each selected rev
	      (setq patch_ptr (car diffs_stack_tuple))   ;;  rev number selected in summary buffer
	      (setq patch_ptr_datetime_str (car (cdr diffs_stack_tuple)))
	      (setq hunk_header (concat "rev " patch_ptr " on " patch_ptr_datetime_str " : " ))
	      (setq patch_ptr_pos (car (cdr (cdr diffs_stack_tuple))))  ;; pointer to pos of rev number in summary buffer
	      (save-window-excursion
		(shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " patch_ptr))  
		(7zr-rename-file-if-exists (concat 7zr-temp-directory patch_ptr) (concat 7zr-temp-directory "7zr-diff_wip") t)		
		(setq diff_file_begin_pos (point))
		(setq diff_file_size (car (cdr (insert-file-contents (concat 7zr-temp-directory "7zr-diff_wip")))))
		(setq diff_file_end_pos (+ diff_file_begin_pos diff_file_size))
		(7zr-delete-file-if-exists (concat 7zr-temp-directory "7zr-diff_wip"))
	      )
	      (save-restriction
		(narrow-to-region diff_file_begin_pos diff_file_end_pos)
		(goto-char (point-min))
		(while (re-search-forward "^\\([0-9]+\\).*$" nil t)   ;; for each hunk in diff file
		  (setq 7zr-view-raw-diff_line_num (7zr-last-integer-in-string (match-string-no-properties 0)))	    
		  (setq hunk_header_begin_pos (1+ (match-beginning 0)))
		  (setq hunk_header_end_pos (1+ (match-end 0)))
		  (beginning-of-line)
		  (newline)
		  (insert hunk_header)
		  (7zr-eval-string
		   (concat "(make-text-button " 
			   (number-to-string hunk_header_begin_pos) 
			   " "  
			   (number-to-string (+ hunk_header_end_pos (length hunk_header))) 
			   "   'action (lambda(x) (set-buffer \""
			   7zr-revisions_tmpbuffer
			   "\")(goto-char "
			   (number-to-string patch_ptr_pos)
			   ")(beginning-of-line)(7zr-summary-view-revision)(beginning-of-buffer)(forward-line "
			   (number-to-string (1- 7zr-view-raw-diff_line_num))
			   ")(7zr-view_datetime)))"
			   )
		   ) ;; 7zr-eval-string
		  (end-of-line)
		  )
		(goto-char (point-min))
		)  ;; save-restriction
	      (setq diffs_stack_tuple (pop diffs_stack))
	      ) ;; while diffs_stack_tuple
;;	   
	  
	    ) ;; let
	  (switch-to-buffer 7zr-all-diffs-buffer)
	  (beginning-of-buffer)
	  (7zr-summary-view-raw-diff-file-highlight)
	  (set-buffer-modified-p nil)	  
	  (toggle-read-only 1)
	  (when (setq major-mode-of-file (7zr-find-auto-mode))       ;; apply auto-mode 
	    (funcall (symbol-value 'major-mode-of-file)))
	  )   ;; progn
      (message "view diffs aborted")
      ) ;; if
    )  ;; let
  )


(defun 7zr-find-auto-mode ()
  "Finds the mode associated with name of current-buffer by looking it up in the auto-mode-alist"
  (let (extension mode)  
    (setq extension (concat "." (7zr-what-is-extension-of-filename (buffer-name))))
    (if (not (string= extension "."))
	(progn
	  (mapc (lambda (x) 
		  (when (string-match (car x) extension)
		    (setq mode (cdr x))
		    )
		  ) auto-mode-alist)
	  mode
	  )
      ; else
      nil
      )
    )
  )


(defun 7zr-view_datetime ()
  "This function simply prints the date from the variable 7zr-view_date, which is the timestamp of the last viewed diff file, in the minibuffer."
  (interactive)
  (message (concat "Revised on " 7zr-view_date))
  )


(defun 7zr-view-toggle-highlight-changes ()
  " When a revision is being reviewed highlight any changes between
the revision and its nearest ancestor and nearest progeny.  Also
allow for jumping to the next or previous change via the d key or
e key, respectively"
  (interactive)
  (if 7zr-view_highlightp
      (progn
	(setq 7zr-view_highlightp nil)
	(message "Not highlighting changes between revisions anymore.")
	)
    (setq 7zr-view_highlightp t)
    (message "Changes between revisions will be highlighted.")
    )


  )


(defun 7zr-diff-format ()
  "Looks at the 7zr-diff-command variable and returns a string of
what format will be outputted."
  (let (format)
    (cond ((string-match "u" 7zr-diff-command)
	   (setq format "unified"))
	  ((search-match "c" 7zr-diff-command)
	   (setq format "context"))
	  ((search-match "y" 7zr-diff-command)
	   (setq format "side-by-side"))
	  ((search-match "e" yzr-diff-command)
	   (setq format "ed script"))
	  (t (setq format "default"))
	  )
    )
  )
     

(defun 7zr-diff-format-defaultp ()
  "Returns t if the variable 7zr-diff-command specifies a default format"
  (if (and (not (string-match "u" 7zr-diff-command))
	   (not (search-match "c" 7zr-diff-command))
	   (not (search-match "y" 7zr-diff-command))
	   (not (search-match "e" 7zr-diff-command))
	   )
      t
    nil
    )
  )

(defun 7zr-set-some-buffer-local-vars ( window last-buffer )
  "One must call (7zr-summary-view-revision_get-buffer-local-vars) to get the vars set by this function"
  (set (make-local-variable '7zr-active-document) 7zr-buffer)
  (set (make-local-variable '7zr-active-document_buffer_file_coding_system) 7zr-buffer_file_coding_system)
  (set (make-local-variable '7zr-active-document_buffer_is_msdos_p) 7zr-buffer_is_msdos_p)
  (set (make-local-variable '7zr-active-document_archive-name) 7zr-archive-name)
  (set (make-local-variable '7zr-active-document_archive-directory) 7zr-archive-directory)
  (set (make-local-variable '7zr-active-document_archive-directory-full) 7zr-archive-directory)
;  (set (make-local-variable '7zr-active-document_archive-directory-full) 7zr-archive-directory-full)
  (set (make-local-variable '7zr-active-document_archive-prefix) 7zr-archive-prefix)
  (set (make-local-variable '7zr-active-document_patch-number) 7zr-patch-number)
  (set (make-local-variable '7zr-active-document_original-version) 7zr-original-version)
;  (set (make-local-variable '7zr-active-document_view-last-line) 7zr-view-last-line)
;  (set (make-local-variable '7zr-active-document_view-last-column) 7zr-view-last-column)
  (set (make-local-variable '7zr-active-document_7zr-revisions_tmpbuffer) 7zr-revisions_tmpbuffer)
  (set (make-local-variable '7zr-active-document_7zr-revisions_tmpbuffer_buffer) 7zr-revisions_tmpbuffer_buffer)
  (set (make-local-variable '7zr-active-document_7zr-pointer-lastviewed_3rd_last) 7zr-pointer-lastviewed_3rd_last)
  (set (make-local-variable '7zr-active-document_7zr-pointer-lastviewed_2nd_last) 7zr-pointer-lastviewed_2nd_last)
  (set (make-local-variable '7zr-active-document_7zr-pointer-lastviewed_last) 7zr-pointer-lastviewed_last)
  (set (make-local-variable '7zr-active-document_7zr-pointer-lastviewed) 7zr-pointer-lastviewed)
  (set (make-local-variable '7zr-active-document_7zr-summary-rev-at-point) 7zr-summary-rev-at-point)
  (set (make-local-variable '7zr-active-document_7zr-os_type) 7zr-os_type)
  (set (make-local-variable '7zr-viewing) t)
 
  (set (make-local-variable '7zr-active-document_last-buffer) last-buffer)
 
  (set (make-local-variable '7zr-window) window)
  
  )

(defun 7zr-summary-view-revision_get-buffer-local-vars ()
  "One must call (7zr-set-some-buffer-local-vars) to first set the variables that are retrieved by this function"
  (setq 7zr-original-version 7zr-active-document_original-version)
 ; (setq 7zr-buffer 7zr-active-document) 
  (setq 7zr-buffer_file_coding_system 7zr-active-document_buffer_file_coding_system)
  (setq 7zr-buffer_is_msdos 7zr-active-document_buffer_is_msdos_p)
  (setq 7zr-archive-name 7zr-active-document_archive-name)
  (setq 7zr-archive-directory 7zr-active-document_archive-directory  )
  (setq 7zr-archive-prefix 7zr-active-document_archive-prefix )
  (setq 7zr-patch-number 7zr-active-document_patch-number )
;  (setq 7zr-view-last-line 7zr-active-document_view-last-line)
;  (setq 7zr-view-last-column 7zr-active-document_view-last-column )
  (setq 7zr-revisions_tmpbuffer 7zr-active-document_7zr-revisions_tmpbuffer)
  (setq 7zr-pointer-lastviewed_3rd_last 7zr-active-document_7zr-pointer-lastviewed_3rd_last)
  (setq 7zr-pointer-lastviewed_2nd_last 7zr-active-document_7zr-pointer-lastviewed_2nd_last)
  (setq 7zr-pointer-lastviewed_last 7zr-active-document_7zr-pointer-lastviewed_last)
  (setq 7zr-pointer-lastviewed 7zr-active-document_7zr-pointer-lastviewed)
  (setq 7zr-os_type 7zr-active-document_7zr-os_type)  
;  (set (make-local-variable '7zr-active-document_7zr-summary-rev-at-point) 7zr-summary-rev-at-point)
  )
     
(defun 7zr-summary-view-revision ()
  (interactive)
  " Reconstruct and then view the revision pointed to by (point)
   in the 7zr-revisions_tmpbuffer summary buffer and call
   7zr-reconstruct-rev-from-patches, unless we are looking at top
   most or bottom most line, or one up from bottom most line, in
   which case we'll open the files from this function instead of
   calling (7zr-reconstruct-rev-from-patches).  This function is called by the interactive command functions that select the next and previous tuples "
  (7zr-summary-view-revision_get-buffer-local-vars)
  (beginning-of-line)
  (setq 7zr-last-buffer (current-buffer))
  (cond ((looking-at 7zr-original-version)          ;; original version
	 (save-window-excursion
	   (setq 7zr-summary-last-line (line-number-at-pos))
	   (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-shell-quote-argument 7zr-original-version)))  
;;	     (setq 7zr-revisions_lastbuffer (current-buffer))
	   )
	 (setq 7zr-pointer_lastviewed "0.0")
	 (save-excursion
	   (forward-line)
	   (looking-at "\\([0-9]+\.?[0-9]*\\)")	      
	   (setq 7zr-pointer-lastviewed_nearest_progeny (match-string-no-properties 1))
	   )
	 (find-file-read-only (concat 7zr-temp-directory  7zr-original-version))
	 (7z-revisions-mode 0)

	 (setq 7zr-summary-reconst-last (concat 7zr-temp-directory  "rev" 7zr-original-version))
	 (7zr-set-some-buffer-local-vars "7zr-summary-view-revision" 7zr-last-buffer)
	 (set (make-local-variable '7zr-window) "7zr-summary-view-revision")
	 (set (make-local-variable '7zr-active-document_archive-directory-full) 7zr-archive-directory)
	 (when 7zr-view_highlightp
	   (7zr-view-highlight-changes nil t)
	   )
	 (7zr-view-mode))

	((looking-at 7zr-prepend-to-latest-revision)  ;; latest revision
	 (forward-line -1)
	 (save-window-excursion
	   (setq 7zr-summary-last-line (line-number-at-pos))
	   (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-shell-quote-argument (concat 7zr-prepend-to-latest-revision  7zr-original-version))))
	   (7zr-rename-file-if-exists (concat 7zr-temp-directory 7zr-prepend-to-latest-revision 7zr-original-version) (concat 7zr-temp-directory "rev" (number-to-string 7zr-patch-number) "_of_" 7zr-original-version))
	   )
;;	   (setq 7zr-revisions_lastbuffer (current-buffer))
	 
	 (find-file-read-only (concat 7zr-temp-directory  "rev" (number-to-string 7zr-patch-number) "_of_" 7zr-original-version))
	 (7z-revisions-mode 0)
	 (7zr-set-some-buffer-local-vars "7zr-summary-view-revision" 7zr-last-buffer)
	 (setq 7zr-pointer-lastviewed (number-to-string 7zr-patch-number))
	 ;  (7zr-parse-standard-diff-type t)
;	   (7zr-view-local-set-keys)
	 (7zr-view-mode)
	  ;(7zr-summary-reconst-last-del)
	 (setq 7zr-summary-reconst-last (concat 7zr-temp-directory  "rev" (number-to-string 7zr-patch-number) "_of_" 7zr-original-version))
	 (when (bound-and-true-p 7zr-last-column)  ; this temporarily fixes the error that always arises when trying to view revision from the very last line in the summary view.
	   (7zr-goto-last-line-column)
	     )  
	 (7zr-set-some-buffer-local-vars "7zr-summary-view-revision" 7zr-last-buffer)
	 (set (make-local-variable '7zr-active-document_archive-directory-full) 7zr-archive-directory))


	; viewing last revision
	((looking-at (number-to-string 7zr-patch-number))   ;; last revision
	 (save-window-excursion
	   nil
	   (setq 7zr-summary-last-line (line-number-at-pos))
	   (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-shell-quote-argument (concat 7zr-prepend-to-latest-revision  7zr-original-version))))
	   (7zr-rename-file-if-exists (concat 7zr-temp-directory 7zr-prepend-to-latest-revision 7zr-original-version) (concat 7zr-temp-directory "rev" (number-to-string 7zr-patch-number) "_of_" 7zr-original-version) t)
;;	    (setq 7zr-revisions_lastbuffer (current-buffer))
	   )
	 (setq 7zr-pointer-lastviewed (number-to-string 7zr-patch-number))	   
	 (find-file-read-only (concat 7zr-temp-directory  "rev" (number-to-string 7zr-patch-number) "_of_" 7zr-original-version))
	 (7z-revisions-mode 0)
	   ; (7zr-summary-reconst-last-del)
	 (setq 7zr-summary-reconst-last (concat 7zr-temp-directory  "rev" (number-to-string 7zr-patch-number) "_of_" 7zr-original-version))
;	   (when (bound-and-true-p 7zr-last-column)  ; this temporarily fixes the error that always arises when trying to view revision from the very last line in the summary view.
	 (7zr-goto-last-line-column)
;	     )  


	 (7zr-set-some-buffer-local-vars "7zr-summary-view-revision" 7zr-last-buffer)
	 (set (make-local-variable '7zr-active-document_archive-directory-full) 7zr-archive-directory)
         nil
	 (when 7zr-view_highlightp	     
;	   (7zr-parse-standard-diff-type t)
	   (7zr-view-highlight-changes t nil)
	   )

	 (7zr-view-mode))

         ;; viewing any other revision
      	((looking-at "\\([0-9]+\.?[0-9]*\\)[ \t]+[0-9]+[ \t]+\\(.*\\)")
	 (setq 7zr-view_date (match-string-no-properties 2)) 
	 (message 7zr-view_date)
	 (setq 7zr-summary-rev-at-point (match-string-no-properties 1))
	   
	 (7zr-reconstruct-rev-from-patches 7zr-summary-rev-at-point)
	   
;	   (when (bound-and-true-p 7zr-last-column)  ; this temporarily fixes the error that always arises when trying to view revision from the very last line in the summary view.
	 (7zr-goto-last-line-column)
;	     )  

	 (7zr-set-some-buffer-local-vars "7zr-summary-view-revision" 7zr-last-buffer)
	 (set (make-local-variable '7zr-active-document_archive-directory-full) 7zr-archive-directory))
	   
	(t nil)
	)
  )



(defun 7zr-summary-quit ()
  (interactive)
  " quit and kill 7zr-revisions buffers "
  (when (and (bound-and-true-p 7zr-active-document_buffer)
	     (buffer-live-p 7zr-active-document_buffer))
    (setq 7zr-goto-this-line-number (line-number-at-pos))
    (with-current-buffer 7zr-active-document_buffer
      (set (make-local-variable '7zr-active-document_summary-line-number) 7zr-goto-this-line-number)
      )

  
    (7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-original-version))
    (7zr-delete-file-if-exists (concat 7zr-temp-directory "rev" 7zr-pointer-lastviewed "_of_" 7zr-original-version))
    (unless (string= 7zr-pointer-lastviewed "") 
      (7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-pointer-lastviewed))
      )
    ) ; when end
  (kill-buffer)
  )


; (provide '7zr-summary-mode)

;;; 7zr-summary-mode.el ends  -----------------------------------


;;; 7zr-view-mode.el begins ----------------------------------------


(defun 7zr-view_jump_to_next_difference ()
  "This function is called from a key binding of 7zr-view-mode. WARNING: At the moment, this function wont work
properly if there are two files whose revisions are being
reviewed at the same time, while jumping back and forth between
them."
  (interactive)
  (setq 7zr-view_jump_current_buffer (current-buffer))
  (when (setq diff_queue_length (buffer-local-value '7zr-diff-queue_length 7zr-view_jump_current_buffer))  

 ; (setq diff_queue_position 7zr-diff-queue_position)
;  (setq diff_queue_position (buffer-local-value '7zr-diff-queue_position 7zr-view_jump_current_buffer))

    (setq 7zr-view_jump_diff_queue (buffer-local-value '7zr-diff-queue 7zr-view_jump_current_buffer))
    (setq 7zr-view_jump_diff_queuev (vconcat 7zr-view_jump_diff_queue))
    (when (and 
	   (<= (+ 7zr-diff-queue_position 1) diff_queue_length)
	   (> diff_queue_length 0))
      (setq 7zr-diff-queue_position (1+ 7zr-diff-queue_position))
;      (goto-char (aref (buffer-local-value '7zr-diff-queue 7zr-view_jump_current_buffer) (1- 7zr-diff-queue_position)))
;      (goto-char (aref 7zr-diff-queue (1- 7zr-diff-queue_position)))
      (goto-char (aref 7zr-view_jump_diff_queuev (1- 7zr-diff-queue_position)))
      )
    )
  )


(defun 7zr-view_jump_to_previous_difference ()
  "This function is called from a key binding of 7zr-view-mode. WARNING: At the moment, this function wont work
properly if there are two files whose revisions are being
reviewed at the same time, while jumping back and forth between
them."
  (interactive)  
  (setq 7zr-view_jump_current_buffer (current-buffer))
 ; (setq diff_queue_position 7zr-diff-queue_position)
;  (setq diff_queue_position (buffer-local-value '7zr-diff-queue_position 7zr-view_jump_current_buffer))
  (when (setq diff_queue_length (buffer-local-value '7zr-diff-queue_length 7zr-view_jump_current_buffer))
    (setq 7zr-view_jump_diff_queue (buffer-local-value '7zr-diff-queue 7zr-view_jump_current_buffer))
    (setq 7zr-view_jump_diff_queuev (vconcat 7zr-view_jump_diff_queue))
    (when (and 
	   (> (- 7zr-diff-queue_position 1) 0)
	   (> diff_queue_length 0))
      (setq 7zr-diff-queue_position (1- 7zr-diff-queue_position))
;      (goto-char (aref (buffer-local-value '7zr-diff-queue 7zr-view_jump_current_buffer) (1- 7zr-diff-queue_position)))
;      (goto-char (aref 7zr-diff-queue (1- 7zr-diff-queue_position)))
      (goto-char (aref 7zr-view_jump_diff_queuev (1- 7zr-diff-queue_position)))
      )
    )
  )
  
(defun 7zr-not-viewing-p ( window )
  (or
       (not (bound-and-true-p 7zr-viewing))
       (not (bound-and-true-p 7zr-window))
       (not (string= 7zr-window window))
       )
  )


(defun 7zr-view-quit ()
  "This function is called from a key binding of 7zr-view-mode"
  (interactive)

  (if (7zr-not-viewing-p "7zr-summary-view-revision")      
      (message "you must be viewing a 7z-revisions revision to use this function")
; (7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-original-version))
; (7zr-delete-file-if-exists (concat 7zr-temp-directory "rev" 7zr-pointer-lastviewed "_of_" 7zr-original-version))
; (7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-pointer-lastviewed))

    (7zr-save-last-line-column)
    (kill-buffer)
    (switch-to-buffer 7zr-revisions_tmpbuffer)
    )
  )

(defun 7zr-view-quit_view_diff ()
  "This function is called from a key binding of 7zr-view-mode and takes the user from viewing a revision to viewing its raw diff"
  (interactive)
  (let (current_line_num (not_found t) diff_line_num (diff_line_num_highest 0) (diff_line_num_highest_pos 0))
    (setq current_line_num (line-number-at-pos))
    (7zr-save-last-line-column)
    (kill-buffer)
    (set-buffer 7zr-revisions_tmpbuffer)
    (7zr-summary-view-raw-diff-file)
    (beginning-of-buffer)
    (while (re-search-forward "^\\([0-9]+\\).*$" nil t)
      (setq diff_line_num (7zr-last-integer-in-string (match-string-no-properties 0)))
      (if (< diff_line_num_highest current_line_num)
	  (progn 
	    (setq diff_line_num_highest diff_line_num)
	    (setq diff_line_num_highest_pos (point))
	    )
	nil
	)
      )
    (beginning-of-buffer)
    (goto-char diff_line_num_highest_pos)
    (beginning-of-line)
    (7zr-view_datetime)
    )
  )

(defun 7zr-save-last-line-column ()
  (setq 7zr-view-last-line (line-number-at-pos))
  (setq 7zr-view-last-column (current-column))
  (when (bound-and-true-p 7zr-active-document_last-buffer)
    (with-current-buffer 7zr-active-document_last-buffer
      (set (make-local-variable '7zr-last-line) 7zr-view-last-line)
      (set (make-local-variable '7zr-last-column) 7zr-view-last-column)
      )
    )
)

(defun 7zr-goto-last-line-column ()
  (when (bound-and-true-p 7zr-active-document_last-buffer)

	(setq 7zr-view-last-line (buffer-local-value '7zr-last-line 7zr-active-document_last-buffer))
	(setq 7zr-view-last-column (buffer-local-value '7zr-last-column 7zr-active-document_last-buffer)) 
	)
  (7zr-goto-line-column 7zr-view-last-line 7zr-view-last-column)


  )

(defun 7zr-goto-line-column ( line column )
  (goto-char (point-min))
  (forward-line (1- line))
  (if (> (- (line-end-position) (1- (+ column (point)))) 0)
      (forward-char column)
    (end-of-line)
    )
  )

    
(defun 7zr-view_next_page ()
  "This function is called from a key binding of 7zr-view-mode"
  (interactive)
  (if (7zr-not-viewing-p "7zr-summary-view-revision")       
      (message "you must be viewing a 7z-revisions revision to use this function")
    (7zr-save-last-line-column)
    (setq 7zr-last-buffer 7zr-active-document_last-buffer)
    (kill-buffer)
;  (set-buffer 7zr-revisions_tmpbuffer)
    (set-buffer 7zr-last-buffer)
    (forward-line 1)
    (7zr-summary-view-revision) 
    (7zr-goto-last-line-column)
    )
  )


(defun 7zr-view_previous_page ()
  "This function is called from a key binding of 7zr-view-mode"
  (interactive)
  (if (7zr-not-viewing-p "7zr-summary-view-revision")      
      (message "you must be viewing a 7z-revisions revision to use this function")
    (7zr-save-last-line-column)
    (setq 7zr-last-buffer 7zr-active-document_last-buffer)
    (kill-buffer)
;  (set-buffer 7zr-revisions_tmpbuffer)
    (set-buffer 7zr-last-buffer)
    (forward-line -1)
    (7zr-summary-view-revision) 
    (7zr-goto-last-line-column)
    )
  )

(defvar 7zr-view-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c d") '7zr-view_jump_to_next_difference)   
    (define-key map (kbd "C-c e") '7zr-view_jump_to_previous_difference)
    (define-key map (kbd "C-c n") '7zr-view_previous_page)
    (define-key map (kbd "C-c p") '7zr-view_next_page)
    (define-key map (kbd "C-c q") '7zr-view-quit)
    (define-key map (kbd "d") '7zr-view_jump_to_next_difference)   
    (define-key map (kbd "e") '7zr-view_jump_to_previous_difference)
    (define-key map (kbd "p") '7zr-view_previous_page)
    (define-key map (kbd "n") '7zr-view_next_page)
    (define-key map (kbd "w") '7zr-view-raw-diff_dow)
    (define-key map (kbd "t") '7zr-view_datetime)
    (define-key map (kbd "u") '7zr-view-quit_view_diff)
    (define-key map (kbd "q") '7zr-view-quit)
    
    (define-key map [menu-bar 7zr-view]
      (cons "7zr-view" (make-sparse-keymap "7zr-view")))
    (define-key map [menu-bar 7zr-view quit]
      '(menu-item "Quit View" 7zr-view-quit
                  :help "Quit viewing this revision"))
    (define-key map [menu-bar 7zr-view quit7zr_goto_line]
      '(menu-item "Quit 7zr + goto line" 7zr-view_quit7zr_then_goto_line_viewed
		  :help "Quit 7z-revisions and goto currently viewed line"))
   (define-key map [menu-bar 7zr-view quit_view_diff]
      '(menu-item "View diff file" 7zr-view-quit_view_diff
                  :help "View raw diff file of this revision."))
    (define-key map [menu-bar 7zr-view jump_next]
      '(menu-item "Jump to next difference" 7zr-view_jump_to_next_difference
                  :help "Jump to the next highlighted difference in current revision"))
    (define-key map [menu-bar 7zr-view jump_prev]
      '(menu-item "Jump to previous difference" 7zr-view_jump_to_previous_difference
                  :help "Jump to the previous highlighted difference in current revision"))
    (define-key map [menu-bar 7zr-view sep] menu-bar-separator)
    (define-key map [menu-bar 7zr-view dow]
      '(menu-item "DOW" 7zr-view-raw-diff_dow
                  :help "On what day of the week was this revision saved?"))

    (define-key map [menu-bar 7zr-view next_page]
      '(menu-item "View Next Revision" 7zr-view_next_page
                  :help "View next revision in sequence."))
    (define-key map [menu-bar 7zr-view prev_page]
      '(menu-item "View Previous Revision" 7zr-view_previous_page
                  :help "View previous revision in sequence."))
    (define-key map [menu-bar 7zr-view 7zr-view_datetime]
      '(menu-item 7zr-view_date 7zr-view_datetime
                  :help "View datetime of current revision."))
    map)
  "Keymap while 7zr-view-mode is active.")

;;;###autoload
(define-minor-mode 7zr-view-mode
  "A temporary minor mode to be activated only when viewing a revision from a 7z-revisions archive."
  nil
  :lighter " 7zrv"
  7zr-view-mode-map)

(provide '7zr-view-mode)

;;;;;; 7zr-view-mode.el ends --------------------------------------------

;;;;;; 7zr-view-raw-diff-mode.el begins

(defun 7zr-view-raw-diff-quit ()
  (interactive)
  " kill current buffer "
; (7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-original-version))
; (7zr-delete-file-if-exists (concat 7zr-temp-directory "rev" 7zr-pointer-lastviewed "_of_" 7zr-original-version))
  (kill-buffer)
  (set-buffer 7zr-revisions_tmpbuffer)
  (7zr-delete-file-if-exists 7zr-pointer-lastviewed_raw_diff_file)
  )


(defun 7zr-view_quit7zr_then_goto_line_viewed ()
  " Quit 7z-revisions and then try to place point at the line corresponding to the last line viewed from 7z-revisions."
  (interactive)
  (7zr-save-last-line-column)
  (setq 7zr-last-buffer 7zr-active-document_7zr-revisions_tmpbuffer)
  (kill-buffer)
;  (set-buffer 7zr-revisions_tmpbuffer)
  (set-buffer 7zr-last-buffer)
  (setq 7zr-goto-this-line-number (line-number-at-pos))
  (7zr-view-raw-diff_line_number_change_to_end  7zr-view-last-line )
  (set (make-local-variable '7zr-active-document_summary-line-number) 7zr-goto-this-line-number)
;  (7zr-summary-quit)
  )

(defun 7zr-last-integer-in-string ( string )
  "Return the last integer found in string passed as parameter,
and is used to find the line number referenced by a diff file.
This function is called by (7zr-view-raw-diff-quit_then_view_revision), (7zr-summary-view-raw-diff-file), (7zr-goto-line-of-last-revision)."
  (let (newstring last_integer_reversed)
    (setq newstring (concat (nreverse (append (vconcat string) nil))))   ;; reverses a string
    (setq last_integer_reversed (replace-regexp-in-string "\\([0-9]*\\).*" "\\1" newstring))
    (string-to-number (concat (nreverse (append (vconcat last_integer_reversed) nil))))
    
    )
)

(defun 7zr-view-raw-diff-quit_then_view_revision ()
"This function is called from a key binding of 7zr-view-raw-diff-file-mode"
  (interactive)
  " kill current buffer "
; (7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-original-version))
; (7zr-delete-file-if-exists (concat 7zr-temp-directory "rev" 7zr-pointer-lastviewed "_of_" 7zr-original-version))
  (beginning-of-line)
;;;  (looking-at ".*")  ; uncomment the 3 comment marks before each statement in this function to allow the view raw diffs screen to jump to the view revision line number instead of the 7zr-view-last-line number, in which case the buttons created become redundant
;;;  (setq 7zr-view-raw-diff_line_num (7zr-last-integer-in-string (match-string-no-properties 0)))   
  (kill-buffer)
  (set-buffer 7zr-revisions_tmpbuffer)
  (7zr-delete-file-if-exists 7zr-pointer-lastviewed_raw_diff_file)
  (7zr-summary-view-revision)
  (goto-line 7zr-summary-last-line)
 ;;; (forward-line (1- 7zr-view-raw-diff_line_num))
  (7zr-view_datetime)
  )

(defun 7zr-view-raw-diff_line_number_change_to_end ( linenum )
  "Quit 7z-revisions and then try to goto the line in your document corresponding to the last line viewed from 7z-revisions, or the line in your document corresponding to the change hunk that was at point, in the case where you were viewing a raw diff file"
  (let ((iter 0))
    (setq 7zr-view-last-line linenum) 
    (while (and (not (looking-at 7zr-prepend-to-latest-revision))
		(looking-at "\\([0-9]+\.?[0-9]*\\)[ \t]+[0-9]+[ \t]+\\(.*\\)")
		)
      
      (when (> iter 0)
	(save-window-excursion
;;	  (when (bound-and-true-p 7zr-last-line)
;;	    (setq 7zr-view-last-line 7zr-last-line))
	  (setq 7zr-view_date (match-string-no-properties 2)) 
	  (setq 7zr-summary-rev-at-point (match-string-no-properties 1))
	  (setq 7zr-pointer-lastviewed_raw_diff_file (concat 7zr-temp-directory 7zr-prepend-to-diff-file 7zr-summary-rev-at-point "_of_" 7zr-original-version))
	  (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " 7zr-summary-rev-at-point))
	  (rename-file (concat 7zr-temp-directory 7zr-summary-rev-at-point) 7zr-pointer-lastviewed_raw_diff_file t)
      
;	   (setq 7zr-revisions_lastbuffer (current-buffer))

	  (setq 7zr-view-last-line (car (7zr-new-linenum-after-diff 7zr-view-last-line 7zr-pointer-lastviewed_raw_diff_file " ")))
	  (7zr-delete-file-if-exists 7zr-pointer-lastviewed_raw_diff_file)
	  )
	)
      (forward-line 1)
      (setq iter (1+ iter))
      ) ;while
    (kill-buffer)
    (switch-to-buffer 7zr-buffer)
    (7zr-goto-last-line-column)
  
    ) ;let
  )

(defun 7zr-view-raw-diff_next_page ()
"This function is called from a key binding of 7zr-view-raw-diff-file-mode"
  (interactive)
;  (setq 7zr-last-buffer 7zr-active-document_7zr-revisions_tmpbuffer_buffer)
  (setq 7zr-last-buffer 7zr-active-document_last-buffer)
  (kill-buffer)
;  (set-buffer 7zr-revisions_tmpbuffer)
  (set-buffer 7zr-last-buffer)
  (forward-line 1)
  (7zr-delete-file-if-exists 7zr-pointer-lastviewed_raw_diff_file)
  (7zr-summary-view-raw-diff-file) 
;  (setq 7zr-view-last-line (car (7zr-new-linenum-after-diff 7zr-view-last-line 7zr-pointer-lastviewed_raw_diff_file 7zr-reconstruct-dtag)))
  )

(defun 7zr-view-raw-diff_previous_page ()
"This function is called from a key binding of 7zr-view-raw-diff-file-mode"
  (interactive)
;  (setq 7zr-last-buffer 7zr-active-document_7zr-revisions_tmpbuffer_buffer)
  (setq 7zr-last-buffer 7zr-active-document_last-buffer)
  (kill-buffer)
;  (set-buffer 7zr-revisions_tmpbuffer)
 (set-buffer 7zr-last-buffer) 
 (forward-line -1)
  (7zr-delete-file-if-exists 7zr-pointer-lastviewed_raw_diff_file)
  (7zr-summary-view-raw-diff-file) 

  )

(defun 7zr-view-raw-diff_next_change_hunk ()
  "This function is called from a key binding of 7zr-view-raw-diff-file-mode"
  (interactive)
  (forward-line 1)
  (beginning-of-line)
  (if (re-search-forward "^\\([0-9]+\\).*$" nil t)
      (beginning-of-line)
    (forward-line -1)
    )
  )

(defun 7zr-view-raw-diff_prev_change_hunk ()
  "This function is called from a key binding of 7zr-view-raw-diff-file-mode"
  (interactive)
  (re-search-backward "^\\([0-9]+\\).*$" nil t)
  )

(defun 7zr-view-raw-diff_save-edited-diff-file ()
  "This function does not work yet.  Save raw diff file currently being edited.  This function is not working yet."
;  (interactive)
  (if (or 
       (not (bound-and-true-p 7zr-viewing))       
       (not (bound-and-true-p 7zr-window))
       (not (string= 7zr-window "7zr-summary-view-raw-diff-file"))
       )
      (message "Cannot save raw diff file from here")
					; else
    (let ((save_pwd default-directory) rev_string)
      (when (bound-and-true-p 7zr-active-document_7zr-summary-rev-at-point)
	(setq rev_string 7zr-active-document_7zr-summary-rev-at-point)
	(rename-file (buffer-name (current-buffer)) rev_string)
	(7zr-update-a-file-from-temp-directory-to-archive "" rev_string)
	(toggle-read-only 1)
	
	)
      
      )
    )
  )
    

(defun 7zr-view-raw-diff-quit7zr_then_goto_line ()
  (interactive)
  (beginning-of-line)
  (unless (looking-at "[0-9]")   ; first somehow make sure that we are looking at a diff header
    (7zr-view-raw-diff_prev_change_hunk))
  (looking-at ".*")
  (setq 7zr-view-raw-diff_line_num (7zr-last-integer-in-string (match-string-no-properties 0)))
  (setq 7zr-last-buffer 7zr-active-document_7zr-revisions_tmpbuffer)
  (kill-buffer)
;  (set-buffer 7zr-revisions_tmpbuffer)
  (set-buffer 7zr-last-buffer)
  (setq 7zr-goto-this-line-number (line-number-at-pos))
  (setq 7zr-view-last-column 0)
;;  (set (make-local-variable '7zr-last-line) 7zr-goto-this-line-number)
  (7zr-view-raw-diff_line_number_change_to_end  (1- 7zr-view-raw-diff_line_num) )
  (set (make-local-variable '7zr-active-document_summary-line-number) 7zr-goto-this-line-number)

  )

(defun 7zr-eval-string (str)
  "Read and evaluate all forms in str, returning results as a
list.  Do not embed variables into the 'action method of the a
button using this function; instead only their respective plain
text or number values"
  (let ((next 0)
        ret)
    (condition-case err
        (while t
          (setq ret (cons (funcall (lambda (ret)
                                     (setq next (cdr ret))
                                     (eval (car ret)))
                                   (read-from-string str next))
                          ret)))
      (end-of-file))
    (nreverse ret)))


(defun 7zr-summary-view-raw-diff-file-highlight ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^<" nil t) 
      (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face '(:background "red" :foreground "black"))

      )
    (goto-char (point-min))
    (while (re-search-forward "^>" nil t) 
      (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face '(:background "green" :foreground "black"))

      )
    )
  )
		       
(defun 7zr-view-raw-diff_dow ()
  (interactive)
    (if (and 7zr-view_date
	     (not (string= 7zr-view_date "")))
	(message (format-time-string "%a" (7zr-parse-timestamp3-to-encoded-timetuple 7zr-view_date)))
      )
    )



(defun 7zr-summary-view-raw-diff-file ()
  "View the raw diff file for selected revision number.
This function is called from a key binding of j in 7zr-summary-mode,
as well as 7zr-view-mode.  The revision number is stored in the
global variable 7zr-summary-rev-at-point.  The actual diff file
filename is stored in 7zr-pointer-lastviewed_raw_diff_file"
  (interactive)
  (beginning-of-line)
  (if (< 7zr-active-document_7zr-revisions_tmpbuffer_lines 3)
      (error "There are no diffs in archive to view!")
					; else
    (cond 
     ((looking-at 7zr-original-version)  ;; if first row is highlighted, view first diff instead
      (forward-line 1)
      (7zr-summary-view-raw-diff-file))
     ((looking-at 7zr-prepend-to-latest-revision) ;; if last row highlighted, view last diff instead
	(forward-line -1)
	(7zr-summary-view-raw-diff-file))

     ;; viewing any  diff file
     ((looking-at "\\([0-9]+\.?[0-9]*\\)[ \t]+[0-9]+[ \t]+\\(.*\\)")
      (setq 7zr-view_date (match-string-no-properties 2)) 
      (save-window-excursion
	(7zr-summary-view-revision_get-buffer-local-vars)
	(setq 7zr-summary-rev-at-point (match-string-no-properties 1))
	(setq 7zr-pointer-lastviewed_raw_diff_file (concat 7zr-temp-directory 7zr-prepend-to-diff-file 7zr-summary-rev-at-point "_of_" 7zr-original-version))
	(shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " 7zr-summary-rev-at-point))
	(rename-file (concat 7zr-temp-directory 7zr-summary-rev-at-point) 7zr-pointer-lastviewed_raw_diff_file t)
	  )
;	   (setq 7zr-revisions_lastbuffer (current-buffer))
      (setq 7zr-last-buffer (current-buffer))
      (find-file 7zr-pointer-lastviewed_raw_diff_file)
      (7z-revisions-mode 0)
      (7zr-set-some-buffer-local-vars "7zr-summary-view-raw-diff-file" 7zr-last-buffer)
	   ;; make text buttons of each hunk which can be clicked to go to its respective context, in lieu of the r keybind
      (beginning-of-buffer)
      (while (re-search-forward "^\\([0-9]+\\).*$" nil t)
	(setq 7zr-view-raw-diff_line_num (7zr-last-integer-in-string (match-string-no-properties 0)))	    
	(7zr-eval-string
	 (concat "(make-text-button " 
		 (number-to-string (match-beginning 0)) 
		 " "  
		 (number-to-string (match-end 0)) 
		 "   'action (lambda(x) (kill-buffer)(7zr-delete-file-if-exists \"" 
		 7zr-pointer-lastviewed_raw_diff_file
		 "\")(set-buffer \""
		 7zr-revisions_tmpbuffer
		 "\")(7zr-summary-view-revision)(beginning-of-buffer)(forward-line "
		 (number-to-string (1- 7zr-view-raw-diff_line_num))
		 ")(7zr-view_datetime)))"
		 )
	 )
	)
	  
      (beginning-of-buffer)
;	   (show-point-mode t)
      (7zr-summary-view-raw-diff-file-highlight)
      (7zr-view-raw-diff-file-mode 1)
      (set-buffer-modified-p nil)
      (toggle-read-only 1)
      (message (concat 7zr-view_date " " (gethash 7zr-summary-rev-at-point (buffer-local-value '7zr-notestab 7zr-active-document_last-buffer)))))
     (t nil)
     ) ; cond
    )
  )

(defvar 7zr-view-raw-diff-file-mode-map 
  (let ((map (make-sparse-keymap)))
    
    (define-key map (kbd "C-c p") '7zr-view-raw-diff_previous_page)
    (define-key map (kbd "C-c n") '7zr-view-raw-diff_next_page)
    (define-key map (kbd "C-c d") '7zr-view-raw-diff_next_change_hunk)   
    (define-key map (kbd "C-c e") '7zr-view-raw-diff_prev_change_hunk)
    (define-key map (kbd "C-c g") '7zr-view-raw-diff-quit7zr_then_goto_line)      
    (define-key map (kbd "C-c q") '7zr-view-raw-diff-quit)
    (define-key map (kbd "p") '7zr-view-raw-diff_previous_page)
    (define-key map (kbd "n") '7zr-view-raw-diff_next_page)
    (define-key map (kbd "d") '7zr-view-raw-diff_next_change_hunk)   

    (define-key map (kbd "e") '7zr-view-raw-diff_prev_change_hunk)

    (define-key map (kbd "t") '7zr-view_datetime)
    (define-key map (kbd "w") '7zr-view-raw-diff_dow)
    (define-key map (kbd "g") '7zr-view-raw-diff-quit7zr_then_goto_line)      
    
    (define-key map (kbd "r") '7zr-view-raw-diff-quit_then_view_revision)
    (define-key map (kbd "q") '7zr-view-raw-diff-quit)

    (define-key map [menu-bar 7zr-view-raw-diff]
      (cons "7zr-view-raw-diff" (make-sparse-keymap "7zr-view-raw-diff")))
    (define-key map [menu-bar 7zr-view-raw-diff quit]
      '(menu-item "Quit View" 7zr-view-raw-diff-quit
                  :help "Quit viewing this raw diff file."))
    (define-key map [menu-bar 7zr-view-raw-diff quit7zr]
      '(menu-item "Quit 7z-revisions" 7zr-view-raw-diff-quit7zr_then_goto_line
                  :help "Quit 7z-revisions and goto line associated with diff."))
    (define-key map [menu-bar 7zr-view-raw-diff quit-view-revision]
      '(menu-item "View revision" 7zr-view-raw-diff-quit_then_view_revision
                  :help "Switch to revision view."))
    (define-key map [menu-bar 7zr-view-raw-diff sep2] menu-bar-separator)
    (define-key map [menu-bar 7zr-view-raw-diff next_diff]
      '(menu-item "Next hunk" 7zr-view-raw-diff_next_change_hunk
                  :help "Skip to next change hunk."))
    (define-key map [menu-bar 7zr-view-raw-diff prev_diff]
      '(menu-item "Prev hunk" 7zr-view-raw-diff_prev_change_hunk
                  :help "Skip to previous change hunk."))

   (define-key map [menu-bar 7zr-view-raw-diff sep] menu-bar-separator)
    (define-key map [menu-bar 7zr-view-raw-diff next_page]
      '(menu-item "View Next Raw Diff" 7zr-view-raw-diff_next_page
                  :help "View next raw diff in the sequence."))
    (define-key map [menu-bar 7zr-view-raw-diff prev_page]
      '(menu-item "View Previous Raw Diff" 7zr-view-raw-diff_previous_page
                  :help "View previous raw diff in the sequence."))
    (define-key map [menu-bar 7zr-view-raw-diff 7zr-view-raw-diff_datetime]
      '(menu-item "Datetime of diff" 7zr-view_datetime
                  :help "View datetime of current diff."))
    map)
  "Keymap while 7zr-view-raw-diff-file-mode is active.")

;;;###autoload
(define-minor-mode 7zr-view-raw-diff-file-mode
  "A temporary minor mode to be activated only when viewing a 7z-revisions diff file."
  nil
  :lighter " 7zrvrd"
  7zr-view-raw-diff-file-mode-map)

(provide '7zr-view-raw-diff-file-mode)

;;;;;; 7zr-view-raw-diff-mode.el ends

;;;;;; 7z-revisions.el begins ---- ----------------------------------

;(if (fboundp 'hl-line-mode)
;    (require 'hl-line+))
;(setq debug-on-error t)
;(hl-line-mode 0)
(defgroup 7z-revisions-mode nil
  "Customization options for `7z-revisions-mode'."
  :group 'external)


(defcustom 7zr-shell-and " && "
  "How to join commands together in the shell. For fish shell,
  you want to customise this to: \" ; and \" instead of the default."
  :tag "Join shell commands"
  :group '7z-revisions-mode
  :type 'string)

(defvar 7zr-date-time-format "%Y-%m-%d_%H:%M:%S"
  "Format of date to func
See help of `format-time-string' for possible replacements")

(defvar 7zr-date-time-format-win "%Y-%m-%d_%H%M%S"
  "Format of date to func, compatible with windows filenames
See help of `format-time-string' for possible replacements")



(defun 7zr-string-starts-with-p (string prefix)
    "Return t if STRING starts with PREFIX."
    (and
     (string-match (rx-to-string `(: bos ,prefix) t)
                   string)
     t))

(defun 7zr-string-ends-with-p (string suffix)
  "Return t if STRING ends with SUFFIX."
  (and (string-match (rx-to-string `(: ,suffix eos) t)
                     string)
       t))


 (defun 7zr-toggle-highlight ()
   (interactive)
   (if (7zr-string-starts-with-p (buffer-name (current-buffer)) "7z-revisions_of_")
       (hl-line-mode 1)
     (hl-line-mode 0)
     )
   )

(defun 7zr-toggle-auto-tags ()
   (interactive)
   (if 7zr-auto-update-tags-in-text-p
       (progn
	 (setq 7zr-auto-update-tags-in-text-p nil)
	 (setq 7zr-update-7z-revisions-tags-in-textp nil)
	 (message "tags are no longer automatically updated on save")
	 )
     (setq 7zr-auto-update-tags-in-text-p t)
     (setq 7zr-update-7z-revisions-tags-in-textp t)
     (message "tags are now automatically updated on each save")
     )
   )





(defun 7zr-create-file-for-archive-created-by-message (&optional NotCalledInteractively)
  (interactive)
  (7zr-populate-initial-global-vars)
  (7zr-get-highest-rev-and-original-version)

  (let (7zr-create-blank-file-tmpbuffer 7zr-save-current-buffer archive_name document_name)

    (setq 7zr-save-current-buffer (current-buffer))
					;    (setq 7zr-create-blank-file-tmpbuffer (generate-new-buffer-name (concat "7zr-create-blank-file_of_" (file-name-nondirectory (buffer-file-name (current-buffer))) )))
    (setq document_name 7zr-buffer)
    (setq archive_name (concat 7zr-archive-directory 7zr-archive-prefix document_name 7zr-archive-extension))
    
    (setq 7zr-create-blank-file-tmpbuffer 7zr-archive-created-by-message)
    (if (file-exists-p archive_name)
	(progn
	  (get-buffer-create 7zr-create-blank-file-tmpbuffer)
	  (set-buffer 7zr-create-blank-file-tmpbuffer)
	  (if NotCalledInteractively
	      (insert (concat 7zr-update-7z-revisions-tag-in-metadata-file_rev (number-to-string (1- 7zr-patch-number))))  ; subtract one because it was incremented in 7zr-get-highest-rev
	    (insert (concat 7zr-update-7z-revisions-tag-in-metadata-file_rev (number-to-string 7zr-patch-number)))
	    )
	  (newline)
	  (insert (concat 7zr-update-7z-revisions-tag-in-metadata-file_original-version 7zr-original-version))
	  (newline)
	  (insert (concat 7zr-update-7z-revisions-tag-in-metadata-file_document_name 7zr-buffer))
	  (newline)
	  (insert (concat 7zr-update-7z-revisions-tag-in-metadata-file_archive-created_datetime 7zr-archive-created_datetime))
	  (newline)
;	  (insert (concat 7zr-update-7z-revisions-tag-in-metadata-file_archive-prefix 7zr-archive-prefix))
;	  (newline)
;	  (insert (concat 7zr-update-7z-revisions-tag-in-metadata-file_archive-extension 7zr-archive-extension))
;	  (newline)
	  (if (equal 7zr-track-md5sum-hashes-p t)
	      (setq 7zr-track-md5sum-hashes_string "t")
	    (setq 7zr-track-md5sum-hashes_string "nil")
	    )
	  (insert (concat 7zr-update-7z-revisions-tag-in-metadata-file_track-md5sum-hashes 7zr-track-md5sum-hashes_string))
	  (newline)
	  (unless (string= 7zr-archive-directory "")
	    (insert (concat 7zr-update-7z-revisions-tag-in-metadata-file_directory-of-archive 7zr-archive-directory))
	    (newline)
	    )
	  (unless (string= 7zr-directory-of-document "")
	    (insert (concat 7zr-update-7z-revisions-tag-in-metadata-file_directory-of-document 7zr-directory-of-document))
	    (newline)
	    )
	  (insert (concat 7zr-update-7z-revisions-tag-in-metadata-os_type (symbol-name system-type)))
	  (newline)
	  (insert (concat 7zr-update-7z-revisions-tag-in-metadata-buffer_file_coding_system  7zr-buffer-file-coding-system))
	  (newline)
	  (insert (concat "7z-revisions-version=" (number-to-string 7z-revisions-version)))
	  (newline)
	  
	  (write-file (concat 7zr-temp-directory 7zr-archive-created-by-message))
	  (kill-buffer 7zr-create-blank-file-tmpbuffer)
	  (set-buffer 7zr-save-current-buffer)

	  (shell-command (concat "7z a " (7zr-shell-quote-argument  archive_name) " " (7zr-shell-quote-argument (concat 7zr-temp-directory 7zr-archive-created-by-message))))
	  )
      
      (message (concat "There first needs to exist an archive named " archive_name " before the 7zr-archive-created-by-message file, entitled '" 7zr-archive-created-by-message  "', can be added to it!"))
      )
    )
  )

; note colors
(setq 7zr-color->number 
      '(("" . 0)
	("red" . 1)
        ("orange" . 2)
        ("yellow" . 3)
        ("green" . 4)
	("blue" . 5)
	("indigo" . 6)
	("violet" . 7)))


;(cdr (assoc "none" 7zr-color->number))

(defun 7zr-input-note ( color )
  "prompt for input of revision note, to be added on next save"
  (interactive "P")
  (setq 7zr-note (read-string (concat "Enter your " (car (rassoc color 7zr-color->number )) " note:" )))
;  (message "%s" 7zr-revision-note)
  )

(defun 7zr-input-note-color ()
  "Prompt user to pick a choice from a list."
  (interactive)
  (let ((choices (mapcar #'car 7zr-color->number)))
    (setq 7zr-note-color 
	  (cdr (assoc (ido-completing-read "note color:" choices ) 7zr-color->number))
	  )
    )
  )

(defun 7zr-notes-annotation ()
  "Edit note on revision"
  (interactive)
  (let (new_note old_note rev_string rev_retval original-version)
    (setq 7zr-last-buffer (current-buffer))
    (cond 
     ((bound-and-true-p 7zr-viewing)  ; i.e. if we are in 7z-revisions listing or view summary edit current note
      (message "1")
      (when (and
	     (string= 7zr-window "7z-revisions")
	     (looking-at "\\([0-9]+\.?[0-9]*\\)")
	     )
	(setq rev_string (match-string-no-properties 1))
	)
      (when   (or
	       (string= 7zr-window "7zr-summary-view-revision")
	       (string= 7zr-window "7zr-summary-view-raw-diff-file")
	       )
	(setq rev_string 7zr-summary-rev-at-point)
	)
      (setq original-version 7zr-active-document_original-version)
      (setq new_note (7zr-edit-note rev_string (gethash rev_string 7zr-notestab)))
      (puthash rev_string new_note 7zr-notestab)
      (7zr-add-string-to-archived-file (concat "(puthash \"" rev_string "\" \"" new_note "\" 7zr-notestab)\n") 7zr-prepend-to-notes-file original-version 7zr-notes-file-extension)

      )
     ((bound-and-true-p 7z-revisions-mode)  ; enter a note for next revision
      (message "2")
      (if (bound-and-true-p 7zr-active-document_note)
	  (setq old_note 7zr-active-document_note)
	(setq old_note "")
	)
      (setq new_note (7zr-edit-note "(next revision)" old_note))
      (when new_note						    
	(set (make-local-variable '7zr-active-document_note) new_note)
	)
      )
      
    
     (t
      (message "The document must have 7zr-revisions-mode enabled before a note can be annotated for the next revision.")
      )
     ) ; cond
    ) ; let
  )

(defun 7zr-edit-note ( rev_string default_note )
    (7zr-escape-quotes (read-from-minibuffer (concat "Edit note rev " rev_string ", C-c C-c when finished:\n") default_note 7zr-minibuffer-input-mode-map))
    )

(defun 7zr-escape-quotes ( notes_string)
  (replace-regexp-in-string "\"" "\\\\\"" notes_string)
  )

(defun 7zr-view-raw-notes-file ()
  "dont use this function, for testing only"
  
   (7zr-populate-initial-global-vars)
   (7zr-get-highest-rev-and-original-version)
   (find-file-read-only (7zr-extract-a-file-to-temp-directory 7zr-prepend-to-notes-file 7zr-original-version 7zr-notes-file-extension))
   (7z-revisions-mode 0)
  ) 

(defun 7zr-create-blank-notes-file-and-add-to-archive ()
  "7zr-prepend-to-notes-file 7zr-notes-file-extension."
  (let (7zr-create-blank-file-tmpbuffer
	7zr-saved-current-buffer
	(notes_file (concat 7zr-temp-directory  7zr-prepend-to-notes-file 7zr-original-version 7zr-notes-file-extension ))	
	)    
    (setq 7zr-saved-current-buffer (current-buffer))
    (setq 7zr-create-blank-file-tmpbuffer (generate-new-buffer-name notes_file))
    (get-buffer-create 7zr-create-blank-file-tmpbuffer)
    (set-buffer 7zr-create-blank-file-tmpbuffer)
    (insert (concat ";; 7z-revisions.el notes file for " 7zr-original-version))  ;; on windows, 7z requires that the file extracted must not be blank,  or it seems to ring an error, so we must add this line.
    (newline)
    (write-file notes_file )
    (kill-buffer)
    (set-buffer 7zr-saved-current-buffer)
    (shell-command (concat 7zr-add-to-archive-command " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " notes_file))
    (7zr-delete-file-if-exists notes_file)
    )
  )


(defun 7zr-examine-created-by-message-file ()
  "This function is called in place of (7zr-examine-archive), and gets the 7zr-patch-number out of the 7zr-created-by-message, creating one if there isnt one, after performing checks to see if there exists an archive.  This function is actually just a wrapper for (7zr-created-by-message-file_increment_latest_revision_number)"

  

  (7zr-populate-initial-global-vars)

;(string-match "No files to process" (shell-command-to-string (concat "7z e -aoa -o" 7zr-temp-directory " "  7zr-archive-name  " " (7zr-shell-quote-argument 7zr-archive-created-by-message)))

  ; first see if there's no existing archive
  (if (not (file-exists-p (concat 7zr-archive-directory 7zr-archive-name)))
      (progn 
	(message (concat "Archive " 7zr-archive-name " has not yet been created yet, neither via 7z-revisions-mode nor by typing M-x 7zr-commit."))
	"-5"  ; return disposition that means there is no archive
	)
    ; else check to see if there is no created-by-message metadata file
    (if (not (eql (string-match "No files to process" (shell-command-to-string (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " "  (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name))  " " (7zr-shell-quote-argument 7zr-archive-created-by-message)))) nil))
	(progn
	  (message (concat "There already exists an archive named " 7zr-archive-name " that is evidently not a 7z-revisions.el archive since it lacks a file named '" 7zr-archive-created-by-message  "' !  You must type M-x 7zr-create-file-for-archive-created-by-message to fix this, if the archive was created by 7z-revisions.el, that is."))
	  (error "-4")
	  "-4" ; return disposition that means theres an archive but there is no created-by-message metadata file
	  
	  ) ; progn
      ; else
      (setq 7zr-examine-last-buffer (current-buffer))
      (find-file (concat 7zr-temp-directory 7zr-archive-created-by-message))
      (7zr-set-some-buffer-local-vars "7zr-examine-created-by-message-file"  7zr-examine-last-buffer)
      (save-excursion  ;; get os type
	(goto-char (point-min))
	(if (re-search-forward (concat "^" 7zr-update-7z-revisions-tag-in-metadata-os_type  "\\(.\\)") nil t)
	    (setq 7zr-os_type (match-string-no-properties 1))
	  (setq 7zr-os_type (symbol-name system-type))
	    )
	(goto-char (point-min))
	(if (re-search-forward (concat "^" 7zr-update-7z-revisions-tag-in-metadata-buffer_file_coding_system  "\\(.\\)") nil t)
	    (setq 7zr-buffer_file_coding_system (match-string-no-properties 1))
;	  (setq 7zr-buffer_file_coding_system buffer-file-coding-system)
	  )
	) ; save-excursion
      (7zr-created-by-message-file_increment_latest_revision_number) ; 7zr-patch-number and 7zr-original-version are populated by this function
					;     (7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-archive-created-by-message))
      (when (string= 7zr-original-version "")
	(error "no 7zr-original-version in 7zr-examine-created-by-message"))
      (if (= 7zr-patch-number -1)
	  (progn 
	    (error "doh")
	    "0" ; disposition means something is wrong
	    )
	
	"1" ; return disposition that means everything is ok
	) ; if
      ) ; if no created by message
    ) ; if archive doesnt exist
  )

	  

(defun 7zr-sans-extension ( filenamestring )
  (replace-regexp-in-string "\\..+" "" filenamestring)
)


(defun 7zr-what-is-extension-of-filename ( filenamestring )
  "Returns the file extension.  Also if there is no extension, then the 7zr-dot variable is set to a blank string.  The variable 7zr-dot must be declared in the calling function. "
  (let ((answer ""))
    (setq 7zr-dot ".")
    (setq answer (replace-regexp-in-string ".*\\." "" filenamestring))
    (if (string= answer filenamestring)
	(setq 7zr-dot "")
      answer
      )
    )
  )




(defun 7zr-insert-string-with-newline ( string )
  (insert-string string)
  (newline)
)

(defun 7zr-what-is-last-revision ()
  "DEFUNCT this function is like (7zr-list-revs) in 7zr-created-by-file-increment-rev but seems like more work actually.  This function is not called by anything."
  (let (7zr-buffer 7zr-buffer-filename 7zr-patch-number 7zr-revisions_lastbuffer 7zr-revisions_tmpbuffer2 7zname patch last-revision)
    (7zr-populate-initial-global-vars)
    (unless (file-directory-p 7zr-temp-directory)
      (make-directory 7zr-temp-directory t))
    (setq 7zr-patch-number 0)
    (if (not (file-exists-p (concat 7zr-archive-directory 7zr-archive-name)))
	(message (concat "Archive " 7zr-archive-name " has not yet been created yet, neither via 7z-revisions-mode nor by typing M-x 7zr-commit."))
    
      (if (eql (string-match "No files to process" (shell-command-to-string (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " "  (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name))  " " (7zr-shell-quote-argument 7zr-archive-created-by-message)))) nil)
	  (progn
	    (7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-archive-created-by-message))
	    (setq 7zr-patch-number 1.0)
	    (setq 7zr-revisions_lastbuffer (current-buffer))
	    (setq 7zr-revisions_tmpbuffer2 (generate-new-buffer-name (concat "7zr-revisions_with-temp-buffer_of_" 7zr-buffer)))
	    (get-buffer-create 7zr-revisions_tmpbuffer2)

	    (set-buffer 7zr-revisions_tmpbuffer2)	    
					;	  (switch-to-buffer 7zr-revisions_tmpbuffer2)
	    (setq 7zr-revisions_with-temp-buffer (current-buffer))    
	    (save-window-excursion
	      (call-process "7z" nil t nil "l" (concat 7zr-archive-directory 7zr-archive-name))
	      (goto-char (point-min))
	      (re-search-forward "[[:blank:]]Date[[:blank:]]+Time[[:blank:]]+Attr[[:blank:]]+Size[[:blank:]]+Compressed[[:blank:]]+Name")
	      (setq name_pos (- (match-end 0) 4))
	      (beginning-of-line)
	      (setq name_column_num (- name_pos (point)))
	      (forward-line 2)
	      (forward-char (1- name_column_num))
	      (setq start_rectangle (point))
	      (end-of-buffer)
	      (beginning-of-line)
	      (forward-line -2)
	      (forward-char (1- name_column_num))
	      (setq end_rectangle (+ (point) 20))
	      (setq rectangle (delete-extract-rectangle start_rectangle end_rectangle))
	      (erase-buffer)
	      (mapc '7zr-insert-string-with-newline rectangle) 
		    
	      (sort-numeric-fields 1 (point-min) (point-max) )
	      (goto-char (point-max))
	      (beginning-of-line)
	      (looking-at "[0-9]+\.[0-9]+")
	      (setq last-revision (match-string-no-properties 0))
	      (setq 7zr-patch-number (string-to-number last-revision))
	      )
	    (set-buffer 7zr-revisions_lastbuffer)
	    (kill-buffer 7zr-revisions_tmpbuffer2)
	    7zr-patch-number
	    )
	)  ; if No files to process
      )   ; if not file exists
    ) ; let
  )
  
(defun 7zr-goto-line-of-last-revision ()
  "Jump to the line number relating to the last hunk of the last revision, messaging the datetime when it was saved"
  (interactive)
  (7zr-populate-initial-global-vars)
  (unless (file-directory-p 7zr-temp-directory)
    (make-directory 7zr-temp-directory t))
  (if (not (file-exists-p 7zr-archive-name))
      (message (concat "Archive " 7zr-archive-name " has not yet been created yet, neither via 7z-revisions-mode nor by typing M-x 7zr-commit."))
    
    (if (eql (string-match "No files to process" (shell-command-to-string (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " "  (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name))  " " (7zr-shell-quote-argument 7zr-archive-created-by-message)))) nil)
	(progn
	  (7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-archive-created-by-message))
	  (setq 7zr-patch-number 1.0)
	  (setq 7zr-revisions_lastbuffer (current-buffer))
	  (setq 7zr-revisions_tmpbuffer2 (generate-new-buffer-name (concat "7zr-revisions_with-temp-buffer_of_" 7zr-buffer)))
	  (get-buffer-create 7zr-revisions_tmpbuffer2)

	  (set-buffer 7zr-revisions_tmpbuffer2)	    
;	  (switch-to-buffer 7zr-revisions_tmpbuffer2)
	  (setq 7zr-revisions_with-temp-buffer (current-buffer))    
	  (save-window-excursion
	    (call-process "7z" nil t nil "l" 7zr-archive-name)
	    (goto-char (point-min))
	    (while (re-search-forward "^\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\) ..... [ 0-9]+ [ 0-9]+  \\(.*\\)" nil t)

	      (setq 7zname (match-string-no-properties 2))
	      (beginning-of-line)
	      (when 
		  (not (string-match "[a-zA-Z]+" 7zname))   ; else it must be a number to be a patch, and so discard the rest
		(setq patch (string-to-number 7zname))
		(when (> patch 7zr-patch-number)            ; if patch is higher it becomes highest
		  (setq 7zr-datetime (match-string-no-properties 1))
		  (setq 7zr-patch-number patch)	  
		  )		    
		)
	      (forward-char)   
	      )  ; while
	    )
	  (set-buffer 7zr-revisions_lastbuffer)
	  (setq 7zr-patch-number-string (number-to-string 7zr-patch-number))
	  (save-window-excursion
	    (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " 7zr-patch-number-string))  
	    (rename-file (concat 7zr-temp-directory 7zr-patch-number-string) (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip) t)
	    )
	  (find-file (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip))	  
	  (end-of-buffer)
	  (re-search-backward "^\\([0-9]+\\).*$" nil t)
	  (setq 7zr_diff_line_num (7zr-last-integer-in-string (match-string-no-properties 0)))
	  (kill-buffer 7zr-revisions_tmpbuffer2)
	  (kill-buffer 7zr-prepend-to-reconstruct_wip)
	  (7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip))
	  (set-buffer 7zr-revisions_lastbuffer)
	  (beginning-of-buffer)
	  (forward-line (1- 7zr_diff_line_num))
	  (message (concat "modified on " 7zr-datetime))
	  )  ; progn
      )
    )
  )


(defun 7zr-created-by-message-file_increment_latest_revision_number ()
  "updates the created-by-message-file, which must reside in the tmp directory before calling this function, after incrementing the lastest_revision number, but returning the unincremented latest_revision number.  This function must be called while the current-buffer is the created-by-message file, or the function will return -1 and do nothing.   The original-version and latest_revision written in created-by-message file overrides what is found in the actual archive list.  If those 2 variables arent found in the file, then they are repopulated to the file from the archive list."

  ; if current-buffer is not the created-by-message file then do nothing, and error
  (let ((saved-line-pos 1) saved-line-pos_revision saved-line-pos_original_version (latest_revisionp 0))
    (if (not (string-equal (buffer-name (current-buffer)) 7zr-archive-created-by-message))
	(progn
	  (error "must be viewing the archive-created-by-message-file buffer before calling this function")
	  -1
	  )
					; else
      (goto-char (point-min))
      (if (re-search-forward (concat "^"  7zr-update-7z-revisions-tag-in-metadata-file_rev "\\([0-9]+\.[0-9]+\\)") nil t)
	  (progn
	    (setq latest_revision_number (match-string-no-properties 1))
	    (beginning-of-line)
	    (delete-region (point) (line-end-position))
	    (setq saved-line-pos (point))	    
	    (setq 7zr-patch-number (string-to-number latest_revision_number))
	    (setq latest_revisionp 1)
	    )
      ; else list archive to populate 7zr-patch-number and original-version variables
	(setq latest_revisionp 0)
	(setq listrevs_returnval (7zr-get-highest-rev-and-original-version))
;	(setq 7zr-patch-number (nth 1 listrevs_returnval))
;	(setq 7zr-original-version (car listrevs_returnval))
	(newline)
	(forward-line -1)
	(beginning-of-line)
	(setq saved-line-pos (point))
	)
      (goto-char saved-line-pos)
      (insert (concat 7zr-update-7z-revisions-tag-in-metadata-file_rev (number-to-string (1+ 7zr-patch-number))))

          ; original-version
      
      (goto-char (point-min))
      (if (and
	   (re-search-forward (concat "^" 7zr-update-7z-revisions-tag-in-metadata-file_original-version "\\(.*\\)$") nil t)
	  ;  allows the original-version and latest_revision written in created-by-message file to override what was found in the actual archive list
	   (not (string= (match-string-no-properties 1) ""))
	   )

	    (setq 7zr-original-version (match-string-no-properties 1))

      ; else list archive to populate original version variable
	(if (= latest_revisionp 1) ; meaning we didnt list the archive yet
	      ; search archive list for original version
	    (progn 
	      (setq listrevs_returnval (7zr-get-highest-rev-and-original-version))
;	(setq 7zr-patch-number (nth 2 listrevs_returnval))
;	      (setq 7zr-original-version (car listrevs_returnval))
	      )
	         ; else just fill in the blanks
	  ) ; if
	  ; need-to-fill-in-original-version
	(newline)
	(insert (concat 7zr-update-7z-revisions-tag-in-metadata-file_original-version 7zr-original-version))
	(newline)
	) ; if


          

      (write-file 7zr-archive-created-by-message)
      (kill-buffer)

      
      (shell-command-to-string (concat "7z a "  (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name))  " " (7zr-shell-quote-argument (concat 7zr-temp-directory 7zr-archive-created-by-message))))
      (7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-archive-created-by-message))
      7zr-patch-number
      ) ; if
    ) ; let
  )
    
 

(defun 7zr-shell-quote-argument ( argument )
  "For some reason shell-quote-argument returns single quotes delimited by double quotes when passed a blank string.  This function is a wrapper which attempts to rectify this."
  (if (string= argument "")
      ""
    (shell-quote-argument argument)
    )
  )

 
(defun 7z-revisions ()
  "This function is called from the main document and finds the 7zr-original-version variable and displays all the revisions in an interactive buffer, which is stored in the variable 7zr-revisions_tmpbuffer, with names of patches, discarding any filenames in the archive that arent numbers per number-or-marker-p, and also gets the 7zr-patch-number.  When a revision is selected and the enter key is pressed, or the u key in the case of displaying the respective diff files, then the (7zr-summary-view-revision) function takes over."
  (interactive)
  (setq cntr 0) ; debug

  
(if (boundp 'evil-mode)
  (add-to-list 'evil-emacs-state-modes '7zr-summary-mode)
  )

  (unless (file-directory-p 7zr-temp-directory)
    (make-directory 7zr-temp-directory t))
  (7zr-populate-initial-global-vars) 
  (7zr-set-archive-directory-full 7zr-archive-directory)
  (setq 7zr-revisions_original_buffer (current-buffer))

  (setq 7zr-buffer-without-extension (7zr-sans-extension 7zr-buffer))
  (setq 7zr-buffer-filename-extension (7zr-what-is-extension-of-filename 7zr-buffer))
  (setq 7zr-latest-revision_size "")
  (setq 7zr-latest-revision_datetime "")

  
  (if (not (file-exists-p (concat 7zr-archive-directory 7zr-archive-name)))
      (message (concat "Archive " 7zr-archive-name " has not yet been created yet, neither via 7z-revisions-mode nor by typing M-x 7zr-commit."))
    
    (if (eql (string-match "No files to process" (shell-command-to-string (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " "  (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name))  " " (7zr-shell-quote-argument 7zr-archive-created-by-message)))) nil)
	(progn
	  
	  (7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-archive-created-by-message))

	  (setq 7zr-patch-number 1.0)
;	  (setq 7zr-view-last-column (current-column))
;	  (setq 7zr-view-last-line (string-to-number (format-mode-line "%l")))
	  (7zr-save-last-line-column)
	  (set (make-local-variable '7zr-window) "document")
	  (setq 7zr-revisions_lastbuffer (current-buffer))
	  (setq 7zr-revisions_tmpbuffer (generate-new-buffer-name (concat "7zr-revisions_of_" 7zr-buffer)))
	  (get-buffer-create 7zr-revisions_tmpbuffer)     ; this is the buffer that will be used to display the final listing   
	  (setq 7zr-revisions_tmpbuffer2 (generate-new-buffer-name (concat "7zr-revisions_with-temp-buffer_of_" 7zr-buffer)))
	  (get-buffer-create 7zr-revisions_tmpbuffer2)   ; this buffer is soon killed
				
;	  (setf (buffer-local-value '7zr-active-document 7zr-revisions_tmpbuffer) '7zr-buffer)

	  (set (make-local-variable '7zr-active-document_buffer) (current-buffer))
	  
	  (with-current-buffer 7zr-revisions_tmpbuffer
	    (set (make-local-variable '7zr-active-document) 7zr-buffer)
	    (set (make-local-variable '7zr-active-document_archive-name) 7zr-archive-name)
	    (set (make-local-variable '7zr-active-document_archive-directory) 7zr-archive-directory)
	    (set (make-local-variable '7zr-active-document_archive-prefix) 7zr-archive-prefix)
	    (set (make-local-variable '7zr-viewing) t)
	    )
;	    )

	  
	  (set-buffer 7zr-revisions_tmpbuffer)
	  (setq 7zr-revisions_tmpbuffer_buffer (current-buffer))

	  (with-current-buffer 7zr-revisions_original_buffer
	    (set (make-local-variable '7zr-active-document_7zr-revisions_tmpbuffer_buffer) 7zr-revisions_tmpbuffer_buffer))
;	  (make-local-variable '7zr-archive-directory)
	  ;(setq 7zr-archive-directory (buffer-local-value '7zr-archive-directory 7zr-revisions_original_buffer))
;(erase-buffer)

	  (let ((zfile (concat 7zr-archive-directory 7zr-archive-name))
		files file sum col timetuple savepoint reached-minuses patch highest-patch 7zdatetime 7zsize 7zname)
	    (set-buffer 7zr-revisions_tmpbuffer2)	    
	    (setq 7zr-revisions_with-temp-buffer (current-buffer))    
	    (save-window-excursion
	      (call-process "7z" nil t nil "l" zfile)
	      (goto-char (point-min))
    
;    (re-search-forward "[[:blank:]]Date[[:blank:]]+Time[[:blank:]]+Attr[[:blank:]]+Size[[:blank:]]+Compressed[[:blank:]]+Name")
;    (re-search-forward 
 ;   (next-line)
	      (beginning-of-line)
	      (while (re-search-forward "^\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\) \\(.....\\) \\([ 0-9]+\\) \\([ 0-9]+\\)  \\(.*\\)" nil t)
		(setq 7zdatetime (match-string-no-properties 1))
		(setq 7zsize (match-string-no-properties 3))
		(setq 7zname (match-string-no-properties 5))
		(beginning-of-line)
	    
;      (setq timetuple (apply #'encode-time (parse-time-string 7zdatetime)))
	    
		(if (and (7zr-string-starts-with-p 7zname 7zr-buffer-without-extension)  ; find original version
			 (not (string= 7zname 7zr-archive-created-by-message))  ; not the archive identifier
			 )
		    (progn
		      (setq 7zr-original-version 7zname)
		      (setq 7zr-current-original-version 7zr-original-version)
		      (make-local-variable '7zr-current-original-version)
		    
		      (set-buffer 7zr-revisions_tmpbuffer)
		      (setq 7zr-current-original-version 7zr-original-version)
		      (make-local-variable '7zr-current-original-version)

		    
		      (insert 7zname)
		      (insert-tab)
		      (insert 7zsize)
		      (insert-tab)
;	      (insert 7zdatetime)
		      (newline)
		      )
		  (if (7zr-string-starts-with-p 7zname 7zr-prepend-to-latest-revision)  ; find latest revision
		      (progn
			(setq 7zr-latest-revision 7zname)
			(setq 7zr-latest-revision_size 7zsize)
			(setq 7zr-latest-revision_datetime 7zdatetime)
			)
		    (when 
			(not (string-match "[a-zA-Z]+" 7zname))   ; else it must be a number to be a patch, and so discard the rest
		    
		      
		      (set-buffer 7zr-revisions_tmpbuffer)
		      (insert 7zname)
		      (insert-tab)
		      (insert 7zsize)
		      (insert-tab)
		      (insert 7zdatetime)
		      (newline)
		      
		      (set-buffer 7zr-revisions_with-temp-buffer)
		      (setq patch (string-to-number 7zname))
		      (when (> patch 7zr-patch-number)            ; if patch is higher it becomes highest
			(setq 7zr-patch-number patch)	  
			)
		    
		      )
		  
		    )
		  )
		(set-buffer 7zr-revisions_with-temp-buffer)
;	      (next-line)
		(forward-char)
		)           ; while
	      )
	    )
    
 ;    (error "Not a valid 7z-revisions.el archive file")
    
	    
	  (set-buffer 7zr-revisions_tmpbuffer)
	  (switch-to-buffer 7zr-revisions_tmpbuffer)
	  (when 7zr-track-md5sum-hashes-p
;	    (with-current-buffer 7zr-active-document_buffer
	      (7zr-revisions-load-hashtable-unsecure)
;	      )
	    )
	 
	  (goto-char (point-min))
	  (when (and 
		 (re-search-forward 7zr-original-version nil t)
;		 (not (string= (format-mode-line "%l") "1"))
		 (not (equal (line-number-at-pos) 1))
		 )
	    (beginning-of-line)
	    (let ((tmpbegin (point)) tmpend tmpsubstring)
	      (forward-line 1)
	      (setq tmpend (point))
	      (setq tmpsubstring (buffer-substring-no-properties tmpbegin tmpend))
	      (delete-region tmpbegin tmpend)
	      (goto-char (point-min))
	      (insert tmpsubstring)
	      )
	    )
	  (goto-char (point-max))
	  (setq 7zr-revisions_tmpbuffer_lines (line-number-at-pos))
	  (insert 7zr-latest-revision)
	  (insert-tab)
	  (insert 7zr-latest-revision_size)
	  (insert-tab)
	  (insert 7zr-latest-revision_datetime)
					;	  (delete-backward-char 1)
	  (when (> 7zr-patch-number 1)
	    (7zr-summary-sort-1))
	  (goto-char (point-min))
	  (7zr-summary-mode)
	  (make-local-variable 'global-hl-line-mode)
	  (toggle-read-only 1)
	  (set-buffer-modified-p nil)
	  (hl-line-mode 1)
	  (kill-buffer 7zr-revisions_with-temp-buffer)
	  (end-of-buffer)
	  (forward-line -1)
	  (set (make-local-variable '7zr-active-document_buffer) 7zr-revisions_original_buffer) 

	  ; try to go to the last line selected when the app was last exited via quit
	  (with-current-buffer 7zr-active-document_buffer
	    (when (bound-and-true-p 7zr-active-document_summary-line-number)
	      (setq 7zr-goto-this-line-number 7zr-active-document_summary-line-number)
	      (with-current-buffer 7zr-revisions_tmpbuffer
		(goto-char (point-min)) 
		(forward-line (1- 7zr-goto-this-line-number))
		)
	      )
	    )
	  (set (make-local-variable '7zr-active-document_7zr-revisions_tmpbuffer_lines) 7zr-revisions_tmpbuffer_lines)
	  (7zr-set-some-buffer-local-vars "7z-revisions" 7zr-revisions_lastbuffer)
	  (7zr-revisions-load-notestab-unsecure)
	  (when 7zr-track-md5sum-hashes-p
	    (with-current-buffer 7zr-revisions_tmpbuffer_buffer
	      (7zr-revisions-load-hashtable-unsecure)
	      )
	    )
	  (set (make-local-variable '7zr-viewing) "true")
	  ;(evil-mode 0)
;	(message 7zr-active-document_original-version)	  
	  )
      ; else fail
      (message (concat "archive Directory =" 7zr-archive-directory))
      (message (concat "There already exists an archive named " (buffer-name (current-buffer)) 7zr-archive-extension " that is evidently not a 7z-revisions.el archive since it lacks a file named '" 7zr-archive-created-by-message  "' !  You must type M-x 7zr-create-file-for-archive-created-by-message to fix this, if the archive was created by 7z-revisions.el."))
      )
    )
  )            ; 7z-revisions

  
(defun 7zr-dired-7z-revisions ()
  "This function can be called from dired using the z key.  This function finds the 7zr-original-version variable and displays all the revisions in an interactive buffer, which is stored in the variable 7zr-revisions_tmpbuffer, with names of patches, discarding any filenames in the archive that arent numbers per number-or-marker-p, and also gets the 7zr-patch-number.  When a revision is selected and the enter key is pressed, or the j key in the case of displaying the respective diff files, then the (7zr-summary-view-revision) function takes over."
  (interactive)
  (setq cntr 0) ; debug
  (unless (file-directory-p 7zr-temp-directory)
    (make-directory 7zr-temp-directory t))
  (setq 7zr-active-document_buffer (file-name-nondirectory (dired-get-file-for-visit)))
  (setq 7zr-active-document 7zr-active-document_buffer)
  (setq 7zr-buffer (file-name-nondirectory (dired-get-file-for-visit)))


  (setq 7zr-archive-directory (file-name-directory (dired-get-file-for-visit)))
  (setq 7zr-revisions_original_buffer (current-buffer))

  (setq 7zr-buffer-without-extension (7zr-sans-extension 7zr-buffer))
  (setq 7zr-buffer-filename-extension (7zr-what-is-extension-of-filename 7zr-buffer))
  (setq 7zr-archive-name 7zr-buffer)  
  (when (7zr-string-starts-with-p 7zr-buffer 7zr-archive-prefix)
    (setq 7zr-buffer (string-remove-prefix 7zr-archive-prefix 7zr-buffer)))
  
  (setq 7zr-buffer-filename 7zr-buffer)
  
  (setq 7zr-latest-revision_size "")
  (setq 7zr-latest-revision_datetime "")
 

  (if (not (string= 7zr-buffer-filename-extension 7zr-archive-extension-suffix))
      (message (concat "File " 7zr-archive-name " is not a " 7zr-archive-extension-suffix " archive."))    
    (if (eql (string-match "No files to process" (shell-command-to-string (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " "  (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name))  " " (7zr-shell-quote-argument 7zr-archive-created-by-message)))) nil)
	(progn
	  (7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-archive-created-by-message))
	  (setq 7zr-patch-number 1.0)
	  (setq 7zr-revisions_lastbuffer (current-buffer))
	  (setq 7zr-revisions_tmpbuffer (generate-new-buffer-name (concat "7zr-revisions_of_" 7zr-buffer-filename)))
	  (get-buffer-create 7zr-revisions_tmpbuffer)
	  (setq 7zr-revisions_tmpbuffer2 (generate-new-buffer-name (concat "7zr-revisions_with-temp-buffer_of_" 7zr-buffer-filename)))
	  (get-buffer-create 7zr-revisions_tmpbuffer2)
;	  (setf (buffer-local-value '7zr-active-document 7zr-revisions_tmpbuffer) '7zr-buffer)
;	  (let* ((#1=#:v 7zr-revisions_tmpbuffer))
;	    (with-current-buffer #1#
;	      (set (make-local-variable '7zr-active-document) 7zr-buffer)))

	  (set-buffer 7zr-revisions_tmpbuffer)
;	  (make-local-variable '7zr-archive-directory)
	  ;(setq 7zr-archive-directory (buffer-local-value '7zr-archive-directory 7zr-revisions_original_buffer))
;(erase-buffer)

	  (let ((ofile 7zr-archive-name)
		files file sum col timetuple savepoint reached-minuses patch highest-patch 7zdatetime 7zsize 7zname)
	    (set-buffer 7zr-revisions_tmpbuffer2)
	  
    
	    (setq 7zr-revisions_with-temp-buffer (current-buffer))
    
	    (save-window-excursion
	    (call-process "7z" nil t nil "l" ofile)
	    (goto-char (point-min))
    
;    (re-search-forward "[[:blank:]]Date[[:blank:]]+Time[[:blank:]]+Attr[[:blank:]]+Size[[:blank:]]+Compressed[[:blank:]]+Name")
;    (re-search-forward 
 ;   (next-line)
	    (beginning-of-line)

	    (while (re-search-forward "^\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\) \\(.....\\) \\([ 0-9]+\\) \\([ 0-9]+\\)  \\(.*\\)" nil t)
	      (setq 7zdatetime (match-string-no-properties 1))
	      (setq 7zsize (match-string-no-properties 3))
	      (setq 7zname (match-string-no-properties 5))
	      (beginning-of-line)
	    
;      (setq timetuple (apply #'encode-time (parse-time-string 7zdatetime)))
	    
	      (if (and (7zr-string-starts-with-p 7zname 7zr-buffer-without-extension)  ; find original version
		       (not (string= 7zname 7zr-archive-created-by-message))  ; not the archive identifier
		       )
		  (progn
		    (setq 7zr-original-version 7zname)
		    (setq 7zr-current-original-version 7zr-original-version)
		    (make-local-variable '7zr-current-original-version)
		    
		    (set-buffer 7zr-revisions_tmpbuffer)
		    (setq 7zr-current-original-version 7zr-original-version)
		    (make-local-variable '7zr-current-original-version)

		    
		    (insert 7zname)
		    (insert-tab)
		    (insert 7zsize)
		    (insert-tab)
;	      (insert 7zdatetime)
		    (newline)
		    )
		(if (7zr-string-starts-with-p 7zname 7zr-prepend-to-latest-revision)  ; find latest revision
		    (progn
		      (setq 7zr-latest-revision 7zname)
		      (setq 7zr-latest-revision_size 7zsize)
		      (setq 7zr-latest-revision_datetime 7zdatetime)
		      )
		  (when 
		      (not (string-match "[a-zA-Z]+" 7zname))   ; else it must be a number to be a patch, and so discard the rest
		    
		      
		    (set-buffer 7zr-revisions_tmpbuffer)
		    (insert 7zname)
		    (insert-tab)
		    (insert 7zsize)
		    (insert-tab)
		    (insert 7zdatetime)
		    (newline)
		      
		    (set-buffer 7zr-revisions_with-temp-buffer)
		    (setq patch (string-to-number 7zname))
		    (when (> patch 7zr-patch-number)            ; if patch is higher it becomes highest
		      (setq 7zr-patch-number patch)	  
		      )
		    
		    )
		  
		  )
		)
	      (set-buffer 7zr-revisions_with-temp-buffer)
;	      (next-line)
	      (forward-char)
	      )           ; while
	    )
	    )
    
 ;    (error "Not a valid 7z archive file")
    
	    
	  (set-buffer 7zr-revisions_tmpbuffer)
	  (when 7zr-track-md5sum-hashes-p
	    (7zr-revisions-load-hashtable-unsecure)
	    )
	  (switch-to-buffer 7zr-revisions_tmpbuffer)
	  (goto-char (point-min))
	  (when (and 
		 (re-search-forward 7zr-original-version nil t)
;		 (not (string= (format-mode-line "%l") "1"))
		 (not (equal (line-number-at-pos) 1))
		 )
	    (beginning-of-line)
	    (let ((tmpbegin (point)) tmpend tmpsubstring)  ; some fiddling with the sorting
	      (forward-line 1)
	      (setq tmpend (point))
	      (setq tmpsubstring (buffer-substring-no-properties tmpbegin tmpend))
	      (delete-region tmpbegin tmpend)
	      (goto-char (point-min))
	      (insert tmpsubstring)
	      )
	    )
	  (goto-char (point-max))
	  (setq 7zr-revisions_tmpbuffer_lines (line-number-at-pos))
	  (insert 7zr-latest-revision)
	  (insert-tab)
	  (insert 7zr-latest-revision_size)
	  (insert-tab)
	  (insert 7zr-latest-revision_datetime)
;	  (delete-backward-char 1)
	  (7zr-summary-sort-1)
	  (goto-char (point-min))
	  (7zr-summary-mode)
	  (make-local-variable 'global-hl-line-mode)
	  (toggle-read-only 1)
	  (set-buffer-modified-p nil)
	  (hl-line-mode 1)
	  (kill-buffer 7zr-revisions_with-temp-buffer) ; 7zr-revisions_tmpbuffer2
	  (end-of-buffer)
	  (forward-line -1)
	  (7zr-set-some-buffer-local-vars "7zr-dired-7z-revisions" (current-buffer))
	  )
      ; else fail
      (message (concat "The file " 7zr-buffer " is not a 7z-revisions.el archive!"))
      )
    )
  )            ; 7zr-dired-7z-revisions

  


(defun 7zr-summary-sort-1 ()
  "sorts 1st column numeric but excludes the very first row
and very last row in the sorting."
  (interactive)
  (let (begin end saved-point-here )
    (setq saved-point-here (point))
    (goto-char (point-min))
    (forward-line 1)
    (setq begin (point))
    (goto-char (point-max))
    (beginning-of-line)
    (backward-char 1)
    (setq end (point))
    (sort-numeric-fields 1 begin end)
    (goto-char saved-point-here)
    )
)

(defun 7zr-summary-sort-date ()
  (interactive)
  (let (begin end saved-point-here )
    (setq saved-point-here (point))
    (goto-char (point-min))
    (forward-line 1)
    (setq begin (point))
    (goto-char (point-max))
    (beginning-of-line)
    (backward-char 1)
    (setq end (point))
    (sort-numeric-fields 3 begin end)
    (goto-char saved-point-here)
    )
)

(defun 7zr-summary-view-revision-note ()
  (interactive)
  (when (looking-at "\\([0-9]+\.?[0-9]*\\)")
    (message (gethash (match-string-no-properties 1) 7zr-notestab))
    )
  )

(defun 7zr-summary-forward-line ()
  (interactive)
  (forward-line)
  (when (looking-at "\\([0-9]+\.?[0-9]*\\)")
    (message (gethash (match-string-no-properties 1) 7zr-notestab))
    )
  )

(defun 7zr-summary-previous-line ()
  (interactive)
  (previous-line)
  (when (looking-at "\\([0-9]+\.?[0-9]*\\)")
    (message (gethash (match-string-no-properties 1) 7zr-notestab))
    )
  )


(defun 7zr-password (proc string)
  "Ask the user for a password when necessary.
PROC is the process running 7z.  STRING is the line that was
output by PROC.  Not used."
  (let (ask)
    (cond
     ((or
       (string-match "^Enter passphrase for key '\\\(.*\\\)': $" string)
       (string-match "^\\\(.*\\\)'s password:" string))
      (setq ask (format "Password for '%s': " (match-string 1 string))))
     ((string-match "^[pP]assword:" string)
      (setq ask "Password:")))

    (when ask
      (process-send-string proc (concat (read-passwd ask nil) "\n"))))
  )


(defun 7zr-process-filter (proc string)
  "Check if PROC is asking for a password and promps the user if so.
STRING is the output line from PROC.  Not used."
  (save-current-buffer
    (set-buffer (process-buffer proc))
    (let ((inhibit-read-only t))
      (7zr-password proc string))))

(defun 7zr-process-sentinel (proc status)
  "Report PROC change to STATUS.  Not used."
  (message "7z %s" (substring status 0 -1)))



(defun 7zr-string-trim-final-newline (string)
  "Removes the last newline character on the string, if there is one at the end of the string.  This function generally needs to be executed twice when running on windows."
  (let ((len (length string)))
    (cond
      ((and (> len 0) (eql (aref string (- len 1)) ?\n))
       (substring string 0 (- len 1)))
      (t string))))


(defun 7zr-list-hashkeys ( hashtable )
  "Returns a sorted list of keys from the hashtable passed as an argument"
  (let ((keylist '()))
    (maphash
     (lambda (key value)
       (add-to-list 'keylist key)
       )
     hashtable
     )
   )
  )

(defun 7zr-list-hashkeys-sorted_numeric-string ( hashtable )
  "Returns a sorted list of keys from the hashtable passed as an argument"
  (let ((keylist '()))
    (maphash
     (lambda (key value)
       (add-to-list 'keylist key)
       )
     hashtable
     )

   (sort keylist (lambda (a b) (< (string-to-number a) (string-to-number b))))  
   )
  )


(defun 7zr-revisions-load-hashtable ()
  "Extracts hashtable from .7z archive and loads it.  If you are loading archives from public platforms and are concerned about security use this function in lieu of the (7zr-revisions-load-hashtable-unsecure) function.  Although, if you are that paranoid, you can always check the raw hash file, and raw notes file using F2 C-f and F2 c-c, respectively, from the main document, before invoking (7z-revisions)."
  (let (rev_string sha1_string last_buffer hash_buffer lines)

    (setq last_buffer (current-buffer))
    (7zr-extract-a-file-to-temp-directory 7zr-prepend-to-hash-file 7zr-original-version "")
    (open-file-read-only (concat 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-original-version))
    (setq hash_buffer (current-buffer))
;    (setq lines (count-lines (point-min) (point-max)))
    (setq lines (/ (nth 7 (file-attributes (concat 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-original-version))) 68))  ; estimate number of lines based on 68 characters per line in the hashfile
    (setq lines (+ lines 30))
    (with-current-buffer last_buffer
      (set (make-local-variable '7zr-hasht) (make-hash-table :size lines :test 'equal))
      )
    (goto-char (point-min))
    (while (re-search-forward "(puthash \"\\([0-9]+\.[0-9]+\\)\" \"\\([a-f0-9]+\\)\"" nil t)
      (setq rev_string (match-string-no-properties 1))
      (setq sha1_string (match-string-no-properties 2))
      (with-current-buffer last_buffer
	(puthash rev_string sha1_string 7zr-hasht)
	)
;  (7zr-add-to-archive-file-that-has-this-prefix-and-extension 7zr-prepend-to-hash-file 7zr-original-version "" line_to_add)
;  (7zr-update-a-file-from-temp-directory-to-archive ( 7zr-prepend-to-hash-file 7zr-original-version "")) 

      )
    (set-buffer last_buffer)
    (kill-buffer hash_buffer)
    
    )
  )

(defun 7zr-revisions-load-hashtable-unsecure ()
  "Extracts hashtable from .7z archive and loads it.  The use of this function is ill-advised, with respect to security, if loading archive files from public platforms.  Any attacker with access to the hash file could embed mallicious code into it.  Best to use the (7zr-revisions-load-hashtable function), albeit slower.  Although, if you are that paranoid, you can always check the raw hash file, and raw notes file using F2 C-f and F2 c-c, respectively, from the main document, before invoking (7z-revisions)."
  (save-window-excursion
    (let ((hash "") lines last_buffer)
      (setq last_buffer (current-buffer))
      (setq 7zr-document-namechanges ())
      (setq 7zr-current-original-version 7zr-original-version)
;      (make-local-variable '7zr-current-original-version)

      (set (make-local-variable '7zr-active-document_original-version) 7zr-original-version)
      
      (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-shell-quote-argument (concat 7zr-prepend-to-hash-file  7zr-current-original-version))))
      (setq hash (7zr-string-trim-final-newline (7zr-string-trim-final-newline (shell-command-to-string (concat 7zr-sha1sum-command " " (7zr-shell-quote-argument (concat 7zr-temp-directory 7zr-prepend-to-hash-file  7zr-current-original-version)) 7zr-sha1sum-post-command (7zr-awk-cmd-string 1))))))
      (unless (and
	   (bound-and-true-p 7zr-active-document_hash-of-hash-file)
	   (string= hash 7zr-active-document_hash-of-hash-file)  ; only load the hashtable if it has changed
	   )
;	    (setq lines (string-to-number (7zr-string-trim-final-newline (shell-command-to-string (concat "awk \"END { print NR }\"" (7zr-shell-quote-argument 7zr-temp-directory) 7zr-prepend-to-hash-file (7zr-shell-quote-argument 7zr-current-original-version))))))
	    (setq lines (/ (nth 7 (file-attributes (concat 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-original-version))) 68))  ; instead of using awk to get the number of lines, number of lines are estimated based on 68 characters per line in the hash file
	    (setq lines (+ lines 30))	  	  
	    (set (make-local-variable '7zr-active-document_hash-of-hash-file) hash)


	    (set (make-local-variable '7zr-hasht)  (make-hash-table :size lines :test 'equal))

;	    (defconst 7zr-hasht (make-hash-table :size lines :test 'equal))

;	  (set (make-local-variable '7zr-hasht) 7zr-hasht)
;	  (make-local-variable '7zr-hasht)
	    (load-file (concat 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-original-version))
	    ) ; unless
	
      ) ; let
    )
  )
  


(defun 7zr-revisions-load-hashtable_last-entry ()
  "Extracts hashtable from .7z archive and returns a 2 element list containing the last revision and the last sha1 entry in the file ."
  (let ((rev_string "") (sha1_string "") last_buffer hash_buffer lines)

    (setq last_buffer (current-buffer))
    (7zr-extract-a-file-to-temp-directory 7zr-prepend-to-hash-file 7zr-original-version "")
    (find-file-read-only (concat 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-original-version))
    (setq hash_buffer (current-buffer))
;    (setq lines (count-lines (point-min) (point-max)))
    (setq lines (/ (nth 7 (file-attributes (concat 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-original-version))) 68))  ; estimate number of lines based on 68 characters per line in the hashfile
    (setq lines (+ lines 30))
;    (with-current-buffer last_buffer
 ;     (defconst 7zr-hasht (make-hash-table :size lines :test 'equal))
  ;    )
    (goto-char (point-max))

    (when (re-search-backward "(puthash \"\\([0-9]+\.[0-9]+\\)\" \"\\([a-f0-9]+\\)\"" nil t)
      (setq rev_string (match-string-no-properties 1))
      (setq sha1_string (match-string-no-properties 2))
;      (with-current-buffer last_buffer
;	(puthash rev_string sha1_string 7zr-hasht)
;	)
;  (7zr-add-to-archive-file-that-has-this-prefix-and-extension 7zr-prepend-to-hash-file 7zr-original-version "" line_to_add)
;  (7zr-update-a-file-from-temp-directory-to-archive ( 7zr-prepend-to-hash-file 7zr-original-version "")) 

      )
    (set-buffer last_buffer)
    (kill-buffer hash_buffer)
    (list rev_string sha1_string)
    )
  )


(defun 7zr-revisions-load-notestab-unsecure ()
  "Extracts the notes hashtable from .7z archive and loads it to the buffer-local variable `7zr-notestab'.  The use of this function is ill-advised, with respect to security, if loading archive files from public platforms.  Any attacker with access to the hash file could embed mallicious code into it.  Best to use the (7zr-revisions-load-notestable) function, albeit slower.  Although, if you are that paranoid, you can always check the raw hash file, and raw notes file using F2 C-f and F2 c-c, respectively, from the main document, before invoking (7z-revisions)."
  (save-window-excursion
    (let ((hash "") lines last_buffer file_attributes_list)
      (setq last_buffer (current-buffer))
      (setq 7zr-current-original-version 7zr-original-version)
;      (make-local-variable '7zr-current-original-version)

      (set (make-local-variable '7zr-active-document_original-version) 7zr-original-version)
      
      (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-shell-quote-argument (concat 7zr-prepend-to-notes-file  7zr-current-original-version 7zr-notes-file-extension))))
      (setq hash (7zr-string-trim-final-newline (7zr-string-trim-final-newline (shell-command-to-string (concat 7zr-sha1sum-command " " (7zr-shell-quote-argument (concat 7zr-temp-directory 7zr-prepend-to-notes-file  7zr-current-original-version 7zr-notes-file-extension)) 7zr-sha1sum-post-command (7zr-awk-cmd-string 1))))))

      ;; (if (and
      ;; 	   (bound-and-true-p 7zr-active-document_hash-of-notes-file)
      ;; 	   (string= hash 7zr-hash-of-notes-file)  ; only load the notes table if it has changed
      ;; 	   )
      ;; 	  nil
					; else
	  ;	    (setq lines (string-to-number (7zr-string-trim-final-newline (shell-command-to-string (concat "awk \"END { print NR }\"" (7zr-shell-quote-argument 7zr-temp-directory) 7zr-prepend-to-hash-file (7zr-shell-quote-argument 7zr-current-original-version))))))
	(setq file_attributes_list (file-attributes (concat 7zr-temp-directory 7zr-prepend-to-notes-file 7zr-original-version 7zr-notes-file-extension)))
(if file_attributes_list
	    (setq lines (/ (nth 7 file_attributes_list) 68))  ; instead of using awk to get the number of lines, number of lines are estimated based on 68 characters per line in the notes file
					; else
	  (setq lines 100)
	  )

		   
	(setq lines (+ lines 30))	  	  
	(set (make-local-variable '7zr-active-document_hash-of-notes-file) hash)
;	    (set (make-local-variable '7zr-active-document_hash_of_notes_file) 7zr-hash-of-notes-file)

	(set (make-local-variable '7zr-notestab)  (make-hash-table :size lines :test 'equal))
;	    (defconst 7zr-notestab (make-hash-table :size lines :test 'equal))

;	  (set (make-local-variable '7zr-notestab) 7zr-notestab)
;	  (make-local-variable '7zr-hasht)
	(when file_attributes_list
	  (load-file (concat 7zr-temp-directory 7zr-prepend-to-notes-file 7zr-original-version 7zr-notes-file-extension))
	  )
;;	)	
      )
    )
  )
    
  

    

(defun 7zr-revisions-validate-rev-p ( rev-pointer )
"Check revision against hash"
  (let ((rev-hash "") (hash-from-table "") (hash-rev-to-check-against ""))
    (setq hash-rev-to-check-against rev-pointer)
    ;; (if (eql direction -1)  ; if we were walking upward the valid hash will be the hash associated with the nearest ancestor?
    ;; 	(setq hash-rev-to-check-against nearest_ancestor)
    ;;   (setq hash-rev-to-check-against rev-pointer)

      (if (file-exists-p (concat 7zr-temp-directory "rev" rev-pointer "_of_" 7zr-original-version))
	  (if (not 7zr-track-md5sum-hashes-p)
	      t         ; if we arent tracking hashes assume that rev is valid
	      
					; else check against hash

	    (setq rev-hash (7zr-string-trim-final-newline (7zr-string-trim-final-newline (shell-command-to-string (concat 7zr-sha1sum-command " " (7zr-shell-quote-argument (concat 7zr-temp-directory "rev" rev-pointer "_of_"  7zr-original-version)) 7zr-sha1sum-post-command (7zr-awk-cmd-string 1))))))
	    (setq hash-from-table (gethash hash-rev-to-check-against 7zr-hasht))
	    (string= rev-hash hash-from-table)

	    )
	nil
	) ; if file exists
      ) ; let
  )
  
  

(defun 7zr-delete-file-if-exists  ( filename )
  " Delete a filename if it exists, and return nil if it doesnt."
  (let (filename_trimmed )
    (setq filename_trimmed (7zr-trim-l (7zr-trim-r filename)))
    (if (and
	 (not (string= filename_trimmed ""))
	 (file-exists-p filename_trimmed )
	 (not (string= filename_trimmed 7zr-temp-directory)))
	(delete-file filename_trimmed)
      nil
      )
    )
  )

(defun 7zr-delete-file-from-temp-if-exists  ( filename )
  " Delete a filename if it exists, and return nil if it doesnt."
  (let (filename_trimmed (saved-directory default-directory))
    (setq filename_trimmed (7zr-trim-l (7zr-trim-r filename)))
    (cd 7zr-temp-directory)
    (if (and
	 (not (string= filename_trimmed ""))
	 (file-exists-p filename_trimmed )
	 (not (string= filename_trimmed 7zr-temp-directory)))
	(delete-file filename_trimmed)
      nil
      )
    (cd saved-directory)
    )
  )

(defun 7zr-rename-file-if-exists ( file newname &optional overwritep)
  " Rename a file if it exists, and returns nil, taking no
action, if it doesnt.  In addition, this over-writes destination
file if that file already exists.  The overwritep option is ignored for now."
  (if (file-exists-p file )
      (rename-file file newname t)
    nil
    )
  )

(defun 7zr-rename-file-from-temp-if-exists ( file newname &optional overwritep)
  " Rename a file if it exists in the 7zr-temp-directory, and
returns nil, taking no action, if it doesnt.  In addition, this
over-writes destination file if that file already exists.  The
overwritep option is ignored for now."
  (when file
    (let (filename_trimmed (saved-directory default-directory))
      (setq filename_trimmed (7zr-trim-l (7zr-trim-r file)))
      (cd 7zr-temp-directory)
    
      (if (file-exists-p filename_trimmed)
	  (rename-file filename_trimmed newname t)
	(progn 
	  (cd saved-directory)	  
	  (error (concat "File " 7zr-temp-directory filename_trimmed " doesnt exist"))
	  )
	)
      (cd saved-directory)
      )
    )
  )

(defun 7zr-newline ()
  "This function inserts control-M followed by control-J, as customary to end a line in a file of dos encoding, for debugging purposes."
  (interactive) 
;(insert 13) ;  (insert 10)
 (newline)
)
	  
(defun 7zr-apply-patch ( wip_buffer linenum_param direction)
  "This is an elisp implementation of the unix patch -p0 command, which only works on diffs of unified format, created by the unix command diff -Na, and returns the new calculated line number.  This patch function does no context checking and just assumes that whats at the lines of each hunk in question are correct, deletes them, and replaces them with the new patch values.  This function is called by (7zr-reconstruct-rev-from-patches_MSWINDOWS).  Also, before the patch functions can be called, the function (7zr-remove-No-newline-msgs-and-return-their-locations-via-globals) first must be called to populate the global variables which tell us if there were any '\\ No newline at end of file' messages in the diff buffer."

  (7zr-remove-No-newline-msgs-and-return-their-locations-via-globals)

  (if (= direction 1)
					; forward toward progeny
      (7zr-apply-patch-forward wip_buffer linenum_param  )
					; else reverse toward ancestors
    (7zr-apply-patch-reverse wip_buffer linenum_param )
    )
  )

(defun 7zr-apply-patch-reverse ( wip_buffer linenum_param )
  "This is an elisp implementation of the unix patch -p0 -R command, which only works on diffs of unified format, created by the unix command diff -Na.  Returns the new calculated line number."
  (let ( (diff-buffer (current-buffer)) diff-string (linenum_offset 0) (hunknum 0) (hunknum-that-changed-linenum 0) last_match_pos current_match_pos)
      ;; reverse toward ancestors
    (goto-char (point-max))
    (while (re-search-backward 7zr-match-diff-standard-line nil t)
      
      (let* ((a-begin (string-to-number (buffer-substring (match-beginning 1)
							  (match-end 1))))
	     (a-end  (let ((b (match-beginning 3))
			   (e (match-end 3)))
		       (if b
			   (string-to-number (buffer-substring b e))
			 a-begin)))
	     (diff-type (buffer-substring (match-beginning 4) (match-end 4)))
	     (b-begin (string-to-number (buffer-substring (match-beginning 5)
							  (match-end 5))))
	     (b-end (let ((b (match-beginning 7))
			  (e (match-end 7)))
		      (if b
			  (string-to-number (buffer-substring b e))
			b-begin)))
	     (a-num_lines (- a-end a-begin))
	     (b-num_lines (- b-end b-begin))
	     saved-point
	     (a-string_stack ())
	     (b-string_stack ())
	     (last_match_pos current_match_pos)
	     (current_match_pos (match-beginning 0))
	     (b-begin-saved b-begin)
	     (a-begin-saved a-begin))
	 ;; fix the beginning and end numbers, because diff is somewhat
	 ;; strange about how it numbers lines
	(if (string-equal diff-type "a")
	    (setq b-end (1+ b-end)
		  a-begin (1+ a-begin)
		  a-end a-begin)
	  (if (string-equal diff-type "d")
	      (setq a-end (1+ a-end)
		    b-begin (1+ b-begin)
		    b-end b-begin)
	     ;; (string-equal diff-type "c")
	    (setq a-end (1+ a-end)
		  b-end (1+ b-end))))

	(setq a-num_lines (- a-end a-begin)
	      b-num_lines (- b-end b-begin))

   ;; try this and see what happens
      ;; (if (string-equal diff-type "a")
      ;; 	  (setq a-end a-begin-saved)
      ;; 	(if (string-equal diff-type "d")
      ;; 	    (setq  b-end b-begin-saved)))

	(setq hunknum (1+ hunknum))
					; track current line number changes
	(if (and
	     (>= linenum_param b-begin )
	     (<= linenum_param b-end ))
	    (setq hunknum-that-changed-linenum hunknum)
					; else
	  (setq a-end-saved a-end b-end-saved b-end) ;debug
	  (when (> linenum_param b-end )
	    (setq linenum_offset (- a-end b-end)))
	  )


	(dotimes (line a-num_lines line)
	  (forward-line)
	  (beginning-of-line)
	  (when (looking-at "<")
	    (forward-char 2)
	    (push (buffer-substring-no-properties (point) (line-end-position)) a-string_stack )
	    )
	  )
	(beginning-of-line)  
	(set-buffer wip_buffer)
	(goto-char (point-min))
	(forward-line (1- b-begin))       ;; goto line in b

	(when (not (string= diff-type "d"))	  ;; "c" and "a" are really just the same
	  (dotimes (line b-num_lines line)  ;; kill lines in b
	    (setq saved-point (point))
	    (forward-line 1)
	    (delete-region saved-point (point))
;	  (delete-region (line-beginning-position) (line-end-position))
;	  (delete-char 1)
	    )
	  )
	                                    ;; "d" for delete in patch -R really just means add
;	(mapcar 'insert (nreverse a-string_stack))  ;; insert lines from a
	(mapcar #'(lambda ( x ) (insert x) (7zr-newline)) (nreverse a-string_stack))
	(set-buffer diff-buffer)
	(goto-char current_match_pos)
	) ; let*
      ) ; while   
;    (kill-buffer diff-buffer)
    (goto-char (point-max))
					; handle \ No newline at end of file msgs
    (if (and 7zr-misc_No-newline-pos1 (not 7zr-misc_No-newline-pos2))
	(backward-delete-char-untabify 1)
					; else
      (if (and (not 7zr-misc_No-newline-pos1) 7zr-misc_No-newline-pos2)
	  (newline)
	)
      )
    (list (+ linenum_param linenum_offset) hunknum-that-changed-linenum )
    ) ; let
  )
  


(defun 7zr-apply-patch-forward ( wip_buffer linenum_param )
  "This is an elisp implementation of the unix patch -p0 command, which only works on diffs of unified format, created by the unix command diff -Na.  Returns the new calculated line number."

  (let ( (diff-buffer (current-buffer)) diff-string (linenum_offset 0) (hunknum 0) (hunknum-that-changed-linenum 0) last_match_pos current_match_pos)
    (set-buffer diff-buffer)

    (goto-char (point-max))
    (while (re-search-backward 7zr-match-diff-standard-line nil t)
      
      (let* ((a-begin (string-to-number (buffer-substring (match-beginning 1)
							  (match-end 1))))
	     (a-end  (let ((b (match-beginning 3))
			   (e (match-end 3)))
		       (if b
			   (string-to-number (buffer-substring b e))
			 a-begin)))
	     (diff-type (buffer-substring (match-beginning 4) (match-end 4)))
	     (b-begin (string-to-number (buffer-substring (match-beginning 5)
							  (match-end 5))))
	     (b-end (let ((b (match-beginning 7))
			  (e (match-end 7)))
		      (if b
			  (string-to-number (buffer-substring b e))
			b-begin)))
	     (a-num_lines (- a-end a-begin))
	     (b-num_lines (- b-end b-begin))
	     saved-point
	     (still_parsing_lines t)
	     (a-string_stack ())
	     (b-string_stack ())
	     (b-begin-saved b-begin)
	     (a-begin-saved a-begin))

	 ;; fix the beginning and end numbers, because diff is somewhat
	 ;; strange about how it numbers lines
	(if (string-equal diff-type "a")
	    (setq b-end (1+ b-end)
		  a-begin (1+ a-begin)
		  a-end a-begin)
	  (if (string-equal diff-type "d")
	      (setq a-end (1+ a-end)
		    b-begin (1+ b-begin)
		    b-end b-begin)
	     ;; (string-equal diff-type "c")
	    (setq a-end (1+ a-end)
		  b-end (1+ b-end))))
	
	(setq a-num_lines (- a-end a-begin)
	      b-num_lines (- b-end b-begin))

   ;; try this and see what happens
      ;; (if (string-equal diff-type "a")
      ;; 	  (setq a-end a-begin-saved)
      ;; 	(if (string-equal diff-type "d")
      ;; 	    (setq  b-end b-begin-saved)))
					; the next lines will start with either < > or -

;	(when (re-search-forward "^>" nil t)
;	  (backward-char 1))
	(setq last_match_pos (match-beginning 0))
	(setq hunknum (1+ hunknum))
					; track current line number changes
	(when (> linenum_param a-end )

	  (setq linenum_offset (- b-end a-end)))
      

      
	(while (and 
		still_parsing_lines 
		(not (eobp)))
	  (forward-line)
	  (beginning-of-line)
	  (cond
	   ((looking-at ">")
	    (forward-char 2)
	    (push (buffer-substring-no-properties (point) (line-end-position)) b-string_stack ))
	   ((looking-at "<")
	    nil)	    
	   ((looking-at "-")
	    nil)	    
	   (t 
	    (setq still_parsing_lines nil))
	    
	   )
	  )
	
	(beginning-of-line)  
	(set-buffer wip_buffer)
	(set-buffer wip_buffer)
	(goto-char (point-min))
	(forward-line (1- a-begin))               ;; goto line in a
;	(when (not (string= diff-type "d"))	  ;; "c" and "a" are really just the same
	(dotimes (line a-num_lines line)          ;; kill lines in a
	  (setq saved-point (point))
	  (forward-line 1)
	  (delete-region saved-point (point))
;	  (delete-region (line-beginning-position) (line-end-position))
;	  (delete-char 1)
	  )
;	  )

;	(mapcar 'insert b-string_stack)          ;; insert lines from b
	(mapcar #'(lambda ( x ) (insert x) (7zr-newline)) (nreverse b-string_stack))
	(set-buffer diff-buffer)
	(goto-char last_match_pos)
	) ; let*
      ) ; while 
;    (kill-buffer diff-buffer)
    (goto-char (point-max))
					; handle \ No newline at end of file msgs
    (if (and 7zr-misc_No-newline-pos1 (not 7zr-misc_No-newline-pos2))
	(newline)
					; else
      (if (and (not 7zr-misc_No-newline-pos1) 7zr-misc_No-newline-pos2)
	  (backward-delete-char-untabify 1)
	)
      )

    (list (+ linenum_param linenum_offset) hunknum-that-changed-linenum )
     
    ) ; let
  )
	

(defun 7zr-reconstruct-get-max-patch-string ()
  " sets  7zr-reconstruct-max-patch-string and 7zr-revisions_tmpbuffer_lines which are max number of lines minus 1"
  (setq 7zr-reconstruct-save-point (point))
  (goto-char (point-max))
  (beginning-of-line)
  (forward-line -1)
  (if (looking-at "\\([0-9]+\.?[0-9]*\\)")
      (progn
	(setq 7zr-reconstruct-max-patch-string (match-string-no-properties 0))
;  (setq 7zr-revisions_tmpbuffer_lines (string-to-number (format-mode-line "%l")))
	(setq 7zr-revisions_tmpbuffer_lines (line-number-at-pos))
	)
					; else

    (error (concat "something went terribly wrong in function 7zr-reconstruct-get-max-patch-string()"))
    )
  (goto-char 7zr-reconstruct-save-point)
  )



(defun 7zr-reconstruct-slow (rev rev-point)
  "This function is only called from 7zr-summary-consolidate-region
and does the same thing as 7zr-reconstruct-rev-from-patches, but slower, hence the name.  This function also returns the notes associated with each revision in the range, in a string"
  (let ( pointer current-patch	(saved-point-pos (point)) hash-of-file	hash-from-table  note (notes_string "") (notes_stack () ) )
    (setq 7zr-patch-command-called-x-times 0)

    (save-window-excursion
      (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-shell-quote-argument 7zr-original-version)))  
      (7zr-rename-file-if-exists (concat 7zr-temp-directory 7zr-original-version) (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip) )
      )
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(forward-line 1)
	(narrow-to-region (point) rev-point)
	(goto-char (point-min))
	(while (re-search-forward "^\\([0-9]+\.[0-9]+\\)" nil t) 
	  (setq current-patch (match-string-no-properties 1))
	  (save-window-excursion
	    (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " current-patch))
	    (shell-command (concat 7zr-patch-command " " (7zr-shell-quote-argument (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip)) " -i " (7zr-shell-quote-argument (concat 7zr-temp-directory current-patch))))

	    (shell-command (concat "touch -r " (7zr-shell-quote-argument (concat 7zr-temp-directory current-patch)) " " (7zr-shell-quote-argument (concat 7zr-temp-directory "7zr-datetime-reference"))))

	    (7zr-delete-file-if-exists (concat 7zr-temp-directory current-patch ))

	  ) ; save-window-excursion
;	  (setq 7zr-patch-command-called-x-times (incf 7zr-patch-command-called-x-times))  ;debug
	  (when 7zr-track-md5sum-hashes-p 
	    (setq hash-from-table (gethash current-patch 7zr-hasht))
	    (setq hash-from-file (7zr-string-trim-final-newline (7zr-string-trim-final-newline (shell-command-to-string (concat 7zr-sha1sum-command " " (7zr-shell-quote-argument (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip )) 7zr-sha1sum-post-command (7zr-awk-cmd-string 1))))))
	    (unless (string= hash-from-table hash-from-file)
	      (message (concat "revision " current-patch  " has incorrect hash!"))
	      )
	    )

	  (when (setq note (gethash current-patch 7zr-notestab)) 
	    (push note notes_stack)
	    )
	  )  ; while		
	(7zr-rename-file-if-exists (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip) (concat 7zr-temp-directory "rev" rev "_of_" 7zr-original-version))
	(7zr-delete-file-if-exists (concat 7zr-temp-directory "rev" 7zr-construct-slow_last "_of_" 7zr-original-version))
	(setq 7zr-construct-slow_last current-patch)
	)
      )
    (mapcar (lambda (x) (setq notes_string (concat notes_string  x "\n" ))) notes_stack)
    notes_string
    ) ; let
  )

(defun 7zr-new-linenum-after-diff ( linenum_param diff-file direction )
  "Returns list, first element of which is the new line number calculated after a number of lines are added above the given linenum_param, or deleted, in the case of a negative number, as would result from patching from the diff-file. The 2nd list element returned is hunk number that resulted in a change to the linenum in question, 0 if unchanged.  The direction parameter is similar to the parameter given to the patch command, e.g. -R for reverse"
  (interactive)
  (let (hunknum-that-changed-linenum hunknum n_diffbuffer n_lastbuffer n_diff_list)
    (setq n_lastbuffer (current-buffer))
    (save-window-excursion
      (find-file-read-only diff-file)
      (setq n_diffbuffer (current-buffer))
      (goto-char (point-min))
      (setq n_diff_list '())
      (setq hunknum 0)
      (setq hunknum-that-changed-linenum 0)
      (setq linenum_offset 0)
      (setq a-end-saved 0 b-end-saved 0) ;debug
      (while (re-search-forward 7zr-match-diff-standard-line nil t)
      
	(let* ((a-begin (string-to-number (buffer-substring (match-beginning 1)
							    (match-end 1))))
	       (a-end  (let ((b (match-beginning 3))
			     (e (match-end 3)))
			 (if b
			     (string-to-number (buffer-substring b e))
			   a-begin)))
	       (diff-type (buffer-substring (match-beginning 4) (match-end 4)))
	       (b-begin (string-to-number (buffer-substring (match-beginning 5)
							    (match-end 5))))
	       (b-end (let ((b (match-beginning 7))
			    (e (match-end 7)))
			(if b
			    (string-to-number (buffer-substring b e))
			  b-begin)))
	       )
	  
	  (setq hunknum (1+ hunknum))

      
	 ;; fix the beginning and end numbers, because diff is somewhat
	 ;; strange about how it numbers lines
	 ;;  (if (string-equal diff-type "a")
	 ;;      (setq b-end (1+ b-end)
	 ;; 	    a-begin (1+ a-begin)
	 ;; 	    a-end a-begin)
	 ;;    (if (string-equal diff-type "d")
	 ;; 	(setq a-end (1+ a-end)
	 ;; 	      b-begin (1+ b-begin)
	 ;; 	      b-end b-begin)
	 ;; ;; (string-equal diff-type "c")
	 ;;      (setq a-end (1+ a-end)
	 ;; 	    b-end (1+ b-end))))

	  ;; try this debug
	  ;; (if (string-equal diff-type "a")
	  ;;     (setq a-end a-begin)
	  ;;   (if (string-equal diff-type "d")
	  ;;  	(setq  b-end b-begin)))
	  (if (string-match "R" direction)
	      (if (and
		   (>= linenum_param b-begin )
		   (<= linenum_param b-end ))
		  (setq hunknum-that-changed-linenum hunknum)
	       ; else
		(setq a-end-saved a-end b-end-saved b-end) ;debug
		(when (> linenum_param b-end )
		  (setq linenum_offset (- a-end b-end)))
		)
	    ; else
	    (when (> linenum_param a-end )

	      (setq linenum_offset (- b-end a-end)))
	    )
	  
	  ) ; let*
	) ; while
      (set-buffer n_lastbuffer)
      (kill-buffer n_diffbuffer)
      (list (+ linenum_param linenum_offset) hunknum-that-changed-linenum )
      ) ;  save-windows-excursion	       
    ) ; let
  )

(defun 7zr-find-longest-line-in-current-buffer ()
  "returns the length of longest line in the buffer"
  (let ((current-line-number 1) (current-max-length 0) (current-length 0))
    (save-excursion

      (goto-char (point-min))
      (while (not (eobp))
	(setq current-length (- (line-end-position) (line-beginning-position)))
	(when (> current-length current-max-length)
	  (setq current-max-length current-length)
	  )
	(setq current-line-number (1+ current-line-number))
	(forward-line 1)
	)
      )
    current-max-length
    )
  )


(defun 7zr-max-element-of-list (first-arg &rest more-args)
  (if more-args
      (let ((max-rest (apply '7zr-max-element-of-list more-args)))
	(if (> (string-to-number first-arg) (string-to-number max-rest))
	    first-arg
	  max-rest))
    first-arg))

(defun 7zr-insert-n-spaces ( n )
  (let ((i 0))
    (while (< i n)
      (insert " ")
      (setq i (1+ i))
      )
    )
  )


(defun 7zr-list-files-in-archive ()
  "create a read-only buffer containing list of all files in the .7z archive, returning the newly created buffer.  This is like 7zr-list-revs but shows all files, not just revision number files, original, and latest.  Also, the newly created buffer only contains the names of the files, and does not include their sizes or timestamps."
  (let (list_of_revs  7zr-list-files-in-archive_lastbuffer 7zr-list-files-in-archive_tmpbuffer 7zr-list-files-in-archive_tmpbuffer2 files file sum col timetuple savepoint reached-minuses patch  
			 (ofile (concat 7zr-document-directory 7zr-archive-prefix 7zr-buffer 7zr-archive-extension)))
;    (setq 7zr-buffer-without-extension (7zr-sans-extension filename))
    (setq 7zr-list-files-in-archive_lastbuffer (current-buffer))
    (setq 7zr-list-files-in-archive_tmpbuffer (generate-new-buffer-name (concat "7zr-list-files-in-archive_of_" 7zr-buffer)))
    (get-buffer-create 7zr-list-files-in-archive_tmpbuffer)
    (set-buffer 7zr-list-files-in-archive_tmpbuffer)
    (save-window-excursion
      (call-process "7z" nil t nil "l" ofile)
      (delete-trailing-whitespace)
      (goto-char (point-min))

      (if (re-search-forward "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\} ..... [ 0-9]+ [ 0-9]+  \\(.*\\)" nil t)
	  (progn
	    
	    (setq first_file_listed (match-string-no-properties 1))
	    (setq first_file_listed_pos (match-beginning 1))
	    (setq left_margin_length (- first_file_listed_pos (line-beginning-position)))
;	    (setq beginning_line (string-to-number (format-mode-line "%l")))  ;  older emacs versions dont have (current-line)
	    (setq beginning_line (line-number-at-pos))
	    (goto-char (point-max))
	    (when (eobp)
	      (forward-line -1))
	    (forward-line -2)
	    (end-of-line)
	    (setq end_line (line-number-at-pos))

;	    (setq longest_line (7zr-find-longest-line beginning_line end_line))
	    (setq longest_line (7zr-find-longest-line-in-current-buffer))   
	    (7zr-insert-n-spaces (1+ (- longest_line (- (line-end-position) (line-beginning-position)))))
	    (setq list_of_files (delete-extract-rectangle first_file_listed_pos (point)))
	    (erase-buffer)
	    (goto-char (point-min))
	    (mapc #'(lambda ( x ) (insert x) (newline)) list_of_files)
	    (goto-char (point-min))
	    (setq current-line 1)

	    (setq end_line (- end_line beginning_line))

	    7zr-list-files-in-archive_tmpbuffer ; returns the newly created buffer
	    )
	)
      )
    )
  )


(defun 7zr-get-highest-rev-and-original-versionDEFUNCT ()
  ".  This function takes almost twice as much time to execute as the other equivalent function.  This function returns a 2 member list containing the original version filename as a string and the latest revision number as a number, as found from listing all the files in the archive.  this function is called by 7zr-update-7z-revisions-tag-in-text_rev and 7zr-created-by-message-file_increment_latest_revision_number.  highest-revision is actually called latest-revision in the code.   this function is called from 7zr-created-by-message-file_increment_latest_revision_number and 7zr-update-7z-revisions-tag-in-text_rev.  This function calls 7zr-list-files-in-archive."

  (let (files-in-archive_buffer max-patch-number original-version  7zr-get-highest-rev-and-original-version_lastbuffer patch  7zname
			 (ofile (concat 7zr-document-directory 7zr-archive-prefix 7zr-buffer 7zr-archive-extension)))
        
;    (setq 7zr-buffer-without-extension (7zr-sans-extension filename))
    (setq max-patch-number 1.0)

    (setq 7zr-get-highest-rev-and-original-version_lastbuffer (current-buffer))
    (setq files-in-archive_buffer (7zr-list-files-in-archive))
    (set-buffer files-in-archive_buffer)
    (goto-char (point-min))

    (while (not (eobp))
      (looking-at ".*")
      (setq 7zname (7zr-trim (match-string-no-properties 0)))
	      
	    
      (if (and (7zr-string-starts-with-p 7zname 7zr-buffer-without-extension)  ; find original version
	       (not (string= 7zname 7zr-archive-created-by-message))  ; not the archive identifier
	       )
	  (progn
	    (setq original-version 7zname)	      
	    
	    )
	(if (7zr-string-starts-with-p 7zname 7zr-prepend-to-latest-revision)  ; find latest revision
	    (progn
	      (setq 7zr-latest-revision 7zname)		      
	      )
	  (when 
	      (not (string-match "[a-za-z]+" 7zname))   ; else it must be a number to be a patch, and so discard it if it contains a letter
	    (setq patch (string-to-number 7zname))
	    (when (> patch max-patch-number)            ; if patch is higher it becomes highest
	      (setq max-patch-number patch)	  
	      )	    
	    )	  
	  )
	)
      (beginning-of-line)
      (forward-line 1)	          
      (setq current-line (1+ current-line))
      )          ; while
    (setq 7zr-original-version original-version)
    (setq 7zr-patch-number max-patch-number)
    (kill-buffer files-in-archive_buffer)
    (set-buffer 7zr-get-highest-rev-and-original-version_lastbuffer)
    (list original-version max-patch-number) ; returns a string and a number
    ) ; let  
  )

(defun 7zr-get-highest-rev-and-original-version ()
  "Old version.  returns a 2 member list containing the original version filename as a string, and the latest revision number as a number, as found from listing all the files in the archive."
  (let ( 7zr-buffer-without-extension  7zr-get-highest-rev-and-original-version_lastbuffer 7zr-list-revs_tmpbuffer 7zr-list-revs_tmpbuffer2 (ofile 7zr-archive-name)
 			 files file sum col timetuple savepoint reached-minuses patch highest-patch 7zdatetime 7zsize listing)    
     
    (setq 7zr-buffer-without-extension (7zr-sans-extension 7zr-buffer)) 
    (setq 7zr-patch-number 0.0)
    (setq 7zr-get-highest-rev-and-original-version_lastbuffer (current-buffer))
    (setq 7zr-list-revs_tmpbuffer (generate-new-buffer-name (concat "7zr-list-revs_of_" 7zr-buffer))) 
    (get-buffer-create 7zr-list-revs_tmpbuffer)
 					;	  (setf (buffer-local-value '7zr-active-document 7zr-list-revs_tmpbuffer) '7zr-buffer)    
    (set-buffer 7zr-list-revs_tmpbuffer)
 			       ;	  (make-local-variable '7zr-archive-directory)
 	  ;(setq 7zr-archive-directory (buffer-local-value '7zr-archive-directory 7zr-list-revs_original_buffer))
 ;(erase-buffer)
    (save-window-excursion
      (call-process "7z" nil t nil "l" ofile)
      (goto-char (point-min))      
      (beginning-of-line)
      (while (re-search-forward "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\} ..... [ 0-9]+ [ 0-9]+  \\(.*\\)" nil t) 
 	(setq listing (match-string-no-properties 1))
 	(beginning-of-line)	 	    
 	(if (and (7zr-string-starts-with-p listing 7zr-buffer-without-extension)  ; find original version
 		 (not (string= listing 7zr-archive-created-by-message))  ; not the archive identifier
 		 )
 	    (progn
 	      (setq 7zr-original-version listing)	      	      
 	      )
 	  (if (7zr-string-starts-with-p listing 7zr-prepend-to-latest-revision)  ; find latest revision
 	      (progn
 		(setq 7zr-latest-revision listing)		
 		)
 	    (when 
 		(not (string-match "[a-zA-Z]+" listing))   ; else it must be a number to be a patch, and so discard the rest	      
 	      (setq patch (string-to-number listing))
 	      (when (> patch 7zr-patch-number)            ; if patch is higher it becomes highest
 		(setq 7zr-patch-number patch)	  
 		) 	      
 	      ) 	    
 	    )
 	  )        	
 	(forward-char)
 	)           ; while
      )
    (kill-buffer 7zr-list-revs_tmpbuffer)
    (set-buffer 7zr-get-highest-rev-and-original-version_lastbuffer)
    (list 7zr-original-version  7zr-patch-number) ; returns a string and a number
    ) ;let  
  )             



  
(defun 7zr-list-revs ( filename )
  "create a read-only buffer containing list of revisions, returning a 3 member list containing the original version filename as a string, the newly created buffer and the latest revision number as a number.  This function is only called from 7zr-line-last-changed-on, so possibly will be DEFUNCT"
  (let (7zr-patch-number 7zr-original-version 7zr-buffer-without-extension 7zr-buffer-without-extension 7zr-list-revs_lastbuffer 7zr-list-revs_tmpbuffer 7zr-list-revs_tmpbuffer2 (ofile 7zr-archive-name)
			 files file sum col timetuple savepoint reached-minuses patch highest-patch 7zdatetime 7zsize 7zname)
    
    
    (setq 7zr-buffer-without-extension (7zr-sans-extension filename)) 
    (setq 7zr-patch-number 1.0)
    (setq 7zr-list-revs_lastbuffer (current-buffer))
    (setq 7zr-list-revs_tmpbuffer (generate-new-buffer-name (concat "7zr-list-revs_of_" filename)))
    (get-buffer-create 7zr-list-revs_tmpbuffer)
    (setq 7zr-list-revs_tmpbuffer2 (generate-new-buffer-name (concat "7zr-list-revs_with-temp-buffer_of_" filename)))
    (get-buffer-create 7zr-list-revs_tmpbuffer2)
					;	  (setf (buffer-local-value '7zr-active-document 7zr-list-revs_tmpbuffer) '7zr-buffer)

	  
    (set-buffer 7zr-list-revs_tmpbuffer)
			       ;	  (make-local-variable '7zr-archive-directory)
	  ;(setq 7zr-archive-directory (buffer-local-value '7zr-archive-directory 7zr-list-revs_original_buffer))
;(erase-buffer)

    (set-buffer 7zr-list-revs_tmpbuffer2)	    
    (setq 7zr-list-revs_with-temp-buffer (current-buffer))    
    (save-window-excursion
      (call-process "7z" nil t nil "l" ofile)
      (goto-char (point-min))
      

      (beginning-of-line)
      (while (re-search-forward "^\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\) \\(.....\\) \\([ 0-9]+\\) \\([ 0-9]+\\)  \\(.*\\)" nil t)
	(setq 7zdatetime (match-string-no-properties 1))
	(setq 7zsize (match-string-no-properties 3))
	(setq 7zname (match-string-no-properties 5))
	(beginning-of-line)
	

	    
	(if (and (7zr-string-starts-with-p 7zname 7zr-buffer-without-extension)  ; find original version
		 (not (string= 7zname 7zr-archive-created-by-message))  ; not the archive identifier
		 )
	    (progn
	      (setq 7zr-original-version 7zname)	      
	      (set-buffer 7zr-list-revs_tmpbuffer)
	      (insert 7zname)
	      (insert-tab)
	      (insert 7zsize)
	      (insert-tab)

	      (newline)
	      )
	  (if (7zr-string-starts-with-p 7zname 7zr-prepend-to-latest-revision)  ; find latest revision
	      (progn
		(setq 7zr-latest-revision 7zname)
		(setq 7zr-latest-revision_size 7zsize)
		(setq 7zr-latest-revision_datetime 7zdatetime)
		)
	    (when 
		(not (string-match "[a-zA-Z]+" 7zname))   ; else it must be a number to be a patch, and so discard the rest
		    
		      
	      (set-buffer 7zr-list-revs_tmpbuffer)
	      (insert 7zname)
	      (insert-tab)
	      (insert 7zsize)
	      (insert-tab)
	      (insert 7zdatetime)
	      (newline)
		      
	      (set-buffer 7zr-list-revs_with-temp-buffer)
	      (setq patch (string-to-number 7zname))
	      (when (> patch 7zr-patch-number)            ; if patch is higher it becomes highest
		(setq 7zr-patch-number patch)	  
		)
	      
	      )
	    
	    )
	  )
	(set-buffer 7zr-list-revs_with-temp-buffer)
	
	(forward-char)
	)           ; while
      )	    
    (set-buffer 7zr-list-revs_tmpbuffer)
    (goto-char (point-min))
    (when (and          ; if 7zr-original-version is not on the first line, then put it there
	   (re-search-forward 7zr-original-version nil t)
;	   (not (string= (format-mode-line "%l") "1"))
	   (not (equal (line-number-at-pos) 1))
	   )
      (beginning-of-line)
      (let ((tmpbegin (point)) tmpend tmpsubstring)
	(forward-line 1)
	(setq tmpend (point))
	(setq tmpsubstring (buffer-substring-no-properties tmpbegin tmpend))
	(delete-region tmpbegin tmpend)
	(goto-char (point-min))
	(insert tmpsubstring)
	)
      )
    (goto-char (point-max))
    (setq 7zr-list-revs_tmpbuffer_lines (line-number-at-pos))
    (insert 7zr-latest-revision)
    (insert-tab)
    (insert 7zr-latest-revision_size)
    (insert-tab)
    (insert 7zr-latest-revision_datetime)
  
    (when (> 7zr-patch-number 1)
      (7zr-summary-sort-1))
    (goto-char (point-min))
    (7zr-set-some-buffer-local-vars "7zr-list-revs" 7zr-list-revs_lastbuffer)
    (toggle-read-only 1)
    (set-buffer-modified-p nil)
    (kill-buffer 7zr-list-revs_with-temp-buffer)
    (end-of-buffer)
    (forward-line -1)
    (beginning-of-line)
    (set-buffer 7zr-list-revs_lastbuffer)
    (list 7zr-original-version  7zr-list-revs_tmpbuffer 7zr-patch-number) ; returns a string and a buffer name as string, and a number
    ) ;let  
  )               ; 7zr-list-revs

  

(defun 7zr-line-last-changed-on ()
  "Displays the date-time and revision number of last time that the line at point has been modified (not the line number per se but the content at the given line number, which may have occupied a different line number in prior revisions because of lines deleted and/or removed above it, which is taken into account).  This function calls 7zr-list-revs and may take a considerable amount of time to complete"
  (interactive)
  (let ( find-archive-name (linenum (line-number-at-pos)) linenum_obj hunk_that_changed_line list-revs_obj revs-buffer rev rev-point pointer current-patch (saved-point-pos (point)) hash-of-file hash-from-table datetime_string 7zr-lastbuffer 7zr-buffer-file-name return_message)


    (setq find-archive-name (concat (file-name-nondirectory (buffer-file-name (current-buffer))) 7zr-archive-extension))
    (unless (file-directory-p 7zr-temp-directory)
      (make-directory 7zr-temp-directory t))
    (save-window-excursion
      (if (not (file-exists-p find-archive-name))
	  (message (concat "Archive " find-archive-name " has not yet been created yet, neither via 7z-revisions-mode nor by typing M-x 7zr-commit."))
	
	(if (string-match "No files to process" (shell-command-to-string (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " "  (7zr-shell-quote-argument find-archive-name)  " " (7zr-shell-quote-argument 7zr-archive-created-by-message))))
	    (error "Not a valid 7z-revisions.el archive file")
	; else

	  (setq 7zr-buffer (buffer-name (current-buffer)))
	  (setq 7zr-lastbuffer (current-buffer))
	  (setq 7zr-archive-name (concat 7zr-archive-prefix 7zr-buffer 7zr-archive-extension))  
	  (if  (not (setq list-revs_obj (7zr-list-revs 7zr-buffer)))
	      (error "cannot get revisions list")
	    ; else
	    (setq revs-buffer (nth 1 list-revs_obj))
	    (set-buffer revs-buffer)
	    (setq 7zr-original-version (car list-revs_obj))
	    
	    (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-shell-quote-argument (concat 7zr-prepend-to-latest-revision  7zr-original-version)))) 
	    (setq line_not_changed t)
	    (setq hunk_that_changed_line 0)
	    (message "This might take a while...")
	    (while line_not_changed

	      (if  	  (looking-at "\\([0-9]+\.[0-9]+\\)")
		  (progn
		    (setq current-patch (match-string-no-properties 1))
		    (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " "  (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " current-patch))
		    (setq diff-file  (concat 7zr-temp-directory current-patch))

		    (setq linenum_obj (7zr-new-linenum-after-diff linenum diff-file "R"))

		    (if (> (setq hunk_that_changed_line (nth 1 linenum_obj)) 0)
			(progn 
			  (setq line_not_changed nil)
			  (if (looking-at "[0-9]+\.[0-9]+[ \t]+[0-9]+[ \t]+\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\)")
			      (progn
				(setq datetime_string (match-string-no-properties 1))
				(setq return_message (concat "from rev " current-patch " last changed on " datetime_string))
				) ; progn
			    ; else
			    (error "not suppose to happen")
			    )
			  ) ; progn
		    ; else
		      (setq linenum (car linenum_obj))
		      (forward-line -1)
		    ;;;  (7zr-delete-file-if-exists (concat 7zr-temp-directory current-patch ))
		      ) ; if changed
		 
		    )  ; progn
	    ; else we are at top of list and line may  not have changed, but we'll say it did just to exit this loop, as hunk_that_changed_line of 0 is what will indicate that the line didnt change.
		(setq line_not_changed nil)
		(setq return_message "line didnt change")
		) ; if looking at a rev
	      )  ; while		
	    ) ; if cannot get revisions list
	  ) ; if not  a valid archive 
	) ; if archive doesnt exist
      ) ; windows excursion

    (set-buffer 7zr-lastbuffer)
    (kill-buffer revs-buffer)
    (message return_message) 
    ) ; let 
  )


(defun 7zr-reconstruct-rev-from-patches_advance_lastview_pointers ( current )
  (setq 7zr-pointer-lastviewed_3rd_last 7zr-pointer-lastviewed_2nd_last)  ; advance lastviewed pointers
  (setq 7zr-pointer-lastviewed_2nd_last 7zr-pointer-lastviewed_last)  ; advance lastviewed pointers
  (setq 7zr-pointer-lastviewed_last 7zr-pointer-lastviewed)           ;
  (setq 7zr-pointer-lastviewed current) 
  )


(defun 7zr-reconstruct-rev-from-patches_MSWINDOWS (rev)
  "This function is called from (7zr-summary-view-revision), after running
 (7z-revisions), once a line item is selected and the enter key is
 pressed.   It calls an elisp implementation of the patch shell command."
  (interactive)

  (setq 7zr-pointer-lastviewed 7zr-lastviewed_saved)

					; rev is the target revision that we are trying to get to, whereas 7zr-pointer-lastviewed is where we are actually still at right now, at line number 7zr-summary-last-line.

  (let ((rev_num (string-to-number rev)) saved_directory (rev_iter_string 7zr-pointer-lastviewed) (rev_iter_string_last "") shell_msg wip_buffer (wip_buffer_name (concat "rev" rev "_of_" 7zr-original-version)) diff_buffer (point_halfway (/ (point-max) 2)))
    

;    (setq 7zr-revisions_tmpbuffer '7zr-active-document_7zr-revisions_tmpbuffer)
;    (set-buffer 7zr-revisions_tmpbuffer)  ; this should instead be a buffer-local variable
    
        ;(set-buffer 7zr-active-document_buffer)
    (setq 7zr-reconstruct-patch-to-apply "")
    (setq rev_num (string-to-number rev))
;    (setq 7zr-summary-current-line (string-to-number (format-mode-line "%l")))
    (setq 7zr-summary-current-line (line-number-at-pos))
    (set (make-local-variable '7zr-active-document_current_line) 7zr-summary-current-line)
    (when (bound-and-true-p 7zr-last-line)
      (setq 7zr-view-last-line 7zr-last-line))
    (when (bound-and-true-p 7zr-last-column)
      (setq 7zr-view-last-column 7zr-last-column))
    
    (setq 7zr-reconstruct-max-patch-string "")
    (setq 7zr-patch-command-called-x-times 0)  ; debug
    (setq 7zr-valid-rev-in-tempp nil)
    (setq 7zr-pointer-lastviewed_num (string-to-number 7zr-pointer-lastviewed))
    (save-excursion
      (save-window-excursion
	(7zr-reconstruct-get-max-patch-string)  ; also gets 7zr-revisions_tmpbuffer_lines 

      ;; First check if there is a rev-pointer.  If there is and its
      ;;  closer to rev than the max ancestor or max progeny then
      ;;  construct rev from last used rev-pointer and make that one
      ;;  the last used one.  If theres no rev-pointer, or max
      ;;  ancestor or max progeny are closer to the rev target, then
      ;;  construct rev starting from latest ancester if target is
      ;;  above the mid line or 7zr-latest- if target lies below the
      ;;  mid line, and make that the latest rev-pointer

	; if there is a rev still in the temp directory lets verify to see if its valid

	(unless (string= 7zr-pointer-lastviewed "")
	  (if (7zr-revisions-validate-rev-p 7zr-pointer-lastviewed) ; 7zr-reconstruct-direction 7zr-pointer-lastviewed_nearest_ancestor)
	      (progn
		(setq 7zr-valid-rev-in-tempp t)
		(setq 7zr-pointer-lastviewed_last 7zr-pointer-lastviewed)
		(rename-file (concat 7zr-temp-directory "rev" 7zr-pointer-lastviewed "_of_" 7zr-original-version) (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip) t)
		(cond		 
		 ((eql 7zr-summary-last-line 1)
		  (setq 7zr-valid-rev-in-tempp nil))		 
		 ((and
		   (> rev_num 7zr-pointer-lastviewed_num)
		   (< (- 7zr-summary-current-line 7zr-summary-last-line) (- 7zr-revisions_tmpbuffer_lines 7zr-summary-current-line))
;;;		     (< (- rev_num 7zr-pointer-lastviewed_num) (- 7zr-revisions_tmpbuffer_lines rev_num))		     
		   )
		  (progn  
		    (setq 7zr-reconstruct-direction 1)
		    (setq 7zr-reconstruct-dtag " ")
		    ))		 
		 ((and
		   (< rev_num 7zr-pointer-lastviewed_num)
;;;		   (< (- 7zr-pointer-lastviewed_num rev_num) rev_num)   
		   (< (- 7zr-summary-last-line 7zr-summary-current-line) 7zr-summary-current-line)
		   )
		  (progn   ; we will walk upwards
		    (setq 7zr-reconstruct-direction -1)
		    (setq 7zr-reconstruct-dtag " -R -t ")
		    ))		 
		    ;; else invalid
		 (t 
		  (setq 7zr-valid-rev-in-tempp nil))
		 ) ; cond
		) ; progn
	    (setq 7zr-valid-rev-in-tempp nil)	; else invalid
	    ) ; if
	  ) ; unless no last viewed end
	
	
	(if (not 7zr-valid-rev-in-tempp)
	    (setq 7zr-pointer-lastviewed "")  ; reset pointer if invalid rev
	  )
      
    

    ; if no last rev in temp or invalid then create last rev from original or latest

	(if (not 7zr-valid-rev-in-tempp)
	    
	    (if (< (string-to-number (format-mode-line "%l")) (/ 7zr-revisions_tmpbuffer_lines 2))
	     

;	    (if (< (line-number-at-pos) (/ 7zr-revisions_tmpbuffer_lines 2))
;	    (if (< (- (point) 80) (/ (- (point-max) 80) 2))
		(progn           ; start at the top and walk downwards
		  (goto-char (point-min))
		  (forward-line 1)
		  (beginning-of-line)
		  (looking-at "\\([0-9]+\.?[0-9]*\\)")
		  (setq 7zr-pointer-lastviewed (match-string-no-properties 0))
		  (setq rev_iter_string 7zr-pointer-lastviewed)

		  (set (make-local-variable '7zr-active-document_pointer_lastviewed) 7zr-pointer-lastviewed)

		  (setq 7zr-reconstruct-direction 1)
		  (setq 7zr-reconstruct-dtag " ")
		  (setq shell-msg (shell-command-to-string (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-shell-quote-argument 7zr-original-version))))

		  (rename-file (concat 7zr-temp-directory 7zr-original-version) (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip) t)

		  )
					; else
	       ; start at bottom of buffer and walk upwards
	      (goto-char (point-max))
	      (beginning-of-line)
	      (forward-line -1)
	      (beginning-of-line)
	      (looking-at "\\([0-9]+\.?[0-9]*\\)")
	      (setq 7zr-pointer-lastviewed (match-string-no-properties 0))
	      (setq rev_iter_string 7zr-pointer-lastviewed)

	      (set (make-local-variable '7zr-active-document_pointer_lastviewed) 7zr-pointer-lastviewed)
	      (setq 7zr-reconstruct-direction -1)
	      (setq 7zr-reconstruct-dtag " -R -t ")
	      (setq shell-msg (shell-command-to-string (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-shell-quote-argument (concat 7zr-prepend-to-latest-revision  7zr-original-version)))))
	      (rename-file (concat 7zr-temp-directory 7zr-prepend-to-latest-revision 7zr-original-version) (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip) t)
		
	      ) ; if line number is upper half vs lower half ends
    ; else  we are using a previous rev thats still in the temp directory and already set up
	  (goto-char (point-min)) 
	  (forward-line (1- 7zr-summary-last-line))  ; goto-line last-line in summary list
	  ) ; if no last rev in temp ends 

	(if (string= 7zr-pointer-lastviewed rev)  ;; if the last used pointer in the temp directory happens to be the one we want

	    (progn
	      (when (= (line-number-at-pos) 2)  ; for some reason the first revision needed a special case, not sure why, debug
		(setq 7zr-reconstruct-patch-to-apply rev)

		(setq shell-msg (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " 7zr-original-version)))
		(when (and (integerp shell-msg)(equal 2 shell-msg))
		  (error (concat "failed to extract original version ")))
		(rename-file (concat 7zr-temp-directory 7zr-original-version) (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip) t)
		(setq shell-msg (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " 7zr-reconstruct-patch-to-apply)))
		(when (and (integerp shell-msg)(equal 2 shell-msg))
		  (error (concat "failed to extract patch " 7zr-reconstruct-patch-to-apply " .")))
;		(setq shell-msg (shell-command (concat 7zr-patch-command 7zr-reconstruct-dtag (7zr-shell-quote-argument (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip)) " -i " (7zr-shell-quote-argument (concat 7zr-temp-directory 7zr-reconstruct-patch-to-apply)))))
;		(when (and (integerp shell-msg) (equal 1 shell-msg))
;		  (error (concat "failed to patch")))

		(find-file (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip))
		(7z-revisions-mode 0)

		;(setq buffer-file-name nil)
		;(rename-buffer wip_buffer_name)
		(setq wip_buffer (current-buffer))
		(with-temp-buffer
		  (setq diff_buffer (current-buffer))
		  (insert-file-contents (concat 7zr-temp-directory 7zr-reconstruct-patch-to-apply)) 
;		  (find-file (concat 7zr-temp-directory 7zr-reconstruct-patch-to-apply))
;		(set-buffer-file-coding-system 7zr-buffer_file_coding_system t t)

		  (7zr-remove-all-control-Ms-from-ends-of-lines)
;		(set-buffer-modified-p nil)
		  (setq 7zr-view-last-line (car (7zr-apply-patch wip_buffer 7zr-view-last-line 1)))
		  )
		(set-buffer wip_buffer)		
		(set-buffer-modified-p nil)
		) ; special case of first revision in list
	      ) ; progn
	  
      ; else we didnt start out at the correct rev, so we must walk the pointer to the correct rev
	  (find-file (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip))
;	  (setq buffer-file-name nil)
;	  (rename-buffer wip_buffer_name)
	  (7z-revisions-mode 0)
	  (setq wip_buffer (current-buffer))
	  (set-buffer 7zr-revisions_tmpbuffer)
					; at this point, 7zr-pointer-lastviewed is now being used to point to the currently viewed rev
	  (while  (not (string= rev_iter_string rev))   ; move the pointer
	    (set-buffer 7zr-revisions_tmpbuffer)
	    (forward-line 7zr-reconstruct-direction)
	    (beginning-of-line)
	    (looking-at "\\([0-9]+\.?[0-9]*\\)")
	    (setq rev_iter_string_last rev_iter_string)           ;
	    (setq rev_iter_string (match-string-no-properties 0))        ;

	    (if (= 7zr-reconstruct-direction -1)  ; up toward ancestors 
		(progn
		  (setq 7zr-pointer-lastviewed_nearest_progeny rev_iter_string_last)
		  (set (make-local-variable '7zr-active-document_pointer_lastviewed_nearest_progeny) 7zr-pointer-lastviewed_nearest_progeny)
		  
		  (setq 7zr-reconstruct-patch-to-apply 7zr-pointer-lastviewed_nearest_progeny)
		  )
	      (if (= 7zr-reconstruct-direction 1) ; down toward progeny
		  (progn
		    (setq 7zr-pointer-lastviewed_string_nearest_ancestor rev_iter_string_last)
		    (set (make-local-variable '7zr-active-document_pointer_lastviewed_nearest_ancestor) 7zr-pointer-lastviewed_string_nearest_ancestor)
		    (setq 7zr-reconstruct-patch-to-apply rev_iter_string)
		    
		    )
		)
	      nil
	      )

	    (when (string= 7zr-reconstruct-patch-to-apply "7z")
	      (error "rare bug"))  ; rare bug

       
;;   If we have walked upward then we would need to have applied the patch of the number we're on but that of the nearest progeny  (actually, and in other words: in order to walk upward toward ancestors we would need to apply the patch of the number we're currently on, and if going downward, apply patch of nearest progeny)
	    (setq shell-msg (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " 7zr-reconstruct-patch-to-apply)))
	    (when (and (integerp shell-msg)(equal 2 shell-msg))
	      (error (concat "failed to extract patch " 7zr-reconstruct-patch-to-apply " .")))
;	    (setq shell-msg (shell-command (concat 7zr-patch-command 7zr-reconstruct-dtag (7zr-shell-quote-argument (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip)) " -i " (7zr-shell-quote-argument (concat 7zr-temp-directory 7zr-reconstruct-patch-to-apply)))))
;	    (when (and (integerp shell-msg) (equal 1 shell-msg))
;	      (error (concat "failed to patch")))
	    (with-temp-buffer
	      (setq diff_buffer (current-buffer))
	      (insert-file-contents (concat 7zr-temp-directory 7zr-reconstruct-patch-to-apply)) 
;	    (find-file-read-only (concat 7zr-temp-directory 7zr-reconstruct-patch-to-apply))
;	    (set-buffer-file-coding-system 7zr-buffer_file_coding_system t t)
	      (7zr-remove-all-control-Ms-from-ends-of-lines)

	      (setq 7zr-view-last-line (car (7zr-apply-patch wip_buffer 7zr-view-last-line 7zr-reconstruct-direction)))
	      )
	    (set-buffer wip_buffer)
	    (7z-revisions-mode 0)
	    (set-buffer-modified-p nil)
;	    (setq 7zr-view-last-line (car (7zr-new-linenum-after-diff 7zr-view-last-line (concat 7zr-temp-directory 7zr-reconstruct-patch-to-apply) 7zr-reconstruct-dtag)))


	    (set (make-local-variable '7zr-last-line) 7zr-view-last-line)
	    (set (make-local-variable '7zr-last-column) 7zr-view-last-column)
	    (7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-reconstruct-patch-to-apply))
;	    (setq 7zr-patch-command-called-x-times (incf 7zr-patch-command-called-x-times))  ;debug

	
	  ; deal with rej

	    ;; (if (file-exists-p (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip ".rej"))
	    ;; 	(progn 

	    
	    ;; 	  (if (eql 7zr-summary_discard_rejp t)
	    ;; 	      (progn
	    ;; 		(7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip ".rej"))
	    ;; 		(7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip ".orig"))
	    ;; 		)	  
	    ;; 	    (progn
	    ;; 	      (7zr-rename-file-if-exists  (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip ".rej") (concat 7zr-temp-directory 7zr-prepend-to-rej rev "_of_" 7zr-original-version ".rej"))
	    ;; 	      (7zr-rename-file-if-exists  (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip ".orig")  (concat 7zr-temp-directory 7zr-prepend-to-rej rev "_of_" 7zr-original-version ".orig"))
	    ;; 	      )
	    ;; 	    )
	    ;; 	  )
	    ;;   )
      
      ;  now get nearest ancestor or progeny pointers

	    (if (= 7zr-reconstruct-direction -1)  ; if headed up toward ancestors
		(progn
		  (beginning-of-line)
		  (setq 7zr-pointer-lastviewed_nearest_progeny rev_iter_string_last)
		  (save-excursion
		    (forward-line -1)
		    (beginning-of-line)
		    (if (looking-at 7zr-original-version)
			(setq 7zr-pointer-lastviewed_string_nearest_ancestor 7zr-original-version)
					; else
		      (progn
			(when (looking-at "\\([0-9]+\.?[0-9]*\\)") ;; debug this: sometimes it fails when continuing, but does not fail when stepping, for no apparent reason (attem2)

			(setq 7zr-pointer-lastviewed_string_nearest_ancestor (match-string-no-properties 1))
			) 
			
			)
		      )
		    )
		  )
					; else
	      (if (= 7zr-reconstruct-direction 1)  ; if headed down toward progeny
		  (progn
		    (setq 7zr-pointer-lastviewed_string_nearest_ancestor rev_iter_string_last)
		;; theres no need for checking sha1sum until we get to final targetted rev
		;    (when 7zr-track-md5sum-hashes-p
		;      (with-current-buffer 7zr-revisions_tmpbuffer
		;	(setq 7zr-reconstruct-rev_hash (gethash rev_iter_string 7zr-hasht))
		;	)
		;      )
		    (save-excursion
		      (forward-line 1)
		      (beginning-of-line)
		      (if (looking-at 7zr-latest-revision)
			  (setq 7zr-pointer-lastviewed_nearest_progeny 7zr-latest-revision)
			(progn
			  (when (looking-at "\\([0-9]+\.?[0-9]*\\)") 
			  (setq 7zr-pointer-lastviewed_nearest_progeny (match-string-no-properties 1))
			  )
			  )
			
			)
		      )
		    ) ; progn
		) ; if
	      ) ; if 
	    
	    )    ; while --> move the pointer end

	  (setq 7zr-lastviewed_saved rev_iter_string)
	  (setq 7zr-pointer-lastviewed rev_iter_string)
	  (7zr-reconstruct-rev-from-patches_advance_lastview_pointers rev_iter_string)


	;;   If we are walking upward we dont apply the patch of the one we're on but that of the nearest progeny
	;;      

	  ) ; if one we want

	(if (not (file-exists-p (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip)))
	    (progn
	      (error (number-to-string  7zr-patch-command-called-x-times))  ;debug1
		
	      )
	  )

	(rename-file (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip) (concat 7zr-temp-directory "rev" 7zr-pointer-lastviewed "_of_" 7zr-original-version) t) 
	(with-current-buffer wip_buffer
	  (rename-buffer wip_buffer_name)
	  (setq buffer-file-name (concat 7zr-temp-directory  "rev" 7zr-pointer-lastviewed "_of_" 7zr-original-version))
	  (7z-revisions-mode 0)
	  (set-buffer-modified-p 1)
	  (save-buffer)
	  (set-auto-mode)
	  (7z-revisions-mode 0)
	  (toggle-read-only 1)
 
;;	  (kill-buffer)
;;	  (find-file-read-only (concat 7zr-temp-directory "rev" 7zr-pointer-lastviewed "_of_" 7zr-original-version))
;;	  (7z-revisions-mode 0)
;;	  (setq wip_buffer (current-buffer))
;	  (write-file (concat 7zr-temp-directory "rev" 7zr-pointer-lastviewed "_of_" 7zr-original-version))
	  )
	
	(when 7zr-track-md5sum-hashes-p
	  (with-current-buffer 7zr-revisions_tmpbuffer
	    (setq 7zr-reconstruct-rev_hash (gethash 7zr-pointer-lastviewed 7zr-hasht))
	    )	    
			
	  (setq 7zr-file-rev_hash (7zr-string-trim-final-newline (7zr-string-trim-final-newline (shell-command-to-string (concat 7zr-sha1sum-command " " (7zr-shell-quote-argument (concat 7zr-temp-directory "rev" 7zr-pointer-lastviewed "_of_"  7zr-original-version)) 7zr-sha1sum-post-command (7zr-awk-cmd-string 1))))))
	  (if (not (string= 7zr-file-rev_hash 7zr-reconstruct-rev_hash))
		(message (concat "revision hash doesnt match! " 7zr-file-rev_hash)))
	    ) ; when tracking md5sums, or sha1sums as the case may be

	) ; save-window-excursion ends
      )           ; --> save-excursion ends
      
    (7zr-summary-reconst-last-del)  ; saves state
    (setq 7zr-revisions_lastbuffer (current-buffer))
    (switch-to-buffer wip_buffer)
;    (7zr-remove-all-control-Ms-from-ends-of-lines)
;; no longer have to do the following if we use a wip_buffer in lieu of a wip file
;    (setq saved_directory default-directory)
;    (find-file-read-only (concat 7zr-temp-directory "rev" 7zr-pointer-lastviewed "_of_" 7zr-original-version))
;    (7z-revisions-mode 0)
;    (cd saved_directory)
    (set (make-local-variable '7zr-active-document) 7zr-buffer)
    (7zr-set-some-buffer-local-vars "7zr-reconstruct-rev-from-patches" 7zr-revisions_lastbuffer)
;    (setf (buffer-local-value '7zr-archive-directory 7zr-revisions_lastbuffer) 'some-val)
;    (make-local-variable '7zr-archive-directory)
 ;   (setq 7zr-archive-directory (buffer-local-value '7zr-archive-directory 7zr-revisions_lastbuffer))
    (when 7zr-view_highlightp
      (7zr-view-highlight-changes t t)
      )

    (7zr-view-mode)
    (message (concat "Revised on " 7zr-view_date " " (gethash 7zr-summary-rev-at-point (buffer-local-value '7zr-notestab 7zr-active-document_last-buffer))))
    )
  )


(defun 7zr-reconstruct-rev-from-patches_LINUX (rev)
  "Original Backup copy: This function is called from 7zr-summary-view-revision, after running
 7z-revisions, once a line item is selected and the enter key is
 pressed.  The difference between this function and the other of similar naming is that this one uses the `patch' shell command whereas the other uses an elisp variant of it."
  (interactive)
  (setq 7zr-pointer-lastviewed 7zr-lastviewed_saved)

					; rev is the target revision that we are trying to get to, whereas 7zr-pointer-lastviewed is where we are actually still at right now, at line number 7zr-summary-last-line.

  (let ((rev_num (string-to-number rev)) saved_directory (rev_iter_string 7zr-pointer-lastviewed) (rev_iter_string_last "") shell_msg )


;    (setq 7zr-revisions_tmpbuffer '7zr-active-document_7zr-revisions_tmpbuffer)
;    (set-buffer 7zr-revisions_tmpbuffer)  ; this should instead be a buffer-local variable
    
        ;(set-buffer 7zr-active-document_buffer)
    (setq 7zr-reconstruct-patch-to-apply "")
    (setq rev_num (string-to-number rev))
;    (setq 7zr-summary-current-line (string-to-number (format-mode-line "%l")))
    (setq 7zr-summary-current-line (line-number-at-pos))
    (set (make-local-variable '7zr-active-document_current_line) 7zr-summary-current-line)
    (when (bound-and-true-p 7zr-last-line)
      (setq 7zr-view-last-line 7zr-last-line))
    (when (bound-and-true-p 7zr-last-column)
      (setq 7zr-view-last-column 7zr-last-column))
    
    (setq 7zr-reconstruct-max-patch-string "")
    (setq 7zr-patch-command-called-x-times 0)  ; debug
    (setq 7zr-valid-rev-in-tempp nil)
    (setq 7zr-pointer-lastviewed_num (string-to-number 7zr-pointer-lastviewed))
    (save-excursion
      (save-window-excursion
	(7zr-reconstruct-get-max-patch-string)  ; also gets 7zr-revisions_tmpbuffer_lines 

      ;; First check if there is a rev-pointer.  If there is and its
      ;;  closer to rev than the max ancestor or max progeny then
      ;;  construct rev from last used rev-pointer and make that one
      ;;  the last used one.  If theres no rev-pointer, or max
      ;;  ancestor or max progeny are closer to the rev target, then
      ;;  construct rev starting from latest ancester if target is
      ;;  above the mid line or 7zr-latest- if target lies below the
      ;;  mid line, and make that the latest rev-pointer

	; if there is a rev still in the temp directory lets verify to see if its valid

	(unless (string= 7zr-pointer-lastviewed "")

	  (if (7zr-revisions-validate-rev-p 7zr-pointer-lastviewed) ; 7zr-reconstruct-direction 7zr-pointer-lastviewed_nearest_ancestor)
	      (progn
		(setq 7zr-valid-rev-in-tempp t)
		(setq 7zr-pointer-lastviewed_last 7zr-pointer-lastviewed)
		(rename-file (concat 7zr-temp-directory "rev" 7zr-pointer-lastviewed "_of_" 7zr-original-version) (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip) t)


		(cond
		 
		 ((eql 7zr-summary-last-line 1)
		  (setq 7zr-valid-rev-in-tempp nil))
		 
		 ((and
		   (> rev_num 7zr-pointer-lastviewed_num)
		   (< (- 7zr-summary-current-line 7zr-summary-last-line) (- 7zr-revisions_tmpbuffer_lines 7zr-summary-current-line))
;;;		     (< (- rev_num 7zr-pointer-lastviewed_num) (- 7zr-revisions_tmpbuffer_lines rev_num))		     
		   )
		  (progn  
		    (setq 7zr-reconstruct-direction 1)
		    (setq 7zr-reconstruct-dtag " ")
		    ))
		 
		 ((and
		   (< rev_num 7zr-pointer-lastviewed_num)
;;;		   (< (- 7zr-pointer-lastviewed_num rev_num) rev_num)   
		   (< (- 7zr-summary-last-line 7zr-summary-current-line) 7zr-summary-current-line)
		   )
		  (progn   ; we will walk upwards
		    (setq 7zr-reconstruct-direction -1)
		    (setq 7zr-reconstruct-dtag " -R -t ")
		    ))

		 
		    ;; else invalid
		 (t 
		  (setq 7zr-valid-rev-in-tempp nil))
		 ) ; cond
		) ; progn

	    (setq 7zr-valid-rev-in-tempp nil)	; else invalid
	    ) ; if
	  ) ; when
	
	
	(if (not 7zr-valid-rev-in-tempp)
	    (setq 7zr-pointer-lastviewed "")  ; reset pointer if invalid rev
	  )
      
    

    ; if no last rev in temp or invalid then create last rev from original or latest

	(if (not 7zr-valid-rev-in-tempp)

;	    (if (< (string-to-number (format-mode-line "%l")) (/ 7zr-revisions_tmpbuffer_lines 2))
	     

	    (if (< (line-number-at-pos) (/ 7zr-revisions_tmpbuffer_lines 2))
		(progn           ; start at the top and walk downwards
		  (goto-char (point-min))
		  (forward-line 1)
		  (beginning-of-line)
		  (looking-at "\\([0-9]+\.?[0-9]*\\)")
		  (setq 7zr-pointer-lastviewed (match-string-no-properties 0))
		  (setq rev_iter_string 7zr-pointer-lastviewed)

		  (set (make-local-variable '7zr-active-document_pointer_lastviewed) 7zr-pointer-lastviewed)

		  (setq 7zr-reconstruct-direction 1)
		  (setq 7zr-reconstruct-dtag " ")
		  (setq shell-msg (shell-command-to-string (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-shell-quote-argument 7zr-original-version))))

		  (rename-file (concat 7zr-temp-directory 7zr-original-version) (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip) t)

		  )
					; else
	       ; start at bottom of buffer and walk upwards
	      (goto-char (point-max))
	      (beginning-of-line)
	      (forward-line -1)
	      (beginning-of-line)
	      (looking-at "\\([0-9]+\.?[0-9]*\\)")
	      (setq 7zr-pointer-lastviewed (match-string-no-properties 0))
	      (setq rev_iter_string 7zr-pointer-lastviewed)

	      (set (make-local-variable '7zr-active-document_pointer_lastviewed) 7zr-pointer-lastviewed)
	      (setq 7zr-reconstruct-direction -1)
	      (setq 7zr-reconstruct-dtag " -R -t ")
	      (setq shell-msg (shell-command-to-string (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-shell-quote-argument (concat 7zr-prepend-to-latest-revision  7zr-original-version)))))
	      (rename-file (concat 7zr-temp-directory 7zr-prepend-to-latest-revision 7zr-original-version) (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip) t)
		
	      ) ; if line number is upper half vs lower half
    ; else  we are using a previous rev thats still in the temp directory and already set up
	  (goto-char (point-min)) 
	  (forward-line (1- 7zr-summary-last-line))  ; goto-line last-line in summary list
	  ) ; if 

	(if (string= 7zr-pointer-lastviewed rev)  ;; if the last used pointer in the temp directory happens to be the one we want

	    (progn
	      (when (= (line-number-at-pos) 2)  ; for some reason the first revision needed a special case, not sure why, debug
		(setq 7zr-reconstruct-patch-to-apply rev)

		(setq shell-msg (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " 7zr-original-version)))
		(when (and (integerp shell-msg)(equal 2 shell-msg))
		  (error (concat "failed to extract original version ")))
		(rename-file (concat 7zr-temp-directory 7zr-original-version) (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip) t)
		(setq shell-msg (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " 7zr-reconstruct-patch-to-apply)))
		(when (and (integerp shell-msg)(equal 2 shell-msg))
		  (error (concat "failed to extract patch " 7zr-reconstruct-patch-to-apply " .")))
		(setq shell-msg (shell-command (concat 7zr-patch-command 7zr-reconstruct-dtag (7zr-shell-quote-argument (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip)) " -i " (7zr-shell-quote-argument (concat 7zr-temp-directory 7zr-reconstruct-patch-to-apply)))))
		(when (and (integerp shell-msg) (equal 1 shell-msg))
		  (error (concat "failed to patch")))
	    
		) ; special case of first revision in list
	      ) ; progn
	  
      ; else we didnt start out at the correct rev, so we must walk the pointer to the correct rev
	  (while  (not (string= rev_iter_string rev))   ; move the pointer
	    (set-buffer 7zr-revisions_tmpbuffer)
	    (forward-line 7zr-reconstruct-direction)
	    (beginning-of-line)
	    (looking-at "\\([0-9]+\.?[0-9]*\\)")
	    (setq rev_iter_string_last rev_iter_string)           ;
	    (setq rev_iter_string (match-string-no-properties 0))        ;

	    (if (= 7zr-reconstruct-direction -1)  ; up toward ancestors 
		(progn
		  (setq 7zr-pointer-lastviewed_nearest_progeny rev_iter_string_last)
		  (set (make-local-variable '7zr-active-document_pointer_lastviewed_nearest_progeny) 7zr-pointer-lastviewed_nearest_progeny)
		  
		  (setq 7zr-reconstruct-patch-to-apply 7zr-pointer-lastviewed_nearest_progeny)
		  )
	      (if (= 7zr-reconstruct-direction 1) ; down toward progeny
		  (progn
		    (setq 7zr-pointer-lastviewed_string_nearest_ancestor rev_iter_string_last)
		    (set (make-local-variable '7zr-active-document_pointer_lastviewed_nearest_ancestor) 7zr-pointer-lastviewed_string_nearest_ancestor)
		    (setq 7zr-reconstruct-patch-to-apply rev_iter_string)
		    
		    )
		)
	      nil
	      )

	    (when (string= 7zr-reconstruct-patch-to-apply "7z")
	      (error "rare bug"))  ; rare bug

       
;;   If we have walked upward then we would need to have applied the patch of the number we're on but that of the nearest progeny  (actually, and in other words: in order to walk upward toward ancestors we would need to apply the patch of the number we're currently on, and if going downward, apply patch of nearest progeny)
	    (setq shell-msg (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " 7zr-reconstruct-patch-to-apply)))
	    (when (and (integerp shell-msg)(equal 2 shell-msg))
	      (error (concat "failed to extract patch " 7zr-reconstruct-patch-to-apply " .")))
	    (setq shell-msg (shell-command (concat 7zr-patch-command 7zr-reconstruct-dtag (7zr-shell-quote-argument (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip)) " -i " (7zr-shell-quote-argument (concat 7zr-temp-directory 7zr-reconstruct-patch-to-apply)))))
	    (when (and (integerp shell-msg) (equal 1 shell-msg))
	      (error (concat "failed to patch")))
	    (setq 7zr-view-last-line (car (7zr-new-linenum-after-diff 7zr-view-last-line (concat 7zr-temp-directory 7zr-reconstruct-patch-to-apply) 7zr-reconstruct-dtag)))
	    (set (make-local-variable '7zr-last-line) 7zr-view-last-line)
	    (set (make-local-variable '7zr-last-column) 7zr-view-last-column)
	    (7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-reconstruct-patch-to-apply))
;	    (setq 7zr-patch-command-called-x-times (incf 7zr-patch-command-called-x-times))  ;debug

	
	  ; deal with rej

	    (if (file-exists-p (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip ".rej"))
		(progn 

	    
		  (if (eql 7zr-summary_discard_rejp t)
		      (progn
			(7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip ".rej"))
			(7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip ".orig"))
			)	  
		    (progn
		      (7zr-rename-file-if-exists  (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip ".rej") (concat 7zr-temp-directory 7zr-prepend-to-rej rev "_of_" 7zr-original-version ".rej"))
		      (7zr-rename-file-if-exists  (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip ".orig")  (concat 7zr-temp-directory 7zr-prepend-to-rej rev "_of_" 7zr-original-version ".orig"))
		      )
		    )
		  )
	      )
      
      ;  now get nearest ancestor or progeny pointers

	    (if (= 7zr-reconstruct-direction -1)  ; if headed up toward ancestors
		(progn
		  (beginning-of-line)
		  (setq 7zr-pointer-lastviewed_nearest_progeny rev_iter_string_last)
		  (save-excursion
		    (forward-line -1)
		    (beginning-of-line)
		    (if (looking-at 7zr-original-version)
			(setq 7zr-pointer-lastviewed_string_nearest_ancestor 7zr-original-version)
		      (progn
			(looking-at "\\([0-9]+\.?[0-9]*\\)")
			(setq 7zr-pointer-lastviewed_string_nearest_ancestor (match-string-no-properties 1)) 
			
			)
		      )
		    )
		  )
					; else
	      (if (= 7zr-reconstruct-direction 1)  ; if headed down toward progeny
		  (progn
		    (setq 7zr-pointer-lastviewed_string_nearest_ancestor rev_iter_string_last)
		    (when 7zr-track-md5sum-hashes-p
;		      (with-current-buffer 7zr-active-document_buffer
		      (setq 7zr-reconstruct-rev_hash (gethash rev_iter_string 7zr-hasht))

;			    )
		      )
		    (save-excursion
		      (forward-line 1)
		      (beginning-of-line)
		      (if (looking-at 7zr-latest-revision)
			  (setq 7zr-pointer-lastviewed_nearest_progeny 7zr-latest-revision)
			(progn
			  (looking-at "\\([0-9]+\.?[0-9]*\\)") 
			  (setq 7zr-pointer-lastviewed_nearest_progeny (match-string-no-properties 1))
			  )
			
			)
		      )
		    ) ; progn
		) ; if
	      ) ; if 
	    
	    )    ; while --> move the pointer end

	  (setq 7zr-lastviewed_saved rev_iter_string)
	  (setq 7zr-pointer-lastviewed rev_iter_string)
	  (7zr-reconstruct-rev-from-patches_advance_lastview_pointers rev_iter_string)


	;;   If we are walking upward we dont apply the patch of the one we're on but that of the nearest progeny
	;;      

	  ) ; if one we want

	(if (not (file-exists-p (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip)))
	    (progn
	      (error (number-to-string  7zr-patch-command-called-x-times))  ;debug1
		
	      )
	  )

	(rename-file (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip) (concat 7zr-temp-directory "rev" 7zr-pointer-lastviewed "_of_" 7zr-original-version) t) 
	(when 7zr-track-md5sum-hashes-p
;	    (with-current-buffer 7zr-active-document_buffer
	  (setq 7zr-reconstruct-rev_hash (gethash 7zr-pointer-lastviewed 7zr-hasht))
;	      )	    
	  (setq 7zr-file-rev_hash (7zr-string-trim-final-newline (7zr-string-trim-final-newline (shell-command-to-string (concat 7zr-sha1sum-command " " (7zr-shell-quote-argument (concat 7zr-temp-directory "rev" 7zr-pointer-lastviewed "_of_"  7zr-original-version)) 7zr-sha1sum-post-command (7zr-awk-cmd-string 1))))))
	  (if (not (string= 7zr-file-rev_hash 7zr-reconstruct-rev_hash))
		(error "revision hash doesnt match!"))
	    ) ; when tracking md5sums, or sha1sums as the case may be

	)
      )           ; --> save-excursion ends
     

 
    (7zr-summary-reconst-last-del)  ; saves state
    (setq 7zr-revisions_lastbuffer (current-buffer))
    (setq saved_directory default-directory)
    (find-file-read-only (concat 7zr-temp-directory "rev" 7zr-pointer-lastviewed "_of_" 7zr-original-version))
    (7z-revisions-mode 0)
    (cd saved_directory)
    (set (make-local-variable '7zr-active-document) 7zr-buffer)
    (7zr-set-some-buffer-local-vars "7zr-reconstruct-rev-from-patches" 7zr-revisions_lastbuffer)
;    (setf (buffer-local-value '7zr-archive-directory 7zr-revisions_lastbuffer) 'some-val)
;    (make-local-variable '7zr-archive-directory)
 ;   (setq 7zr-archive-directory (buffer-local-value '7zr-archive-directory 7zr-revisions_lastbuffer))
    (when 7zr-view_highlightp
      (7zr-view-highlight-changes t t)
      )

    (7zr-view-mode)
    (message (concat "Revised on " 7zr-view_date " " (gethash 7zr-summary-rev-at-point (buffer-local-value '7zr-notestab 7zr-active-document_last-buffer))))
    )
  )


 
(defun 7zr-summary-reconst-last-del ()
  "saves state of summary for future runs"
  (when (and
	 (boundp '7zr-summary-reconst-last)
	 (not (string= 7zr-summary-reconst-last 7zr-pointer-lastviewed)))
    (when  (not (string= 7zr-summary-reconst-last ""))
      (7zr-delete-file-if-exists (concat 7zr-temp-directory "rev" 7zr-summary-reconst-last "_of_" 7zr-original-version))
      (7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-summary-reconst-last))
      (7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-reconstruct-patch-to-apply))
      (setq 7zr-summary-last-line (line-number-at-pos))   ;debug
      (setq 7zr-pointer-lastviewed_last_nearest_progeny 7zr-pointer-lastviewed_nearest_progeny)
      ;; debug, thrown this in temporarily
     (unless (string= 7zr-pointer-lastviewed 7zr-reconstruct-max-patch-string)
	(7zr-delete-file-if-exists (concat 7zr-temp-directory "rev" 7zr-reconstruct-max-patch-string "_of_" 7zr-original-version))
	(7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-reconstruct-max-patch-string))
	)
     )

    )
  (setq 7zr-summary-last-line (line-number-at-pos))
  (setq 7zr-summary-reconst-last 7zr-pointer-lastviewed)
  )



(defvar 7zr-match-diff3-line "^====\\(.?\\)\C-m?$"
  "Pattern to match lines produced by diff3 that describe differences.")

(defvar 7zr-match-diff-line
  (let ((x "\\([0-9]+\\)\\(\\|,\\([0-9]+\\)\\)"))
    (concat "^" x "\\([acd]\\)" x "\C-m?$"))
  "Pattern to match lines produced by diff that describe differences.")

(defvar 7zr-match-diff-line2
  (let ((x "\\([0-9]+\\)\\(\\|,\\([0-9]+\\)\\)"))
    (concat "^" x "\\([acd]\\)" x "\C-m?$"))
  "Pattern to match lines produced by diff that describe differences.")

(defvar 7zr-match-diff-standard-line
  (let ((x "\\([0-9]+\\)\\(\\|,\\([0-9]+\\)\\)"))
    (concat "^" x "\\([acd]\\)" x "\C-m?$"))
  "Pattern to match lines produced by diff that describe differences.")

(defvar 7zr-match-diff-standard-hunk
  (let ((x "\\([0-9]+\\)\\(\\|,\\([0-9]+\\)\\)")
        (a "< \\(.*\\)")
	(b "> \\(.*\\)"))
    (concat "^" x "\\([acd]\\)" x "\C-j\\(< .*\C-j\\)*\\(---\\)?\C-j?\\(> .*\C-j\\)*" )
    )
  "matches all diff-types crudely from a standard diff.")

(defvar 7zr-match-sha1-hashtable-entry
  "(puthash \"\\([0-9]+\.[0-9]+\\)\" \"\\([a-f0-9]+\\)\" 7zr-hasht)"
  "matches each line in the hashtable file that contains sha1 hashfiles of revisions"
)

(defvar 7zr-match-notes-hashtable-entry
  "(puthash \"\\([0-9]+\.[0-9]+\\)\" \"\\(.*\\)\" 7zr-notestab)"
  "matches each line in the hashtable file that contains notes on revisions"
)

(defvar 7zr-match-notes-hashtable-entry_color
  "(puthash \"c\\([0-9]+\.[0-9]+\\)\" \"\\([0-7]\\)\" 7zr-notestab)"
  "matches each line in the hashtable file that contains notes on revisions"
)

(defun 7zr-view-highlight-changes ( ancestorp progenyp )
  "Highlights changes in 7zr-view and enables the d key to jump
to the next difference in the buffer, and the e key to jump to
the previous.  This function is called from 7zr-reconstruct-rev-from-patches_LINUX, 7zr-reconstruct-rev-from-patches_MSWINDOWS and 7zr-summary-view-revision"

  (setq 7zr-highlight-changes_diff_list '())
  (setq 7zr-highlight-changes_diff_listv [])
  (setq 7zr-highlight-changes_num_of_diff_changes 0)

  (cond ((and ancestorp progenyp)
	 (setq 7zr-highlight-changes_diff_list (append 7zr-highlight-changes_diff_list (7zr-parse-standard-diff-type t)))

	 (setq 7zr-highlight-changes_diff_list (append 7zr-highlight-changes_diff_list (7zr-parse-standard-diff-type nil)))

	)
	(ancestorp
	 (setq 7zr-highlight-changes_diff_list (append 7zr-highlight-changes_diff_list (7zr-parse-standard-diff-type t))))
	(progenyp 
	 (setq 7zr-highlight-changes_diff_list (append 7zr-highlight-changes_diff_list (7zr-parse-standard-diff-type nil))))
	(t 
	 (error "something went wrong in 7zr-view-highlight-changes"))
	)

 ;; (let* ((#1=#:v 7zr-revisions_tmpbuffer))
 ;; 	    (with-current-buffer #1#
 ;; 	      (set (make-local-variable '7zr-active-document) 7zr-buffer)))

  (when (> (setq 7zr-highlight-changes_num_of_diff_changes (length 7zr-highlight-changes_diff_list)) 0)

    (setq 7zr-highlight-changes_current_buffer (current-buffer))
    (setq 7zr-diff-queue_length (length 7zr-highlight-changes_diff_list))
    (setq 7zr-diff-queue (sort 7zr-highlight-changes_diff_list '<))
;    (setq 7zr-highlight-changes_diff_listv (vconcat 7zr-highlight-changes_diff_list))
;    (setq-local 7zr-diff-queue 7zr-highlight-changes_diff_listv)
; (let* ((#1=#:v 7zr-highlight-changes_current_buffer)) 	    (with-current-buffer #1#  	      (set (make-local-variable '7zr-diff-queue) 7zr-highlight-changes_diff_listv)))


    
    (setq 7zr-diff-queue_position 0)
    (setq 7zr-view-highlighted_buffer (current-buffer))

    )
  )

(defun 7zr-remove-No-newline-at-end-of-file-msgs-from-current-buffer ()
  "Removes from the current buffer all of the annoying warning messages seen in diffs performed against files that dont end in newlines, in the form of \ No newline at end of file"

  (goto-char (point-min))
  (while (re-search-forward "^\\\\ No newline at end of file[
]+" nil t)
    (replace-match "")
    )
  (goto-char (point-min))
  )  

(defun 7zr-remove-No-newline-msgs-and-return-their-locations-via-globals ()
  "This function is called while in the presence of a buffer containing a diff file.  It removes from the current buffer all of the annoying warning messages seen in diffs performed against files that dont end in newlines, in the form of \ No newline at end of file, but also sets the global variables 7zr-misc_No-newline-pos1 and 7zr-misc_No-newline-pos2 to t or nil depending on whether there was a \ No newline msg on the left or right side, or both, of the greater than or less than sign of the diff, respectively.  This function becomes necessary as just one newline character at the end of a file can change its checksum value."

  (goto-char (point-min))
  (if (setq 7zr-misc_No-newline-pos1 (re-search-forward "^\\\\ No newline at end of file[
]+" nil t))
      (if (= 7zr-misc_No-newline-pos1 (point-max))  ;; if the message is at the end of the buffer then there is only one and we are done.
	  (progn
	    (setq 7zr-misc_No-newline-pos1 nil)
	    (setq 7zr-misc_No-newline-pos2 t)
	    (replace-match "")
	    )
					; else there may just be 2 messages in the buffer
	(setq 7zr-misc_No-newline-pos1 t)
	(if (setq 7zr-misc_No-newline-pos2 (re-search-forward "^\\\\ No newline at end of file[
]+" nil t))
	    (progn
	      (replace-match "")
	      (setq 7zr-misc_No-newline-pos2 t)
	      )
					; else nope, there was only one message
	  (setq 7zr-misc_No-newline-pos2 nil)	
	  )
	)
    )
  (goto-char (point-min))
  )

(defun 7zr-remove-all-control-Ms-from-ends-of-lines ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "$" nil t)
    (replace-match "")
    )
  (goto-char (point-min))
  )



(defun 7zr-add-dos-newlines-to-diff ()
  "DEFUNCT"
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "^---$" nil t)
    (insert "")
    (forward-line 1)
    (beginning-of-line)
    )

  (goto-char (point-min))
  (while (re-search-forward "^[0-9]" nil t)
    (end-of-line)
    (insert "")
    (forward-line 1)
    (beginning-of-line)
    )
  (goto-char (point-min))
  (while (re-search-forward "
" nil t)
    (replace-match "
")
    )
  (goto-char (point-min))
  
  )

(defun 7zr-make-sure-there-are-only-dos-newlines ()
  "DEFUNCT"
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "[^]$" nil t)
    (insert "")
    (forward-line 1)
    (beginning-of-line)
    )
  (goto-char (point-min))
  )

(defun 7zr-set-archive-directory( target_directory )
  (let ((saved_dir default-directory))
    (cd target_directory)
    (setq 7zr-archive-directory default-directory)
    (cd saved_dir)
    )
  )

(defun 7zr-set-archive-directory-full( target_directory )
  (let ((saved_dir default-directory))
    (cd target_directory)
    (setq 7zr-archive-directory-full default-directory)
    (cd saved_dir)
    )
  )
    
(defun 7zr-parse-standard-diff-type ( parse-ancestorp )
  "This function returns a list containing the beginning point of
every difference and is only used for highlighting changes when
reviewing revisions.  An unintended consequence of running this function is that it causes the line of the highlighted text to become untabified when calling the function (7zr-map-difference-ranges-to-point-ranges).  This function is called from (7zr-view-highlight-changes)"
  (setq 7zr-parse-standard-diff-type_diff_list '())
  (message (concat "archive directory: " 7zr-active-document_archive-directory-full))
;  (cd 7zr-archive-directory-full)
  (save-window-excursion
    (if parse-ancestorp
	(progn

	  (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-active-document_archive-directory-full 7zr-archive-name)) " " 7zr-pointer-lastviewed))
	  (setq 7zr-parse-standard-diff-type_diff-file 7zr-pointer-lastviewed)
;	  (7zr-rename-file-from-temp-if-exists  7zr-pointer-lastviewed (concat "7zr-parse-" 7zr-original-version))
	  )
					; else
      (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-active-document_archive-directory-full 7zr-archive-name)) " " 7zr-pointer-lastviewed_nearest_progeny))
      (setq 7zr-parse-standard-diff-type_diff-file 7zr-pointer-lastviewed_nearest_progeny)      
;      (7zr-rename-file-from-temp-if-exists 7zr-pointer-lastviewed_nearest_progeny (concat  "7zr-parse-" 7zr-original-version))
      )
    )
  
  (setq 7zr-parse-standard-diff-type_lastbuffer (current-buffer))
;  (with-temp-buffer
    (setq 7zr-parse-create-blank-file-tmpbuffer (generate-new-buffer-name "parse"))   ;; debugging
    (get-buffer-create 7zr-parse-create-blank-file-tmpbuffer)  ;; debugging
    (set-buffer 7zr-parse-create-blank-file-tmpbuffer)    ;; debugging

    (setq 7zr-misc_save-directory-path default-directory)
    (cd 7zr-temp-directory)

    (insert-file-contents 7zr-parse-standard-diff-type_diff-file)
;      (find-file (concat "7zr-parse-" 7zr-original-version))
;    (set-buffer-file-coding-system 7zr-buffer_file_coding_system t t)
    (cd 7zr-misc_save-directory-path)  
;  (7z-revisions-mode 0)

  
    
    (7zr-remove-No-newline-at-end-of-file-msgs-from-current-buffer)
    (7zr-remove-all-control-Ms-from-ends-of-lines)
;  (set-buffer-modified-p nil)  ; mark no changes to buffer
;  (toggle-read-only 1)
    (setq 7zr-parse-standard-diff-type_diffbuffer (current-buffer))
  ;(set-buffer 7zr-parse-standard-diff-type_diffbuffer)
  
    (goto-char (point-min))

    (while (re-search-forward 7zr-match-diff-standard-line nil t)
      (let* ((a-begin (string-to-number (buffer-substring (match-beginning 1)
							  (match-end 1))))
	     (a-end  (let ((b (match-beginning 3))
			   (e (match-end 3)))
		       (if b
			   (string-to-number (buffer-substring b e))
			 a-begin)))
	     (diff-type (buffer-substring (match-beginning 4) (match-end 4)))
	     (b-begin (string-to-number (buffer-substring (match-beginning 5)
							  (match-end 5))))
	     (b-end (let ((b (match-beginning 7))
			  (e (match-end 7)))
		      (if b
			  (string-to-number (buffer-substring b e))
			b-begin)))
	     (a-num_lines (- a-end a-begin))
	     (b-num_lines (- b-end b-begin))
	     
	     ; (hunk (match-string-no-properties 0))

	     ; (hunk-a (replace-regexp-in-string "^---\n\\(.*\C-j\\).*" ""  hunk))
	     ; (hunk-a (replace-regexp-in-string "^[<|>] " "" hunk-a)
	     ; (hunk-b (replace-regexp-in-string "\\(.*\C-j\\).*\C-j---\C-j" ""   hunk))
	     ; (hunk-b (replace-regexp-in-string "^[<|>] " "" hunk-b)

	     (a-column_pos [])
	     (b-column_pos [])
	     (a-line_pos [])
	     (b-line_pos [])
	     (a-words [])
	     (b-words [])
	     (b-begin-saved b-begin)
	     (a-begin-saved a-begin)
	     a-offset-points b-offset-points a-begin-pt a-end-pt b-begin-pt b-end-pt
	     c-begin c-end c-begin-pt c-end-pt column_current column_last difference-ranges)
      
	 ;; fix the beginning and end numbers, because diff is somewhat
	 ;; strange about how it numbers lines
	(if (string-equal diff-type "a")
	    (setq b-end (1+ b-end)
		  a-begin (1+ a-begin)
		  a-end a-begin)
	  (if (string-equal diff-type "d")
	      (setq a-end (1+ a-end)
		    b-begin (1+ b-begin)
		    b-end b-begin)
	     ;; (string-equal diff-type "c")
	    (setq a-end (1+ a-end)
		  b-end (1+ b-end))))

 ;      (when (and (not parse-ancestorp) (string= diff-type "d"))

	(setq a-num_lines (- a-end a-begin)
	      b-num_lines (- b-end b-begin))

   ;; try this and see what happens
      ;; (if (string-equal diff-type "a")
      ;; 	  (setq a-end a-begin-saved)
      ;; 	(if (string-equal diff-type "d")
      ;; 	    (setq  b-end b-begin-saved)))
     
	(dotimes (line a-num_lines line)
	  (forward-line)
	  (beginning-of-line)
	  (forward-char 2)
	  (setq a-offset_point_last (point) )
	  (setq column_last 0)
	  (while (and (not (looking-at "\n"))
		      (not (eobp)))
	  
	    (forward-char 1)
	    (skip-chars-forward "^\n ")
	    (setq column_current (- (current-column) 2))
	    (setq a-column_pos (vconcat a-column_pos (make-vector 1 (list column_last column_current))))
	    (setq a-words (vconcat a-words (make-vector 1 (buffer-substring-no-properties a-offset_point_last (point)))))
	    (setq a-line_pos (vconcat a-line_pos (make-vector 1 (+ a-begin line))))
	    (setq a-offset_point_last (point))
	    (setq column_last column_current)
	    )
	  )
	(beginning-of-line)  
	
	(dotimes (line b-num_lines line)
	  (forward-line)
	  (beginning-of-line)
	  (if (looking-at "---")
	      (forward-line 1))
	  (forward-char 2)
	  (setq b-offset_point_last (point) )
	  (setq column_last 0)
	  (while (and (not (looking-at "\n"))
		      (not   (eobp)))
	    (forward-char 1)
	    (skip-chars-forward "^\n ")
	    (setq column_current (- (current-column) 2))
	    (setq b-column_pos (vconcat b-column_pos (make-vector 1 (list column_last column_current))))
	    (setq b-words (vconcat b-words (make-vector 1 (buffer-substring-no-properties b-offset_point_last (point)))))
	    (setq b-line_pos (vconcat b-line_pos (make-vector 1 (+ b-begin line))))
	    (setq b-offset_point_last (point))
	    (setq column_last column_current)
	    )
	  )
;(incf cntr)(when (= cntr 17)(debug)) ; debug2
	
	(set-buffer 7zr-parse-standard-diff-type_lastbuffer)
	

	
	(cond ((and 
		parse-ancestorp
		(not (string= diff-type "d")))
	       (setq difference-ranges (7zr-longest-common-word-subsequence-difference-ranges a-words b-words t))

	       (setq 7zr-parse-standard-diff-type_diff_list (append 7zr-parse-standard-diff-type_diff_list (7zr-map-difference-ranges-to-point-ranges difference-ranges b-line_pos b-column_pos t))))
	      ((and 
		(not parse-ancestorp)
		(not (string= diff-type "d")))
	       (setq difference-ranges (7zr-longest-common-word-subsequence-difference-ranges a-words b-words nil))

	       (setq 7zr-parse-standard-diff-type_diff_list (append 7zr-parse-standard-diff-type_diff_list (7zr-map-difference-ranges-to-point-ranges difference-ranges a-line_pos a-column_pos nil))))
	      ((not parse-ancestorp)
	       (setq difference-ranges (list (list 0 (1- (length a-words)))))

	       (setq 7zr-parse-standard-diff-type_diff_list (append 7zr-parse-standard-diff-type_diff_list (7zr-map-difference-ranges-to-point-ranges difference-ranges a-line_pos a-column_pos nil))))
	      ) ; cond
	(set-buffer 7zr-parse-standard-diff-type_diffbuffer)

;	) ; when not delete in progeny     
	) ; let*
      ) ;while


  (kill-buffer 7zr-parse-standard-diff-type_diffbuffer)
;    ) ; with-temp-buffer end
  (set-buffer 7zr-parse-standard-diff-type_lastbuffer)
  (switch-to-buffer 7zr-parse-standard-diff-type_lastbuffer)
;  (7zr-delete-file-from-temp-if-exists (concat "7zr-parse-" 7zr-original-version))
  7zr-parse-standard-diff-type_diff_list
  
 
  ) ; 7zr-parse-standard-diff-type

(defun 7zr-parse-standard-diff-type_BACKUP ( ancestorp )
  "This function returns a list containing the beginning point of
every difference and is only used for highlighting changes when
reviewing revisions.  An unintended consequence of running this function is that the line of the highlighted text becomes untabified."
  (setq 7zr-parse-standard-diff-type_diff_list '())
  (save-window-excursion
    (if ancestorp
	(progn

	  (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " 7zr-pointer-lastviewed))
	  (7zr-rename-file-from-temp-if-exists  7zr-pointer-lastviewed (concat "7zr-parse-" 7zr-original-version))
	  )
      
      (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " 7zr-pointer-lastviewed_nearest_progeny))
      (7zr-rename-file-from-temp-if-exists 7zr-pointer-lastviewed_nearest_progeny (concat  "7zr-parse-" 7zr-original-version))
      )
    )
  
  (setq 7zr-parse-standard-diff-type_lastbuffer (current-buffer))
  (let ((save_directory default-directory))
    (find-file (concat 7zr-temp-directory "7zr-parse-" 7zr-original-version))
    (7z-revisions-mode 0)
;    (untabify (point-min) (point-max))
    (cd save_directory))

  (7zr-remove-No-newline-at-end-of-file-msgs-from-current-buffer)
  (set-buffer-modified-p nil)  ; mark no changes to buffer
  (toggle-read-only 1)
  (setq 7zr-parse-standard-diff-type_diffbuffer (current-buffer))
  (set-buffer 7zr-parse-standard-diff-type_diffbuffer)
  
  (goto-char (point-min))

  (while (re-search-forward 7zr-match-diff-standard-line nil t)
    
    (let* ((a-begin (string-to-number (buffer-substring (match-beginning 1)
							(match-end 1))))
	   (a-end  (let ((b (match-beginning 3))
			 (e (match-end 3)))
		     (if b
			 (string-to-number (buffer-substring b e))
		       a-begin)))
	   (diff-type (buffer-substring (match-beginning 4) (match-end 4)))
	   (b-begin (string-to-number (buffer-substring (match-beginning 5)
							(match-end 5))))
	   (b-end (let ((b (match-beginning 7))
			(e (match-end 7)))
		    (if b
			(string-to-number (buffer-substring b e))
		      b-begin)))
	   (a-num_lines (- a-end a-begin))
	   (b-num_lines (- b-end b-begin))
	     
	     ; (hunk (match-string-no-properties 0))

	     ; (hunk-a (replace-regexp-in-string "^---\n\\(.*\C-j\\).*" ""  hunk))
	     ; (hunk-a (replace-regexp-in-string "^[<|>] " "" hunk-a)
	     ; (hunk-b (replace-regexp-in-string "\\(.*\C-j\\).*\C-j---\C-j" ""   hunk))
	     ; (hunk-b (replace-regexp-in-string "^[<|>] " "" hunk-b)

	   (a-column_pos [])
	   (b-column_pos [])
	   (a-line_pos [])
	   (b-line_pos [])
	   (a-words [])
	   (b-words [])
	   (b-begin-saved b-begin)
	   (a-begin-saved a-begin)
	   a-offset-points b-offset-points a-begin-pt a-end-pt b-begin-pt b-end-pt
	   c-begin c-end c-begin-pt c-end-pt column_current column_last difference-ranges)
      
	 ;; fix the beginning and end numbers, because diff is somewhat
	 ;; strange about how it numbers lines
      (if (string-equal diff-type "a")
	  (setq b-end (1+ b-end)
		a-begin (1+ a-begin)
		a-end a-begin)
	(if (string-equal diff-type "d")
	    (setq a-end (1+ a-end)
		  b-begin (1+ b-begin)
		  b-end b-begin)
	     ;; (string-equal diff-type "c")
	  (setq a-end (1+ a-end)
		b-end (1+ b-end))))

 ;      (when (and (not ancestorp) (string= diff-type "d"))

      (setq a-num_lines (- a-end a-begin)
	    b-num_lines (- b-end b-begin))

   ;; try this and see what happens
      ;; (if (string-equal diff-type "a")
      ;; 	  (setq a-end a-begin-saved)
      ;; 	(if (string-equal diff-type "d")
      ;; 	    (setq  b-end b-begin-saved)))
     
      (dotimes (line a-num_lines line)
	(forward-line)
	(beginning-of-line)
	(forward-char 2)
	(setq a-offset_point_last (point) )
	(setq column_last 0)
	(while (and (not (looking-at "\n"))
		    (not (eobp)))
	  
	  (forward-char 1)
	  (skip-chars-forward "^\n ")
	  (setq column_current (- (current-column) 2))
	  (setq a-column_pos (vconcat a-column_pos (make-vector 1 (list column_last column_current))))
	  (setq a-words (vconcat a-words (make-vector 1 (buffer-substring-no-properties a-offset_point_last (point)))))
	  (setq a-line_pos (vconcat a-line_pos (make-vector 1 (+ a-begin line))))
	  (setq a-offset_point_last (point))
	  (setq column_last column_current)
	  )
	)
      (beginning-of-line)  
	
      (dotimes (line b-num_lines line)
	(forward-line)
	(beginning-of-line)
	(if (looking-at "---")
	    (forward-line 1))
	(forward-char 2)
	(setq b-offset_point_last (point) )
	(setq column_last 0)
	(while (and (not (looking-at "\n"))
		    (not   (eobp)))
	  (forward-char 1)
	  (skip-chars-forward "^\n ")
	  (setq column_current (- (current-column) 2))
	  (setq b-column_pos (vconcat b-column_pos (make-vector 1 (list column_last column_current))))
	  (setq b-words (vconcat b-words (make-vector 1 (buffer-substring-no-properties b-offset_point_last (point)))))
	  (setq b-line_pos (vconcat b-line_pos (make-vector 1 (+ b-begin line))))
	  (setq b-offset_point_last (point))
	  (setq column_last column_current)
	  )
	)
;(incf cntr)(when (= cntr 17)(debug)) ; debug2
	
      (set-buffer 7zr-parse-standard-diff-type_lastbuffer)
	
      (cond ((and 
	      ancestorp
	      (not (string= diff-type "d")))
	     (progn
	       
	       (setq difference-ranges (7zr-longest-common-word-subsequence-difference-ranges a-words b-words t))
;	       (debug)
	       (setq 7zr-parse-standard-diff-type_diff_list (append 7zr-parse-standard-diff-type_diff_list (7zr-map-difference-ranges-to-point-ranges difference-ranges b-line_pos b-column_pos t)))
	       ))
	    ((and 
	      (not ancestorp)
	      (not (string= diff-type "d")))
	     (progn
	       (setq difference-ranges (7zr-longest-common-word-subsequence-difference-ranges a-words b-words nil))
;	       (debug)
	       (setq 7zr-parse-standard-diff-type_diff_list (append 7zr-parse-standard-diff-type_diff_list (7zr-map-difference-ranges-to-point-ranges difference-ranges a-line_pos a-column_pos nil)))
	       ))
	    ((not ancestorp)
	     (progn	     		  
	       (setq difference-ranges (list (list 0 (1- (length a-words)))))
;	       (debug)
	       (setq 7zr-parse-standard-diff-type_diff_list (append 7zr-parse-standard-diff-type_diff_list (7zr-map-difference-ranges-to-point-ranges difference-ranges a-line_pos a-column_pos nil)))
	       ))
	    ) ; cond
      (set-buffer 7zr-parse-standard-diff-type_diffbuffer)

;	) ; when not delete in progeny     
      ) ; let*
    ) ;while


  (kill-buffer 7zr-parse-standard-diff-type_diffbuffer)
  (set-buffer 7zr-parse-standard-diff-type_lastbuffer)
  (switch-to-buffer 7zr-parse-standard-diff-type_lastbuffer)
  (7zr-delete-file-from-temp-if-exists (concat "7zr-parse-" 7zr-original-version))
  7zr-parse-standard-diff-type_diff_list
  
 
  ) ; 7zr-parse-standard-diff-type


(defun 7zr-make-array ( lij  init_ele)
  "Functionally equivalent to make-vector, but can accept a list as the first
argument specifying 2d or 3d matrix dimensions.
Called from (7zr-longest-common-word-subsequence-difference-ranges)"
  (if (listp lij)
      (if (eql (length lij) 2)	  
	  (progn
	    (let ( *i* *j* m-array)
	      (setq *i* (car lij))
	      (setq *j* (car (cdr lij)))
	      
	      (setq m-array (make-vector *i* []))
	      (dotimes (i *i*)
		(aset m-array i (make-vector *j* init_ele)))
	      m-array
	      )
	    )
	(if (eql (length lij) 3)
	    (progn
	       (let ( *i* *j* *k* m-array) 

		 (setq *i* (car lij))
		 (setq *j* (car (cdr lij)))
		 (setq *k* (car (cdr (cdr lij))))
		 
	      
		 (setq m-array (make-vector *i* []))

		 (dotimes (i *i*)
		   (setq m-array2 (make-vector *j* []))
		   (dotimes (j *j*)
		     (aset m-array2 j (make-vector *k* init_ele))
		     )		   
		   (aset m-array i m-array2)
		 )
		 
		 m-array
		 
		 )
	       )
	  ;else 4d or higher
	    
	  )
	)
	
    (make-vector lij init_ele)
    )
  )



(defun 7zr-aref2 ( array i &optional j )
  "Functional equivalent of aref, but can be used with multidimentional arrays.  Called from (7zr-longest-common-word-subsequence-difference-ranges)"
  (if (equal j nil)
      (aref array i)
    (progn
      (setq asub (aref array i))
      (aref asub j)
      )
    )
  )


(defun 7zr-setf2 ( array i j &optional x )
  "Functional equivalent of setf, but can be used with multidimentional arrays.  Called from (7zr-longest-common-word-subsequence-difference-ranges)."
  (if (equal x nil)
      (aset array i j)
    (progn
      (setq asub2 (aref array i ))
      (aset asub2 j x)
      )
    )
  )




(defun 7zr-longest-common-word-subsequence-difference-ranges (v1 v2 ancestorp )
  "Returns a list of ranges of consecutive word numbers that appear in v2 but do not appear in v1, given that ancestorp is true.  Otherwise returns range of words appearing in v2 and not in v1.  This function is only used for highlighting changes when reviewing revisions.  ANCESTORP means that we are looking at the difference between the current revision and its nearest respective ancestor."
  (let* ( 
	 (l1 (length v1))
	 (l2 (length v2))
	 (p1_suf l1)
	 (p2_suf l2)

	 (p1_pre 0)
	 (p2_pre 0)
	 (p1 0)
	 (p2 0)
	 (suffix nil)
	 (suffix_wn nil)
	 (prefix nil)
	 (prefix_wn nil)
	 (results [])
	 (differences nil)
	 (differences_wn nil)
	 (differences_wn_v nil)
	 (dispositions [])
	 (difference_ranges '()) 
	 range_car max1 max2 current_word_num last_word_num i lcs-length disposition dr dd dc q1 q2 resultv resultv_wn common-subsequence-cnt
	 )

;; ; find common prefix and suffix


    (while (and             ; eliminate matching words from the ending 
	    (> p1_suf 0)
	    (> p2_suf 0)
	    (string= (aref v1 (1- p1_suf)) (aref v2 (1- p2_suf))))
      (push (aref v2 (1- p2_suf)) suffix)
      (if ancestorp
	  (push (1- p2_suf) suffix_wn)
	(push (1- p1_suf) suffix_wn)
	)      	 
      (setq p1_suf (1- p1_suf))
      (setq p2_suf (1- p2_suf))
      )


    (while (and               ; eliminate matching words at the beginning
	    (< p1_pre p1_suf)
	    (< p2_pre p2_suf)
	    (string= (aref v1 p1_pre) (aref v2 p2_pre)))
      (push (aref v2  p2_pre) prefix)
      (if ancestorp
	  (push  p2_pre prefix_wn)
	(push  p1_pre prefix_wn)
	)
      (setq p1_pre (1+ p1_pre))
      (setq p2_pre (1+ p2_pre))
      )
    (setq p1 p1_pre)
    (setq p2 p2_pre)
    
   ; if the prefix and suffix positions ever meet, then we are done

    (setq p1_prefix_equals_p1_suffixp (eql p1_pre p1_suf))
    (setq p2_prefix_equals_p2_suffixp (eql p2_pre p2_suf))
 
    (cond ((or (and  p1_prefix_equals_p1_suffixp  p2_prefix_equals_p2_suffixp) 
	       
	       (and  p1_prefix_equals_p1_suffixp  (not ancestorp)) 	 
	       (and  p2_prefix_equals_p2_suffixp  ancestorp) 	 
	       )
	   nil ) ; error return nil

	  ((and 
	    p1_prefix_equals_p1_suffixp
	    ancestorp)
	   (list (list p1_pre (1- p2_suf))))         ; return added words of v2
	  ((and
	    p2_prefix_equals_p2_suffixp
	    (eql p2_pre p2_suf)
	    (not ancestorp))
	   (list (list p1_pre (1- p1_suf))))       ; return added words of v1
	  (t 
	  
     ; else proceed to find differences
	   
	   (setq results (7zr-make-array (list (+ 1 (- p1_suf p1_pre)) (+ 1 (- p2_suf p2_pre))) 0))
	   (setq dispositions (7zr-make-array (list (+ 1 (- p1_suf p1_pre))  (+ 1 (- p2_suf p2_pre))) nil))
	   
	   (setq max2 (- p2_suf p2_pre))
	   (setq max1 (- p1_suf p1_pre))
         ;  find the longest common word sequence length
	   (setq p1 0)
	   (while (< p1 max1)
	     (setq p2 0)
	     (while (< p2 max2)
	       (setq q1 (1+ p1)
		     q2 (1+ p2))
	       
	       (if (string= (aref v1 (+ p1 p1_pre)) (aref v2 (+ p2 p2_pre))) 
		   (progn
		     (7zr-setf2 results q1 q2 (1+ (7zr-aref2 results p1 p2)))
		     (7zr-setf2 dispositions q1 q2 '(1 1 1))
		     )
		 (if (> (7zr-aref2 results p1 q2) (7zr-aref2 results q1 p2))
		     (progn
		       (7zr-setf2 results q1 q2 (7zr-aref2 results p1 q2))	     
		       (7zr-setf2 dispositions q1 q2 '(1 0 0))
		       )
		   (7zr-setf2 results q1 q2 (7zr-aref2 results q1 p2))	     
		   (7zr-setf2 dispositions q1 q2 '(0 1 0))
		   )
		 )
	       (setq p2 (1+ p2))
	       )
	     (setq p1 (1+ p1))
	     )
   ; p1 and p2 are not used from here on


         ; Using the dispositions matrix backtrack the results matrix to get the actual sequence of words
	   (setq lcs-length (7zr-aref2 results q1 q2))
	   (setq common-subsequence-cnt lcs-length)
	   (while (and 
		   (> q1 0)
		   (> q2 0)
		   (setq disposition (7zr-aref2 dispositions q1 q2))
	   ;(> common-subsequence-cnt 0)   ; we dont need this if we are looking for differences and not lcs
		   )
    
	     (setq dr (car disposition))
	     (setq dc (car (cdr disposition)))
	     (setq dd (car (cdr (cdr disposition))))
	  
	     (cond ((eql dd 1)
		    (progn
		      (setq common-subsequence-cnt (1- common-subsequence-cnt))
		      (push (aref v2 (+ (1- q2) p2_pre)) resultv)
		      (push 0 differences)
		      (if ancestorp
			  (push (+ (1- q2) p2_pre) resultv_wn)
			(push (+ (1- q1) p1_pre) resultv_wn)
			)
		      )) 
		   ((and (eql dc 1) ancestorp)
		    (progn	  
		      (push (aref v2 (+ (1- q2) p2_pre)) differences)
		      (push (+ (1- q2) p2_pre) differences_wn)
		      ))
		   ((and (eql dr 1) (not ancestorp))
		    (progn	  
		      (push (aref v1 (+ (1- q1) p1_pre)) differences)
		      (push (+ (1- q1) p1_pre) differences_wn)
		      ))
		   (t nil)
		   )
	     
	     (setq q1 (- q1 dr))
	     (setq q2 (- q2 dc))
	     )  ; while
   
	; loose ends
	   (while (and (> q1 q2) (not ancestorp))	    
	     (push (aref v1 (+ (1- q1) p1_pre)) differences)
	     (push (+ (1- q1) p1_pre) differences_wn)
	     (setq q1 (1- q1))
	     )    
	   (while (and (< q1 q2) ancestorp)	    
	     (push (aref v2 (+ (1- q2) p2_pre)) differences)
	     (push (+ (1- q2) p2_pre) differences_wn)
	     (setq q2 (1- q2))
	     )    


    ; convert word difference sequences to a list of ranges of consecutive numbers

	   (if differences_wn
	       (progn
		 (setq difference_ranges '())
		 (setq differences_wn_v (vconcat differences_wn))
		 (setq range_car  (aref differences_wn_v 0))
		 (setq last_word_num (1- range_car))
		 (dotimes (i (length differences_wn_v) t)
		   (unless (eql (setq current_word_num (aref differences_wn_v i)) (1+ last_word_num))
		     (push (list range_car last_word_num) difference_ranges)
		     (setq range_car current_word_num)
		     )
		   (setq last_word_num current_word_num)
		   
		   )
		 (push (list range_car current_word_num) difference_ranges)
	
;	(pp differences)
;	(pp difference_ranges)
		 difference_ranges
		 ) 
	     nil ) ; if no differences
	   ) ; if error
	  ) ; if done early

    ) ; let*
  
  ) ; 7zr-longest-common-word-subsequence-difference-ranges


(defun 7zr-map-difference-ranges-to-point-ranges ( difference_ranges line_pos_v column_pos_v ancestorp)
  "This function returns a list of the beginning points of each
range of differences which were provided by the difference_ranges
list, and highlights the differences in the current-buffer.  This
function is only used for highlighting changes when reviewing
revisions.  An unintended consequence of running this function is that it causes the line of the highlighted text to become untabified."
  (save-excursion
    
    (toggle-read-only 0)
;    (debug)
    (let ((begin (point))
	   (end (point))
	   (difference_ranges (reverse difference_ranges))
	   (point-ranges '())
	   (beginning_point_list '())
	   line_pos_first
	   line_pos_current 
	   column_pos_current
	   line_end_position_col

;	   7zr-map-differences_column_pos_last
;	   7zr-map-difference_line_pos_last
	   )
       (if (and 
       	   (> (length difference_ranges) 0)
       	   (not (eql (car (cdr (car difference_ranges))) -1)))
	   (progn
	     (setq line_pos_first (aref line_pos_v 0))
	     (mapc
	      (lambda (x) 
		(goto-char (point-min))
		
	 ; region begins
		(setq line_pos_current (aref line_pos_v (car x)))
		(setq column_pos_current (car (aref column_pos_v (car x))))
	 ;; (when (and 
	 ;; 	(> line_pos_current  0)
	 ;; 	(>= column_pos_current 0))
		(forward-line (- line_pos_current 1))
;		(untabify (point) (line-end-position))  ;; debug: to deal with tab characters in document that cause the program to crash without error
		(end-of-line)
		(setq line_end_position_col (current-column))
		(beginning-of-line)
		(if (<= column_pos_current line_end_position_col)
		    (progn    
		      (forward-char column_pos_current)
		      (setq begin (point))
		      (push (point) beginning_point_list)
		      (setq 7zr-map-difference_line_pos_last line_pos_current)
	 
	 ; region ends

		      (setq line_pos_current (aref line_pos_v (car (cdr x))))
		      (setq column_pos_current (car (cdr (aref column_pos_v (car (cdr x))))))
		      (goto-char (point-min))
		      (forward-line (- line_pos_current 1))		
;		(forward-line  (- line_pos_current 7zr-map-difference_line_pos_last))
;		  (untabify (point) (line-end-position))    ;; debug: to deal with tab characters in document that cause the program to crash without error
		      (end-of-line)
		      (setq line_end_position_col (current-column))
		      (beginning-of-line)

		      (if (<= column_pos_current line_end_position_col )
			  (progn
			    (forward-char column_pos_current)
;		(forward-char (- column_pos_current 7zr-map-differences_column_pos_last))
			    (setq end (point))

;		(when ancestorp	(incf cntr)(when (= cntr 11)(debug)))  ;debug11
	     

	 ; highlight region
			    (if ancestorp
				(put-text-property begin end 'font-lock-face '(:background "green"))
			      (if (equal (get-text-property begin 'font-lock-face) '(:background "green"))
				  (put-text-property begin end 'font-lock-face '(:foreground "red" :background "green"))
				(put-text-property begin end 'font-lock-face '(:background "red"))
				)
			      )
	 

   ;   (add-text-properties begin end    '(comment t face highlight))
			    (list begin end)
			    ) ; progn2 ends
					; else
;			(pop beginning_point_list)
		;	(error "if2")
			) ; if2 ends
		      ) ; progn ends
					; else
;		  (error "if")
		  ) ; if ends
;	 )
		) difference_ranges)  ; mapc end
	     ) ; progn
	 ) ; if    
       (toggle-read-only 1)
       (set-buffer-modified-p nil) 
       beginning_point_list
       
       ) ; let*
    ) ; save-excursion
  ) ; 7zr-map-difference-ranges-to-point-ranges



;;;   7z-revisions-mode.el begins  ------------------------------------

(defun 7zr-create-archive ()
  "Creates the 7z archive.   This function also inserts a few entries, being 
  the original file which is timestamp appended, a file containing hash values of revisions,
  and a file, whose filename is a message that indicates that the archive was 
  created by 7z-revisions.el, inside of which contains the highest revision number, the original version name, the document name, and other information"
  (let ((buffer-name-as-a-number (string-to-number (buffer-name (current-buffer)))) (tmphashvalue "") (7zr-dot ".") (saved-working-directory default-directory))

    ;debug
  (if (or (string= 7zr-archive-directory_default "") (string= 7zr-archive-directory_default "."))
      (setq 7zr-archive-directory (file-name-directory (buffer-file-name (current-buffer))))
    (7zr-set-archive-directory 7zr-archive-directory_default)
    )    
;  (setq 7zr-archive-directory-full 7zr-archive-directory)
  (7zr-set-archive-directory-full 7zr-archive-directory)


    (if (and 
    	 (floatp buffer-name-as-a-number)
    	 (string= (buffer-name (current-buffer)) (number-to-string buffer-name-as-a-number)))
    	(error "To create a 7z-revisions.el archive from this document, it's name must not take the form of a floating number.") 
    					; else
      (unless (file-directory-p 7zr-temp-directory)
	(make-directory 7zr-temp-directory t))
      (save-excursion
	(save-window-excursion
	  (7zr-populate-initial-global-vars)
					;	first copy the original version
	  (setq 7zr-buffer-filename-extension (7zr-what-is-extension-of-filename 7zr-buffer))
	  (setq 7zr-archive-created_datetime (7zr-current-date-time-win)) 
	  (setq 7zr-original-version (concat (7zr-sans-extension (file-name-nondirectory (buffer-file-name (current-buffer)))) 7zr-archive-created_datetime 7zr-dot 7zr-buffer-filename-extension))    
	  (setq 7zr-current-original-version 7zr-original-version)
	  (make-local-variable '7zr-current-original-version)
	  (copy-file 7zr-buffer-filename  (concat 7zr-temp-directory 7zr-original-version))
	  (shell-command (concat "7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-shell-quote-argument (concat 7zr-temp-directory  7zr-original-version))))

					;       	then create a hash file
	  (cd 7zr-temp-directory)
	  
	  (setq tmphashvalue (7zr-string-trim-final-newline (7zr-string-trim-final-newline (shell-command-to-string (concat 7zr-sha1sum-command " " (7zr-shell-quote-argument 7zr-original-version) 7zr-sha1sum-post-command (7zr-awk-cmd-string 1))))))


	  (append-to-file (concat "(puthash \"" 7zr-original-version  "\" \"" tmphashvalue "\" 7zr-hasht)\n") nil (concat 7zr-prepend-to-hash-file 7zr-original-version))
	  (append-to-file (concat "(puthash \"0.0\" \"" tmphashvalue "\" 7zr-hasht)\n") nil (concat 7zr-prepend-to-hash-file 7zr-original-version))
	  (7zr-rename-file-if-exists   7zr-original-version  (concat 7zr-prepend-to-latest-revision 7zr-original-version) t)
	  (cd saved-working-directory)

					;	add original version and latest revision documents, which are the same
	
	  (shell-command (concat "7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-shell-quote-argument (concat 7zr-temp-directory 7zr-prepend-to-latest-revision  7zr-original-version))))
	  (shell-command (concat "7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-shell-quote-argument (concat 7zr-temp-directory 7zr-prepend-to-hash-file  7zr-original-version))))

	(shell-command (concat "echo> " 7zr-temp-directory (7zr-shell-quote-argument 7zr-archive-created-by-message)))
	
	(7zr-create-file-for-archive-created-by-message t)  ; the argument t means not called interactively

	  (7zr-create-blank-notes-file-and-add-to-archive)
  
	  (7zr-delete-file-from-temp-if-exists (concat 7zr-prepend-to-latest-revision 7zr-original-version))
	  (7zr-delete-file-from-temp-if-exists  (concat  7zr-prepend-to-hash-file 7zr-original-version))
	  (7zr-delete-file-from-temp-if-exists  (concat 7zr-archive-created-by-message))
	  )
	)
     ) ; if ends
    (message (concat "Created archive " 7zr-archive-name " for " 7zr-original-version))
    
    ) ; let ends

  )

(defun 7zr-get-creation-date-from-original-version ( original-version )
  " The parameter can contain a timestamp formatted either YYYY-MM-DD_hhmmss or YYYY-MM-DD_hh:mm:ss"
  (let (revstring (original-version_sans-extension (7zr-sans-extension original-version))  strlength asdf1 asdf2 return_val)

    (setq strlength (length original-version_sans-extension))
    (setq revstring (concat (nreverse (append (vconcat original-version_sans-extension) nil))))   ;; reverses a string
    (if (and   ; if we are dealing with a timestamp whose hour minute second is delimited by ":" 
	   (setq asdf1 (string-match ":" revstring))
	   (setq asdf2 (string-match ":" revstring (1+ asdf1)))
	   (equal (- asdf2 asdf1) 3)
	   (eq asdf1 2)
	   )

	  (setq return_val (substring original-version_sans-extension (- (- strlength asdf2) 14) strlength))
	  
					; else normal timestamp
      (setq return_val (substring original-version_sans-extension (- strlength 17) strlength))
      )
    )
  )
	  

      


    
(defun 7zr-rename-document-and-its-archive_files ( new-name )
  "Renames both current buffer and file it's visiting to the new-name parameter, which must include path info.  Also renames archive, commenting a record of the name change in the hashtable file as well as the notes file."
  (let (
	(old_buffer_name (buffer-name))
	(old_archive_name 7zr-archive-name)
	(old_archive_directory 7zr-archive-directory)
	(old_filename (buffer-file-name))
	old_archive_fullname
	old_original-version
	newname_extension
	(newname_sans_directory_sans_extension (7zr-sans-extension (file-name-nondirectory new-name)))
	(newname_sans_directory (file-name-nondirectory new-name))
	new_archive_fullname
	tmphashvalue
	old_note
	add_to_notesfile_string
	add_to_hashfile_string
	(namechange_date-time_string (7zr-current-date-time-win))
	(7zr-dot ".")  ; passed back from (7zr-what-is-extension-of-filename)
	)
    (7zr-revisions-load-notestab-unsecure)
    (setq old_archive_fullname (concat old_archive_directory old_archive_name))
    (setq old_original-version 7zr-original-version)
    (setq newname_extension (7zr-what-is-extension-of-filename new-name))

    (if (vc-backend old_filename)
	(vc-rename-file old_filename new-name 1)
      (7zr-rename-file-if-exists old_filename new-name 1)

      (set-visited-file-name new-name t t)
;	(rename-buffer new-name)
      (set-buffer-modified-p nil)
      )

    (7zr-populate-initial-global-vars)  ; check for archive directory tag in document
    (setq 7zr-original-version (concat newname_sans_directory_sans_extension (7zr-get-creation-date-from-original-version old_original-version) 7zr-dot newname_extension)) 
    (setq new_archive_fullname (concat 7zr-archive-directory 7zr-archive-name))

    (7zr-rename-file-if-exists old_archive_fullname new_archive_fullname)

    (7zr-7z-rn new_archive_fullname old_original-version 7zr-original-version)
    (7zr-7z-rn new_archive_fullname (concat 7zr-prepend-to-notes-file old_original-version 7zr-notes-file-extension) (concat 7zr-prepend-to-notes-file 7zr-original-version 7zr-notes-file-extension))
    (7zr-7z-rn new_archive_fullname (concat 7zr-prepend-to-latest-revision old_original-version) (concat 7zr-prepend-to-latest-revision 7zr-original-version))
    (7zr-7z-rn new_archive_fullname (concat 7zr-prepend-to-hash-file old_original-version) (concat 7zr-prepend-to-hash-file 7zr-original-version))

    ; edit metadata file original-version and document_name 
	  (7zr-update-metadata-file_tag 7zr-original-version 7zr-update-7z-revisions-tag-in-metadata-file_original-version )
	  (7zr-update-metadata-file_tag 7zr-buffer 7zr-update-7z-revisions-tag-in-metadata-file_document_name )

					; add commented entry in hashtable to keep track of name change
;    (setq tmphashvalue (7zr-s-trim-whitespace-r (shell-command-to-string (concat 7zr-sha1sum-command " " (7zr-shell-quote-argument new-name) 7zr-sha1sum-post-command (7zr-awk-cmd-string 1)))))  

    (setq add_to_hashfile_string (concat
				  "(puthash \"" 7zr-original-version   "\" \"" (number-to-string 7zr-patch-number) "\" 7zr-hasht)\n"
				  "(add-to-list '7zr-document-namechanges (list \"" old_original-version "\" \"" 7zr-original-version "\" \""  namechange_date-time_string "\"))\n" 
				  )
	  )
;    (7zr-add-to-hashfile add_to_hashfile_string)
    (7zr-add-string-to-archived-file add_to_hashfile_string 7zr-prepend-to-hash-file 7zr-original-version)

					; add an entry to the notes file
    (setq old_note (gethash (number-to-string 7zr-patch-number) 7zr-notestab))
    (setq add_to_notesfile_string (concat 
			       "(puthash \"" (number-to-string 7zr-patch-number)   "\" \"Filename changed from " old_buffer_name " to " newname_sans_directory  " on " namechange_date-time_string " :" old_note "\" 7zr-notestab)\n"
			       "(puthash \"c" (number-to-string 7zr-patch-number)   "\" \"1\" 7zr-notestab)\n"
			       )
	  )
    (7zr-add-string-to-archived-file add_to_notesfile_string 7zr-prepend-to-notes-file 7zr-original-version 7zr-notes-file-extension)
    ) ; let
  )

  

(defun 7zr-rename-document-and-its-archive ()
  "Renames current document and its respective .7z archive file, along with a few key files in the archive, commenting a record of the name change in the hashtable file as well as the notes file.
Tries to be version control aware."
  (interactive)
  
  (if (and
       (not (bound-and-true-p 7z-revisions-mode))
       (not buffer-read-only)
       )
      (message "You must be viewing a document that has 7z-revisions-mode enabled and not buffer-read-only to use this function")
					; else
    (let ((name (buffer-name)) (filename (buffer-file-name)) newname newname_sans_directory)
      (7zr-populate-initial-global-vars)  ; check for archive directory tag in document
      (7zr-get-highest-rev-and-original-version)
      (7zr-revisions-load-hashtable-unsecure)
      (if (not filename)
	  (message "Buffer '%s' is not visiting a file!" name)
	(setq newname (read-file-name "Rename document to:" name))
	(setq newname_sans_directory (file-name-nondirectory newname))
	(when (and
	       (not (string= newname_sans_directory name))
	       (not (string= newname_sans_directory ""))
	       )
	  (if (get-buffer newname)
	      (message "A buffer named '%s' already exists!" newname)
	    
	    (when (yes-or-no-p (concat "Rename " name " to " newname " ... Really?"))
	      (7zr-rename-document-and-its-archive_files newname)
	      )
	    )
	  )
	)
      )
    )
  )
    

(defun 7zr-7z-rn ( archive-filename source-filename target-filename )
  "Rename a file in a 7z archive.  First, extract the source-file, then delete the file from the archive, then rename the file, then add the new target-filename to archive.  The archive-filename parameter is expected to contain a fully qualified file name."
  (save-window-excursion
    (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument archive-filename) " " (7zr-shell-quote-argument source-filename)))
    (shell-command (concat "7z d " (7zr-shell-quote-argument archive-filename) " " (7zr-shell-quote-argument source-filename)))
    (7zr-rename-file-from-temp-if-exists source-filename target-filename)
    (shell-command (concat "7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on " (7zr-shell-quote-argument archive-filename) " " (7zr-shell-quote-argument (concat 7zr-temp-directory  target-filename))))
    (7zr-delete-file-from-temp-if-exists target-filename)
    )
  )
  


;(7zr-remove-all-colons-from-all-listings-in-archive)
(defun 7zr-remove-all-colons-from-all-listings-in-archive ()
  "call this to remove colons from the timestamps in the filenames of archived files, in order to make them compliant with microsoft windows.  Also edit them out of the hash file.   Ideally, this function should not be interactive, so as to make it easy to call this function accidently; it would be best to have to write the function call into your document and then press C-x C-e in order to execute it.  This function can only be called on a linux or mac os, i think."
  (interactive)
  (let (list-buffer last-buffer original-version listing newlisting (hashfile-listing "") (saved-working-directory default-directory))
    (unless (file-directory-p 7zr-temp-directory)
      (make-directory 7zr-temp-directory t))
    (save-excursion
      (save-window-excursion
	(setq last-buffer (current-buffer))
	(7zr-populate-initial-global-vars)
	(setq list-buffer (7zr-list-files-in-archive))
	(set-buffer list-buffer)
	(goto-char (point-min))
	(while (not (eobp))
	  (looking-at ".*")
	  (setq listing (7zr-trim (match-string-no-properties 0)))
	  (when (string-match ":" listing)

	    (setq newlisting (replace-regexp-in-string ":" "" listing))

	    (7zr-7z-rn (concat 7zr-archive-directory 7zr-archive-name) listing newlisting)
	    (message (concat "Renamed listing " listing " to " newlisting ))
	    (setq listing newlisting)
	    )
	  (when (7zr-string-starts-with-p listing 7zr-prepend-to-hash-file)  ; we need to also search and replace the hash file later
	    (setq hashfile-listing listing)		      
	    )
		
	  (beginning-of-line)
	  (forward-line 1)
	  
	  
	  (setq current-line (1+ current-line))
	  )          ; while

	

	(when (and
	       7zr-track-md5sum-hashes-p
	       (not (string= hashfile-listing ""))
	       )
	  (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-shell-quote-argument hashfile-listing)))
	  (cd 7zr-temp-directory)
	  (find-file hashfile-listing)
	  (goto-char (point-min))
;	  (when (re-search-forward "_[0-9][0-9]:[0-9][0-9]:[0-9][0-9]" nil t);
;	    (forward-word -1)
;	    (delete-char -1)
;	    (forward-word -1)
;	    (delete-char -1))

	  (while (re-search-forward ":" nil t)	    
	    (replace-match "")
	    )

	  (write-file hashfile-listing)
	  (kill-buffer list-buffer)
 	  (cd saved-working-directory)
	
	  (shell-command (concat "7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-shell-quote-argument (concat 7zr-temp-directory  hashfile-listing))))	
	  (7zr-delete-file-from-temp-if-exists hashfile-listing)
	  )
	)
      )
    
    )
  )



(defun 7zr-string-ends-in-newline-p ( input_string )
  "Returns t if input_string ends in a newline, otherwise nil."
  (let ((input_string_length (length input_string)))
    (if (string= "\n" (substring input_string (1- input_string_length) input_string_length))
	t
      nil
      )
    )
  )


(defun 7zr-add-string-to-archived-file ( string_to_add  file_prefix  file_original_version &optional file_extension populate-p )
  "This function extracts the file to the temp directory, then appends the passed string to the end of the file, then zips the file back into the archive file, deleting the extracted file afterward.  If the string_to_add parameter does not end in a newline character then one is appended to the end."
  (save-window-excursion
    (or file_extension
	(setq file_extension ""))
    (if (7zr-string-ends-in-newline-p string_to_add)
	nil
      (setq string_to_add (concat string_to_add "\n"))
      )     
;    (7zr-populate-initial-global-vars)
    (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-shell-quote-argument (concat file_prefix  file_original_version file_extension)))) ; this concat doesnt work without original-version

    (append-to-file string_to_add nil (concat 7zr-temp-directory file_prefix file_original_version file_extension))
	  
    (shell-command (concat "7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-shell-quote-argument (concat 7zr-temp-directory file_prefix  file_original_version file_extension))))
    )
  )


(defun 7zr-add-to-hashfile ( string_to_add )
  "So far this function is not being called by anything."
  (save-window-excursion
    (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-shell-quote-argument (concat 7zr-prepend-to-hash-file  7zr-original-version))))

    (append-to-file string_to_add nil (concat 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-original-version))
	  
    (shell-command (concat "7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-shell-quote-argument (concat 7zr-temp-directory 7zr-prepend-to-hash-file  7zr-original-version))))
    )
  )


(defun 7zr-create-diff-against-latest-version ()
  "current becomes latest.  7zr-patch-number global var must contain current max revision number before calling this function.  This function is called by 7zr-append-archive"
  (save-window-excursion
    (unless (file-directory-p 7zr-temp-directory)
      (make-directory 7zr-temp-directory t))
    (when (or (string= 7zr-original-version "")
	      (eql 7zr-original-version nil))
      (error "no 7zr-original-version"))
    (let ((tmphashvalue "") (7zr-patch-number-string "") (saved-working-directory default-directory))
      (setq 7zr-patch-number (1+ 7zr-patch-number))
      (setq 7zr-patch-number-string (number-to-string 7zr-patch-number))

      (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-shell-quote-argument (concat 7zr-prepend-to-latest-revision 7zr-original-version))))

      (copy-file 7zr-buffer-filename (concat 7zr-temp-directory 7zr-prepend-to-current-version 7zr-buffer) t)

      (shell-command (concat 7zr-diff-command " " (7zr-shell-quote-argument (concat 7zr-temp-directory 7zr-prepend-to-latest-revision  7zr-original-version)) " " (7zr-shell-quote-argument (concat 7zr-temp-directory 7zr-prepend-to-current-version  7zr-buffer)) " > " (7zr-shell-quote-argument (concat 7zr-temp-directory 7zr-patch-number-string))))
      (when (/= (car (nthcdr 7 (file-attributes (concat 7zr-temp-directory 7zr-patch-number-string) 'string))) 0)  ; if there are changes, i.e. if size is not zero
    
	(shell-command (concat "7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-shell-quote-argument (concat 7zr-temp-directory 7zr-patch-number-string))))

					; add to hashfile
	
	(when 7zr-track-md5sum-hashes-p
	      (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-shell-quote-argument (concat 7zr-prepend-to-hash-file  7zr-original-version))))
	      (setq tmphashvalue (7zr-s-trim-whitespace-r (shell-command-to-string (concat 7zr-sha1sum-command " " (7zr-shell-quote-argument (concat 7zr-temp-directory 7zr-prepend-to-current-version  7zr-buffer)) 7zr-sha1sum-post-command (7zr-awk-cmd-string 1)))))

	      (append-to-file (concat "(puthash \"" 7zr-patch-number-string   "\" \"" tmphashvalue "\" 7zr-hasht)\n") nil (concat 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-original-version))
	      
	      (shell-command (concat "7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-shell-quote-argument (concat 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-original-version))))  ;; for some reason 7z u doesnt seem to work when updating a dos encoded file from linux, in this instance



	      ) ; when md5sum enabled
	) ; when changes
      
      ;; because of an apparent bug, dealing with file names that contain spaces, we must first change directory to /tmp/ before calling the rename-files function, then change it back to the working directory
      (cd 7zr-temp-directory)
      (rename-file (concat 7zr-prepend-to-current-version 7zr-buffer) (concat 7zr-prepend-to-latest-revision 7zr-original-version) t)
      (cd saved-working-directory)
      
      (shell-command (concat "7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on " (7zr-shell-quote-argument 7zr-archive-directory)(7zr-shell-quote-argument 7zr-archive-name) " " (7zr-shell-quote-argument (concat 7zr-temp-directory 7zr-prepend-to-latest-revision 7zr-original-version))))
      
      ;; because of an apparent bug, dealing with file names that contain spaces, we must first change directory to /tmp/ before calling the rename-files function, then change it back to the working directory
      (cd 7zr-temp-directory)
      (7zr-delete-file-if-exists 7zr-patch-number-string)
      (when 7zr-track-md5sum-hashes-p
	(7zr-delete-file-if-exists (concat 7zr-prepend-to-hash-file 7zr-original-version))
	)
      (7zr-delete-file-if-exists (concat 7zr-prepend-to-latest-revision 7zr-original-version))
      (cd saved-working-directory)
      (message (concat "Updated " 7zr-archive-name " with rev " 7zr-patch-number-string " of " 7zr-original-version))

      ) ; let
    )
  )


(defun 7zr-append-archive ()
  "Patch to create the latest version then create a diff against that and append it to the archive."
  (let (examine_disposition)

; (7zr-examine-archive)
    (setq examine_disposition (7zr-examine-created-by-message-file))
    (if (string= examine_disposition "1")
	(progn

	  (7zr-create-diff-against-latest-version)
	  (when (bound-and-true-p 7zr-active-document_note)  ; enter a note for next revision
;	    (puthash (number-to-string 7zr-patch-number) 7zr-active-document_note 7zr-notestab)
	    (7zr-add-string-to-archived-file (concat "(puthash \"" (number-to-string 7zr-patch-number) "\" \"" 7zr-active-document_note "\" 7zr-notestab)\n") 7zr-prepend-to-notes-file 7zr-original-version 7zr-notes-file-extension)
	    (set (make-local-variable '7zr-active-document_note) nil)
	    )
	  ) ; progn
					; else
      (error (concat "uhoh, examine_disposition was " examine_disposition))
      )
    )
  )

(defun 7zr-alter-7z-revisions-tag_rev ()
  "DEFUNCT This function isnt being called by anything.  Also sets the current value in register ?8, which can be inserted in the minibuffer with C-x r 8"
;  (interactive "P")
  (set-register ?8 7zr-update-7z-revisions-tag-in-text_rev)
  (setq 7zr-update-7z-revisions-tag-in-text_rev (read-from-minibuffer (concat "Change 7z-revisions tag from " 7zr-update-7z-revisions-tag-in-text_rev  " to :" )))
  )


(defun 7zr-update-tag_original-version ()
  (goto-char (point-min))
  (when (and
	 (not (string= 7zr-update-7z-revisions-tag-in-text_original-version ""))
	 (not (string= 7zr-original-version "")))
    (save-match-data
      (setq tag-pos (search-forward 7zr-update-7z-revisions-tag-in-text_original-version nil t))
      (when tag-pos
	(looking-at ".*")
	(unless (7zr-string-starts-with-p (match-string-no-properties 0) "\")" )
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert 7zr-original-version)
	  )
	)	
      )
    )
  )

(defun 7zr-update-tag_sha1-of-last-revision ()
  "Called by 7zr-update-7z-revisions-tags-in-text, this function inserts the sha1 value of the last revision at the end of its respective tag within the document.  The function is only called if the rev tag is also present somewhere in the first 7000 characters of the document."
  (let (tag_pos last-entry-tuple last-rev last-hashvalue)
    (goto-char (point-min))

    (when (and
	   (not (string= 7zr-update-7z-revisions-tag-in-text_sha1-of-last-revision ""))
;	   (not (string= (number-to-string 7zr-patch-number) "1.0"))
	   )
      (save-match-data
	(setq tag-pos (search-forward 7zr-update-7z-revisions-tag-in-text_sha1-of-last-revision nil t))
	(when tag-pos
	  (looking-at ".*")
	  (unless (7zr-string-starts-with-p (match-string-no-properties 0) "\")" )
	    (delete-region (match-beginning 0) (match-end 0))
	    (setq tag_pos (point))
	    (setq last-entry-tuple (7zr-revisions-load-hashtable_last-entry))
	    (setq last-hashvalue (car (cdr last-entry-tuple)))
	    (setq last-rev (car last-entry-tuple))

 	    (when (string= last-rev (number-to-string 7zr-patch-number))
		(insert last-hashvalue)
		)
	    )	  
	  )
	)
      )
    )
  )
  
	    
	  	
(defun 7zr-extract-metafile-to-temp-directory ()
  "The metafile is the 7zr-archive-created-by-message file"
  (save-window-excursion
    (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-shell-quote-argument 7zr-archive-created-by-message)))
    )
  )

(defun 7zr-extract-a-file-to-temp-directory ( prefix filename &optional suffix )
  "extracts file, returning the file name in a string"
  (or suffix
      (setq suffix ""))  ; default value for suffix is blank string
    
  (save-window-excursion
    (shell-command (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-shell-quote-argument (concat prefix  filename suffix))))
    )
  (concat 7zr-temp-directory prefix filename suffix)
  )

(defun 7zr-update-a-file-from-temp-directory-to-archive ( prefix filename &optional suffix)
  "Use 7zr-extract-a-file-to-temp-directory to extract the file, and this function to zip it.  Or use 7zr-add-to-archive-file-that-has-this-prefix-and-extension to do both."
  (save-window-excursion
    (or suffix
	(setq suffix ""))  ; default value for suffix is blank string

    (shell-command (concat "7z a " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-shell-quote-argument (concat 7zr-temp-directory  prefix  filename suffix))))
    )
  )


(defun 7zr-select-tag-to-insert-into-document ()
  "If the current buffer is the archive created by message buffer, as invoked from (7zr-archive-edit-metadata-file), then use one set of tags, otherwise use another"
  (interactive)

  (if (not (string= (buffer-name (current-buffer)) 7zr-archive-created-by-message))
      (progn
	
	(let ((choices (mapcar #'car 7zr-tag_choice->tag_text)))
	  (setq tag_choice 
		(cdr (assoc (ido-completing-read "tag choice:" choices ) 7zr-tag_choice->tag_text))	  
		)
	  (if tag_choice
	      (insert (symbol-value tag_choice))
	    )
	  )
	)
    
    ; else

    (let ((choices (mapcar #'car 7zr-tag_choice->metadata-file-tag_text))) ; keys
      (setq tag_choice 
	    (cdr (assoc (ido-completing-read "metadata file tag choice:" choices ) 7zr-tag_choice->metadata-file-tag_text))	  
	    )
      (if tag_choice
	  (insert (symbol-value tag_choice))
	)
      )
    )
  )

   
(defun 7zr-update-metafile-to-archive ()
  "The metafile is the 7zr-archive-created-by-message file"
  (save-window-excursion
    (shell-command (concat "7z a " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " (7zr-shell-quote-argument (concat 7zr-temp-directory  7zr-archive-created-by-message))))
    )
  )


;(7zr-archive-edit-metadata-file)
(defun 7zr-modify-raw-notes-file ()
  "Use with caution"
  (interactive)
  
;  (if (not (bound-and-true-p 7zr-window))
;      (message "Sorry, you cannot use this function here.")
    (unless (bound-and-true-p 7zr-viewing)
      (7zr-populate-initial-global-vars)
      (7zr-get-highest-rev-and-original-version)
 ;     )
    (7zr-archive-edit-metadata-file 7zr-prepend-to-notes-file 7zr-original-version 7zr-notes-file-extension)
    )
  )


(defun 7zr-order-raw-notes-file ()
  (interactive)
  )

(defun 7zr-modify-raw-hash-file ()
  "Use with caution.  Actually should not use at all."
  (interactive)
  ;(if (not (bound-and-true-p 7zr-window))
  ;    (message "Sorry, you cannot use this function here.")

  (unless (bound-and-true-p 7zr-viewing)

    (7zr-populate-initial-global-vars)
    (7zr-get-highest-rev-and-original-version)
      
    (7zr-archive-edit-metadata-file 7zr-prepend-to-hash-file 7zr-original-version)
    )
  )

(defun 7zr-input-archive-directory ()
  (interactive)
  (let (inputted_data)
    (setq inputted_data (read-string "Enter new default relative directory of archive:"))
    (when (yes-or-no-p (concat "Change relative directory from " 7zr-archive-directory_default " to " inputted_data " really?"))
      
      (setq 7zr-archive-directory_default inputted_data)
      (7zr-set-archive-directory inputted_data)
      )
    )
  )

(defun 7zr-archive-edit-metadata-file ( &optional prefix file extension )
  (interactive)
  (or file
      (progn
	(setq file 7zr-archive-created-by-message)
	(setq prefix "")
	(setq extension "")
	))      
  (or extension
      (setq extension ""))

  (setq 7zr-last-buffer (current-buffer))

  (if (bound-and-true-p 7zr-viewing)  ; i.e. if we are in 7z-revisions listing or view summary
      (progn

	(7zr-extract-metafile-to-temp-directory)
	(find-file-read-only (concat 7zr-temp-directory 7zr-archive-created-by-message))
	(7z-revisions-mode 0)
	(set (make-local-variable '7zr-metadata-file-archive-name) 7zr-active-document_archive-name)
	(set (make-local-variable '7zr-metadata-file-archive-directory) 7zr-active-document_archive-directory)
	(set (make-local-variable '7zr-metadata-file-archive-prefix) 7zr-active-document_archive-prefix)
	(7zr-set-some-buffer-local-vars "7zr-archive-edit-metadata-file_from_7z-revisions" 7zr-last-buffer)
	(7zr-metafile-mode)
	)
					; else
    (unless (bound-and-true-p 7zr-viewing)
      (7zr-populate-initial-global-vars)
      )

    (if (file-exists-p (concat 7zr-archive-directory 7zr-archive-name))
	(progn
;	  (7zr-extract-metafile-to-temp-directory)
	  (7zr-extract-a-file-to-temp-directory prefix file extension)
;	  (find-file (concat 7zr-temp-directory 7zr-archive-created-by-message))
	  (find-file-read-only (concat 7zr-temp-directory prefix file extension))
	  (7z-revisions-mode 0)
	  (7zr-set-some-buffer-local-vars "7zr-archive-edit-metadata-file_from_document" 7zr-last-buffer)
	  (7zr-metafile-mode)
	  (set (make-local-variable '7zr-metafile_archive-name) 7zr-archive-name)
	  (set (make-local-variable '7zr-metafile_archive-directory) 7zr-archive-directory)
	  (set (make-local-variable '7zr-metafile_archive-prefix) 7zr-archive-prefix)
	  (set (make-local-variable '7zr-metafile_filename) (concat prefix file extension))
	  (set (make-local-variable '7zr-window) "7zr-archive-edit-metadata-file_from_document")

	  )
					; else
      (message "You must be in a document associated with a 7z-revisions.el archive before this function may be called.")
      )
    )
  )

(defun 7zr-archive-save-metadata-file ()
  (interactive)
   ; if current-buffer is not the archive-created-by-message file then do nothing, and error
  (let ( (archive-directory 7zr-metafile_archive-directory) (archive-name 7zr-metafile_archive-name) (archive-prefix 7zr-metafile_archive-prefix) (filename_sans_directory 7zr-metafile_filename))


;    (if (not (string-equal (buffer-name (current-buffer)) 7zr-archive-created-by-message))

    (if (not (and
	      (bound-and-true-p 7zr-window)
	      (7zr-string-starts-with-p 7zr-window "7zr-archive-edit")
	      )
	     )
	(progn
	  (error "must be viewing an archived file buffer before calling this function")
	  -1
	  )
					; else
      (unless (string= 7zr-metafile_archive-name "")
	(save-window-excursion
	  (save-buffer)       
	  (kill-buffer) ;; we apparently need to kill the buffer to flush the file system before adding to archive
	  (shell-command (concat "7z a " (7zr-shell-quote-argument (concat archive-directory  archive-prefix archive-name)) " " (7zr-shell-quote-argument (concat 7zr-temp-directory   filename_sans_directory))))   ; wait a second, isn't archive-prefix normally included in archive-name automatically?
	  )
	)
      )      
    )
  )

(defun 7zr-metafile-exit ()
  (interactive)
  (7zr-archive-save-metadata-file)
  )


(defun 7zr-update-metadata-file_tags_from_document ()
  "DEFUNCT Dont think we need this function anymore since theres now a function to edit and save the metadata file directly, called 7zr-archive-edit-metadata-file"
;  (interactive)
;  (setq 7zr-absolute-directory-of-archive (7zr-update-metadata-file_tag_from_document 7zr-update-7z-revisions-tag-in-text_absolute-directory-of-archive "absolute-directory-of-archive=" ))
  (setq 7zr-archive-directory (7zr-update-metadata-file_tag_from_document 7zr-update-7z-revisions-tag-in-text_directory-of-archive 7zr-update-7z-revisions-tag-in-metadata-file_directory-of-archive ))
;  (setq 7zr-relative-directory-of-document  (7zr-update-metadata-file_tag_from_document 7zr-update-7z-revisions-tag-in-text_directory-of-document "relative-directory-of-document="))
  )

  

(defun 7zr-find-tag-in-text ( string_of_tag_in_text )
  "returns the tag value found in the first 7000 characters of current buffer, as a string.   The 7000 character delimit is set by 7zr-tag-in-text-end-bounds."
  (save-excursion
    (save-restriction
      (widen)
      (let (tag-pos (tag_value ""))
	(when (and
	       (not (string= string_of_tag_in_text ""))
	       t)
	  (save-match-data
	    (goto-char (point-min))
	    
	    (setq tag_pos (search-forward string_of_tag_in_text 7zr-tag-in-text-end-bounds t))
	    (when tag_pos
	      (looking-at ".*")
	      (setq tag_value (7zr-trim (buffer-substring (match-beginning 0) (match-end 0))))
	      (when (7zr-string-starts-with-p tag_value "\")")  ;; this line is added so that the tag in the 7z-revisions.el code doesnt spring the trap
		(setq tag_value "")
		)
	      )
	    )
	  )
	tag_value
	) ; let
      
      )
    )
  )

(defun 7zr-update-metadata-file_tag ( tag_value string_of_tag_in_metadata_file )
  
  (if (numberp tag_value)
      (setq tag_value (number-to-string tag_value))
    )
  
  (7zr-extract-metafile-to-temp-directory)
  (find-file (concat 7zr-temp-directory 7zr-archive-created-by-message))
  (7z-revisions-mode 0)
  (goto-char (point-min))
  (if (search-forward string_of_tag_in_metadata_file nil t)
      (progn
	(goto-char (match-end 0))
	(delete-region (point) (line-end-position))
	(insert tag_value)
	(newline)
	
	)
					; else put in the tag
    (goto-char (point-max))
    (newline)
    (insert string_of_tag_in_metadata_file tag_value)
    (newline)
    (7zr-flush-lines-to-double-space-max-line-spacing-for-buffer)
    )
   (save-window-excursion
     (save-buffer)
     (kill-buffer 7zr-archive-created-by-message)
		  
;		  (shell-command (concat "7z a " (7zr-shell-quote-argument 7zr-archive-directory)(7zr-shell-quote-argument 7zr-archive-name) " " (7zr-shell-quote-argument 7zr-temp-directory)  (7zr-shell-quote-argument 7zr-archive-created-by-message)))	  ; why doesnt this work?   ;; apparently the buffer has to be killed first before running this function, or it doesnt work and shows no error
     )
   
   (7zr-update-metafile-to-archive)  ;; apparently the buffer has to be killed first before running this function, or it doesnt work and shows no error
   )

   

(defun 7zr-update-metadata-file_tag_from_document  ( string_of_tag_in_text string_of_tag_in_metadata_file )
  "returns the tag value found, as a string"
  (let (tag-pos (tag_value ""))
    (7zr-populate-initial-global-vars)
    (save-excursion
      (setq tag_value (7zr-find-tag-in-text string_of_tag_in_text ))
      (if (and
	   (not (string= tag_value ""))
	   (not (7zr-string-starts-with-p tag_value "."))
	   )
	  (7zr-update-metadata-file_tag tag_value string_of_tag_in_metadata_file )
	)
      )
    tag_value
    ) ; let
  )

  

(defun 7zr-populate-initial-global-vars ()

  (setq 7zr-buffer_file_coding_system buffer-file-coding-system)
  (setq 7zr-buffer_is_msdos_p (string-match "dos" (symbol-name buffer-file-coding-system)))
  

  (setq 7zr-buffer (file-name-nondirectory (buffer-file-name (current-buffer))))
  (setq 7zr-buffer-filename (buffer-file-name (current-buffer))) ; includes path
;  (setq 7zr-active-document_buffer (current-buffer))
  
  (setq 7zr-revisions_original_buffer (current-buffer))
;  (setq 7zr-active-document (buffer-name 7zr-active-document_buffer))
;  (setq 7zr-archive-directory (file-name-directory (buffer-file-name (current-buffer))))  ;debug 

  (setq 7zr-buffer-without-extension (7zr-sans-extension 7zr-buffer)) 

    ;debug
  (if (or (string= 7zr-archive-directory_default "") (string= 7zr-archive-directory_default "."))
      (setq 7zr-archive-directory (file-name-directory (buffer-file-name (current-buffer))))
    (7zr-set-archive-directory 7zr-archive-directory_default)
    )    

  (7zr-set-archive-directory-full 7zr-archive-directory)



  (setq 7zr-temp-string-variable (7zr-find-tag-in-text 7zr-update-7z-revisions-tag-in-text_directory-of-archive))
  (when (not (string= "" 7zr-temp-string-variable))
    (7zr-set-archive-directory 7zr-temp-string-variable)
      )
  (when (string= "" (setq 7zr-archive-prefix (7zr-find-tag-in-text 7zr-update-7z-revisions-tag-in-text_archive-prefix)))
    (setq 7zr-archive-prefix 7zr-archive-prefix_default)
    )
  (when (string= "" (setq 7zr-archive-extension (7zr-find-tag-in-text 7zr-update-7z-revisions-tag-in-text_archive-extension)))
    (setq 7zr-archive-extension 7zr-archive-extension_default)
    )
  (setq 7zr-archive-extension-suffix (replace-regexp-in-string "\\." "" 7zr-archive-extension))
  (setq 7zr-archive-name (concat 7zr-archive-prefix 7zr-buffer 7zr-archive-extension))
  (setq 7zr-document-directory (file-name-directory (buffer-file-name (current-buffer))))
  (setq 7zr-buffer-file-coding-system (symbol-name buffer-file-coding-system))
  )



  
(defun 7zr-update-7z-revisions-tags-in-text ()
  " e.g. 7z-revisions.el_rev=  
This function inserts the highest revision number + 1 to
the point at which the 7zr-update-7z-revision-tag-in-text tag is
located in the text.
this feature automatically executes before each save if the 7zr-auto-update-tags-in-text-p variable is set to t."

;; If your document anywhere contains the tag text
;; 7z-revisions.el_rev= followed by nothing or any number, then upon
;; execution of the function 7zr-update-7z-revisions-tag-in-text_rev,
;; the current revision number, incremented by 1, will be appended to
;; the end of the equals sign.  The incremented value is inserted into
;; the document because the idea is that the user would call this
;; function before calling save, where upon the actual increment value
;; is saved to the datafile, and thus the values will match in the
;; end.

  (interactive)
  
  (let (highest-rev_retval)
    (when  (eql 7zr-update-7z-revisions-tags-in-textp t)
      (save-excursion
	(goto-char (point-min))
	(setq tag-pos (search-forward 7zr-update-7z-revisions-tag-in-text_rev nil t))
	(when tag-pos
	  (when (looking-at "[0-9]+\.?[0-9]+")
	    (delete-region (match-beginning 0) (match-end 0))
	    )	
	  
	  (when (or
		 (looking-at " ")
		 (looking-at "$"))

	    (7zr-populate-initial-global-vars)
	    (setq highest-rev_retval (7zr-get-highest-rev-and-original-version))

	    (setq 7zr-patch-number (nth 1 highest-rev_retval))
	    (setq 7zr-original-version (car highest-rev_retval))
	    (cond
	     ((not (file-exists-p (concat 7zr-archive-directory 7zr-archive-name)))  ; just before archive is created
	      (insert "0.0"))	      
	     (t		       
	      (insert (number-to-string (1+ 7zr-patch-number)))
	      (7zr-update-tag_original-version)
	      (7zr-update-tag_sha1-of-last-revision))	     	     
	     )
	    )
	  )
	)
      )
    )
  )


(defun 7zr-commit ()
  "Call this function to save the latest changes to the archive.
This is called automatically with each save when 7z-revisions-mode
is invoked."
  (interactive)
  (7zr-populate-initial-global-vars)

  (if (file-exists-p (concat 7zr-archive-directory 7zr-archive-name))
      (7zr-append-archive)
    (7zr-create-archive)
    )
  )
  

(defun 7zr-after-save-func ()
  "Commit the current file."
  (7zr-commit)
)


(defun 7zr-save-buffer ()
  "Combines updating 7z-revision tags in document and saving the documentq"
  (interactive)
  (if (not (bound-and-true-p 7z-revisions-mode))
      (message "7z-revisions-mode must first be enabled before the 7zr-save-buffer command can be executed")
    (7zr-update-7z-revisions-tags-in-text)
    (save-buffer)
    )
  )

(defun 7zr-jump-to-this-timestamp-for-all-7z-buffers ()
  (interactive)
  (message "This feature is not yet implemented")
 )


(defun 7zr-turn-on-7z-revisions-mode ()
  "For convenience of readability, put this in add-hook to automatically turn on the 7z-revisions-mode minor mode when opening a file of a given type.  For example: (add-hook 'org-mode-hook '7zr-turn-on-7z-revisions-mode)"
  (interactive)
  (7z-revisions-mode 1)
  )

(defun 7z-revisions-mode_before-save-hook ()
  (when (and
	 (bound-and-true-p 7z-revisions-mode)
	 7zr-auto-update-tags-in-text-p
	 7zr-update-7z-revisions-tags-in-textp
	 )
    (7zr-update-7z-revisions-tags-in-text)
    )
  )

(add-hook 'before-save-hook #'7z-revisions-mode_before-save-hook)

;;;###autoload
(define-minor-mode 7z-revisions-mode
  "Automatically 7z any changes made when saving buffer."
  nil
  :lighter " 7zr"
  7z-revisions-mode-map
  (if 7z-revisions-mode
      (add-hook 'after-save-hook '7zr-after-save-func t t)
    (remove-hook 'after-save-hook '7zr-after-save-func t))
  )

(provide '7z-revisions-mode)

(add-to-list 'minor-mode-alist '(7z-revisions-mode " 7zr"))

;;; 7z-revisions-mode.el ends   -------------------------------------------

