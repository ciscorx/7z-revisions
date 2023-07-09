					; 7zrs.el
; This script will update or create a 7r-revision archive, useful in a terminal where screen or emacsclient is not available.
; This script is only to be called from the command line and called like so: emacs target_file.txt -Q --eval "(progn (load-file \"7zrs.el\"))" 
;   where the target file must precede the command line arguments.
; The following code was imported from 7z-revisions.el:
; add function 7zr-save-buffer, but disable 7z-revisions-mode must be enabled in (7zr-save-buffer) 
; add var 7zr-update-7z-revisions-tags-in-textp
; add var 7zr-update-7z-revisions-tags-in-text_rev
; add var 7zr-archive-directory-default
; add var 7zr-archive-directory-full
; add function 7zr-set-archive-directory-full
; add var 7zr-update-7z-revisions-tag-in-text_directory-of-archive
; add var 7zr-tag-in-text-end-bounds
; add var 7zr-update-7z-revisions-tag-in-text_archive-prefix "7z-revisions.el_archive-prefix=")
; add var (setq 7zr-archive-prefix_default "")  ; hide archive file by using a "." character as the prefix (only works on linux and mac os)
; add var (setq 7zr-update-7z-revisions-tag-in-text_archive-extension "7z-revisions.el_archive-extension=")
; add var (setq 7zr-archive-extension_default ".7z")  ; change this variable to change archive extension
; add var (setq 7zr-temp-directory-linux temporary-file-directory)
; add var (setq 7zr-temp-directory 7zr-temp-directory-linux)
; add function (defun 7zr-what-is-extension-of-filename ( filenamestring )
; add function (defun 7zr-current-date-time-win ()
; add var (setq 7zr-date-time-format-win "%Y-%m-%d_%H%M%S")
; add function  (defun 7zr-shell-quote-argument ( argument )
; add var (setq 7zr-prepend-to-hash-file "7zr-sha1-")
; add function (defun 7zr-append-archive ()
; add function (defun 7zr-examine-created-by-message-file ()
; add var (setq 7zr-archive-created-by-message "7z-revisions.el created this archive")  ;; this string should probably not end in a file extension which has a 7zr-turn-on-7zr-revisions-mode hook associated with it, or it will cause emacs to crash when creating a new archive  
; add var (setq 7zr-archive-directory "")  ;; for internal use only, intial value is overwritten
; add var (setq 7zr-archive-name "")
; add function (defun 7zr-rename-file-if-exists ( file newname &optional overwritep)
; add var (setq 7zr-prepend-to-latest-revision "7zr-latest-")
; add function (defun 7zr-create-file-for-archive-created-by-message (&optional NotCalledInteractively)
; add function (defun 7zr-string-starts-with-p (string prefix)
; add var (setq 7zr-update-7z-revisions-tag-in-metadata-file_rev "latest_revision=")
; add var (setq 7zr-update-7z-revisions-tag-in-metadata-file_original-version "original-version=")
; add var (setq 7zr-update-7z-revisions-tag-in-metadata-file_document_name "document_name=")
; add var (setq 7zr-update-7z-revisions-tag-in-metadata-file_archive-created_datetime "archive-created_datetime=")
; add var (setq 7zr-track-md5sum-hashes-p t)
; add var (setq 7zr-update-7z-revisions-tag-in-metadata-file_track-md5sum-hashes "track-sha1sum-hashes=")
; add var (setq 7zr-update-7z-revisions-tag-in-metadata-file_directory-of-archive "directory-of-archive=")
; add var (setq 7zr-directory-of-document "")  ;; not implemented yet
; add var (setq 7zr-update-7z-revisions-tag-in-metadata-os_type "created_on_os_system-type=")
; add var (setq 7zr-update-7z-revisions-tag-in-metadata-buffer_file_coding_system "buffer-file-coding-system=")
; add var (setq 7z-revisions-version 3.9)
; add function (defun 7zr-create-blank-notes-file-and-add-to-archive ()
; add var (setq 7zr-prepend-to-notes-file "7zr-notes-")
; add var (setq 7zr-notes-file-extension ".7zrn") 
; add var (setq 7zr-add-to-archive-command "7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on")
; add function (defun 7zr-delete-file-if-exists  ( filename )
; add function (defun 7zr-trim-l (str)
; add function (defun 7zr-trim-r (str)
; add function (defun 7zr-delete-file-from-temp-if-exists  ( filename )
; add function (defun 7zr-set-some-buffer-local-vars ( window last-buffer )
; add var (setq 7zr-revisions_tmpbuffer "")
; add var (setq 7zr-revisions_tmpbuffer_buffer nil)
; add var (setq 7zr-pointer-lastviewed_3rd_last "") 
; add var (setq 7zr-pointer-lastviewed_2nd_last "") 
; add var (setq 7zr-pointer-lastviewed_last "")
; add var (setq 7zr-pointer-lastviewed "")
; add var (setq 7zr-summary-lastrev-at-point "")
; add var (setq 7zr-summary-rev-at-point "")
; add var (setq 7zr-summary-lastlinenum-at-point 0)
; add var (setq 7zr-summary-linenum-at-point 0)
; add var (setq 7zr-os_type (symbol-name system-type))
; add var (setq 7zr-revisions_tmpbuffer_lines 0)
; add function (defun 7zr-created-by-message-file_increment_latest_revision_number ()
; add function (defun 7zr-create-diff-against-latest-version ()
; add var (setq 7zr-prepend-to-current-version "7zr-current-")   ; not used
; add var (setq 7zr-diff-command "diff -Na")
; add var (setq 7zr-patch-number 0.0)
; add var (setq 7zr-original-version "")
; Bugs: the target file must not be in the temp directory nor in a subdirectory under the temp directory or funny things happen
; --

(setq 7zr-update-7z-revisions-tags-in-textp t)  ; allow tags to be updated in text
(setq 7zr-update-7z-revisions-tag-in-text_rev "7z-revisions.el_rev=")
(setq 7zr-archive-directory-full "")
(setq 7zr-archive-directory_default "")  ;; e.g. set to "../" to make put archives in parent directory, but leave blank for same directory as document
(setq 7zr-tag-in-text-end-bounds 7777)  ; all tags in text must reside with in the first 7777 characters of the beginning of the document in order to be seen
(setq 7zr-update-7z-revisions-tag-in-text_archive-prefix "7z-revisions.el_archive-prefix=")
(setq 7zr-update-7z-revisions-tag-in-text_directory-of-archive "7z-revisions.el_directory-of-archive=")   
(setq 7zr-archive-prefix_default "")  ; hide archive file by using a "." character as the prefix (only works on linux and mac os)
(setq 7zr-update-7z-revisions-tag-in-text_archive-extension "7z-revisions.el_archive-extension=")
(setq 7zr-archive-extension_default ".7z")  ; change this variable to change archive extension
(setq 7zr-temp-directory-linux temporary-file-directory)
(setq 7zr-temp-directory 7zr-temp-directory-linux)
(setq 7zr-date-time-format-win "%Y-%m-%d_%H%M%S")
(setq 7zr-prepend-to-hash-file "7zr-sha1-")
(setq 7zr-archive-created-by-message "7z-revisions.el created this archive")  ;; this string should probably not end in a file extension which has a 7zr-turn-on-7zr-revisions-mode hook associated with it, or it will cause emacs to crash when creating a new archive  
(setq 7zr-archive-directory "")  ;; for internal use only, intial value is overwritten
(setq 7zr-archive-name "")
(setq 7zr-prepend-to-latest-revision "7zr-latest-")
(setq 7zr-update-7z-revisions-tag-in-metadata-file_rev "latest_revision=")
(setq 7zr-update-7z-revisions-tag-in-metadata-file_original-version "original-version=")
(setq 7zr-update-7z-revisions-tag-in-metadata-file_document_name "document_name=")
(setq 7zr-update-7z-revisions-tag-in-metadata-file_archive-created_datetime "archive-created_datetime=")
(setq 7zr-track-md5sum-hashes-p t)
(setq 7zr-update-7z-revisions-tag-in-metadata-file_track-md5sum-hashes "track-sha1sum-hashes=")
(setq 7zr-update-7z-revisions-tag-in-metadata-file_directory-of-archive "directory-of-archive=")
(setq 7zr-directory-of-document "")  ;; not implemented yet
(setq 7zr-update-7z-revisions-tag-in-metadata-os_type "created_on_os_system-type=")
(setq 7zr-update-7z-revisions-tag-in-metadata-os_type "created_on_os_system-type=")
(setq 7zr-update-7z-revisions-tag-in-metadata-buffer_file_coding_system "buffer-file-coding-system=")
(setq 7z-revisions-version 3.9)
(setq 7zr-prepend-to-notes-file "7zr-notes-")
(setq 7zr-notes-file-extension ".7zrn") 
(setq 7zr-add-to-archive-command "7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on")
(setq 7zr-revisions_tmpbuffer "")
(setq 7zr-revisions_tmpbuffer_buffer nil)
(setq 7zr-pointer-lastviewed_3rd_last "") 
(setq 7zr-pointer-lastviewed_2nd_last "") 
(setq 7zr-pointer-lastviewed_last "")
(setq 7zr-pointer-lastviewed "")
(setq 7zr-summary-lastrev-at-point "")
(setq 7zr-summary-rev-at-point "")
(setq 7zr-summary-lastlinenum-at-point 0)
(setq 7zr-summary-linenum-at-point 0)
(setq 7zr-os_type (symbol-name system-type))
(setq 7zr-revisions_tmpbuffer_lines 0)
(setq 7zr-prepend-to-current-version "7zr-current-")   ; not used
(setq 7zr-diff-command "diff -Na")
(setq 7zr-patch-number 0.0)
(setq 7zr-original-version "")
; ---

(defun 7zr-set-archive-directory-full( target_directory )
  (let ((saved_dir default-directory))
    (cd target_directory)
    (setq 7zr-archive-directory-full default-directory)
    (cd saved_dir)
    )
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

(defun 7zr-current-date-time-win ()
  "insert the current date and time into current buffer, the format of which is compatible with windows xp filenames.
Uses `current-date-time-format' for the formatting the date/time."
       (interactive)
       (format-time-string 7zr-date-time-format-win (current-time))
       )


(defun 7zr-shell-quote-argument ( argument )
  "For some reason shell-quote-argument returns single quotes delimited by double quotes when passed a blank string.  This function is a wrapper which attempts to rectify this."
  (if (string= argument "")
      ""
    (shell-quote-argument argument)
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
    (if (not (eql (string-match-p "No files to process" (shell-command-to-string (concat "7z e -aoa -o" (7zr-shell-quote-argument 7zr-temp-directory) " "  (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name))  " " (7zr-shell-quote-argument 7zr-archive-created-by-message)))) nil))
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


(defun 7zr-rename-file-if-exists ( file newname &optional overwritep)
  " Rename a file if it exists, and returns nil, taking no
action, if it doesnt.  In addition, this over-writes destination
file if that file already exists.  The overwritep option is ignored for now."
  (if (file-exists-p file )
      (rename-file file newname t)
    nil
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
	  (insert "\n")
	  (insert (concat 7zr-update-7z-revisions-tag-in-metadata-file_original-version 7zr-original-version))
	  (insert "\n")
	  (insert (concat 7zr-update-7z-revisions-tag-in-metadata-file_document_name 7zr-buffer))
	  (insert "\n")
	  (insert (concat 7zr-update-7z-revisions-tag-in-metadata-file_archive-created_datetime 7zr-archive-created_datetime))
	  (insert "\n")
;	  (insert (concat 7zr-update-7z-revisions-tag-in-metadata-file_archive-prefix 7zr-archive-prefix))
;	  (insert "\n")
;	  (insert (concat 7zr-update-7z-revisions-tag-in-metadata-file_archive-extension 7zr-archive-extension))
;	  (insert "\n")
	  (if (equal 7zr-track-md5sum-hashes-p t)
	      (setq 7zr-track-md5sum-hashes_string "t")
	    (setq 7zr-track-md5sum-hashes_string "nil")
	    )
	  (insert (concat 7zr-update-7z-revisions-tag-in-metadata-file_track-md5sum-hashes 7zr-track-md5sum-hashes_string))
	  (insert "\n")
	  (unless (string= 7zr-archive-directory "")
	    (insert (concat 7zr-update-7z-revisions-tag-in-metadata-file_directory-of-archive 7zr-archive-directory))
	    (insert "\n")
	    )
	  (unless (string= 7zr-directory-of-document "")
	    (insert (concat 7zr-update-7z-revisions-tag-in-metadata-file_directory-of-document 7zr-directory-of-document))
	    (insert "\n")
	    )
	  (insert (concat 7zr-update-7z-revisions-tag-in-metadata-os_type (symbol-name system-type)))
	  (insert "\n")
	  (insert (concat 7zr-update-7z-revisions-tag-in-metadata-buffer_file_coding_system  7zr-buffer_file_coding_system))
	  (insert "\n")
	  (insert (concat "7z-revisions-version=" (number-to-string 7z-revisions-version)))
	  (insert "\n")
	  
	  (write-file (concat 7zr-temp-directory 7zr-archive-created-by-message))
	  (kill-buffer 7zr-create-blank-file-tmpbuffer)
	  (set-buffer 7zr-save-current-buffer)

	  (shell-command (concat "7z a " (7zr-shell-quote-argument  archive_name) " " (7zr-shell-quote-argument (concat 7zr-temp-directory 7zr-archive-created-by-message))))
	  )
      
      (message (concat "There first needs to exist an archive named " archive_name " before the 7zr-archive-created-by-message file, entitled '" 7zr-archive-created-by-message  "', can be added to it!"))
      )
    )
  )



(defun 7zr-string-starts-with-p (string prefix)
    "Return t if STRING starts with PREFIX."
    (and
     (string-match-p (rx-to-string `(: bos ,prefix) t)
                   string)
     t))


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
    (insert "\n")
    (write-file notes_file )
    (kill-buffer)
    (set-buffer 7zr-saved-current-buffer)
    (shell-command (concat 7zr-add-to-archive-command " " (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name)) " " notes_file))
    (7zr-delete-file-if-exists notes_file)
    )
  )

  
  

(defun 7zr-delete-file-if-exists  ( filename )
  " Delete a filename if it exists, and return nil if it doesnt."
  (let (filename_trimmed )
    (setq filename_trimmed (7zr-trim-l (7zr-trim-r filename)))
    (if (and
	 (not (string= filename_trimmed ""))
	 (file-exists-p filename_trimmed )
	 (not (string= filename_trimmed 7zr-temp-directory)))
	(progn
	  (delete-file filename_trimmed)
	  t
	  )
      nil
      )
    )
  )
       

(defun 7zr-trim-l (str)
  (replace-regexp-in-string "^[ \t]+"  "" str) )

(defun 7zr-trim-r (str)
  (replace-regexp-in-string "[ \t]+$"  "" str) )

(defun 7zr-delete-file-from-temp-if-exists  ( filename )
  " Delete a file from the directory specified in the global variable 7zr-temp-directory, if the file exists, and return nil if it doesnt."
  (let (filename_trimmed (saved-directory default-directory))
    (setq filename_trimmed (7zr-trim-l (7zr-trim-r filename)))
    (cd 7zr-temp-directory)
    (if (and
	 (not (string= filename_trimmed ""))
	 (file-exists-p filename_trimmed )
	 (not (string= filename_trimmed 7zr-temp-directory)))
	(progn 
	  (delete-file filename_trimmed)
	  t
	  )
      nil
      )
    (cd saved-directory)
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
 ; (set (make-local-variable '7zr-active-document_7zr-pointer-lastviewed_nearest_progeny)  7zr-pointer-lastviewed_nearest_progeny)
  (set (make-local-variable '7zr-active-document_7zr-summary-lastrev-at-point) 7zr-summary-lastrev-at-point)
  
  (set (make-local-variable '7zr-active-document_7zr-summary-rev-at-point) 7zr-summary-rev-at-point)

  (set (make-local-variable '7zr-active-document_7zr-summary-lastlinenum-at-point) 7zr-summary-lastlinenum-at-point)
  
  (set (make-local-variable '7zr-active-document_7zr-summary-linenum-at-point) 7zr-summary-linenum-at-point)
  
  (set (make-local-variable '7zr-active-document_7zr-os_type) 7zr-os_type)
  (set (make-local-variable '7zr-viewing) t)
 
  (set (make-local-variable '7zr-active-document_last-buffer) last-buffer)
  (set (make-local-variable '7zr-active-document_7zr-revisions_tmpbuffer_lines) 7zr-revisions_tmpbuffer_lines)
  (set (make-local-variable '7zr-window) window)

  (cond
   ((or
     (string= window "7zr-summary-view-revision")
     (string= window "7zr-summary-view-raw-diff-file")
     (string= window "7zr-summary-all-diffs-of-region-to-1-buffer")
     (string= window "7zr-reconstruct-rev-from-patches")
     )
    (set (make-local-variable '7zr-active-document_summary_buffer) last-buffer)
    (set (make-local-variable '7zr-active-document_7zr-revisions_original_buffer) (buffer-local-value '7zr-active-document_7zr-revisions_original_buffer last-buffer))
     (set (make-local-variable '7zr-active-document_7zr-revisions_original_buffer_name) (buffer-local-value '7zr-active-document_7zr-revisions_original_buffer_name last-buffer))
;;    (set (make-local-variable '7zr-active-document_7zr-pointer-lastviewed) 7zr-pointer-lastviewed)
   ; (setq  7zr-revisions_original_buffer (buffer-local-value '7zr-active-document_7zr-revisions_original_buffer last-buffer))
   ; (setq 7zr-revisions_original_buffer_name (buffer-local-value '7zr-active-document_7zr-revisions_original_buffer_name last-buffer))
    (set (make-local-variable '7zr-active-document_7zr-patch-number) (buffer-local-value '7zr-active-document_7zr-patch-number last-buffer))
    
    (set (make-local-variable '7zr-active-document_7zr-revisions_tmpbuffer_lines) (buffer-local-value '7zr-active-document_7zr-revisions_tmpbuffer_lines last-buffer))
    )
      
      

   ((or
     (string= window "7z-revisions"))
    (set (make-local-variable '7zr-active-document_7zr-revisions_original_buffer) 7zr-revisions_original_buffer)
    (set (make-local-variable '7zr-active-document_7zr-revisions_original_buffer_name) 7zr-revisions_original_buffer_name)
    (set (make-local-variable '7zr-active-document_7zr-patch-number) 7zr-patch-number)
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
      (if (re-search-forward (concat "^"  7zr-update-7z-revisions-tag-in-metadata-file_rev "\\([0-9]+\\.[0-9]+\\)") nil t)
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
	(insert "\n")
	(previous-line)
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
	(insert "\n")
	(insert (concat 7zr-update-7z-revisions-tag-in-metadata-file_original-version 7zr-original-version))
	(insert "\n")
	) ; if


          

      (write-file 7zr-archive-created-by-message)
      (kill-buffer)

      
      (shell-command-to-string (concat "7z a "  (7zr-shell-quote-argument (concat 7zr-archive-directory 7zr-archive-name))  " " (7zr-shell-quote-argument (concat 7zr-temp-directory 7zr-archive-created-by-message))))
      (7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-archive-created-by-message))
      7zr-patch-number
      ) ; if
    ) ; let
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
;;	      (setq tmphashvalue (7zr-s-trim-whitespace-r (shell-command-to-string (concat 7zr-sha1sum-command " " (7zr-shell-quote-argument (concat 7zr-temp-directory 7zr-prepend-to-current-version  7zr-buffer)) 7zr-sha1sum-post-command (7zr-awk-cmd-string 1)))))
	      (setq tmphashvalue (sha1 (current-buffer)))

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
    ) ; save windows excursion
  )

;; -----
(setq debug-on-error t)
(setq hithere "hi")
(debug-on-variable-change 'hithere)
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
	  
;;	  (setq tmphashvalue (7zr-string-trim-final-newline (7zr-string-trim-final-newline (shell-command-to-string (concat 7zr-sha1sum-command " " (7zr-shell-quote-argument 7zr-original-version) 7zr-sha1sum-post-command (7zr-awk-cmd-string 1))))))
	  (setq tmphashvalue (sha1 (current-buffer)))

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
	   (setq asdf1 (string-match-p ":" revstring))
	   (setq asdf2 (string-match-p ":" revstring (1+ asdf1)))
	   (equal (- asdf2 asdf1) 3)
	   (eq asdf1 2)
	   )

	  (setq return_val (substring original-version_sans-extension (- (- strlength asdf2) 14) strlength))
	  
					; else normal timestamp
      (setq return_val (substring original-version_sans-extension (- strlength 17) strlength))
      )
    )
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
	(insert "\n")
	
	)
					; else put in the tag
    (goto-char (point-max))
    (insert "\n")
    (insert string_of_tag_in_metadata_file tag_value)
    (insert "\n")
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
  (let (tag_in_text)
    (setq 7zr-buffer-file-coding-system buffer-file-coding-system)
    (setq 7zr-buffer_is_msdos_p (string-match-p "dos" (symbol-name buffer-file-coding-system)))
    
    (setq 7zr-buffer-filename_including_dir (buffer-file-name (current-buffer)))
    (when (not 7zr-buffer-filename_including_dir)
      (error "Buffer must be associated with a file.")
      )
    (setq 7zr-buffer (file-name-nondirectory 7zr-buffer-filename_including_dir))
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



    (setq tag_in_text (7zr-find-tag-in-text 7zr-update-7z-revisions-tag-in-text_directory-of-archive))
    (when (not (string= "" tag_in_text))
      (7zr-set-archive-directory tag_in_text)
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
    (setq 7zr-buffer_file_coding_system (symbol-name buffer-file-coding-system))
    )
  )


(defun 7zr-update-7z-revisions-tags-in-text ()
  " e.g. 7z-revisions.el_rev=8.0  
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
	  (when (looking-at "[0-9]+\\.[0-9]+")
	    (delete-region (match-beginning 0) (match-end 0));
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
    (7zr-update-7z-revisions-tags-in-text)
    (save-buffer)
  )


	  

(defun 7zr-sans-extension ( filenamestring )
;;  (replace-regexp-in-string "\\..+" "" filenamestring)
  (file-name-sans-extension filenamestring)
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
 		(not (string-match-p "[a-zA-Z]+" listing))   ; else it must be a number to be a patch, and so discard the rest	      
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

; find the first file associated buffer in memory and update its 7z-revisions archive, or create one if none exists for that file, and then exit emacs
(catch 'found
  (dolist (buffer (buffer-list))
    (when (buffer-file-name buffer)
      (switch-to-buffer buffer)
      (throw 'found t))))
(set-buffer-modified-p t)(7zr-commit)(kill-emacs)
