;; 7z-revisions.el --- To save and review backup revisions using a .7z
;; archive, providing word by word differential highlighting.  Also,
;; provides syntax coloring when viewing raw diff files.
;;
;; authors/maintainers: ciscorx@gmail.com                                                                    
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3 of the License,
;; or (at your option) any later version.
;;
;; Commentary:
;;
;; 7z-revisions-mode is an Emacs minor mode that saves the current
;; buffer to a 7-zip archive of the same name, whenever a save-buffer
;; command is issued.  A timestamp in the form of MMDDYY-HHMMSS is
;; appended to the archived file.  If the .7z archive file already
;; exists then it incrementally saves the latest revision by adding a
;; new patch to the archive.  The .7z extension can be altered to
;; something else, such as .8z, by setting the global variable
;; 7zr-archive-extension to ".8z".  Additionally, the function
;; 7z-revisions can be called interactively to view or consolidate
;; past revisions in the archive.
;; 
;; Some useful commands when editing your document:
;;     M-x 7zr-line-last-changed-on
;;     M-x 7zr-goto-line-of-last-revision 
;;
;; When 7z-revisions is called, the following key bindings take
;;   effect: Enter = view the selected revision, j = view raw diff
;;   file of selected revision, a = view all selected diff files in
;;   one buffer, q = quit, c = consolidate region, g = goto date, h =
;;   toggle highlight differences
;;
;; While viewing a revision: q = quit, n = next, p = previous.  Also,
;;   when highlight changes is enabled, d = jump to next
;;   difference/change, e = jump to previous change, j = view the raw
;;   diff file
;;
;; While viewing a raw diff file: q = quit, n = next, p = previous,
;;   r = switch to revision view,  d = jump to next
;;   change hunk, e = jump to previous change hunk
;;
;;
;; There are also some functions in the menu which provide for
;; consoldating the current days worth of changes, or last hour worth
;; of changes, etc.
;;
;; Also, sha1sum hash values for each revision are saved in the
;; hashtable stored in the archive and can be search from the menu.
;;
;; While in dired-mode, the key binding z = dired-7z-revisions, which
;;   views the 7z-revisions archive at point
;;
;; Required features:
;;   hl-line+.el
;;   p7zip
;;   diffutils  ( just the patch and diff commands )
;;
;; When running on windows, access to additional dos commands is necessary, such as patch, diff, awk, fciv, and optionally grep.
;;   Install diffutils for windows:
;;     http://gnuwin32.sourceforge.net/packages/diffutils.htm and then
;;     append C:\Program Files\GnuWin32\bin, or whatever directory
;;     that happens to contain diff.exe, to the path variable in
;;     control panel -> system -> Advanced -> Environment Variables.
;;     Alternatively, you could just throw all the files in
;;     c:\windows\system32
;;   Install patch.exe for windows:
;;     http://gnuwin32.sourceforge.net/packages/patch.htm then put it
;;     in the same directory that contains diff.exe
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
;; - File names must contain at least 1 alphabetical character or
;;     underscore or hyphen, and in this regard, cannot take the form
;;     of a real number, e.g. "1.0".
;; - Each archive can only track one file.  (let's call this a
;;     feature)
;; - There's no way to add revision notes.
;; - Buffer local variables arent working properly enough to allow for
;;     two archives to be opened at once.
;; - Words added to beginning of line additionally highlight following
;;     word green. In some cases highlighting is off by 1 word.
;; 
;;  This program was written using emacs 23.3.1 on ubuntu 12.04, but
;;     is compatible with windows-xp and probably windows 7

;;; 7zr-summary-mode.el begins ------------------------

(setq debug-on-error t)

;; GLOBAL VARIABLES ----------------------
(setq 7zr-archive-extension ".7z")
(setq 7zr-archive-extension-suffix (replace-regexp-in-string "\\." "" 7zr-archive-extension))
(setq 7zr-temp-directory-windows-nt "/tmp/")   ; temp directory to use on windows machines
(setq 7zr-temp-directory-linux "/tmp/")
(setq 7zr-temp-directory-apple "/tmp/")
(setq 7zr-view_highlightp t)
(setq 7zr-view_date "")
; (setq 7zr-buffer (minibuffer-selected-window))
; (setq 7zr-buffer-FQPN (buffer-file-name 7zr-buffer))
; (setq 7zr-buffer-filename (file-name-nondirectory 7zr-buffer-FQPN))
(setq 7zr-original-version "")
(setq 7zr-pointer-lastviewed "")
(setq 7zr-pointer-lastviewed_last "")
(setq 7zr-pointer-lastviewed_2nd_last "") 
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
(setq 7zr-prepend-to-hash-file "7zr-sha1-")
(setq 7zr-prepend-to-rej "7zr-rej-")
(setq 7zr-construct-slow_last "0")
(setq 7zr-hash-of-hash-file "")
(setq 7zr-summary-reconst-last "")
(setq 7zr-archive-created-by-message "archive created by 7z-revisions.el")
; (defvar 7zr-hasht (make-hash-table :size 20000 :test 'equal))
(setq 7zr-summary-last-line 0)  ; line number of revision in the 7z-revisions display, not to be mistaken for the last line number of point in the document ( which would be related to 7zr-view-last-pos or 7zr-view-last-line)
(setq 7zr-view-last-column 0)
(setq 7zr-view-last-line 0)  ; line number of point when 7z-revisions is invoked
(setq 7zr-patch-number 0.0)
(setq 7zr-revisions_tmpbuffer "7zr_revisions_tmpbuffer")
(setq 7zr-revisions-examine-tmpbuffer "7zr_revisions_examine_tmpbuffer")
(setq 7zr-revisions_tmpbuffer_lines 0)
(setq 7zr-revisions_original_buffer (current-buffer))
(setq 7zr-summary_discard_rejp nil)
(setq 7zr-map-difference_line_pos_last 1) 
(setq 7zr-map-differences_column_pos_last 0) 
(setq 7zr-revisions_tmpbuffer "")  ;; name of buffer that lists all revisions
(setq 7zr-revisions_lastbuffer "")  ;; name of the buffer from which 7z-revisions was called
(setq 7zr-disable-summary-goto-sha1 nil)  ;; used for windows compatibility
(setq 7zr-diff-command "diff -Na")
(setq 7zr-patch-command "patch -p0")
(setq 7zr-sha1sum-command-windows "fciv.exe -sha1")
(setq 7zr-sha1sum-post-command-windows " | more +3 ")
(setq 7zr-sha1sum-command-linux "sha1sum")
(setq 7zr-sha1sum-post-command-linux " ")
(setq 7zr-mswindows-requirements-failed nil)
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (setq 7zr-temp-directory 7zr-temp-directory-windows-nt)
  (cond ((not (executable-find "patch.exe"))
	 (setq 7zr-mswindows-requirements-failed "Need patch.exe from http://gnuwin32.sourceforge.net/packages/patch.htm"))
	((not (executable-find "diff.exe"))
	 (setq 7zr-mswindows-requirements-failed "Need diff.exe from http://gnuwin32.sourceforge.net/packages/diffutils.htm"))
	((not (executable-find "awk.exe"))
	 (setq 7zr-mswindows-requirements-failed "Need awk.exe from http://gnuwin32.sourceforge.net/packages/gawk.htm"))
	((not (executable-find "fciv.exe"))
	 (setq 7zr-mswindows-requirements-failed "Need fciv.exe for sha1sum https://www.microsoft.com/en-us/download/confirmation.aspx?id=11533"))
	((not (executable-find "7z.exe"))
	 (setq 7zr-mswindows-requirements-failed "Need 7z.exe from https://www.7-zip.org/download.html")))
  (when (not (executable-find "grep.exe"))  ; dont fail if we cant find grep since its only used in one trivial function
    (setq 7zr-disable-summary-goto-sha1 t))
  (when 7zr-mswindows-requirements-failed
    (error 7zr-mswindows-requirements-failed))
  (setq 7zr-sha1sum-command 7zr-sha1sum-command-windows)
  (setq 7zr-sha1sum-post-command 7zr-sha1sum-post-command-windows)
  (defun 7zr-awk-cmd-string ( fieldnum )
    (concat " | awk \"{print $" (number-to-string fieldnum) "}\"")
    )
  )
 ((string-equal system-type "darwin") ; Mac OS X
  (setq 7zr-temp-directory 7zr-temp-directory-apple)
  (setq 7zr-sha1sum-command 7zr-sha1sum-command-linux)
  (setq 7zr-sha1sum-post-command 7zr-sha1sum-post-command-linux)
  (defun 7zr-awk-cmd-string ( fieldnum )
    (concat " | awk '{print $" (number-to-string fieldnum) "}'")
    )
  )
 
 ((string-equal system-type "gnu/linux") ; linux
  (setq 7zr-sha1sum-command 7zr-sha1sum-command-linux)
  (setq 7zr-sha1sum-post-command 7zr-sha1sum-post-command-linux)
  (setq 7zr-temp-directory 7zr-temp-directory-linux)
  (defun 7zr-awk-cmd-string ( fieldnum )
    (concat " | awk '{print $" (number-to-string fieldnum) "}'")
    )
  )
 (t  ; default os
  (setq 7zr-sha1sum-command 7zr-sha1sum-command-linux)
  (setq 7zr-sha1sum-post-command 7zr-sha1sum-post-command-linux)
  (setq 7zr-temp-directory 7zr-temp-directory-linux)
  (defun 7zr-awk-cmd-string ( fieldnum )
    (concat " | awk '{print $" (number-to-string fieldnum) "}'")
    )
  )
 )


; (add-hook 'after-change-major-mode-hook '7zr-toggle-highlight)
; (mouse-leave-buffer-hook)

;; BUFFER LOCAL VARIABLES--------------------------------------
(setq 7zr-archive-file-name-directory "")
(make-variable-buffer-local '7zr-diff-queue_position)
(setq 7zr-diff-queue_position 0)
(make-variable-buffer-local '7zr-diff-queue_length)
(make-variable-buffer-local '7zr-diff-queue)
(make-variable-buffer-local '7zr-active-document)           ; 7zr-buffer
(make-variable-buffer-local '7zr-active-document_original)  ; 7zr-original-version
(make-variable-buffer-local '7zr-active-document_buffer)
(make-variable-buffer-local '7zr-active-document_pointer_lastviewed)
(make-variable-buffer-local '7zr-active-document_pointer_lastviewed_last)
(make-variable-buffer-local '7zr-active-document_pointer_lastviewed_2nd_last)
(make-variable-buffer-local '7zr-active-document_pointer_lastviewed_nearest_ancestor)
(make-variable-buffer-local '7zr-active-document_pointer_lastviewed_nearest_progeny)
(make-variable-buffer-local '7zr-active-document_patch_number)

;; HOOKS----------------------------------------
;; add hook to dired mode to view 7z-revisions archives using the z command
(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map (kbd "z")  (lambda () (interactive) (dired-7z-revisions)))
	    )
	  )

(defcustom 7zr-summary-mode-hook '(7zr-summary-mode-hook-identify)
  "Normal hook run when entering 7zr-summary mode and many related modes."
  :type 'hook
  :options '(turn-on-hl-line-mode)
  :group '7zr)

(defvar 7zr-summary-mode-variant nil
  "Non-nil if this buffer's major mode is a variant of 7zr-summary mode.
Use (derived-mode-p \\='7zr-summary-mode) instead.")


(defvar 7zr-summary-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\e\t" 'kill-buffer)
    (define-key map (kbd "<RET>") '7zr-summary-view-revision)
    (define-key map (kbd "q") '7zr-summary-quit)
    (define-key map (kbd "a") '7zr-summary-all-diffs-of-region-to-1-buffer)
    (define-key map (kbd "g") '7zr-summary-goto-date)
    (define-key map (kbd "#") '7zr-summary-goto-sha1)
    (define-key map (kbd "c") '7zr-summary-consolidate-region)
    (define-key map (kbd "h") '7zr-view-toggle-highlight-changes)
    (define-key map (kbd "j") '7zr-summary-view-raw-diff-file)
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
  "Mark that this mode has run `7zr-summary-mode-hook'.
This is how `toggle-7zr-summary-mode-auto-fill' knows which buffers to operate on."
  (set (make-local-variable '7zr-summary-mode-variant) t))



(defun turn-on-hl-line-mode ()
  (interactive)
  (hl-line-mode 1)
)


(defun 7zr-trim-l (str)
  (replace-regexp-in-string "^ +"  "" str) )

(defun 7zr-trim-r (str)
  (replace-regexp-in-string " +$"  "" str) )

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
Uses `current-date-time-format' for the formatting the date/time."
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

(setq 7zr-match-timestamp2 "\\(20[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)")



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
		(setq timetuple (apply #'encode-time (parse-time-string (match-string 0))))
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


(defun 7zr-summary-goto-sha1 ()
  (interactive)
  (if 7zr-disable-summary-goto-sha1
      (message "Missing grep.exe, which can be downloaded from http://gnuwin32.sourceforge.net/packages/grep.htm")
    (let (inputted_sha1 actual_sha1 inputted_timetuple sha1point begin end point_saved revision)
      (setq inputted_sha1 (read-string "Enter sha1sum hashvalue:"))

      (save-window-excursion
	(shell-command (concat "7z e -aoa -o" 7zr-temp-directory " " 7zr-archive-name " " 7zr-prepend-to-hash-file 7zr-current-original-version))
	(if (string= "" (setq revision (7zr-s-trim-whitespace-r (shell-command-to-string (concat "grep " 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-current-original-version " -e " inputted_sha1  (7zr-awk-cmd-string 2))))))
	    (message "Hashvalue not found in the hashtable")
	  (setq actual_sha1 (7zr-s-trim-whitespace-r (shell-command-to-string (concat "grep " 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-current-original-version " -e " inputted_sha1  (7zr-awk-cmd-string 3)))))
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
	  (when (not (search-forward revision nil t))  ; re-search-forward is troublesome because of the . in the revision number
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
    (when (not (string-match-p ":" inputted_date))
      (setq inputted_date (concat inputted_date " 0:00"))
      )
	    (condition-case ex
		(setq inputted_timetuple (apply #'encode-time (parse-time-string inputted_date)))
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
the patch files to be deleted in the region."
; the files to be diffed are from and to, but from patch will
; not be deleted

  (interactive "r")
  (if (> from to)
      (let ((tem to))
	(setq to from from tem)))
  (let
      ( (abort_function nil) from-patch to-patch froms-nearest-ancestor end-at-latest-version start-at-original-version 7zr-consolidate-line_from point-from-minus-first-line max-patch max-line datetime-reference-timestamp )
    (setq 7zr-summary-consolidate-string "")
    (setq 7zr-patch-command-called-x-times 0)
    (setq 7zr-summary-consolidate-to-minus-last-patch 0)
    (setq 7zr-summary-consolidate-from-minus-1 "")
    (save-excursion
      (save-restriction
	(goto-char from)  ; from will actually be 1 up from selected from
	(beginning-of-line)
	(setq 7zr-consolidate-line_from (string-to-number (format-mode-line "%l")))
	
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
	
	(when (not (looking-at "\\([0-9]+\.[0-9]+\\)"))
	  (setq end-at-latest-version t)
	  (forward-line -1)
	  (looking-at "\\([0-9]+\.[0-9]+\\)")
	  )
	
	(setq to-patch (match-string-no-properties 1))
	(forward-line -1)
	(setq 7zr-consolidate-line_to (string-to-number (format-mode-line "%l")))
	(setq point-to-minus-last-line (line-end-position))
	
	(goto-char (point-max))
	(beginning-of-line)
	(forward-line -1)
	(setq max-line (string-to-number (format-mode-line "%l")))
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
		  (shell-command (concat "7z e -aoa -o" 7zr-temp-directory " " 7zr-archive-name " "  7zr-original-version))
		  (7zr-rename-file-if-exists (concat 7zr-temp-directory  7zr-original-version) (concat 7zr-temp-directory "rev0.0_of_" 7zr-original-version))

		  )
		)
	    ; else it starts at from-patch
	    (7zr-reconstruct-slow from-patch point-from-minus-first-line)
	    )
	  (if end-at-latest-version  ; ends at latest version ?
	      (progn
		(save-window-excursion
		  (shell-command (concat "7z e -aoa -o" 7zr-temp-directory " " 7zr-archive-name " " 7zr-prepend-to-latest-revision 7zr-original-version))
		  (setq datetime-reference-timestamp (car (nthcdr 5 (file-attributes (concat 7zr-prepend-to-latest-revision 7zr-original-version ))))) 
;		  (7zr-copy-attrib-modified-datetime (concat 7zr-temp-directory "7zr-datetime-reference")
;						     (concat 7zr-temp-directory 7zr-prepend-to-latest-revision 7zr-original-version)) 
;		  (shell-command  (concat "touch -r " 7zr-temp-directory 7zr-prepend-to-latest-revision 7zr-original-version " " 7zr-temp-directory "7zr-datetime-reference"))
		  (7zr-rename-file-if-exists (concat 7zr-temp-directory 7zr-prepend-to-latest-revision 7zr-original-version) (concat 7zr-temp-directory "rev" to-patch "_of_" 7zr-original-version ))
		  
		  )
		)
	  ; else it ends at to-patch
	    (7zr-reconstruct-slow to-patch to)
	    (setq datetime-reference-timestamp (car (nthcdr 5 (file-attributes (concat 7zr-temp-directory to-patch ))))) 
	    ) ; if

	  (save-window-excursion
	    (shell-command (concat 7zr-diff-command " "  7zr-temp-directory "rev" from-patch "_of_" 7zr-original-version " "  7zr-temp-directory "rev" to-patch "_of_" 7zr-original-version  " > " 7zr-temp-directory "tmpdiff"))
	    (7zr-rename-file-if-exists (concat 7zr-temp-directory "tmpdiff") (concat 7zr-temp-directory to-patch))
		      

	    (shell-command (concat "7z d " 7zr-archive-name " " (7zr-trim-r (7zr-trim-l 7zr-summary-consolidate-string))))
	    (set-file-times (concat 7zr-temp-directory to-patch) datetime-reference-timestamp)
;	    (7zr-copy-attrib-modified-datetime (concat 7zr-temp-directory 7zr-datetime-reference) (concat 7zr-temp-directory to-patch))
;(shell-command (concat "touch -r " 7zr-temp-directory "7zr-datetime-reference " 7zr-temp-directory to-patch))
	    (shell-command (concat "7z u -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on " 7zr-archive-name " " 7zr-temp-directory to-patch))
	    (7zr-delete-file-if-exists (concat  7zr-temp-directory "7zr-datetime-reference"))

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
	(setq line_from (string-to-number (format-mode-line "%l")))
	
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
	
	(when (not (looking-at "\\([0-9]+\.[0-9]+\\)"))
	  (if (or (= line_from (string-to-number (format-mode-line "%l")))
		  (= 1         (string-to-number (format-mode-line "%l"))))
	      (setq abort_function t)  ;; from is at last line
	    (forward-line -1))    ;; else assume that just end is at last line
	  )
	(looking-at "\\([0-9]+\.[0-9]+\\)")		
	(setq to-patch (match-string-no-properties 1))
;;	(forward-line -1)
	(setq 7zr-consolidate-line_to (string-to-number (format-mode-line "%l")))
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
	  (setq 7zr-all-diffs-buffer (concat "7zr-diffs_from_" from-patch "_to_" to-patch "_of_" 7zr-buffer-filename))
	  (set-buffer (get-buffer-create 7zr-all-diffs-buffer))
	  (erase-buffer)
	  (let (diffs_stack_tuple patch_ptr patch_ptr_pos patch_ptr_datetime_str diff_file_begin_pos diff_file_end_pos diff_file_size hunk_header_begin_pos hunk_header_end_pos hunk_header hunk_datetime_str)


	    (setq diffs_stack_tuple (pop diffs_stack))
	    (while diffs_stack_tuple                     ;; for each selected rev
	      (setq patch_ptr (car diffs_stack_tuple))   ;;  rev number selected in summary buffer
	      (setq patch_ptr_datetime_str (car (cdr diffs_stack_tuple)))
	      (setq hunk_header (concat "rev " patch_ptr " on " patch_ptr_datetime_str " : " ))
	      (setq patch_ptr_pos (car (cdr (cdr diffs_stack_tuple))))  ;; pointer to pos of rev number in summary buffer
	      (save-window-excursion
		(shell-command (concat "7z e -aoa -o" 7zr-temp-directory " " 7zr-archive-name " " patch_ptr))  
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
		(goto-char (point-max))
		)  ;; save-restriction
	      (setq diffs_stack_tuple (pop diffs_stack))
	      ) ;; while diffs_stack_tuple
;;	   
	  
	    ) ;; let
	  (switch-to-buffer 7zr-all-diffs-buffer)
	  (beginning-of-buffer)
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
    
      


(defun 7zr-summary-view-revision ()
  (interactive)
  " Reconstruct and then view the revision pointed to by (point)
   in the 7zr-revisions_tmpbuffer summary buffer and call
   7zr-reconstruct-rev-from-patches, unless we are looking at top
   most or bottom most line, or one up from bottom most line, in
   which case we'll open the files from this function instead of
   calling 7z-reconstruct-rev-from-patches."
  (beginning-of-line)
  (cond ((looking-at 7zr-original-version)
	 (progn
	   (save-window-excursion
	     (setq 7zr-summary-last-line (string-to-number (format-mode-line "%l")))
	     (shell-command (concat "7z e -aoa -o" 7zr-temp-directory " " 7zr-archive-name " " 7zr-original-version))  
;;	     (setq 7zr-revisions_lastbuffer (current-buffer))
	     )
	   (setq 7zr-pointer_lastviewed "0.0")
	   (save-excursion
	     (forward-line)
	     (looking-at "\\([0-9]+\.?[0-9]*\\)")	      
	     (setq 7zr-pointer-lastviewed_nearest_progeny (match-string-no-properties 1))
	     )
	   (find-file-read-only (concat 7zr-temp-directory  7zr-original-version))
	   (when 7zr-view_highlightp
	     (7zr-view-highlight-changes nil t)
	     )
;	   (7zr-view-local-set-keys)
	   (7zr-view-mode)
	   (setq 7zr-summary-reconst-last (concat 7zr-temp-directory  "rev" 7zr-original-version))	
	   ))

	((looking-at 7zr-prepend-to-latest-revision)
	 (progn
	   (forward-line -1)
	   (save-window-excursion
	     (setq 7zr-summary-last-line (string-to-number (format-mode-line "%l")))
	     (shell-command (concat "7z e -aoa -o" 7zr-temp-directory " " 7zr-archive-name " " 7zr-prepend-to-latest-revision 7zr-original-version))
	     (7zr-rename-file-if-exists (concat 7zr-temp-directory 7zr-prepend-to-latest-revision 7zr-original-version) (concat 7zr-temp-directory "rev" (number-to-string 7zr-patch-number) "_of_" 7zr-original-version))
	     )
;;	   (setq 7zr-revisions_lastbuffer (current-buffer))
	  
	   (find-file-read-only (concat 7zr-temp-directory  "rev" (number-to-string 7zr-patch-number) "_of_" 7zr-original-version))
	   (setq 7zr-pointer-lastviewed (number-to-string 7zr-patch-number))
	 ;  (7zr-parse-standard-diff-type t)
;	   (7zr-view-local-set-keys)
	   (7zr-view-mode)
	  ;(7zr-summary-reconst-last-del)
	   (setq 7zr-summary-reconst-last (concat 7zr-temp-directory  "rev" (number-to-string 7zr-patch-number) "_of_" 7zr-original-version))
	  (7zr-goto-last-line-column)

	   ))


	; viewing last revision
      ((looking-at (number-to-string 7zr-patch-number))
       (progn
	 (save-window-excursion
	   nil
	    (setq 7zr-summary-last-line (string-to-number (format-mode-line "%l")))
	    (shell-command (concat "7z e -aoa -o" 7zr-temp-directory " " 7zr-archive-name " " 7zr-prepend-to-latest-revision 7zr-original-version))
	    (7zr-rename-file-if-exists (concat 7zr-temp-directory 7zr-prepend-to-latest-revision 7zr-original-version) (concat 7zr-temp-directory "rev" (number-to-string 7zr-patch-number) "_of_" 7zr-original-version) t)
;;	    (setq 7zr-revisions_lastbuffer (current-buffer))
	   )
	  (setq 7zr-pointer-lastviewed (number-to-string 7zr-patch-number))
	  (find-file-read-only (concat 7zr-temp-directory  "rev" (number-to-string 7zr-patch-number) "_of_" 7zr-original-version))
	 (when 7zr-view_highlightp	     
;	   (7zr-parse-standard-diff-type t)
	   (7zr-view-highlight-changes t nil)
	   )

	 (7zr-view-mode)
	   ; (7zr-summary-reconst-last-del)
	  (setq 7zr-summary-reconst-last (concat 7zr-temp-directory  "rev" (number-to-string 7zr-patch-number) "_of_" 7zr-original-version))
	  (7zr-goto-last-line-column)
         nil
	 ))

         ;; viewing any other revision
      	((looking-at "\\([0-9]+\.?[0-9]*\\)[ \t]+[0-9]+[ \t]+\\(.*\\)")
	 (progn
	   (setq 7zr-view_date (match-string-no-properties 2)) 
	   (message 7zr-view_date)
	   (setq 7zr-summary-rev-at-point (match-string-no-properties 1))
	   (7zr-reconstruct-rev-from-patches 7zr-summary-rev-at-point)
	   (7zr-goto-last-line-column)
	   ))
	(t nil)
	)
  )



(defun 7zr-summary-quit ()
  (interactive)
  " quit and kill 7zr-revisions buffers "
  
  (7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-original-version))
  (7zr-delete-file-if-exists (concat 7zr-temp-directory "rev" 7zr-pointer-lastviewed "_of_" 7zr-original-version))
  (when (not (string= 7zr-pointer-lastviewed "")) 
    (7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-pointer-lastviewed))
    )
  (kill-buffer)
  )


; (provide '7zr-summary-mode)

;;; 7zr-summary-mode.el ends  -----------------------------------


;;; 7zr-view-mode.el begins ----------------------------------------


(defun 7zr-view_jump_to_next_difference ()
  "This function is called from a key binding of 7zr-view-mode"
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
  "This function is called from a key binding of 7zr-view-mode"
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
  

(defun 7zr-view-quit ()
  "This function is called from a key binding of 7zr-view-mode"
  (interactive)
; (7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-original-version))
; (7zr-delete-file-if-exists (concat 7zr-temp-directory "rev" 7zr-pointer-lastviewed "_of_" 7zr-original-version))
; (7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-pointer-lastviewed))
  (7zr-save-last-line-column)
  (kill-buffer)
  (switch-to-buffer 7zr-revisions_tmpbuffer)
  )

(defun 7zr-view-quit_view_diff ()
  "This function is called from a key binding of 7zr-view-mode"
  (interactive)
  (let (current_line_num (not_found t) diff_line_num (diff_line_num_highest 0) (diff_line_num_highest_pos 0))
    (setq current_line_num (string-to-number (format-mode-line "%l")))
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
  (setq 7zr-view-last-line (string-to-number (format-mode-line "%l")))
  (setq 7zr-view-last-column (current-column))
)

(defun 7zr-goto-last-line-column ()
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
  (7zr-save-last-line-column)
  (kill-buffer)
  (set-buffer 7zr-revisions_tmpbuffer)
  (forward-line 1)
  (7zr-summary-view-revision) 
  (7zr-goto-last-line-column)
  )

(defun 7zr-view_previous_page ()
  "This function is called from a key binding of 7zr-view-mode"
  (interactive)
  (7zr-save-last-line-column)
  (kill-buffer)
  (set-buffer 7zr-revisions_tmpbuffer)
  (forward-line -1)
  (7zr-summary-view-revision) 
  (7zr-goto-last-line-column)
  )


(defvar 7zr-view-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") '7zr-view_jump_to_next_difference)   
    (define-key map (kbd "e") '7zr-view_jump_to_previous_difference)
    (define-key map (kbd "p") '7zr-view_previous_page)
    (define-key map (kbd "n") '7zr-view_next_page)
    (define-key map (kbd "t") '7zr-view_datetime)
    (define-key map (kbd "j") '7zr-view-quit_view_diff)
    (define-key map (kbd "q") '7zr-view-quit)
    (define-key map [menu-bar 7zr-view]
      (cons "7zr-view" (make-sparse-keymap "7zr-view")))
    (define-key map [menu-bar 7zr-view quit]
      '(menu-item "Quit View" 7zr-view-quit
                  :help "Quit viewing this revision"))
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


(defun 7zr-last-integer-in-string ( string )
  "Return the last integer found in string passed as parameter,
and is used to find the line number referenced by a diff file.
This function is called by
7zr-view-raw-diff-quit_then_view_revision,
7zr-summary-view-raw-diff-file, 7zr-goto-line-of-last-revision."
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
  (beginning-of-buffer)
 ;;; (forward-line (1- 7zr-view-raw-diff_line_num))
  (7zr-view_datetime)
  )

(defun 7zr-view-raw-diff_next_page ()
"This function is called from a key binding of 7zr-view-raw-diff-file-mode"
  (interactive)
  (kill-buffer)
  (set-buffer 7zr-revisions_tmpbuffer)
  (forward-line 1)
  (7zr-delete-file-if-exists 7zr-pointer-lastviewed_raw_diff_file)
  (7zr-summary-view-raw-diff-file) 
  )

(defun 7zr-view-raw-diff_previous_page ()
"This function is called from a key binding of 7zr-view-raw-diff-file-mode"
  (interactive)
  (kill-buffer)
  (set-buffer 7zr-revisions_tmpbuffer)
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


(defun 7zr-summary-view-raw-diff-file ()
  "View the raw diff file for selected revision number.
This function is called from a key binding of 7zr-summary-mode, as well as 7zr-view-mode"
  (interactive)
  (beginning-of-line)
  (cond ((looking-at 7zr-original-version)  ;; if first row is highlighted, view first diff instead
	 (progn
	   (forward-line 1)
	   (7zr-summary-view-raw-diff-file)
	   ))
	((looking-at 7zr-prepend-to-latest-revision) ;; if last row highlighted, view last diff instead
	 (progn
	   (forward-line -1)
	   (7zr-summary-view-raw-diff-file)
	   ))

         ;; viewing any  diff file
      	((looking-at "\\([0-9]+\.?[0-9]*\\)[ \t]+[0-9]+[ \t]+\\(.*\\)")
	 (progn
	   (setq 7zr-view_date (match-string-no-properties 2)) 


	   (save-window-excursion
	     (setq 7zr-summary-rev-at-point (match-string-no-properties 1))
	     (setq 7zr-pointer-lastviewed_raw_diff_file (concat 7zr-temp-directory 7zr-prepend-to-diff-file 7zr-summary-rev-at-point "_of_" 7zr-original-version))
	     (shell-command (concat "7z e -aoa -o" 7zr-temp-directory " " 7zr-archive-name " " 7zr-summary-rev-at-point))
	     (rename-file (concat 7zr-temp-directory 7zr-summary-rev-at-point) 7zr-pointer-lastviewed_raw_diff_file t)
	     )
;	   (setq 7zr-revisions_lastbuffer (current-buffer))
	   (find-file 7zr-pointer-lastviewed_raw_diff_file)
	   
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
	   (7zr-view-raw-diff-file-mode 1)
	   (set-buffer-modified-p nil)
	   (toggle-read-only 1)
	   (message 7zr-view_date)

	   ))
	(t nil)
	)
  )

(defvar 7zr-view-raw-diff-file-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") '7zr-view-raw-diff_previous_page)
    (define-key map (kbd "n") '7zr-view-raw-diff_next_page)
    (define-key map (kbd "d") '7zr-view-raw-diff_next_change_hunk)   
    (define-key map (kbd "e") '7zr-view-raw-diff_prev_change_hunk) 
    (define-key map (kbd "t") '7zr-view_datetime)
    (define-key map (kbd "r") '7zr-view-raw-diff-quit_then_view_revision)
    (define-key map (kbd "q") '7zr-view-raw-diff-quit)
    (define-key map [menu-bar 7zr-view-raw-diff]
      (cons "7zr-view-raw-diff" (make-sparse-keymap "7zr-view-raw-diff")))
    (define-key map [menu-bar 7zr-view-raw-diff quit]
      '(menu-item "Quit View" 7zr-view-raw-diff-quit
                  :help "Quit viewing this raw diff file."))
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

(require 'hl-line+)
(setq debug-on-error t)
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




(defun string-starts-with-p (string prefix)
    "Return t if STRING starts with PREFIX."
    (and
     (string-match (rx-to-string `(: bos ,prefix) t)
                   string)
     t))

(defun string-ends-with-p (string suffix)
  "Return t if STRING ends with SUFFIX."
  (and (string-match (rx-to-string `(: ,suffix eos) t)
                     string)
       t))


 (defun 7zr-toggle-highlight ()
   (interactive)
   (if (string-starts-with-p (buffer-name (current-buffer)) "7z-revisions_of_")
       (hl-line-mode 1)
     (hl-line-mode 0)
     )
   )





(defun 7zr-create-blank-file-for-archive-created-by-message ()
  (let (7zr-create-blank-file-tmpbuffer 7zr-save-current-buffer)
    (setq 7zr-save-current-buffer (current-buffer))
    (setq 7zr-create-blank-file-tmpbuffer (generate-new-buffer-name (concat "7zr-create-blank-file_of_" (file-name-nondirectory (buffer-file-name (current-buffer))) )))
    (get-buffer-create 7zr-create-blank-file-tmpbuffer)
    (set-buffer 7zr-create-blank-file-tmpbuffer)
    (write-file (concat 7zr-temp-directory 7zr-archive-created-by-message))
    (kill-buffer 7zr-archive-created-by-message)
    (set-buffer 7zr-save-current-buffer)
    )
  )
  


(defun 7zr-examine-archive ()
  " find 7zr-original-version and create a buffer 7zr-revisions-tmp-buffer with names of patches, and discard any filenames that arent numbers per number-or-marker-p, also get 7zr-patch-number"
  (setq 7zr-buffer (buffer-name (current-buffer)))
  (setq 7zr-buffer-filename-without-extension (7zr-sans-extension 7zr-buffer))
  (setq 7zr-buffer-filename (buffer-name (current-buffer)))
  (setq 7zr-archive-name (concat 7zr-buffer-filename 7zr-archive-extension))  
  (setq 7zr-patch-number 0.0)
  (setq 7zr-revisions_lastbuffer (current-buffer))
  (setq 7zr-revisions-examine-tmpbuffer (generate-new-buffer-name (concat "7zr-examination_of_" 7zr-buffer-filename)))
  (get-buffer-create 7zr-revisions-examine-tmpbuffer)
  (set-buffer 7zr-revisions-examine-tmpbuffer)
  (let ((ofile 7zr-archive-name)
        files file sum col timetuple savepoint reached-minuses patch highest-patch)

      (setq 7zr-revisions_with-temp-buffer (current-buffer))

      (call-process "7z" nil t nil "l" ofile)

      (goto-char (point-min))
      (if (re-search-forward "^Listing archive:" nil t)
          (progn
            (forward-line 2)
            ;(setq sum (buffer-substring (point) (point-max)))
            (re-search-forward "Name")
            (setq col (- (current-column) 4))
            (re-search-forward "^-[-]+")
            (forward-line 1)
	    (set-buffer 7zr-revisions_with-temp-buffer)
            (while 
		(not (looking-at "^-[-]+"))
	      (set-buffer 7zr-revisions_with-temp-buffer)
;	      (beginning-of-line)
;	      (looking-at "^\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)" )
;	      (setq timetuple (apply #'encode-time (parse-time-string (match-string 0))))
	      (beginning-of-line)
              (forward-char col)
              (setq file (buffer-substring (point)
                                           (progn (forward-line 1)
                                                  (1- (point)))))
		   
	      (if (and (string-starts-with-p file 7zr-buffer-filename-without-extension)
		       (not (string-match 7zr-archive-created-by-message file)))  
		  (setq 7zr-original-version file)
		(when (not (string-match "[a-zA-Z]+" file))   ; else it must be a number and contain no letters to match 		   
		     (progn
		       (set-buffer 7zr-revisions-examine-tmpbuffer)
		       (insert file)
		       (newline)
		       (setq patch (string-to-number file))
		       (and (> patch 7zr-patch-number)            ; if patch is higher it becomes highest
			    (setq 7zr-patch-number patch)
			    
			    )
		       )
		     )
		)
	      (set-buffer 7zr-revisions_with-temp-buffer)

	      )
	    )

        (error "Not a valid 7z-revisions.el archive file")
	)


    (set-buffer 7zr-revisions-examine-tmpbuffer)
    (goto-char (point-min))
    (goto-char (point-max))
    (mark-whole-buffer)
 ;   (sort-numeric-fields)
    
    )
)

(defun 7zr-sans-extension ( filenamestring )
  (replace-regexp-in-string "\\..+" "" filenamestring)
)


(defun 7zr-what-is-extension-of-filename ( filenamestring )
  (let ((answer ""))
    (setq answer (replace-regexp-in-string ".*\\." "" filenamestring))
    (if (string= answer filenamestring)
	""
      answer
      )
    )
  )

(defun 7zr-refresh-all-bookmarks ()
   "For every bookmark in the bookmark-default-file that points to a file that has an associated 7z-revisions.el .7z archive, update the bookmark to point to the correct location indicated by its associated context.  This becomes useful as the script file grows.  "
  ;; to do this, first verify whether there is an associated .7z
  ;; archive, then whether the context around the bookmark pointer is
  ;; accurate.  If not, we use 7z-summary-view, going backward
  ;; revision by revision, starting at latest version, until the
  ;; context around the bookmark pointer matches that in the
  ;; bookmark-default-file for each respective bookmark.  Then we
  ;; count how many lines have been added to the document before each
  ;; pointer, from each diff starting with the revision in question,
  ;; and ending with the latest diff, and add that to the original
  ;; pointer to updated its location, but first ofcourse verifiying
  ;; whether the new pointer matches the specified context.

   (interactive)
   (let ((last_buffer (current-buffer))
	 bookmark_buffer)
     (save-excursion
       (save-window-excursion
	 (find-file-read-only bookmark-default-file)
	 (setq bookmark_buffer (current-buffer))
	 (while (re-search-forward "^(?(\"\\(.*\\)\"\n (filename . \"\\(.*\\)\")\n (front-context-string . \"\\(.*\\)\")\n (rear-context-string . \"\\(.*\\)\")\n (position . \\([0-9]+\\)))" nil t)
	   (setq bkmrk_ptr_name (match-string-no-properties 1))
	   (setq bkmrk_ptr_file (match-string-no-properties 2))
	   (setq bkmrk_ptr_front-context  (match-string-no-properties 3))
	   (setq bkmrk_ptr_rear-context  (match-string-no-properties 4))	   
	   (setq bkmrk_ptr_pos  (string-to-number (match-string-no-properties 5)))	       
;

	   ))))
   )

(defun 7zr-goto-line-of-last-revision ()
  "Jump to the line number relating to the last hunk of the last revision, messaging the datetime when it was saved"
  (interactive)
  (setq 7zr-buffer (buffer-name (current-buffer)))
  (setq 7zr-buffer-filename 7zr-buffer)
  (setq 7zr-archive-name (concat 7zr-buffer-filename 7zr-archive-extension))  
  (when (not (file-directory-p 7zr-temp-directory))
    (make-directory 7zr-temp-directory t))
  (if (not (file-exists-p 7zr-archive-name))
      (message (concat "Archive " 7zr-archive-name " has not yet been created yet, neither via 7z-revisions-mode nor by typing M-x 7zr-commit."))
    
    (if (eql (string-match "No files to process" (shell-command-to-string (concat "7z e -aoa -o" 7zr-temp-directory " "  7zr-archive-name  " " (shell-quote-argument 7zr-archive-created-by-message)))) nil)
	(progn
	  (7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-archive-created-by-message))
	  (setq 7zr-patch-number 1.0)
	  (setq 7zr-revisions_lastbuffer (current-buffer))
	  (setq 7zr-revisions_tmpbuffer2 (generate-new-buffer-name (concat "7zr-revisions_with-temp-buffer_of_" 7zr-buffer-filename)))
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
	    (shell-command (concat "7z e -aoa -o" 7zr-temp-directory " " 7zr-archive-name " " 7zr-patch-number-string))  
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




;(let* ((#1=#:v 7zr-revisions_tmpbuffer3))  (with-current-buffer #1#    (set (make-local-variable '7zr-active-document) "hithere")))

(defun 7z-revisions ()
  " find 7zr-original-version and create a buffer 7zr-revisions-tmp-buffer with names of patches, and discard any filenames that arent numbers per number-or-marker-p, also get 7zr-patch-number"
  (interactive)
  (setq cntr 0) ; debug
  (when (not (file-directory-p 7zr-temp-directory))
    (make-directory 7zr-temp-directory t))
  (setq 7zr-active-document_buffer (current-buffer))
  (setq 7zr-active-document (buffer-name 7zr-active-document_buffer))
  (setq 7zr-buffer (buffer-name (current-buffer)))


  (setq 7zr-archive-file-name-directory (file-name-directory (buffer-file-name (current-buffer))))
  (setq 7zr-revisions_original_buffer (current-buffer))
  (setq 7zr-buffer-filename-without-extension (7zr-sans-extension 7zr-buffer))
  (setq 7zr-buffer-filename-extension (7zr-what-is-extension-of-filename 7zr-buffer))
  (setq 7zr-buffer-filename 7zr-buffer)
  (setq 7zr-archive-name (concat 7zr-buffer-filename 7zr-archive-extension))  
  (setq 7zr-latest-revision_size "")
  (setq 7zr-latest-revision_datetime "")
 

  (if (not (file-exists-p 7zr-archive-name))
      (message (concat "Archive " 7zr-archive-name " has not yet been created yet, neither via 7z-revisions-mode nor by typing M-x 7zr-commit."))
    
    (if (eql (string-match "No files to process" (shell-command-to-string (concat "7z e -aoa -o" 7zr-temp-directory " "  7zr-archive-name  " " (shell-quote-argument 7zr-archive-created-by-message)))) nil)
	(progn
	  (7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-archive-created-by-message))
	  (setq 7zr-patch-number 1.0)
;	  (setq 7zr-view-last-column (current-column))
;	  (setq 7zr-view-last-line (string-to-number (format-mode-line "%l")))
	  (7zr-save-last-line-column)
	  (setq 7zr-revisions_lastbuffer (current-buffer))
	  (setq 7zr-revisions_tmpbuffer (generate-new-buffer-name (concat "7zr-revisions_of_" 7zr-buffer-filename)))
	  (get-buffer-create 7zr-revisions_tmpbuffer)
	  (setq 7zr-revisions_tmpbuffer2 (generate-new-buffer-name (concat "7zr-revisions_with-temp-buffer_of_" 7zr-buffer-filename)))
	  (get-buffer-create 7zr-revisions_tmpbuffer2)
;	  (setf (buffer-local-value '7zr-active-document 7zr-revisions_tmpbuffer) '7zr-buffer)
	  (let* ((#1=#:v 7zr-revisions_tmpbuffer))
	    (with-current-buffer #1#
	      (set (make-local-variable '7zr-active-document) 7zr-buffer)))

	  (set-buffer 7zr-revisions_tmpbuffer)
;	  (make-local-variable '7zr-archive-file-name-directory)
	  ;(setq 7zr-archive-file-name-directory (buffer-local-value '7zr-archive-file-name-directory 7zr-revisions_original_buffer))
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
	    
	      (if (and (string-starts-with-p 7zname 7zr-buffer-filename-without-extension)  ; find original version
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
		(if (string-starts-with-p 7zname 7zr-prepend-to-latest-revision)  ; find latest revision
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
	  (7zr-revisions-load-hashtable)
	  (switch-to-buffer 7zr-revisions_tmpbuffer)
	  (goto-char (point-min))
	  (when (and 
		 (re-search-forward 7zr-original-version nil t)
		 (not (string= (format-mode-line "%l") "1")))
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
	  (let* ((#1=#:v 7zr-revisions_tmpbuffer))
		      (with-current-buffer #1#
			(set (make-local-variable '7zr-active-document_original) 7zr-original-version)))
		  
	  )
      ; else fail
      (message (concat "There already exists an archive named " (buffer-name (current-buffer)) 7zr-archive-extension " that is not a 7z-revisions.el archive!"))
      )
    )
  )            ; 7z-revisions

  
(defun dired-7z-revisions ()
  " find 7zr-original-version and create a buffer 7zr-revisions-tmp-buffer with names of patches, and discard any filenames that arent numbers per number-or-marker-p, also get 7zr-patch-number"
  (interactive)
  (setq cntr 0) ; debug
  (when (not (file-directory-p 7zr-temp-directory))
    (make-directory 7zr-temp-directory t))
  (setq 7zr-active-document_buffer (file-name-nondirectory (dired-get-file-for-visit)))
  (setq 7zr-active-document 7zr-active-document_buffer)
  (setq 7zr-buffer (file-name-nondirectory (dired-get-file-for-visit)))


  (setq 7zr-archive-file-name-directory (file-name-directory (dired-get-file-for-visit)))
  (setq 7zr-revisions_original_buffer (current-buffer))
  (setq 7zr-buffer-filename-without-extension (7zr-sans-extension 7zr-buffer))
  (setq 7zr-buffer-filename-extension (7zr-what-is-extension-of-filename 7zr-buffer))
  (setq 7zr-buffer-filename 7zr-buffer)
  (setq 7zr-archive-name 7zr-buffer)  
  (setq 7zr-latest-revision_size "")
  (setq 7zr-latest-revision_datetime "")
 

  (if (not (string= 7zr-buffer-filename-extension 7zr-archive-extension-suffix))
      (message (concat "File " 7zr-archive-name " is not a " 7zr-archive-extension-suffix " archive."))    
    (if (eql (string-match "No files to process" (shell-command-to-string (concat "7z e -aoa -o" 7zr-temp-directory " "  7zr-archive-name  " " (shell-quote-argument 7zr-archive-created-by-message)))) nil)
	(progn
	  (7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-archive-created-by-message))
	  (setq 7zr-patch-number 1.0)
	  (setq 7zr-revisions_lastbuffer (current-buffer))
	  (setq 7zr-revisions_tmpbuffer (generate-new-buffer-name (concat "7zr-revisions_of_" 7zr-buffer-filename)))
	  (get-buffer-create 7zr-revisions_tmpbuffer)
	  (setq 7zr-revisions_tmpbuffer2 (generate-new-buffer-name (concat "7zr-revisions_with-temp-buffer_of_" 7zr-buffer-filename)))
	  (get-buffer-create 7zr-revisions_tmpbuffer2)
;	  (setf (buffer-local-value '7zr-active-document 7zr-revisions_tmpbuffer) '7zr-buffer)
	  (let* ((#1=#:v 7zr-revisions_tmpbuffer))
	    (with-current-buffer #1#
	      (set (make-local-variable '7zr-active-document) 7zr-buffer)))

	  (set-buffer 7zr-revisions_tmpbuffer)
;	  (make-local-variable '7zr-archive-file-name-directory)
	  ;(setq 7zr-archive-file-name-directory (buffer-local-value '7zr-archive-file-name-directory 7zr-revisions_original_buffer))
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
	    
	      (if (and (string-starts-with-p 7zname 7zr-buffer-filename-without-extension)  ; find original version
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
		(if (string-starts-with-p 7zname 7zr-prepend-to-latest-revision)  ; find latest revision
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
	  (7zr-revisions-load-hashtable)
	  (switch-to-buffer 7zr-revisions_tmpbuffer)
	  (goto-char (point-min))
	  (when (and 
		 (re-search-forward 7zr-original-version nil t)
		 (not (string= (format-mode-line "%l") "1")))
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
	  (7zr-summary-sort-1)
	  (goto-char (point-min))
	  (7zr-summary-mode)
	  (make-local-variable 'global-hl-line-mode)
	  (toggle-read-only 1)
	  (set-buffer-modified-p nil)
	  (hl-line-mode 1)
	  (kill-buffer 7zr-revisions_with-temp-buffer)
	  (let* ((#1=#:v 7zr-revisions_tmpbuffer))
		      (with-current-buffer #1#
			(set (make-local-variable '7zr-active-document_original) 7zr-original-version)))
	  (end-of-buffer)
	  (forward-line -1)
	  )
      ; else fail
      (message (concat "The file " 7zr-buffer " is not a 7z-revisions.el archive!"))
      )
    )
  )            ; dired-7z-revisions

  


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
STRING is the output line from PROC."
  (save-current-buffer
    (set-buffer (process-buffer proc))
    (let ((inhibit-read-only t))
      (7zr-password proc string))))

(defun 7zr-process-sentinel (proc status)
  "Report PROC change to STATUS."
  (message "7z %s" (substring status 0 -1)))



(defun string-trim-final-newline (string)
  (let ((len (length string)))
    (cond
      ((and (> len 0) (eql (aref string (- len 1)) ?\n))
       (substring string 0 (- len 1)))
      (t string))))

(defun 7zr-extract-archive ()
"DEFUNCT"
  (shell-command (concat "7z e -aoa -o" 7zr-temp-directory " " 7zr-archive-name " " 7zr-original-version))
  (set-buffer 7zr-revisions_tmpbuffer)
  (while (not (eobp))
    (set-buffer 7zr-revisions_tmpbuffer)
    (beginning-of-line)
    (looking-at "\\([0-9]+\.?[0-9]*\\)")
    (shell-command (concat "7z e -aoa -o" 7zr-temp-directory " " 7zr-archive-name " " (match-string 0)))
    (set-buffer 7zr-revisions_tmpbuffer)
    (next-line)
    ) 
)


(defun 7zr-revisions-load-hashtable_backup ()
  "DEFUNCT"
  (shell-command (concat "7z e -aoa -o" 7zr-temp-directory " " 7zr-archive-name " " 7zr-prepend-to-hash-file 7zr-original-version))
  (defconst 7zr-hasht (make-hash-table :size 1000 :test 'equal))
  (load-file (concat 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-original-version))
  )


(defun 7zr-revisions-load-hashtable ()
  "Extracts hashtable from .7z archive and loads it"
  (save-window-excursion
    (let ((hash "") lines)
      (setq 7zr-current-original-version 7zr-original-version)
      (make-local-variable '7zr-current-original-version)

      (set (make-local-variable '7zr-active-document_original) 7zr-original-version)
		  
      (shell-command (concat "7z e -aoa -o" 7zr-temp-directory " " 7zr-archive-name " " 7zr-prepend-to-hash-file 7zr-current-original-version))
      (setq hash (string-trim-final-newline (shell-command-to-string (concat 7zr-sha1sum-command " " 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-current-original-version 7zr-sha1sum-post-command (7zr-awk-cmd-string 1)))))
      (if (not (string= hash 7zr-hash-of-hash-file))  ; only load the hashtable if it has changed
	  (progn
	    (setq lines (string-to-number (string-trim-final-newline (shell-command-to-string (concat "awk \"END { print NR }\"" 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-current-original-version)))))
	    (setq lines (+ lines 30))	  	  
	    (setq 7zr-hash-of-hash-file hash)
	    (set (make-local-variable '7zr-active-document_hash_of_hash_file) 7zr-hash-of-hash-file)

	    (defconst 7zr-hasht (make-hash-table :size lines :test 'equal))
;	  (set (make-local-variable '7zr-hasht) 7zr-hasht)
;	  (make-local-variable '7zr-hasht)
	    (load-file (concat 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-current-original-version))
	    )
	nil
	)
      )
    )
  )

    

(defun 7zr-revisions-validate-revp ( rev-pointer )
"Check revision against hash"
  (let ((rev-hash "") (hash-from-table "") (hash-rev-to-check-against ""))
    (setq hash-rev-to-check-against rev-pointer)
    ;; (if (eql direction -1)  ; if we were walking upward the valid hash will be the hash associated with the nearest ancestor?
    ;; 	(setq hash-rev-to-check-against nearest_ancestor)
    ;;   (setq hash-rev-to-check-against rev-pointer)

      (if (file-exists-p (concat 7zr-temp-directory "rev" rev-pointer "_of_" 7zr-original-version))
    ; check against hash
	  (progn 
	    (setq rev-hash (string-trim-final-newline (shell-command-to-string (concat 7zr-sha1sum-command " " 7zr-temp-directory "rev" rev-pointer "_of_" 7zr-original-version 7zr-sha1sum-post-command (7zr-awk-cmd-string 1)))))
	    (setq hash-from-table (gethash hash-rev-to-check-against 7zr-hasht))
	    (if (string= rev-hash hash-from-table)
		t
	      nil
	      )
	    )
	nil
	)
     ; )
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
	(delete-file filename_trimmed)
      nil
      )
    )
  )

(defun 7zr-rename-file-if-exists ( file newname &optional overwritep)
  " Rename a file if it exists, and returns nil, taking no
action, if it doesnt.  In addition, this over-writes destination
file if that file already exists.  The overwritep option is ignored."
  (interactive)
  (if (file-exists-p file )
      (rename-file file newname t)
    nil
    )
  )
	

(defun 7zr-reconstruct-get-max-patch-string ()
  " sets  7zr-reconstruct-max-patch-string and 7zr-revisions_tmpbuffer_lines which are max number of lines minus 1"
  (setq 7zr-reconstruct-save-point (point))
  (goto-char (point-max))
  (beginning-of-line)
  (forward-line -1)
  (looking-at "\\([0-9]+\.?[0-9]*\\)")
  (setq 7zr-reconstruct-max-patch-string (match-string-no-properties 0))
  (setq 7zr-revisions_tmpbuffer_lines (string-to-number (format-mode-line "%l")))
  (goto-char 7zr-reconstruct-save-point)
)

(defun 7zr-reconstruct-sl ()
  "DEFUNCT"
  (beginning-of-line)
  (looking-at "\\([0-9]+\.[0-9]+\\)")
  (setq 7zr-sl-rev (match-string-no-properties 1))
  (7zr-reconstruct-slow 7zr-sl-rev (line-end-position))
  (find-file-read-only (concat 7zr-temp-directory "rev" 7zr-sl-rev "_of_" 7zr-original-version))
  (7zr-view-local-set-keys)
)


(defun 7zr-reconstruct-slow (rev rev-point)
  "This function is only called from 7zr-summary-consolidate-region
and does the same thing as 7zr-reconstruct-rev-from-patches"
  (let ( pointer current-patch	(saved-point-pos (point)) hash-of-file	hash-from-table  )
    (setq 7zr-patch-command-called-x-times 0)

    (save-window-excursion
      (shell-command (concat "7z e -aoa -o" 7zr-temp-directory " " 7zr-archive-name " " 7zr-original-version))  
      (7zr-rename-file-if-exists (concat 7zr-temp-directory 7zr-original-version) (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip) )
      )
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(forward-line 1)
	(narrow-to-region (point) rev-point)
	(goto-char (point-min))
	(while (re-search-forward "\\([0-9]+\.[0-9]\\)" nil t)
	  (setq current-patch (match-string-no-properties 1))
	  (save-window-excursion
	    (shell-command (concat "7z e -aoa -o" 7zr-temp-directory " " 7zr-archive-name " " current-patch))
	    (shell-command (concat 7zr-patch-command " " 7zr-temp-directory 7zr-prepend-to-reconstruct_wip " < " 7zr-temp-directory current-patch))

	    (shell-command (concat "touch -r " 7zr-temp-directory current-patch " " 7zr-temp-directory "7zr-datetime-reference"))

	    (7zr-delete-file-if-exists (concat 7zr-temp-directory current-patch ))
	  )
;	  (setq 7zr-patch-command-called-x-times (incf 7zr-patch-command-called-x-times))  ;debug
	  (setq hash-from-table (gethash current-patch 7zr-hasht))
	  (setq hash-from-file (string-trim-final-newline (shell-command-to-string (concat 7zr-sha1sum-command " " 7zr-temp-directory 7zr-prepend-to-reconstruct_wip 7zr-sha1sum-post-command (7zr-awk-cmd-string 1)))))
	  (when (not (string= hash-from-table hash-from-file))
	    (message (concat "revision " current-patch  " has incorrect hash!"))
	    )
	  )  ; while		
	(7zr-rename-file-if-exists (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip) (concat 7zr-temp-directory "rev" rev "_of_" 7zr-original-version))
	(7zr-delete-file-if-exists (concat 7zr-temp-directory "rev" 7zr-construct-slow_last "_of_" 7zr-original-version))
	(setq 7zr-construct-slow_last current-patch)
	)
      )
    )
  )

(defun 7zr-new-linenum-after-diff ( linenum_param diff-file direction )
  "Returns list, first element of which is the new line number calculated after a number of lines are added above the given linenum_param, or deleted, in the case of a negative number, as would result from patching from the diff-file. The 2nd list element returned is hunk number that resulted in a change to the linenum in question, 0 if unchanged.  The direction parameter is similar to the parameter given to the patch command, e.g. -R for reverse"
  (interactive)
  (let (hunknum-that-changed-linenum hunknum n_diffbuffer n_lastbuffer n_diff_list)
    (setq n_lastbuffer (current-buffer))
    (save-window-excursion
      (find-file-read-only diff-file)
      (setq n_diffbuffer (current-buffer))
      (set-buffer n_diffbuffer)
      (goto-char (point-min))
      (setq n_diff_list '())
      (setq hunknum 0)
      (setq hunknum-that-changed-linenum 0)
      (setq linenum_offset 0)
    
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

	  (if (string-equal diff-type "a")
	      (setq a-end a-begin)
	    (if (string-equal diff-type "d")
		(setq  b-end b-begin)))

	  (if (and
	       (>= linenum_param b-begin )
	       (<= linenum_param b-end ))
	      (setq hunknum-that-changed-linenum hunknum)
	  ; else
	    (when (and (> linenum_param b-end )
		  (string-match "R" direction))
	      (setq linenum_offset (- a-end b-end)))
	    (when (and (> linenum_param a-end )
		       (not (string-match "R" direction)))   
	      (setq linenum_offset (- b-end a-end)))	     
      
	    ) ; if
	  ) ; let*
	) ; while
	(set-buffer n_lastbuffer)
	(kill-buffer n_diffbuffer)
	(list (+ linenum_param linenum_offset) hunknum-that-changed-linenum )
      ) ;  save-windows-excursion	       
    ) ; let
  )


(defun DEFUNCT7zr-list-revs ()
  "returns name of buffer created, which lists the revs"
  (setq 7zr-patch-number 0.0)
  
  (let ((ofile 7zr-archive-name)
	(7zr-revisions_lastbuffer (current-buffer))
	(7zr-revisions-list-revs-tmpbuffer (generate-new-buffer-name (concat "7zr-list-revs_of_" 7zr-buffer-filename)))
        files file sum col timetuple savepoint reached-minuses patch highest-patch 7zr-revisions_with-temp-buffer )
    (get-buffer-create 7zr-revisions-list-revs-tmpbuffer)
    (set-buffer 7zr-revisions-list-revs-tmpbuffer)
    (setq 7zr-revisions_with-temp-buffer (current-buffer))

    (call-process "7z" nil t nil "l" ofile)

    (goto-char (point-min))
    (if (re-search-forward "^Listing archive:" nil t)
	(progn
	  (forward-line 2)
            ;(setq sum (buffer-substring (point) (point-max)))
	  (re-search-forward "Name")
	  (setq col (- (current-column) 4))
	  (re-search-forward "^-[-]+")
	  (forward-line 1)
	  (set-buffer 7zr-revisions_with-temp-buffer)
	  (while 
	      (not (looking-at "^-[-]+"))
	    (set-buffer 7zr-revisions_with-temp-buffer)
;	      (beginning-of-line)
;	      (looking-at "^\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)" )
;	      (setq timetuple (apply #'encode-time (parse-time-string (match-string 0))))
	    (beginning-of-line)
	    (forward-char col)
	    (setq file (buffer-substring (point)
					 (progn (forward-line 1)
						(1- (point)))))
	    
	    (if (and (string-starts-with-p file 7zr-buffer-filename-without-extension)
		     (not (string-match 7zr-archive-created-by-message file)))  
		(setq 7zr-original-version file)
	      (when (not (string-match "[a-zA-Z]+" file))   ; else it must be a number and contain no letters to match 		   
		(progn
		  (set-buffer 7zr-revisions-list-revs-tmpbuffer)
		  (insert file)
		  (newline)
		  (setq patch (string-to-number file))
		  (and (> patch 7zr-patch-number)            ; if patch is higher it becomes highest
		       (setq 7zr-patch-number patch)
		       
		       )
		  )
		)
	      )
	    (set-buffer 7zr-revisions_with-temp-buffer)
	    
	    )
	  )
      
      (error "Not a valid 7z-revisions.el archive file")
      )


    (set-buffer 7zr-revisions-list-revs-tmpbuffer)
    (goto-char (point-min))
    (goto-char (point-max))
    (mark-whole-buffer)
 ;   (sort-numeric-fields)
    
    )
  )
  
(defun 7zr-list-revs ( filename )
  "create a read-only buffer containing list of revisions, returning the newly created buffer, and original version as a string"
  (let (7zr-patch-number 7zr-original-version 7zr-buffer-filename-without-extension 7zr-buffer-filename-without-extension 7zr-list-revs_lastbuffer 7zr-list-revs_tmpbuffer 7zr-list-revs_tmpbuffer2 (ofile 7zr-archive-name)
			 files file sum col timetuple savepoint reached-minuses patch highest-patch 7zdatetime 7zsize 7zname)
    
    
    (setq 7zr-buffer-filename-without-extension (7zr-sans-extension filename)) 
    (setq 7zr-patch-number 1.0)
    (setq 7zr-list-revs_lastbuffer (current-buffer))
    (setq 7zr-list-revs_tmpbuffer (generate-new-buffer-name (concat "7zr-list-revs_of_" filename)))
    (get-buffer-create 7zr-list-revs_tmpbuffer)
    (setq 7zr-list-revs_tmpbuffer2 (generate-new-buffer-name (concat "7zr-list-revs_with-temp-buffer_of_" filename)))
    (get-buffer-create 7zr-list-revs_tmpbuffer2)
					;	  (setf (buffer-local-value '7zr-active-document 7zr-list-revs_tmpbuffer) '7zr-buffer)

	  
    (set-buffer 7zr-list-revs_tmpbuffer)
			       ;	  (make-local-variable '7zr-archive-file-name-directory)
	  ;(setq 7zr-archive-file-name-directory (buffer-local-value '7zr-archive-file-name-directory 7zr-list-revs_original_buffer))
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
	

	    
	(if (and (string-starts-with-p 7zname 7zr-buffer-filename-without-extension)  ; find original version
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
	  (if (string-starts-with-p 7zname 7zr-prepend-to-latest-revision)  ; find latest revision
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
    (when (and 
	   (re-search-forward 7zr-original-version nil t)
	   (not (string= (format-mode-line "%l") "1")))
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
    (toggle-read-only 1)
    (set-buffer-modified-p nil)
    (kill-buffer 7zr-list-revs_with-temp-buffer)
    (end-of-buffer)
    (forward-line -1)
    (beginning-of-line)
    (set-buffer 7zr-list-revs_lastbuffer)
    (list 7zr-original-version  7zr-list-revs_tmpbuffer) ; returns a string and a buffer
    ) ;let  
  )               ; 7zr-list-revs

  

(defun 7zr-line-last-changed-on ()
  "find the datetime of the last time that the current line had been changed."
  (interactive)
  (let ( find-archive-name (linenum (string-to-number (format-mode-line "%l"))) linenum_obj hunk_that_changed_line list-revs_obj revs-buffer rev rev-point pointer current-patch (saved-point-pos (point)) hash-of-file hash-from-table datetime_string 7zr-lastbuffer 7zr-buffer-file-name return_message)


    (setq find-archive-name (concat (file-name-nondirectory (buffer-file-name (current-buffer))) 7zr-archive-extension))
    (when (not (file-directory-p 7zr-temp-directory))
      (make-directory 7zr-temp-directory t))
    (save-window-excursion
      (if (not (file-exists-p find-archive-name))
	  (message (concat "Archive " find-archive-name " has not yet been created yet, neither via 7z-revisions-mode nor by typing M-x 7zr-commit."))
	
	(if (string-match "No files to process" (shell-command-to-string (concat "7z e -aoa -o" 7zr-temp-directory " "  find-archive-name  " " (shell-quote-argument 7zr-archive-created-by-message))))
	    (error "Not a valid 7z-revisions.el archive file")
	; else

	  (setq 7zr-buffer (buffer-name (current-buffer)))
	  (setq 7zr-lastbuffer (current-buffer))
	  (setq 7zr-archive-name (concat 7zr-buffer 7zr-archive-extension))  
	  (if  (not (setq list-revs_obj (7zr-list-revs 7zr-buffer)))
	      (error "cannot get revisions list")
	    ; else
	    (setq revs-buffer (nth 1 list-revs_obj))
	    (set-buffer revs-buffer)
	    (setq 7zr-original-version (car list-revs_obj))
	    
	    (shell-command (concat "7z e -aoa -o" 7zr-temp-directory " " 7zr-archive-name " " 7zr-prepend-to-latest-revision 7zr-original-version)) 
	    (setq line_not_changed t)
	    (setq hunk_that_changed_line 0)

	    (while line_not_changed

	      (if  	  (looking-at "\\([0-9]+\.[0-9]+\\)")
		  (progn
		    (setq current-patch (match-string-no-properties 1))
		    (shell-command (concat "7z e -aoa -o" 7zr-temp-directory " "  7zr-archive-name " " current-patch))
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



(defun 7zr-reconstruct-rev-from-patches (rev)
  "This function is called from 7zr-summary-view-revision, after running
 7z-revisions(), once a line item is selected and the enter key is
 pressed."
  (interactive)
  (let (rev_num (string-to-number rev))
    (set-buffer 7zr-revisions_tmpbuffer)  ; this should instead be a buffer-local variable
    (setq 7zr-reconstruct-patch-to-apply "")
    (setq rev_num (string-to-number rev))
    (setq 7zr-summary-current-line (string-to-number (format-mode-line "%l")))
    (set (make-local-variable '7zr-active-document_current_line) 7zr-summary-current-line)
    (setq 7zr-reconstruct-max-patch-string "")
    (setq 7zr-patch-command-called-x-times 0)
    (setq 7zr-valid-rev-in-tempp nil)
    (setq 7zr-pointer-lastviewed_num (string-to-number 7zr-pointer-lastviewed))
    (save-excursion
      (save-window-excursion
	(7zr-reconstruct-get-max-patch-string)  ; also gets 7zr-revisions_tmpbuffer_lines 

      ;; First check if there is a rev-pointer, if there is and its
      ;;  closer to rev than the max ancestor or max progeny then
      ;;  construct rev from last used rev-pointer and make that one
      ;;  the last used one.  if theres no rev-pointer, or max
      ;;  ancestor or max progeny are closer to the rev target, then
      ;;  construct rev starting from latest ancester if target is
      ;;  above the mid line or 7zr-latest- if target lies below the
      ;;  mid line, and make that the latest rev-pointer

	; if there is a rev still in the temp directory lets verify to see if its valid
	(when (not (string= 7zr-pointer-lastviewed ""))
	  (if (7zr-revisions-validate-revp 7zr-pointer-lastviewed) ; 7zr-reconstruct-direction 7zr-pointer-lastviewed_nearest_ancestor)
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
		 (t (setq 7zr-valid-rev-in-tempp nil))
		 ) ; cond
		) ; progn
	    (setq 7zr-valid-rev-in-tempp nil)	; else
	    ) ; if
	  ) ; when
	
	
	(if (not 7zr-valid-rev-in-tempp)
	    (setq 7zr-pointer-lastviewed "")  ; else invalid rev
	  )
      
    

    ; if no last rev in temp or invalid then create last rev from original or latest

	(if (not 7zr-valid-rev-in-tempp)
	    (if (< (string-to-number (format-mode-line "%l")) (/ 7zr-revisions_tmpbuffer_lines 2))
		(progn           ; start at the top and walk downwards
		  (goto-char (point-min))
		  (setq 7zr-reconstruct-direction 1)
		  (setq 7zr-reconstruct-dtag " ")
		  (shell-command (concat "7z e -aoa -o" 7zr-temp-directory " " 7zr-archive-name " " 7zr-original-version))  
		  (rename-file (concat 7zr-temp-directory 7zr-original-version) (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip) t)
		  )
	      (progn   ; start at bottom of buffer and walk upwards
		(goto-char (point-max))
		(beginning-of-line)
		(forward-line -1)
		(beginning-of-line)
		(looking-at "\\([0-9]+\.?[0-9]*\\)")
		(setq 7zr-pointer-lastviewed (match-string-no-properties 0))
		(set (make-local-variable '7zr-active-document_pointer_lastviewed) 7zr-pointer-lastviewed)
		(setq 7zr-reconstruct-direction -1)
		(setq 7zr-reconstruct-dtag " -R -t ")
		(shell-command (concat "7z e -aoa -o" 7zr-temp-directory " " 7zr-archive-name " " 7zr-prepend-to-latest-revision 7zr-original-version))  
		(rename-file (concat 7zr-temp-directory 7zr-prepend-to-latest-revision 7zr-original-version) (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip) t)
		)
	      ) ; if line number is upper half vs lower half
    ; else  we are using a previous rev thats still in the temp directory and already set up
	  (goto-line 7zr-summary-last-line) 
	  )

	(if (string= 7zr-pointer-lastviewed rev)  ;; if the last used pointer in the temp directory happens to be the one we want
	    nil         ; skip this til later


      ; else we didnt start out at the correct rev, so we must walk the pointer to the correct rev

	  (while  (not (string= 7zr-pointer-lastviewed rev))   ; move the pointer
	    (set-buffer 7zr-revisions_tmpbuffer)
	    (forward-line 7zr-reconstruct-direction)
	    (beginning-of-line)
	    (looking-at "\\([0-9]+\.?[0-9]*\\)")
	    (setq 7zr-pointer-lastviewed_3rd_last 7zr-pointer-lastviewed_2nd_last)  ; advance lastviewed pointers
	    (setq 7zr-pointer-lastviewed_2nd_last 7zr-pointer-lastviewed_last)  ; advance lastviewed pointers
	    (setq 7zr-pointer-lastviewed_last 7zr-pointer-lastviewed)           ;
	    (setq 7zr-pointer-lastviewed (match-string-no-properties 0))        ;

	    (if (= 7zr-reconstruct-direction -1)  ; up toward ancestors 
		(progn
		  (setq 7zr-pointer-lastviewed_nearest_progeny 7zr-pointer-lastviewed_last)
		  (set (make-local-variable '7zr-active-document_pointer_lastviewed_nearest_progeny) 7zr-pointer-lastviewed_nearest_progeny)
		  
		  (setq 7zr-reconstruct-patch-to-apply 7zr-pointer-lastviewed_nearest_progeny)
		  )
	      (if (= 7zr-reconstruct-direction 1) ; down toward progeny
		  (progn
		    (setq 7zr-pointer-lastviewed_nearest_ancestor 7zr-pointer-lastviewed_last)
		    (set (make-local-variable '7zr-active-document_pointer_lastviewed_nearest_ancestor) 7zr-pointer-lastviewed_nearest_ancestor)
		    (setq 7zr-reconstruct-patch-to-apply 7zr-pointer-lastviewed)
		    
		    )
		)
	      nil
	      )

	    		  (when (string= 7zr-reconstruct-patch-to-apply "7z")
		    (debug))  ; rare bug

       
;;   If we have walked upward then we dont want to apply the patch of the number we're on but that of the nearest progeny  (actually, and in other words: if we are in the process of a going upward toward ancestors we want to apply the patch of the number we're currently on, and if going down apply patch of nearest progeny)
	    (shell-command (concat "7z e -aoa -o" 7zr-temp-directory " " 7zr-archive-name " " 7zr-reconstruct-patch-to-apply))
	    (shell-command (concat 7zr-patch-command 7zr-reconstruct-dtag 7zr-temp-directory 7zr-prepend-to-reconstruct_wip " < " 7zr-temp-directory 7zr-reconstruct-patch-to-apply))

	    (setq 7zr-view-last-line (car (7zr-new-linenum-after-diff 7zr-view-last-line (concat 7zr-temp-directory 7zr-reconstruct-patch-to-apply) 7zr-reconstruct-dtag)))
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
		  (setq 7zr-pointer-lastviewed_nearest_progeny 7zr-pointer-lastviewed_last)
		  (forward-line -1)
		  (beginning-of-line)
		  (if (looking-at 7zr-original-version)
		      (setq 7zr-pointer-lastviewed_nearest_ancestor 7zr-original-version)
		    (progn
		      (looking-at "\\([0-9]+\.?[0-9]*\\)")
		      (setq 7zr-pointer-lastviewed_nearest_ancestor (match-string-no-properties 1)) 
		      
		      )
		    )
		  (forward-line 1)
		  )
	;;     
	      (if (= 7zr-reconstruct-direction 1)  ; if headed down toward progeny
		  (progn
		    (setq 7zr-pointer-lastviewed_nearest_ancestor 7zr-pointer-lastviewed_last)
		    (setq 7zr-reconstruct-rev_hash (gethash 7zr-pointer-lastviewed 7zr-hasht))
		    (forward-line 1)
		    (beginning-of-line)
		    (if (looking-at 7zr-latest-revision)
			(setq 7zr-pointer-lastviewed_nearest_progeny 7zr-latest-revision)
		      (progn
			(looking-at "\\([0-9]+\.?[0-9]*\\)") 
			(setq 7zr-pointer-lastviewed_nearest_progeny (match-string-no-properties 1))
			)
		      
		      )
		    (forward-line -1)
		    )
		)
	      )
	    
	    )    ;; --> move the pointer end
	;;   If we are walking upward we dont apply the patch of the one we're on but that of the nearest progeny
	;;      
	
	  (setq 7zr-reconstruct-rev_hash (gethash 7zr-pointer-lastviewed 7zr-hasht))
	  (if (not (file-exists-p (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip)))
	      (progn
		(message (number-to-string  7zr-patch-command-called-x-times))  ;debug1
		
		)
	    )
	  (rename-file (concat 7zr-temp-directory 7zr-prepend-to-reconstruct_wip) (concat 7zr-temp-directory "rev" 7zr-pointer-lastviewed "_of_" 7zr-original-version) t) 
	  (setq 7zr-file-rev_hash (string-trim-final-newline (shell-command-to-string (concat 7zr-sha1sum-command " " 7zr-temp-directory "rev" 7zr-pointer-lastviewed "_of_" 7zr-original-version 7zr-sha1sum-post-command (7zr-awk-cmd-string 1)))))
	  (if (not (string= 7zr-file-rev_hash 7zr-reconstruct-rev_hash))
	      (message "revision hash doesnt match!"))
	  )
	)
      )           ; --> save-excursion ends
     
 
    (7zr-summary-reconst-last-del)
    (setq 7zr-revisions_lastbuffer (current-buffer))
    
    (find-file-read-only (concat 7zr-temp-directory "rev" 7zr-pointer-lastviewed "_of_" 7zr-original-version))
    (set (make-local-variable '7zr-active-document) 7zr-buffer)
    
;    (setf (buffer-local-value '7zr-archive-file-name-directory 7zr-revisions_lastbuffer) 'some-val)
;    (make-local-variable '7zr-archive-file-name-directory)
 ;   (setq 7zr-archive-file-name-directory (buffer-local-value '7zr-archive-file-name-directory 7zr-revisions_lastbuffer))
    (when 7zr-view_highlightp
      (7zr-view-highlight-changes t t)
      )
    (message (concat "Revised on " 7zr-view_date))
    (7zr-view-mode)

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
      (setq 7zr-summary-last-line (string-to-number (format-mode-line "%l")))   ;debug
      (setq 7zr-pointer-lastviewed_last_nearest_progeny 7zr-pointer-lastviewed_nearest_progeny)
      ;; debug, thrown this in temporarily
     (when (not (string= 7zr-pointer-lastviewed 7zr-reconstruct-max-patch-string))
	(7zr-delete-file-if-exists (concat 7zr-temp-directory "rev" 7zr-reconstruct-max-patch-string "_of_" 7zr-original-version))
	(7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-reconstruct-max-patch-string))
	)
     )

    )
  (setq 7zr-summary-last-line (string-to-number (format-mode-line "%l")))
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


(defun 7zr-view-highlight-changes ( ancestorp progenyp )
  "Highlights changes in 7zr-view and enables the d key to jump to
the next difference in the buffer, and the e key to jump to the
previous."

  (setq 7zr-highlight-changes_diff_list '())
  (setq 7zr-highlight-changes_diff_listv [])
  (setq 7zr-highlight-changes_num_of_diff_changes 0)
  (cond ((and ancestorp progenyp)
	 (setq 7zr-highlight-changes_diff_list (append 7zr-highlight-changes_diff_list (7zr-parse-standard-diff-type t)))

	 (setq 7zr-highlight-changes_diff_list (append 7zr-highlight-changes_diff_list (7zr-parse-standard-diff-type nil)))

	)
	(ancestorp
	 (setq 7zr-highlight-changes_diff_list (append 7zr-highlight-changes_diff_list (7zr-parse-standard-diff-type t))))
	(t 
	 (setq 7zr-highlight-changes_diff_list (append 7zr-highlight-changes_diff_list (7zr-parse-standard-diff-type nil))))
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

(defun 7zr-parse-standard-diff-type ( ancestorp )
  "This function returns a list containing the beginning point of
every difference and is only used for highlighting changes when
reviewing revisions"
  (setq 7zr-parse-standard-diff-type_diff_list '())
  (save-window-excursion
    (if ancestorp
	(progn
	  (shell-command (concat "7z e -aoa -o" 7zr-temp-directory " " 7zr-archive-file-name-directory 7zr-archive-name " " 7zr-pointer-lastviewed))
	  (7zr-rename-file-if-exists (concat 7zr-temp-directory 7zr-pointer-lastviewed) (concat 7zr-temp-directory "7zr-parse-" 7zr-original-version))
	  )
      
      (shell-command (concat "7z e -aoa -o" 7zr-temp-directory " " 7zr-archive-file-name-directory 7zr-archive-name " " 7zr-pointer-lastviewed_nearest_progeny))
      (7zr-rename-file-if-exists (concat 7zr-temp-directory 7zr-pointer-lastviewed_nearest_progeny) (concat 7zr-temp-directory "7zr-parse-" 7zr-original-version))
      )
    )
  
  (setq 7zr-parse-standard-diff-type_lastbuffer (current-buffer))
  (find-file-read-only (concat 7zr-temp-directory "7zr-parse-" 7zr-original-version))
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
      (if (string-equal diff-type "a")
	  (setq a-end a-begin-saved)
	(if (string-equal diff-type "d")
	    (setq  b-end b-begin-saved)))

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
	       (setq 7zr-parse-standard-diff-type_diff_list (append 7zr-parse-standard-diff-type_diff_list (7zr-map-difference-ranges-to-point-ranges difference-ranges b-line_pos b-column_pos t)))
	       ))
	    ((and 
	      (not ancestorp)
	      (not (string= diff-type "d")))
	     (progn
	       (setq difference-ranges (7zr-longest-common-word-subsequence-difference-ranges a-words b-words nil))
	       (setq 7zr-parse-standard-diff-type_diff_list (append 7zr-parse-standard-diff-type_diff_list (7zr-map-difference-ranges-to-point-ranges difference-ranges a-line_pos a-column_pos nil)))
	       ))
	    ((not ancestorp)
	     (progn	     		  
	       (setq difference-ranges (list (list 0 (1- (length a-words)))))
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
  (7zr-delete-file-if-exists (concat 7zr-temp-directory "7zr-parse-" 7zr-original-version))
  7zr-parse-standard-diff-type_diff_list
  
 
  ) ; 7zr-parse-standard-diff-type




(defun 7zr-make-array ( lij  init_ele)
  "Actually its make-vector, but can accept a list as the first
argument specifying 2d or 3d matrix dimensions.
Used by 7zr-longest-common-word-subsequence-difference-ranges"
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
  "Used by longest-common-word-subsequence"
  (if (equal j nil)
      (aref array i)
    (progn
      (setq asub (aref array i))
      (aref asub j)
      )
    )
  )


(defun 7zr-setf2 ( array i j &optional x )
  "Used by longest-common-word-subsequence"
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
		   (when (not (eql (setq current_word_num (aref differences_wn_v i)) (1+ last_word_num)))
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
range of differences, in addition to highlighting the differences
in the current-buffer.  This function is only used for
highlighting changes when reviewing revisions"
  (save-excursion
    
    (toggle-read-only 0)
    
    (let* ((begin (point))
	   (end (point))
	   (difference_ranges (reverse difference_ranges))
	   (point-ranges '())
	   (beginning_point_list '())
	   line_pos_first
	   line_pos_current 
	   column_pos_current
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
		(forward-char (- column_pos_current 0))
		(setq begin (point))
		(push (point) beginning_point_list)
		(setq 7zr-map-difference_line_pos_last line_pos_current)
	 
	 ; region ends

		(setq line_pos_current (aref line_pos_v (car (cdr x))))
		(setq column_pos_current (car (cdr (aref column_pos_v (car (cdr x))))))
		(goto-char (point-min))
		(forward-line (- line_pos_current 1))		
;		(forward-line  (- line_pos_current 7zr-map-difference_line_pos_last))
		(forward-char (- column_pos_current 0))
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
;	 )
		) difference_ranges)
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
  "Creates the 7z archive.   This function also inserts a few entries, 
  the original file which is timestamp appended, a file containing hash values of revisions,
  and a file of size 0 named in the form of a message indicating that the archive was 
  created by 7z-revisions.el"
  (let ((tmphashvalue ""))
    (when (not (file-directory-p 7zr-temp-directory))
      (make-directory 7zr-temp-directory t))
    (save-excursion
      (save-window-excursion
	(setq 7zr-buffer-filename-extension (7zr-what-is-extension-of-filename 7zr-buffer-filename))
	
	(setq 7zr-original-version (concat (7zr-sans-extension (file-name-nondirectory (buffer-file-name (current-buffer)))) (7zr-current-date-time-win) "." 7zr-buffer-filename-extension))    
	(setq 7zr-current-original-version 7zr-original-version)
	(make-local-variable '7zr-current-original-version)
	(copy-file 7zr-buffer-filename  (concat 7zr-temp-directory 7zr-original-version))
	(shell-command (concat "7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on " 7zr-archive-name " " 7zr-temp-directory 7zr-original-version))

	(setq tmphashvalue (string-trim-final-newline (shell-command-to-string (concat 7zr-sha1sum-command " " 7zr-temp-directory 7zr-original-version 7zr-sha1sum-post-command (7zr-awk-cmd-string 1)))))


	(append-to-file (concat "(puthash \"" 7zr-original-version  "\" \"" tmphashvalue "\" 7zr-hasht)\n") nil (concat 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-original-version))
	(rename-file   (concat 7zr-temp-directory 7zr-original-version)  (concat 7zr-temp-directory 7zr-prepend-to-latest-revision 7zr-original-version) t)
	(shell-command (concat "7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on " 7zr-archive-name " " 7zr-temp-directory 7zr-prepend-to-latest-revision 7zr-original-version))
	(shell-command (concat "7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on " 7zr-archive-name " " 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-original-version))
;	(shell-command (concat "echo> " 7zr-temp-directory (shell-quote-argument 7zr-archive-created-by-message)))
	(7zr-create-blank-file-for-archive-created-by-message)
	(shell-command (concat "7z a " 7zr-archive-name " " 7zr-temp-directory (shell-quote-argument 7zr-archive-created-by-message)))
  
	(delete-file (concat 7zr-temp-directory 7zr-prepend-to-latest-revision 7zr-original-version))
	(delete-file (concat 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-original-version))
	(delete-file (concat 7zr-temp-directory 7zr-archive-created-by-message))
	)
      )
    )
  (message (concat "Created archive " 7zr-archive-name " for " 7zr-original-version))
  
  )


(defun 7zr-remove-colons-from-timestamps-in-filenames-in-archive ()
  "call this to remove colons from the timestamps in the filenames of archived files, in order to make them compliant with microsoft windows.  Also edit them out of the hash file.  This only works with newer versions of 7-zip, and wont work for version 9.20, since it doesnt have the rename (rn) option.  This function is not interactive, so you'll have to write the following text into your document (7zr-remove-colons-from-timestamps-in-filenames-in-archive) and then press C-x C-e"
  (when (not (file-directory-p 7zr-temp-directory))
    (make-directory 7zr-temp-directory t))
  (save-excursion
    (save-window-excursion
      (setq 7zr-buffer-filename (buffer-name (current-buffer)))
      (setq 7zr-buffer-filename-without-extension (7zr-sans-extension 7zr-buffer-filename)) 
      (setq 7zr-buffer-filename-extension (7zr-what-is-extension-of-filename 7zr-buffer-filename))	     
      (setq 7zr-archive-name (concat 7zr-buffer-filename 7zr-archive-extension))  
      (setq 7zr-original-version (concat (7zr-sans-extension (file-name-nondirectory (buffer-file-name (current-buffer)))) (7zr-current-date-time) "." 7zr-buffer-filename-extension))
      (setq 7zr-original-version-win (concat (7zr-sans-extension (file-name-nondirectory (buffer-file-name (current-buffer)))) (7zr-current-date-time-win) "." 7zr-buffer-filename-extension))    

     (shell-command (concat "7z rn " 7zr-archive-name " " 
			     7zr-prepend-to-latest-revision 7zr-original-version " " 7zr-prepend-to-latest-revision 7zr-original-version-win " "
			     7zr-original-version  " " 7zr-original-version-win " "
			     7zr-prepend-to-hash-file 7zr-original-version  " " 7zr-prepend-to-hash-file 7zr-original-version-win))

      (shell-command (concat "7z e -aoa -o" 7zr-temp-directory " " 7zr-archive-name " " 7zr-prepend-to-hash-file 7zr-original-version-win))
      (find-file (concat 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-original-version-win))
      (goto-char (point-min))
      (when (re-search-forward "_[0-9][0-9]:[0-9][0-9]:[0-9][0-9]" nil t)
	(forward-word -1)
	(delete-char -1)
	(forward-word -1)
	(delete-char -1))
      (save-buffer)
      (kill-buffer)

      (shell-command (concat "7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on " 7zr-archive-name " " 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-original-version-win))	
      (7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-original-version-win))
      )
    )
  
  (message (concat "Took out colon delimiters in timestamp of filename in " 7zr-archive-name " for " 7zr-original-version))
  )
  


(defun 7zr-create-diff-against-latest-version ()
  "current becomes latest"
  (save-window-excursion
    (when (not (file-directory-p 7zr-temp-directory))
      (make-directory 7zr-temp-directory t))
  (let ((tmphashvalue "") (7zr-patch-number-string ""))
    (setq 7zr-patch-number (1+ 7zr-patch-number))
    (setq 7zr-patch-number-string (number-to-string 7zr-patch-number))
    (shell-command (concat "7z e -aoa -o" 7zr-temp-directory " " 7zr-archive-name " " 7zr-prepend-to-latest-revision 7zr-original-version))
    (shell-command (concat "7z e -aoa -o" 7zr-temp-directory " " 7zr-archive-name " " 7zr-prepend-to-hash-file 7zr-original-version))
    (copy-file 7zr-buffer-filename (concat 7zr-temp-directory 7zr-prepend-to-current-version 7zr-buffer-filename) t)
    (shell-command (concat 7zr-diff-command " "  7zr-temp-directory 7zr-prepend-to-latest-revision 7zr-original-version " " 7zr-temp-directory 7zr-prepend-to-current-version 7zr-buffer-filename " > " 7zr-temp-directory 7zr-patch-number-string))
    (when (/= (car (nthcdr 7 (file-attributes (concat 7zr-temp-directory 7zr-patch-number-string) 'string))) 0)  ; if changes
    
      (shell-command (concat "7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on " 7zr-archive-name " " 7zr-temp-directory 7zr-patch-number-string))

      (setq tmphashvalue (7zr-s-trim-whitespace-r (shell-command-to-string (concat 7zr-sha1sum-command " " 7zr-temp-directory 7zr-prepend-to-current-version 7zr-buffer-filename 7zr-sha1sum-post-command (7zr-awk-cmd-string 1)))))  
      (append-to-file (concat "(puthash \"" 7zr-patch-number-string   "\" \"" tmphashvalue "\" 7zr-hasht)\n") nil (concat 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-original-version))

      (shell-command (concat "7z u -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on " 7zr-archive-name " " 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-original-version))
    ) 
    (rename-file (concat 7zr-temp-directory 7zr-prepend-to-current-version 7zr-buffer-filename) (concat 7zr-temp-directory 7zr-prepend-to-latest-revision 7zr-original-version) t)
    (shell-command (concat "7z u -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on " 7zr-archive-name " " 7zr-temp-directory 7zr-prepend-to-latest-revision 7zr-original-version))

    (delete-file (concat 7zr-temp-directory 7zr-patch-number-string)) 
    (delete-file (concat 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-original-version))
    (delete-file (concat 7zr-temp-directory 7zr-prepend-to-latest-revision 7zr-original-version))
    (message (concat "Updated " 7zr-archive-name " with rev " 7zr-patch-number-string " of " 7zr-original-version))

    )
  )
)


(defun 7zr-append-archive ()
  "Patch to create the latest version then create a diff against that and append it to the archive.
   This func runs 7zr-examine-archive to find 7zr-revision-filename"
  (7zr-examine-archive)
  (7zr-create-diff-against-latest-version)
  (kill-buffer 7zr-revisions-examine-tmpbuffer)
)



(defun 7zr-commit ()
  "Call this function to save the latest changes to the archive.
This is called automatically with each save when 7z-revisions-mode
is invoked."
  (interactive)
  (setq 7zr-buffer-filename (buffer-name (current-buffer))) 
  (setq 7zr-buffer-filename-without-extension (7zr-sans-extension 7zr-buffer-filename)) 
  (setq 7zr-archive-name (concat 7zr-buffer-filename 7zr-archive-extension))  
  (if 
      (file-exists-p 7zr-archive-name)
      (7zr-append-archive)
    (7zr-create-archive)
    )
  )
  

(defun 7zr-after-save-func ()
  "Commit the current file."
  (7zr-commit)
)


;;;###autoload
(define-minor-mode 7z-revisions-mode
  "Automatically 7z any changes made when saving with this
mode turned on and optionally push them too."
  :lighter " 7zr"
  (if 7z-revisions-mode
      (add-hook 'after-save-hook '7zr-after-save-func t t)
    (remove-hook 'after-save-hook '7zr-after-save-func t)))

(provide '7z-revisions-mode)

(add-to-list 'minor-mode-alist '(7z-revisions-mode " 7zr"))

;;; 7z-revisions-mode.el ends   -------------------------------------------




