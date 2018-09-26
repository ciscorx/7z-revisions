;; 7z-revisions.el --- To save and review revisions using a .7z archive

;; author/maintainer: ciscorx@gmail.com                                                                    

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

  ;; Commentary:
  ;;
  ;; 7z-revisions-mode is an Emacs minor mode that saves the current
  ;; buffer to a 7-zip archive of the same name, whenever a
  ;; save-buffer command is issued.  A timestamp in the form of
  ;; MMDDYY-HHMMSS is appended to the archived file.  If the .7z
  ;; archive file already exists then it reconstructs the latest
  ;; revision from the diff patches in the archive, creates a diff
  ;; against that reconstruction and then appends it to the archive.
  ;; Additionally, the function 7z-revisions can be called
  ;; interactively to view or consolidate past revisions in the
  ;; archive.
  ;; 
  ;; When in 7z-revisions-mode, Return = view revision at point, q =
  ;; quit, c = consolidate region, g = goto date.  While viewing
  ;; individual past revisions, q = quit, n = next, p = previous.
  ;; When highlight changes is enabled then d = jump to next change, e
  ;; = jump to previous change
  ;;
  ;; There are also some functions in the menu which provide for
  ;; consoldating the current days worth of changes, or last hour
  ;; worth of changes, etc.
  ;;
  ;; Also, sha1sum hash values for each revision are saved in the
  ;; hashtable stored in the archive.

  ;; Required features:
  ;;   hl-line+.el
  ;;   p7zip
  ;;   diffutils  ( just the patch and diff commands )
  ;;
  ;; Bugs:
  ;;
  ;; - File names must contain at least 1 alphabetical character or
  ;; underscore or hyphen, and in this regard, cannot take the form of a
  ;; real number, e.g. "1.0".  
  ;; - Each archive can only track one file.  (let's call this a
  ;; feature)
  ;; - There's no way to add commit or revision notes.
  ;; - buffer local variables arent working properly enough to allow
  ;;     for two archives to be opened at once.  e.g. It appears that
  ;;     elisp has trouble with using a buffer local variable to store
  ;;     a vector; it only seems to store the first element.  However,
  ;;     elisp seems to have no problem with buffer local lists.  
  ;; 
  ;;  This program was written using emacs 23.3.1 on ubuntu 12.04.

;;; 7zr-summary-mode.el code ------------------------

(setq debug-on-error t)
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
    (define-key map (kbd "g") '7zr-summary-goto-date)
    (define-key map (kbd "c") '7zr-summary-consolidate-region)
;    (define-key map (kbd "r") '7zr-reconstruct-sl)
    (define-key map [menu-bar 7zr-summary]
      (cons "7zr-summary" (make-sparse-keymap "7zr-summary")))


    (define-key map [menu-bar 7zr-summary view-revision]
      '(menu-item "View Revision" 7zr-summary-view-revision
                  :help "View the Revision at Point"))
   (define-key map [menu-bar 7zr-summary goto-sha]
      '(menu-item "Goto sha1 hashvalue" 7zr-summary-goto-sha1
                  :help "Find revision pertaining to given sha1sum hash value."))
   (define-key map [menu-bar 7zr-summary goto-date]
      '(menu-item "Goto Date" 7zr-summary-goto-date
                  :help "Goto a particular date or nearest to at any rate."))
    (define-key map [menu-bar 7zr-summary sep] menu-bar-separator)
    (define-key map [menu-bar 7zr-summary consolidate_region]
      '(menu-item "Consolidate Region" 7zr-summary-consolidate-region
                  :help "Consolidate selected region"))
    (define-key map [menu-bar 7zr-summary consolidate_hour]
      '(menu-item "Consolidate Last Hour" 7zr-summary-consolidate-last-hour
                  :help "Consolidate last hours worth of revisions"))
    (define-key map [menu-bar 7zr-summary consolidate_today]
      '(menu-item "Consolidate Todays Changes" 7zr-summary-consolidate-today
                  :help "Consolidate todays revisions"))
    (define-key map [menu-bar 7zr-summary sep] menu-bar-separator)
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
  "Keymap for `7zr-summary-mode'.
Many other modes, such as `mail-mode', `outline-mode' and `indented-7zr-summary-mode',
inherit all the commands defined in this map.")



(define-derived-mode 7zr-summary-mode nil "7zr-sum"
  "Major mode for humans to read the summary of a 7z archive.
In this mode, hl-mode is active and one can select a revision
to view from a 7z archive.
\\{7zr-summary-mode-map}
Turning on 7z-summary mode runs the normal hook `7zr-summary-mode-hook'."
  (set (make-local-variable '7zr-summary-mode-variant) t)
 ; (hl-line-mode t)
)


(defalias 'indented-7zr-summary-mode '7zr-summary-mode)

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


(defun 7zr-view_jump_to_next_difference ()
  (interactive)

  (setq 7zr-view_jump_current_buffer (current-buffer))
 ; (setq diff_queue_position 7zr-diff-queue_position)
;  (setq diff_queue_position (buffer-local-value '7zr-diff-queue_position 7zr-view_jump_current_buffer))
  (setq diff_queue_length (buffer-local-value '7zr-diff-queue_length 7zr-view_jump_current_buffer))
  (setq 7zr-view_jump_diff_queue (buffer-local-value '7zr-diff-queue 7zr-view_jump_current_buffer))
  (setq 7zr-view_jump_diff_queuev (vconcat 7zr-view_jump_diff_queue))
    (when (and 
	   (<= (+ 7zr-diff-queue_position 1) diff_queue_length)
	   (> diff_queue_length 0))
      (incf 7zr-diff-queue_position)
;      (goto-char (aref (buffer-local-value '7zr-diff-queue 7zr-view_jump_current_buffer) (1- 7zr-diff-queue_position)))
;      (goto-char (aref 7zr-diff-queue (1- 7zr-diff-queue_position)))
      (goto-char (aref 7zr-view_jump_diff_queuev (1- 7zr-diff-queue_position)))
      )
    )
  


(defun 7zr-view_jump_to_previous_difference ()
  (interactive)
  
  (setq 7zr-view_jump_current_buffer (current-buffer))
 ; (setq diff_queue_position 7zr-diff-queue_position)
;  (setq diff_queue_position (buffer-local-value '7zr-diff-queue_position 7zr-view_jump_current_buffer))
  (setq diff_queue_length (buffer-local-value '7zr-diff-queue_length 7zr-view_jump_current_buffer))
  (setq 7zr-view_jump_diff_queue (buffer-local-value '7zr-diff-queue 7zr-view_jump_current_buffer))
  (setq 7zr-view_jump_diff_queuev (vconcat 7zr-view_jump_diff_queue))
    (when (and 
	   (> (- 7zr-diff-queue_position 1) 0)
	   (> diff_queue_length 0))
      (decf 7zr-diff-queue_position)
;      (goto-char (aref (buffer-local-value '7zr-diff-queue 7zr-view_jump_current_buffer) (1- 7zr-diff-queue_position)))
;      (goto-char (aref 7zr-diff-queue (1- 7zr-diff-queue_position)))
      (goto-char (aref 7zr-view_jump_diff_queuev (1- 7zr-diff-queue_position)))
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
))

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
))

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
))

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
))


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
  (let (inputted_sha1 inputted_timetuple sha1point begin end point_saved revision)
    (setq inputted_sha1 (read-string "Enter sha1sum hashvalue:"))

    (save-window-excursion
      (shell-command (concat "7z e -aoa -o" 7zr-temp-directory " " 7zr-archive-name " " 7zr-prepend-to-hash-file 7zr-current-original-version))
      (if (string= "" (setq revision (string-trim-final-newline (shell-command-to-string (concat "grep " 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-current-original-version " -e " inputted_sha1  " | awk '{print $2}'")))))
	  (message "Hashvalue not found in the hashtable")
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
	(when (not (re-search-forward revision nil t))
	  (message (concat "Revision " revision " not found!"))
	  (goto-char point_saved)
	  )
	(widen)

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

(defun 7zr-summary-consolidate-input ()
  "Promps user for region to consolidate and then runs 7zr-revision-summary-consolidate-region"
  (interactive "r")
  (if (> from to)
      (let ((tem to))
	(setq to from from tem)))
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char from)
      (while (not (eobp))
	(or (save-excursion (skip-chars-forward " \t") (eolp))
	    (center-line))
	(forward-line 1)))))



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
      ( (abort_function nil) from-patch to-patch froms-nearest-ancestor end-at-latest-version start-at-original-version 7zr-consolidate-line_from point-from-minus-first-line )
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
	     (yes-or-no-p (concat "Consolidate the highlighted patches, from " from-patch " to " to-patch " ? ")))
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
		  (shell-command (concat "touch -r " 7zr-temp-directory 7zr-prepend-to-latest-revision 7zr-original-version " " 7zr-temp-directory "7zr-datetime-reference"))
		  (7zr-rename-file-if-exists (concat 7zr-temp-directory 7zr-prepend-to-latest-revision 7zr-original-version) (concat 7zr-temp-directory "rev" to-patch "_of_" 7zr-original-version ))
		  
		  )
		  )
	  ; else it ends at to-patch
	    (7zr-reconstruct-slow to-patch to)
	    ) ; if

	  (save-window-excursion
	    (shell-command (concat 7zr-diff-command " "  7zr-temp-directory "rev" from-patch "_of_" 7zr-original-version " "  7zr-temp-directory "rev" to-patch "_of_" 7zr-original-version  " > " 7zr-temp-directory "tmpdiff"))
	    (7zr-rename-file-if-exists (concat 7zr-temp-directory "tmpdiff") (concat 7zr-temp-directory to-patch))
		      

	    (shell-command (concat "7z d " 7zr-archive-name " " (7zr-trim-r (7zr-trim-l 7zr-summary-consolidate-string))))
	    (shell-command (concat "touch -r " 7zr-temp-directory "7zr-datetime-reference " 7zr-temp-directory to-patch))
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



    
(defun 7zr-view-toggle-highlight-changes ()
  (interactive)
  (if 7zr-view_highlightp
      (setq 7zr-view_highlightp nil)
    (setq 7zr-view_highlightp t)
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
  (interactive)
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
  " Reconstruct and then view the revision pointed to by (point) in 
   the 7z-summary buffer and call 7zr-reconstruct-rev-from-patches, unless we are looking at top most or bottom most line, or one up from bottom most line, in which case we'll open the files from this function instead of calling 7z-reconstruct-rev-from-patches."
  (beginning-of-line)
  (cond ((looking-at 7zr-original-version)
	 (progn
	   (save-window-excursion
	     (setq 7zr-summary-last-line (string-to-number (format-mode-line "%l")))
	     (shell-command (concat "7z e -aoa -o" 7zr-temp-directory " " 7zr-archive-name " " 7zr-original-version))  
	     (setq 7zr-revisions_lastbuffer (current-buffer))
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
	   (setq 7zr-revisions_lastbuffer (current-buffer))
	  
	   (find-file-read-only (concat 7zr-temp-directory  "rev" (number-to-string 7zr-patch-number) "_of_" 7zr-original-version))
	   (setq 7zr-pointer-lastviewed (number-to-string 7zr-patch-number))
	 ;  (7zr-parse-standard-diff-type t)
;	   (7zr-view-local-set-keys)
	   (7zr-view-mode)
	  ;(7zr-summary-reconst-last-del)
	   (setq 7zr-summary-reconst-last (concat 7zr-temp-directory  "rev" (number-to-string 7zr-patch-number) "_of_" 7zr-original-version))







	   ))
	  
      ((looking-at (number-to-string 7zr-patch-number))
       (progn
	 (save-window-excursion
	   (setq 7zr-summary-last-line (string-to-number (format-mode-line "%l")))
	   (shell-command (concat "7z e -aoa -o" 7zr-temp-directory " " 7zr-archive-name " " 7zr-prepend-to-latest-revision 7zr-original-version))
	   (7zr-rename-file-if-exists (concat 7zr-temp-directory 7zr-prepend-to-latest-revision 7zr-original-version) (concat 7zr-temp-directory "rev" (number-to-string 7zr-patch-number) "_of_" 7zr-original-version) t)
	   (setq 7zr-revisions_lastbuffer (current-buffer))
	   )
	 (setq 7zr-pointer-lastviewed (number-to-string 7zr-patch-number))
	 (find-file-read-only (concat 7zr-temp-directory  "rev" (number-to-string 7zr-patch-number) "_of_" 7zr-original-version))
	 (when 7zr-view_highlightp	     
;	   (7zr-parse-standard-diff-type t)
	   (7zr-view-highlight-changes t nil)
	   )
	 (7zr-view-local-set-keys)
	   
	   ; (7zr-summary-reconst-last-del)
	 (setq 7zr-summary-reconst-last (concat 7zr-temp-directory  "rev" (number-to-string 7zr-patch-number) "_of_" 7zr-original-version))
;	  (7z-revisions-view-mode)	    
	 ))

      	((looking-at "\\([0-9]+\.?[0-9]*\\)")
	 (progn
	   (message (match-string-no-properties 0))
	   (setq 7zr-summary-rev-at-point (match-string-no-properties 1))
	   (7zr-reconstruct-rev-from-patches 7zr-summary-rev-at-point)
	   ))
	(t nil)
	)
  )

		    
(defun 7zr-view-local-set-keys ()
  " To inspect the change, type C-h b   ..or..  M-x describe-bindings "
  (interactive)
  (if (not (eql major-mode 'fundamental-mode))
      (progn
	(use-local-map (copy-keymap (current-local-map)))
	(local-set-key (kbd "p") '7zr-view_previous_page)
	(local-set-key (kbd "n") '7zr-view_next_page)
	(local-set-key (kbd "q") '7zr-view-quit)
	)
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

(defun 7zr-view-quit ()
(interactive)
  " kill current buffer "
; (7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-original-version))
; (7zr-delete-file-if-exists (concat 7zr-temp-directory "rev" 7zr-pointer-lastviewed "_of_" 7zr-original-version))
; (7zr-delete-file-if-exists (concat 7zr-temp-directory 7zr-pointer-lastviewed))
(kill-buffer)
)

(defun 7zr-view_next_page ()
  (interactive)
  (setq 7zr-view-last-pos (point))
  (kill-buffer)
  (switch-to-buffer 7zr-revisions_lastbuffer)
  (forward-line 1)
  (7zr-summary-view-revision) 
  (if (<= 7zr-view-last-pos (point-max))
      (goto-char 7zr-view-last-pos)
    )
  )

(defun 7zr-view_previous_page ()
  (interactive)
  (setq 7zr-view-last-pos (point))
  (kill-buffer)
  (switch-to-buffer 7zr-revisions_lastbuffer)
  (forward-line -1)
  (7zr-summary-view-revision) 
  (if (<= 7zr-view-last-pos (point-max))
      (goto-char 7zr-view-last-pos)
    )
  )



  

; (provide '7zr-summary-mode)

;;; 7zr-summary-mode.el ends here --------------------------------



(defvar 7zr-view-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") '7zr-view_jump_to_next_difference)   
    (define-key map (kbd "e") '7zr-view_jump_to_previous_difference)
    (define-key map (kbd "p") '7zr-view_previous_page)
    (define-key map (kbd "n") '7zr-view_next_page)
    (define-key map (kbd "q") '7zr-view-quit)
    (define-key map [menu-bar 7zr-view]
      (cons "7zr-view" (make-sparse-keymap "7zr-view")))
    (define-key map [menu-bar 7zr-view quit]
      '(menu-item "Quit View" 7zr-view-quit
                  :help "Quit viewing this revision"))
    (define-key map [menu-bar 7zr-view jump_next]
      '(menu-item "Jump to Difference" 7zr-view_jump_to_next_difference
                  :help "Jump to the next highlighted difference in current revision"))
    (define-key map [menu-bar 7zr-view jump_prev]
      '(menu-item "Jump to previous Difference" 7zr-view_jump_to_next_difference
                  :help "Jump to the previous highlighted difference in current revision"))
    (define-key map [menu-bar 7zr-view sep] menu-bar-separator)
    (define-key map [menu-bar 7zr-view next_page]
      '(menu-item "View Next Revision" 7zr-view_next_page
                  :help "View next revision in sequence."))
    (define-key map [menu-bar 7zr-view prev_page]
      '(menu-item "View Previous Revision" 7zr-view_previous_page
                  :help "View next revision in sequence."))
    map)
  "Keymap while 7zr-view-mode is active.")

;;;###autoload
(define-minor-mode 7zr-view-mode
  "A temporary minor mode to be activated only specific to a buffer."
  nil
  :lighter " 7zrv"
  7zr-view-mode-map)

(provide '7zr-view-mode)



;;;;;; 7z-revisions.el begins here ----------------------------------

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



; (add-hook 'after-change-major-mode-hook '7zr-toggle-highlight)
; (mouse-leave-buffer-hook)

;; BUFFER LOCAL VARIABLES
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

;; GLOBAL VARIABLES
(setq 7zr-diff-command "diff -Na")
(setq 7zr-patch-command "patch -p0")
(setq 7zr-temp-directory "/tmp/")
(setq 7zr-view_highlightp t)
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
(setq 7zr-temp-string-variable "")
(setq 7zr-temp-number-variable 0)
(setq 7zr-prepend-to-reconstruct_wip "7zr-prepend-to-reconstruct_wip")
(setq 7zr-prepend-to-latest-revision "7zr-latest-")
(setq 7zr-prepend-to-current-version "7zr-current-")   ; not used
(setq 7zr-prepend-to-hash-file "7zr-sha1-")
(setq 7zr-prepend-to-rej "7zr-rej-")
(setq 7zr-hash-of-hash-file "")
(setq 7zr-summary-reconst-last "")
(setq 7zr-archive-created-by-message "archive created by 7z-revisions.el")
; (defvar 7zr-hasht (make-hash-table :size 20000 :test 'equal))
(setq 7zr-summary-last-line 0)
(setq 7zr-patch-number 0.0)
(setq 7zr-revisions_tmpbuffer "7zr_revisions_tmpbuffer")
(setq 7zr-revisions-examine-tmpbuffer "7zr_revisions_examine_tmpbuffer")
(setq 7zr-revisions_tmpbuffer_lines 0)
(setq 7zr-revisions_original_buffer (current-buffer))
(setq 7zr-summary_discard_rejp nil)
(setq 7zr-map-difference_line_pos_last 1) 
(setq 7zr-map-differences_column_pos_last 0) 









(defun 7zr-examine-archive ()
  " find 7zr-original-version and create a buffer 7zr-revisions-tmp-buffer with names of patches, and discard any filenames that arent numbers per number-or-marker-p, also get 7zr-patch-number"
(setq 7zr-buffer (buffer-name (current-buffer)))
(setq 7zr-buffer-filename-without-extension (sans-extension 7zr-buffer))
(setq 7zr-buffer-filename (buffer-name (current-buffer)))
(setq 7zr-archive-name (concat 7zr-buffer-filename ".7z"))  
(setq 7zr-patch-number 0.0)
(setq 7zr-revisions_lastbuffer (current-buffer))
(setq 7zr-revisions-examine-tmpbuffer (generate-new-buffer-name (concat "7zr-examination_of_" 7zr-buffer-filename)))
(get-buffer-create 7zr-revisions-examine-tmpbuffer)
(set-buffer 7zr-revisions-examine-tmpbuffer)
(erase-buffer)


  (let ((ofile 7zr-archive-name)
        files file sum col timetuple savepoint reached-minuses patch highest-patch)
    (with-temp-buffer
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

        (error "Not a valid 7z archive file")
	)
      )
    (set-buffer 7zr-revisions-examine-tmpbuffer)
    (goto-char (point-min))
    (goto-char (point-max))
    (mark-whole-buffer)
 ;   (sort-numeric-fields)
    
    )
)

(defun sans-extension ( filenamestring )
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


;(let* ((#1=#:v 7zr-revisions_tmpbuffer3))  (with-current-buffer #1#    (set (make-local-variable '7zr-active-document) "hithere")))

(defun 7z-revisions ()
  " find 7zr-original-version and create a buffer 7zr-revisions-tmp-buffer with names of patches, and discard any filenames that arent numbers per number-or-marker-p, also get 7zr-patch-number"
  (interactive)
  (when (not (file-directory-p 7zr-temp-directory))
    (make-directory 7zr-temp-directory t))
  (setq 7zr-active-document_buffer (current-buffer))
  (setq 7zr-active-document (buffer-name 7zr-active-document_buffer))
  (setq 7zr-buffer (buffer-name (current-buffer)))


  (setq 7zr-archive-file-name-directory (file-name-directory (buffer-file-name (current-buffer))))
  (setq 7zr-revisions_original_buffer (current-buffer))
  (setq 7zr-buffer-filename-without-extension (sans-extension 7zr-buffer))
  (setq 7zr-buffer-filename-extension (7zr-what-is-extension-of-filename 7zr-buffer))
  (setq 7zr-buffer-filename 7zr-buffer)
  (setq 7zr-archive-name (concat 7zr-buffer-filename  ".7z"))  
  (setq 7zr-latest-revision_size "")
  (setq 7zr-latest-revision_datetime "")
 

  (if (not (file-exists-p 7zr-archive-name))
      (message (concat "Archive " 7zr-archive-name " has not yet been created yet, neither via 7z-revisions-mode nor by typing M-x 7zr-commit."))
    
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
	  (hl-line-mode 1)
	  (kill-buffer 7zr-revisions_with-temp-buffer)
	  (let* ((#1=#:v 7zr-revisions_tmpbuffer))
		      (with-current-buffer #1#
			(set (make-local-variable '7zr-active-document_original) 7zr-original-version)))
		  
	  )
      ; else fail
      (message (concat "There already exists an archive named " (buffer-name (current-buffer)) ".7z that is not a 7z-revisions.el archive!"))
      )
    )
  )            ; 7z-revisions

  


(defun 7zr-summary-sort-1 ()
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

(defun archive-7z-extract (archive name)
"DEFUNCT"
  (call-process "7z" nil t nil "x" archive name "-so")
  (goto-char (point-min))
  (re-search-forward "^Extracting  ")
  (forward-char (length name))
  (delete-region (point-min) (point))
  (goto-char (point-max))
  (forward-line -2)
  (delete-region (point) (point-max))
  t)


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
    (setq hash (string-trim-final-newline (shell-command-to-string (concat "sha1sum " 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-current-original-version " | awk '{print $1}'"))))
    (if (not (string= hash 7zr-hash-of-hash-file))  ; only load the hashtable if it has changed
	(progn
	  (setq lines (string-to-number (string-trim-final-newline (shell-command-to-string (concat "awk 'END { print NR }' " 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-current-original-version)))))
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
	    (setq rev-hash (string-trim-final-newline (shell-command-to-string (concat "sha1sum " 7zr-temp-directory "rev" rev-pointer "_of_" 7zr-original-version " | awk '{print $1}'"))))
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
  


(defun 7zr-delete-file-if-exists ( file )
" Delete a file if it exists, and return nil if it doesnt."
  (if (and
       (file-exists-p file )
       (not (string= file 7zr-temp-directory)))
      (delete-file file)
    nil
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
;    (7z-revisions-view-mode)
  (7zr-view-local-set-keys)
)


(defun 7zr-reconstruct-slow (rev rev-point)
"This function is only called from 7zr-summary-consolidate-region
and does the same thing as 7zr-reconstruct-rev-from-patches"
  (let ( pointer current-patch	(saved-point-pos (point)) hash-of-file	hash-from-table  )
    (setq 7zr-patch-command-called-x-times 0)
    (setq 7zr-construct-slow_last)
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
	  (setq 7zr-patch-command-called-x-times (incf 7zr-patch-command-called-x-times))  ;debug
	  (setq hash-from-table (gethash current-patch 7zr-hasht))
	  (setq hash-from-file (string-trim-final-newline (shell-command-to-string (concat "sha1sum " 7zr-temp-directory 7zr-prepend-to-reconstruct_wip " | awk '{print $1}'"))))
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


(defun 7zr-reconstruct-rev-from-patches (rev)
  "This function is called from 7zr-summary-view-revision, after running
 7z-revisions(), once a line item is selected and the enter key is
 pressed."
  (interactive)
  (let (rev_num (string-to-number rev))
    (set-buffer 7zr-revisions_tmpbuffer)  ; this is redundant
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


		(if (and
		     (> rev_num 7zr-pointer-lastviewed_num)
		     (< (- 7zr-summary-current-line 7zr-summary-last-line) (- 7zr-revisions_tmpbuffer_lines 7zr-summary-current-line))
	;	 (< (- rev_num 7zr-pointer-lastviewed_num) (- 7zr-revisions_tmpbuffer_lines rev_num))
		 )		 
		    (progn  
		      (setq 7zr-reconstruct-direction 1)
		      (setq 7zr-reconstruct-dtag " ")
		      
		      )
		  (if (and
		       (< rev_num 7zr-pointer-lastviewed_num)
;		   (< (- 7zr-pointer-lastviewed_num rev_num) rev_num)   
		       (< (- 7zr-summary-last-line 7zr-summary-current-line) 7zr-summary-current-line)   		   )
		      (progn   ; we will walk upwards
			(setq 7zr-reconstruct-direction -1)
			(setq 7zr-reconstruct-dtag " -R -t ")
		    			
			)	         
		    (setq 7zr-valid-rev-in-tempp nil)
		    )
		  )
		)
	
	    (setq 7zr-valid-rev-in-tempp nil)
	    )
	  )
	
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
	      )
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
       
;;   If we are walking upward we dont want to apply the patch of the one we're on but that of the nearest progeny
	    (shell-command (concat "7z e -aoa -o" 7zr-temp-directory " " 7zr-archive-name " " 7zr-reconstruct-patch-to-apply))
	    (shell-command (concat 7zr-patch-command 7zr-reconstruct-dtag 7zr-temp-directory 7zr-prepend-to-reconstruct_wip " < " 7zr-temp-directory 7zr-reconstruct-patch-to-apply))
	    (setq 7zr-patch-command-called-x-times (incf 7zr-patch-command-called-x-times))  ;debug

	
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
	  (setq 7zr-file-rev_hash (string-trim-final-newline (shell-command-to-string (concat "sha1sum " 7zr-temp-directory "rev" 7zr-pointer-lastviewed "_of_" 7zr-original-version " | awk '{print $1}'"))))
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
    (7zr-view-mode)
;    (7zr-view-local-set-keys)
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

   ; (use-local-map (copy-keymap (current-local-map)))
   ; (local-set-key (kbd "d") '7zr-view_jump_to_next_difference)   ; debug
   ; (local-set-key (kbd "e") '7zr-view_jump_to_previous_difference)
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


      (setq a-num_lines (- a-end a-begin)
	    b-num_lines (- b-end b-begin))


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
		(setf (aref m-array i) (make-vector *j* init_ele)))
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
		     (setf (aref m-array2 j) (make-vector *k* init_ele))
		     )		   
		   (setf (aref m-array i) m-array2)
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
      (setf (aref array i) j)
    (progn
      (setq asub2 (aref array i ))
      (setf (aref asub2 j ) x)
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
     (decf p1_suf)
     (decf p2_suf)
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
     (incf p1_pre)
     (incf p2_pre)
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
	    (incf p2)
	    )
	  (incf p1)
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
		   (decf common-subsequence-cnt)
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
	  (decf q1)
	 )    
	(while (and (< q1 q2) ancestorp)	    
	  (push (aref v2 (+ (1- q2) p2_pre)) differences)
	  (push (+ (1- q2) p2_pre) differences_wn)
	  (decf q2)
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
	   line_pos_current column_pos_current
	   )
      
      (mapc 
       (lambda (x) 
	 (goto-char (point-min))
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
	 
	 (setq line_pos_current (aref line_pos_v (car (cdr x))))
	 (setq column_pos_current (car (cdr (aref column_pos_v (car (cdr x))))))
	 (forward-line  (- line_pos_current 7zr-map-difference_line_pos_last))
	 (forward-char (- column_pos_current 7zr-map-differences_column_pos_last))
	 (setq end (point))
	 
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

    (setq 7zr-buffer-filename-extension (7zr-what-is-extension-of-filename 7zr-buffer-filename))
      
    (setq 7zr-original-version (concat (sans-extension (file-name-nondirectory (buffer-file-name (current-buffer)))) (7zr-current-date-time) "." 7zr-buffer-filename-extension))    
    (setq 7zr-current-original-version 7zr-original-version)
    (make-local-variable '7zr-current-original-version)
    (shell-command (concat "cp " 7zr-buffer-filename " " 7zr-temp-directory 7zr-original-version))
    (shell-command (concat "7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on " 7zr-archive-name " " 7zr-temp-directory 7zr-original-version))

    (setq tmphashvalue (string-trim-final-newline (shell-command-to-string (concat "sha1sum " 7zr-temp-directory 7zr-original-version " | awk '{print $1}'"))))


    (append-to-file (concat "(puthash \"" 7zr-original-version  "\" \"" tmphashvalue "\" 7zr-hasht)\n") nil (concat 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-original-version))
    (rename-file   (concat 7zr-temp-directory 7zr-original-version)  (concat 7zr-temp-directory 7zr-prepend-to-latest-revision 7zr-original-version) t)
    (shell-command (concat "7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on " 7zr-archive-name " " 7zr-temp-directory 7zr-prepend-to-latest-revision 7zr-original-version))
    (shell-command (concat "7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on " 7zr-archive-name " " 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-original-version))
    (shell-command (concat "touch " 7zr-temp-directory (shell-quote-argument 7zr-archive-created-by-message)))
    (shell-command (concat "7z a " 7zr-archive-name " " 7zr-temp-directory (shell-quote-argument 7zr-archive-created-by-message)))
  
    (delete-file (concat 7zr-temp-directory 7zr-prepend-to-latest-revision 7zr-original-version))
    (delete-file (concat 7zr-temp-directory 7zr-prepend-to-hash-file 7zr-original-version))
    (delete-file (concat 7zr-temp-directory 7zr-archive-created-by-message))
    )
  (message (concat "Created archive " 7zr-archive-name " for " 7zr-original-version))
  
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

    (copy-file 7zr-buffer-filename (concat 7zr-temp-directory 7zr-prepend-to-current-version 7zr-buffer-filename))
    (shell-command (concat 7zr-diff-command " "  7zr-temp-directory 7zr-prepend-to-latest-revision 7zr-original-version " " 7zr-temp-directory 7zr-prepend-to-current-version 7zr-buffer-filename " > " 7zr-temp-directory 7zr-patch-number-string))
    (when (/= (car (nthcdr 7 (file-attributes (concat 7zr-temp-directory 7zr-patch-number-string) 'string))) 0)  ; if changes
    
      (shell-command (concat "7z a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on " 7zr-archive-name " " 7zr-temp-directory 7zr-patch-number-string))

      (setq tmphashvalue (string-trim-final-newline (shell-command-to-string (concat "sha1sum " 7zr-temp-directory 7zr-prepend-to-current-version 7zr-buffer-filename " | awk '{print $1}'"))))  
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
   (setq 7zr-buffer-filename-without-extension (sans-extension 7zr-buffer-filename)) 
   (setq 7zr-archive-name (concat 7zr-buffer-filename ".7z"))  
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




