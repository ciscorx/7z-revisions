# 7z-revisions
This Emacs app is not really a version control system, but simply an incremental backup system that easily lets one browse and search past saves of a single file, keeping incremental diffs in a .7z archive, facilitating quick audits.   Useful when git is considered over-kill.<br/>
Compatible with windows and linux, and probably mac.

## Commentary:


 Wouldnt it be nice to be able to go back and double check to see
 exactly what you saved, when you saved it, in addition to perhaps
 what you may have accidentally deleted?  7z-revisions is now
 available, an indispensable #emacs #orgmode companion that does
 exactly that.  When in 7z-revisions-mode, the diff of the current
 buffer with respect to the last time it was saved, is saved to a
 7-zip archive of the same name, whenever a save-buffer command is
 issued, incrementally saving the latest revision by adding a new
 patch to the archive.  Optionally, the .7z extension can be altered
 to something else, such as .8z for example, by setting the global
 variable 7zr-archive-extension to ".8z".  Additionally, the function
 7z-revisions can be called interactively to view or consolidate past
 revisions in the archive, providing word by word differential
 highlighting.  In addition, syntax coloring is applied when viewing
 raw diff files.<br/>

 If your document anywhere contains a specific tag in the text, such
 as 7z-revisions.el_rev= followed by nothing or any number, then upon
 execution of the function 7zr-update-7z-revisions-tags-in-text, which
 is automatically called upon save if the
 7zr-auto-update-tags-in-text-p variable is set to t, the highest
 revision number, incremented by 1, will be inserted to the end of
 that tag, in this case at the end of that equals sign, replacing the
 previous number, if present.<br/>
 
 A tag of, for instance, 7zr-revisions.el_directory-of-archive=../ will
 specify the parent directory as the directory where the archive
 resides, etc.<br/> 
 For another example, 7z-revision.el_sha1-of-last-revision= causes the
 insertion of the sha1sum hash value of the last revision to be placed
 after the tag, which e.g. in a blockchain sort of way, establishes a
 forensically authentic journal.<br/>
 For your convenience, the tags can be inserted into the document or
 into the metadata file using the
 7zr-select-tag-to-insert-into-document function.  However, the tag which displays the current revision must be present in the document, or else none of the other tags will work.<br/>
 All tags must be present in the first 7777 characters of the document, or they will not work.<br/>
 
## Commands<br/>
### Some useful commands when editing your document:<br/>
**M-x** **7zr-line-last-changed-on** = displays the date-time and revision number of last time that the line at point has been modified (not the line number per se but the content at the given line number, which may have occupied a different line number in prior revisions because of lines deleted and/or removed above it, which is taken into account)<br/>
**M-x** **7zr-goto-line-of-last-revision** = jump to the line that was last changed in the current document, or more precisely, the line associated with the first hunk of the last changes<br/>
**M-x** **7zr-archive-edit-metadata-file**<br/>
**M-x** **7zr-archive-save-metadata-file**<br/>
**M-x** **7zr-select-tag-to-insert-into-document**<br/> 
**M-x** **7zr-update-7z-revisions-tag-in-text** = insert current revision number +1 at the end of a revision number tag, and other info after their respective tags.<br/>
**M-x** **7zr-create-file-for-archive-created-by-message** = put a metadata file into the archive, used for associating archive with 7z-revisions.el<br/>
**M-x** **7zr-revision-note-annotation**<br/>
**M-x** **7zr-modify-raw-hash-file**<br/>
**M-x** **7zr-modify-raw-notes-file**<br/>
**M-x** **7zr-rename-document-and-its-archive**<br/>
**M-x** **7z-revisions-mode** = updates the archive every time your document is saved, by automatically calling **M-x** **7zr-commit**<br/>
**M-x** **7z-revisions** = starts the 7z-revisions list buffer to view past revisions<br/>
     
### When **7z-revisions-mode** is active, the following two sequence key bindings take effect while editing your document:<br/>
 F2 F3 = 7z-revisions,<br/>
 F2 t = auto update tags,<br/>
 F2 3 = update tags,<br/>
 F2 CTRL-s = update tags & save buffer,<br/>
 F2 l = goto line last changed,<br/>
 F2 p = when was line of point last changed,<br/>
 F2 s = select tag to insert,<br/>
 F2 c = enter a revision note,<br/>
 F2 CTRL-c = view/edit raw notes file,<br/>
 F2 CTRL-r = rename document & archive,<br/>
 F2 CTRL-f = edit metafile (created-by-message file),<br/>
 F2 ` = exit 7z-revisions-mode.<br/>


### When **M-x** **7z-revisions** is started, the following key bindings take effect:<br/>
 Enter = view revision at point,<br/> 
 q = quit 7z-revisions,<br/>
 c = consolidate region,<br/>
 g = prompt for entering a date, or date-time, and then goto that date-time inputted,<br/>
 h = toggle highlight differences,<br/>
 u = view the diff file at point,<br/>
 a = view all selected diff files in one buffer,<br/>
 \# = prompt input of sha1 checksum and search for it<br/>
 c = edit revision note

### While viewing individual past revisions:<br/>
 q = quit to 7z-revisions buffer,<br/>
 n = next revision,<br/>
 p = previous revision,<br/> 
 u = view the diff file,<br/>
 g = Quit 7z-revisions and then try to goto the line in your document corresponding to the last line viewed from 7z-revisions.<br/>
 

### When highlight changes is enabled in view mode:<br/>
 d = jump to next difference/change,<br/> 
 e = jump to previous change

### While viewing a diff file:<br/>
 q = quit to 7z-revisions buffer,<br/>
 n = next diff file,<br/>
 p = previous diff file,<br/>
 r = switch to revision view,<br/>
 d = jump to next change hunk,<br/>
 e = jump to previous change hunk, <br/>
 g = Quit 7z-revisions and then try to goto the line in your document corresponding to the change hunk that was at point.<br/>
 

 There are also some functions in the menu which provide for
 consoldating the current days worth of changes, or last hour
 worth of changes, etc.

 Also, if you know part of a sha1hash value and dont know the
 revision, theres a menu option to goto the revision pertaining to the
 hash.

## Required features:<br/>
   hl-line+.el,<br/>
   p7zip,<br/>
   diffutils  ( just the diff command )

### On Windows
 When running on Microsoft Windows, access to additional dos commands becomes necessary, such as diff, awk, fciv, and optionally grep.<br/>
   Install diffutils for windows: http://gnuwin32.sourceforge.net/packages/diffutils.htm and then append C:\Program Files\GnuWin32\bin, or whatever directory that happens to contain diff.exe, to the path variable in control panel -> system -> Advanced -> Environment Variables.  Alternatively, you could just throw all the files in c:\windows\system32<br/>
   Download awk from http://gnuwin32.sourceforge.net/packages/gawk.htm<br/>
   Download the sha1sum equivalent, fciv, https://www.microsoft.com/en-us/download/confirmation.aspx?id=11533 and put it in the same directory.<br/>
   Download 7zip https://www.7-zip.org/download.html and then put 7z.exe and 7z.dll in windows/system32 directory, or any directory listed in the path environment variable.<br/>
   Download grep http://gnuwin32.sourceforge.net/packages/grep.htm (optional) and then follow the same instructions as above.

 
## Known Bugs:

 - File names must contain at least 1 alphabetical character or
 underscore or hyphen, and in this regard, cannot take the form of a
 real number, e.g. "1.0", and errors in such event.  (let's call this a feature for now)
 - Each archive can only track one file.  (let's call this a
 feature also)
 - Words added to beginning of line additionally highlight following
     word green. In some cases highlighting is off by 1 word.

  This program was written using emacs 23.2.1 on ubuntu 12.04, but is
    compatible with windows-xp and probably windows 7
