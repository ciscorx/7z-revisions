# 7z-revisions
Emacs app that saves and reviews past revisions of a single file via a .7z archive, when git is over-kill.

 Commentary:

 7z-revisions-mode is an Emacs minor mode that saves the current
 buffer to a 7-zip archive of the same name, whenever a
 save-buffer command is issued.  A timestamp in the form of
 MMDDYY-HHMMSS is appended to the archived file.  If the .7z
 archive file already exists then it reconstructs the latest
 revision from the diff patches in the archive, creates a diff
 against that reconstruction and then appends it to the archive.
 Additionally, the function 7z-revisions can be called
 interactively to view or consolidate past revisions in the
 archive.
 
 When in 7z-revisions-mode, Return = view revision at point, q =
 quit, c = consolidate region, g = goto date.  While viewing
 individual past revisions, q = quit, n = next, p = previous.
 When highlight changes is enabled then d = jump to next change, e
 = jump to previous change

 There are also some functions in the menu which provide for
 consoldating the current days worth of changes, or last hour
 worth of changes, etc.

 Also, sha1sum hash values for each revision are saved in the
 hashtable stored in the archive.

 Required features:
   hl-line+.el
   p7zip
   diffutils  ( just the patch and diff commands )

 Bugs:

 - File names must contain at least 1 alphabetical character or
 underscore or hyphen, and in this regard, cannot take the form of a
 real number, e.g. "1.0".  
 - Each archive can only track one file.  (let's call this a
 feature)
 - There's no way to add commit or revision notes.
 - buffer local variables arent working properly enough to allow
     for two archives to be opened at once.  e.g. It appears that
     elisp has trouble with using a buffer local variable to store
     a vector; it only seems to store the first element.  However,
     elisp seems to have no problem with buffer local lists.  
 
  This program was written using emacs 23.3.1 on ubuntu 12.04.
