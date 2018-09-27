# 7z-revisions
Emacs app that saves and allows one to view revisions of a single file, via a .7z archive, useful when git is considered over-kill.
Also, provides word by word differential highlighting. 


 Commentary:

 7z-revisions-mode is an Emacs minor mode that saves the current
 buffer to a 7-zip archive of the same name, whenever a save-buffer
 command is issued.  A timestamp in the form of MMDDYY-HHMMSS is
 appended to the archived file.  If the .7z archive file already
 exists then incrementally saves the latest revision by adding a patch
 to the archive.  Additionally, the function 7z-revisions can be
 called interactively to view or consolidate past revisions in the
 archive.
 
 When 7z-revisions is called the following key bindings are enabled:
 Enter = view revision at point, 
 q = quit, 
 c = consolidate region, 
 g = goto date,  
 h = toggle highlight differences,
 
 While viewing individual past revisions:
 q = quit, 
 n = next, 
 p = previous,  

 When highlight changes is enabled in view mode:
 d = jump to next difference/change, 
 e = jump to previous change,

 There are also some functions in the menu which provide for
 consoldating the current days worth of changes, or last hour
 worth of changes, etc.

 Also, if you know part of a sha1hash value and dont know the
 revision, theres a menu option to goto the revision pertaining to the
 hash.

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
 - buffer local variables arent working properly enough to allow for
     two archives to be opened at once.  More precisely, it appears
     that elisp has trouble with using a buffer local variable to
     store a vector; it only seems to store the first element.
     However, elisp seems to have no problem with buffer local lists.
 
  This program was written using emacs 23.3.1 on ubuntu 12.04.
